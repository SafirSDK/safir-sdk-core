/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#include <Safir/Dob/Internal/RequestInQueue.h>

#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/BlobOperations.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Internal/ScopeExit.h>
#include <boost/interprocess/sync/scoped_lock.hpp>


namespace Safir
{
namespace Dob
{
namespace Internal
{
    RequestInQueue::RequestInQueue(const size_t capacity):
        m_capacity(capacity),
        m_size(0),
        m_currentlyDispatchingRequest(NULL),
        m_noPushedRequests(0),
        m_noOverflows(0),
        m_noDispatchedRequests(0),
        m_noAttachedResponses(0),
        m_noDispatchedResponses(0),
        m_simulateFull(false)
    {

    }

    void RequestInQueue::RemoveCurrentReference()
    {
        m_currentlyDispatchingRequest = NULL;
    }

    void RequestInQueue::FinishDispatchRequests(Requests& toDispatch,
                                                Requests& dispatchedRequests,
                                                const size_t& numDispatched,
                                                bool& isNoLongerFull)
    {
        ScopedRequestInQueueLock lck(m_lock);

        if (m_size == m_capacity && numDispatched > 0)
        {
            isNoLongerFull = true;
        }

        m_noDispatchedRequests += static_cast<Typesystem::Int32>(numDispatched);

        if (!dispatchedRequests.empty())
        {
            m_dispatchedRequests.splice(m_dispatchedRequests.end(),dispatchedRequests);
        }

        if (!toDispatch.empty())
        {
            m_unhandledRequests.splice(m_unhandledRequests.begin(),toDispatch);
        }
    }

    void RequestInQueue::DispatchRequests(const DispatchRequestFunc & dispatchFunc,
                                          const ActionFunc & postFullAction)
    {
        //This is set to true in the guard if the queue has gone from a full to a not-full state.
        bool isNoLongerFull = false;

        {
            Requests toDispatch;
            Requests dispatchedRequests;
            size_t numDispatched = 0;

            //This will cause the FinishDispatchResponses method to be called when the scope exits
            //normally or if exitDispatch is used or there is an exception.
            ScopeExit spliceBackGuard(boost::bind(&RequestInQueue::FinishDispatchRequests,
                                                  this,
                                                  boost::ref(toDispatch),
                                                  boost::ref(dispatchedRequests),
                                                  boost::cref(numDispatched),
                                                  boost::ref(isNoLongerFull)));

            {
                ScopedRequestInQueueLock lck(m_lock);
                m_unhandledRequests.swap(toDispatch);
            }

            const size_t oldNumUnhandled = toDispatch.size();

            if (oldNumUnhandled == 0)
            {
                return;
            }

            bool exitDispatch;
            bool postpone;

            for (Requests::iterator it = toDispatch.begin();
                 it != toDispatch.end();) //note the missing ++it, due to iterator invalidation below
            {
                Requests::iterator current = it;

                //Guarantee that we set the m_currentlyDispatchingRequest variable to NULL
                //when we leave the scope
                ScopeExit zeroiseReferenceGuard(boost::bind(&RequestInQueue::RemoveCurrentReference,
                                                            this));
                m_currentlyDispatchingRequest = &current;
                ++it;

                dispatchFunc(*current,exitDispatch,postpone);

                if(m_currentlyDispatchingRequest == NULL)
                {
                    toDispatch.erase(current);
                    //AttachResponse has been called from within the callback, so we don't have to move
                    //the request anywhere (that was done in AttachResponse)
                }
                else if (postpone)
                {
                    //dont move the request anywhere.
                }
                else
                {
                    //Move the element to list of handled requests.
                    dispatchedRequests.splice(dispatchedRequests.end(),toDispatch,current);
                }

                if (exitDispatch)
                {
                    break;
                }
            }

            numDispatched = oldNumUnhandled - toDispatch.size();
        }

        //Execute the postaction if the queue was full.
        if (isNoLongerFull && postFullAction != NULL)
        {
            postFullAction();
        }
    }

    void RequestInQueue::AttachResponse(const ResponseId responseId, const ConnectionId & sender, const char * const blob, const AttachResponseChecker & checker)
    {
        ScopedRequestInQueueLock lck(m_lock);

        //it may be the response to the request we're currently dispatching
        if (m_currentlyDispatchingRequest != NULL && (*m_currentlyDispatchingRequest)->GetResponseId() == responseId)
        {
            DistributionData response
                (response_tag,
                sender,
                (*m_currentlyDispatchingRequest)->GetSenderId(),
                (*m_currentlyDispatchingRequest)->GetRequestId(),
                blob);

            //Move request and response to the handled requests list.
            m_handledRequests.push_back(std::make_pair(**m_currentlyDispatchingRequest,response));
            m_currentlyDispatchingRequest = NULL;
        }
        else
        {
            //find the request with the correct responseId
            Requests::iterator findIt;
            for (findIt = m_dispatchedRequests.begin(); findIt != m_dispatchedRequests.end(); ++findIt)
            {
                if (findIt->GetResponseId() == responseId)
                {
                    break;
                }
            }

            ENSURE(findIt != m_dispatchedRequests.end(),
                << "RequestInQueue::AttachResponse: There was no request with responseId "
                << responseId << " when trying to attach response of type "
                << Safir::Dob::Typesystem::Operations::GetName(Safir::Dob::Typesystem::BlobOperations::GetTypeId(blob)));

            DistributionData response(response_tag,sender,findIt->GetSenderId(),findIt->GetRequestId(),blob);

            //note that the checker function may throw an exception at this point.
            //(That is in fact its sole purpose)
            checker(*findIt,response);

            //Move request and response to the handled requests list.
            m_handledRequests.push_back(std::make_pair(*findIt,response));
            m_dispatchedRequests.erase(findIt);
        }
        ++m_noAttachedResponses;
    }

    bool RequestInQueue::PushRequest(DistributionData request)
    {
        ScopedRequestInQueueLock lck(m_lock);
        if ((m_size < m_capacity) && m_simulateFull == 0) //not full
        {
            request.SetResponseId(m_responseIdGenerator.GetNextResponseId());
            m_unhandledRequests.push_back(request);
            ++m_size;
            ++m_noPushedRequests;
            return true;
        }
        else
        {
            ++m_noOverflows;
            return false;
        }
    }

    void RequestInQueue::FinishDispatchResponses(RequestsAndResponses& queue, const size_t& numDispatched)
    {
        ScopedRequestInQueueLock lck(m_lock);

        m_size -= numDispatched;
        m_noDispatchedResponses += static_cast<Typesystem::Int32>(numDispatched);

        if (!queue.empty())
        {
            m_handledRequests.splice(m_handledRequests.begin(),queue);
        }
    }


    void RequestInQueue::DispatchResponses(const DispatchResponseFunc & dispatchFunc)
    {
        RequestsAndResponses toDispatch;
        size_t numDispatched = 0;

        //This will cause the FinishDispatchResponses method to be called when the scope exits
        //normally or if exitDispatch is used or there is an exception.
        ScopeExit spliceBackGuard(boost::bind(&RequestInQueue::FinishDispatchResponses,
                                              this,
                                              boost::ref(toDispatch),
                                              boost::cref(numDispatched)));

        {
            ScopedRequestInQueueLock lck(m_lock);
            m_handledRequests.swap(toDispatch);
        }

        const size_t oldSize = toDispatch.size();

        if (oldSize == 0)
        {
            return;
        }

        bool dontRemove;

        for (RequestsAndResponses::iterator it = toDispatch.begin();
             it != toDispatch.end();) //note the missing ++it, see below for explanation
        {
            dispatchFunc(it->second,dontRemove);

            //since list.erase returns an iterator to the next element we handle the
            //iterator incrementation here rather than in the for statement
            if (dontRemove)
            {
                ++it;
            }
            else
            {
                it = toDispatch.erase(it);
            }
        }

        numDispatched = oldSize - toDispatch.size();
    }

}
}
}
