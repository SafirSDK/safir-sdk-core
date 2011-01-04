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
#include <Safir/Dob/Internal/RequestOutQueue.h>

#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Internal/ScopeExit.h>
#include <boost/bind.hpp>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    RequestOutQueue::RequestOutQueue(const size_t capacity):
        m_capacity(capacity),
        m_size(0),
        m_currentlyDispatchingRequest(NULL),
        m_noTimeouts(0),
        m_noPushedRequests(0),
        m_noOverflows(0),
        m_noDispatchedRequests(0),
        m_noAttachedResponses(0),
        m_noDispatchedResponses(0),
        m_simulateFull(false)
    {

    }

    bool RequestOutQueue::PushRequest(const DistributionData & request)
    {
        ScopedRequestOutQueueLock lck(m_lock);
        if ((m_size < m_capacity) && m_simulateFull == 0) //not full
        {
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

    void RequestOutQueue::RemoveCurrentReference()
    {
        m_currentlyDispatchingRequest = NULL;
    }

    void RequestOutQueue::FinishDispatchRequests(Requests& toDispatch,
                                                 Requests& dispatchedRequests,
                                                 const size_t& numDispatched)
    {
        ScopedRequestOutQueueLock lck(m_lock);

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

    void RequestOutQueue::DispatchRequests(const DispatchRequestFunc & dispatchFunc)
    {
        Requests toDispatch;
        Requests dispatchedRequests;
        size_t numDispatched = 0;

        //This will cause the FinishDispatchResponses method to be called when the scope exits
        //normally or if exitDispatch is used or there is an exception.
        ScopeExit spliceBackGuard(boost::bind(&RequestOutQueue::FinishDispatchRequests,
                                              this,
                                              boost::ref(toDispatch),
                                              boost::ref(dispatchedRequests),
                                              boost::cref(numDispatched)));

        {
            ScopedRequestOutQueueLock lck(m_lock);
            m_unhandledRequests.swap(toDispatch);
        }

        const size_t oldNumUnhandled = toDispatch.size();

        if (oldNumUnhandled == 0)
        {
            return;
        }

        bool handled;
        for (Requests::iterator it = toDispatch.begin();
             it != toDispatch.end();) //note the missing ++it, we do that below
        {
            Requests::iterator current = it;

            //Guarantee that we set the m_currentlyDispatchingRequest variable to NULL
            //when we leave the scope
            ScopeExit zeroiseReferenceGuard(boost::bind(&RequestOutQueue::RemoveCurrentReference,
                                                        this));
            m_currentlyDispatchingRequest = &current;
            ++it;

            dispatchFunc(*current,handled);

            if(m_currentlyDispatchingRequest == NULL)
            {
                toDispatch.erase(current);
                //AttachResponse has been called from within the callback, so we don't have to move
                //the request anywhere (that was done in AttachResponse)
            }
            else if (handled)
            {
                //Move the element to list of handled requests.
                dispatchedRequests.splice(dispatchedRequests.end(),toDispatch,current);
            }
            else
            {
                //Leave the request in toDispatch, since it wasnt handled
            }
        }

        numDispatched = oldNumUnhandled - toDispatch.size();
    }

    void RequestOutQueue::FinishDispatchResponses(RequestsAndResponses& queue,
                                                  const size_t& numDispatched)
    {
        ScopedRequestOutQueueLock lck(m_lock);

        m_size -= numDispatched;
        m_noDispatchedResponses += static_cast<Typesystem::Int32>(numDispatched);

        if (!queue.empty())
        {
            m_handledRequests.splice(m_handledRequests.begin(),queue);
        }
    }

    void RequestOutQueue::DispatchResponses(const DispatchResponseFunc & dispatchFunc)
    {
        RequestsAndResponses toDispatch;
        size_t numDispatched = 0;

        //This will cause the FinishDispatchResponses method to be called when the scope exits
        //normally or if exitDispatch is used or there is an exception.
        ScopeExit spliceBackGuard(boost::bind(&RequestOutQueue::FinishDispatchResponses,
                                              this,
                                              boost::ref(toDispatch),
                                              boost::cref(numDispatched)));

        {
            ScopedRequestOutQueueLock lck(m_lock);
            m_handledRequests.swap(toDispatch);
        }

        const size_t oldSize = toDispatch.size();

        if (oldSize == 0)
        {
            return;
        }

        bool exitDispatch;

        for (RequestsAndResponses::iterator it = toDispatch.begin();
             it != toDispatch.end();) //note the missing ++it, see below for explanation
        {
            dispatchFunc(it->second, it->first, exitDispatch);

            //since list.erase returns an iterator to the next element we handle the
            //iterator incrementation here rather than in the for statement
            it = toDispatch.erase(it);

            if (exitDispatch)
            {
                break;
            }
        }

        numDispatched = oldSize - toDispatch.size();
    }

    void RequestOutQueue::AttachResponse(const DistributionData& response)
    {
        ScopedRequestOutQueueLock lck(m_lock);

        const InternalRequestId requestId = response.GetRequestId();

        //it may be the response to the request we're currently dispatching
        if (m_currentlyDispatchingRequest != NULL && (*m_currentlyDispatchingRequest)->GetRequestId() == requestId)
        {
            //Move request and response to the handled requests list.

            // workaround for VS2010            
            //m_handledRequests.push_back(std::make_pair(**m_currentlyDispatchingRequest,response));
            RequestsAndResponsesPair x1 =  std::make_pair(**m_currentlyDispatchingRequest,response);
            m_handledRequests.push_back(x1);

             m_currentlyDispatchingRequest = NULL;
        }
        else
        {
            //find the request with the correct requestId
            Requests::iterator findIt;
            for (findIt = m_dispatchedRequests.begin(); findIt != m_dispatchedRequests.end(); ++findIt)
            {
                if (findIt->GetRequestId() == requestId)
                {
                    break;
                }
            }

            if (findIt == m_dispatchedRequests.end())
            {
                lllout << "RequestOutQueue::AttachResponse: There was no request with requestId "
                       << requestId << " when trying to attach response of type "
                       << Safir::Dob::Typesystem::Operations::GetName(response.GetTypeId()) << std::endl
                       << ". Assuming that it timed out and was given an auto-response by dose_main." << std::endl;
                return;
            }
            //Move request and response to the handled requests list.            

            // workaround for VS2010            
            // m_handledRequests.push_back(std::make_pair(*findIt,response));
            RequestsAndResponsesPair x1 =  std::make_pair(*findIt,response);
            m_handledRequests.push_back(x1);

            m_dispatchedRequests.erase(findIt);
        }
        ++m_noAttachedResponses;
    }

    void RequestOutQueue::RequestTimeout(const InternalRequestId & requestId)
    {
        ScopedRequestOutQueueLock lck(m_lock);

        //find the request with the correct requestId

        //try the dispatched list first (most likely that it will be here)
        Requests::iterator findIt;
        for (findIt = m_dispatchedRequests.begin(); findIt != m_dispatchedRequests.end(); ++findIt)
        {
            if (findIt->GetRequestId() == requestId)
            {
                break;
            }
        }

        if (findIt == m_dispatchedRequests.end())
        {
            //try the list of unhandled requests
            for (findIt = m_unhandledRequests.begin(); findIt != m_unhandledRequests.end(); ++findIt)
            {
                if (findIt->GetRequestId() == requestId)
                {
                    //move the request to the list of dispatched requests, and set the iterator to point
                    //to that element.
                    m_dispatchedRequests.splice(m_dispatchedRequests.end(),m_unhandledRequests,findIt);
                    findIt = m_dispatchedRequests.end();
                    --findIt;
                    break;
                }
            }
        }

        if (findIt == m_unhandledRequests.end())
        {
            lllerr << "RequestInQueue::RequestTimeout: There was no request with requestId "
                << requestId << " to time out. Ignoring this call." << std::endl;
            return;
        }

        ++m_noTimeouts;
    }


    void RequestOutQueue::ForEachDispatchedRequest(const ForEachDispatchedRequestFunc& foreachFunc) const
    {
        std::vector<DistributionData> dispatchedRequests;
        //Lock the class and copy the requests, so we take the lock for as short a time as possible
        {
            ScopedRequestOutQueueLock lck(m_lock);
            std::copy(m_dispatchedRequests.begin(),m_dispatchedRequests.end(),std::back_inserter(dispatchedRequests));
        }
        std::for_each(dispatchedRequests.begin(),dispatchedRequests.end(),foreachFunc);
    }
}
}
}

