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

#ifndef __DOSE_REQUEST_IN_QUEUE_H__
#define __DOSE_REQUEST_IN_QUEUE_H__

#include <boost/noncopyable.hpp>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include <Safir/Dob/Internal/ConsumerQueueContainer.h>
#include <Safir/Dob/Internal/LeveledLock.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API RequestInQueue:
        public SharedMemoryObject,
        private boost::noncopyable
    {
    public:
        typedef boost::function<void(const DistributionData & request, bool & exitDispatch, bool & postpone)>
            DispatchRequestFunc;
        typedef boost::function<void (void)> ActionFunc;

        typedef boost::function<void(const DistributionData & response, bool & dontRemove)> DispatchResponseFunc;

        typedef boost::function<void(const DistributionData & request, const DistributionData & response)> AttachResponseChecker;

        /** Constructor */
        explicit RequestInQueue(const size_t capacity);

        void DispatchRequests(const DispatchRequestFunc & dispatchFunc, const ActionFunc & postFullAction);

        void AttachResponse(const ResponseId responseId, const ConnectionId & sender, const char * const blob, const AttachResponseChecker & checker);

        //return false on overflow (not const-ref since responseId needs to be set)
        bool PushRequest(DistributionData request);

        void DispatchResponses(const DispatchResponseFunc & dispatchFunc);

        /** Returns the number of elements currently in the queue
        * If simulateFull is set it will return capacity instead, so that it appears that the queue is full
        */
        size_t size() const
        {
            ScopedRequestInQueueLock lck(m_lock);
            return m_simulateFull != 0 ? m_capacity : m_size;
        }

        /** Get the capacity of the queue. */
        size_t capacity() const
        {
            ScopedRequestInQueueLock lck(m_lock);
            return m_capacity;
        }

        Typesystem::Int32 NumberOfDispatchedReq() const
        {
            ScopedRequestInQueueLock lck(m_lock);
            return m_noDispatchedRequests;
        }

        bool SimulateFull() const {return m_simulateFull != 0;}
        void SimulateFull(const bool simulateFull) {m_simulateFull = simulateFull?1:0;}
    private:

        class ResponseIdGenerator
        {
        public:
        //ResponseId handling
            ResponseId GetNextResponseId() {return m_nextResponseId++;}
        private:
            ResponseId m_nextResponseId;
        };

        ResponseIdGenerator m_responseIdGenerator;

        //Locking Policy:
        //This class uses a non-recursive lock, since there should be no
        //recursive locking (all callbacks from within Dispatch* are
        //made with the lock unlocked).
        //Any attempts to take the lock recursively are to be regarded as
        //programming errors.
        typedef Safir::Dob::Internal::LeveledLock<boost::interprocess::interprocess_mutex,
                                                  REQUEST_IN_QUEUE_LOCK_LEVEL,
                                                  NO_MASTER_LEVEL_REQUIRED> RequestInQueueLock;
        mutable RequestInQueueLock m_lock;
        typedef boost::interprocess::scoped_lock<RequestInQueueLock> ScopedRequestInQueueLock;

        size_t m_capacity;
        size_t m_size;

        typedef Containers<DistributionData>::list Requests;
        typedef std::pair<DistributionData,DistributionData> RequestsAndResponsesPair;
        typedef Containers<RequestsAndResponsesPair>::list RequestsAndResponses;

        void RemoveCurrentReference();
        void FinishDispatchRequests(Requests& toDispatch,
                                    Requests& dispatchedRequests,
                                    const size_t& numDispatched,
                                    bool& isNoLongerFull);

        void FinishDispatchResponses(RequestsAndResponses& queue, const size_t& numDispatched);

        Requests m_unhandledRequests;
        Requests m_dispatchedRequests;
        RequestsAndResponses m_handledRequests;

        Requests::iterator * m_currentlyDispatchingRequest;

        Typesystem::Int32 m_noPushedRequests;  //number of requests successfully pushed onto an in-queue
        Typesystem::Int32 m_noOverflows; //number of overflows when trying to push
        Typesystem::Int32 m_noDispatchedRequests; //number of requests dispatched to receiving application
        Typesystem::Int32 m_noAttachedResponses; //number of responses sent from receiving application
        Typesystem::Int32 m_noDispatchedResponses; //number of responses picked up by dose_main and sent on

        AtomicUint32 m_simulateFull;

        friend void StatisticsCollector(RequestInQueue&, void*);
    };

    typedef ConsumerQueueContainer<RequestInQueue> RequestInQueueContainer;
    typedef RequestInQueueContainer::QueuePtr RequestInQueuePtr;
}
}
}

#endif

