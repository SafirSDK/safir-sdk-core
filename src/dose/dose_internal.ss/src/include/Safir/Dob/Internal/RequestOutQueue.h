/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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

#ifndef __DOSE_REQUEST_OUT_QUEUE_H__
#define __DOSE_REQUEST_OUT_QUEUE_H__

#include <boost/noncopyable.hpp>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/LeveledLock.h>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API RequestOutQueue:
        public SharedMemoryObject,
        private boost::noncopyable
    {
    public:
        explicit RequestOutQueue(const size_t capacity);


        typedef boost::function<void(const DistributionData & request,
                                     bool & handled)> DispatchRequestFunc;

        typedef boost::function<void(const DistributionData & response,
                                     const DistributionData & request,
                                     bool & exitDispatch)> DispatchResponseFunc;

        typedef boost::function<void(const DistributionData & request)> ForEachDispatchedRequestFunc;

        /**Checks if the queue is full. Will also return true if queue is set to simulate overflows. */
        bool full() const
        {
            ScopedRequestOutQueueLock lck(m_lock);
            return m_size == m_capacity || (m_simulateFull != 0);
        }

        /** Returns the number of elements currently in the queue
        * If simulateFull is set it will return capacity instead, so that it appears that the queue is full.
        */
        size_t size() const
        {
            ScopedRequestOutQueueLock lck(m_lock);
            return m_simulateFull != 0 ? m_capacity : m_size;
        }

        /** Get the capacity of the queue. */
        size_t capacity() const
        {
            ScopedRequestOutQueueLock lck(m_lock);
            return m_capacity;
        }

        /**
         * Add a request to the of the out queue.
         */
        bool PushRequest(const DistributionData & request);

        /** Dispatch the requests (used by dose_main) */
        void DispatchRequests(const DispatchRequestFunc & dispatchFunc);

        void DispatchResponses(const DispatchResponseFunc & dispatchFunc);

        void AttachResponse(const DistributionData& response);

        void RequestTimeout(const InternalRequestId & requestId);

        void ForEachDispatchedRequest(const ForEachDispatchedRequestFunc& foreachFunc) const;

        bool SimulateFull() const {return m_simulateFull != 0;}
        void SimulateFull(const bool simulateFull) {m_simulateFull = simulateFull?1:0;}

    private:
        //Locking Policy:
        //This class uses a non-recursive lock, since there should be no
        //recursive locking (all callbacks from within Dispatch* are
        //made with the lock unlocked).
        //Any attempts to take the lock recursively are to be regarded as
        //programming errors.
        typedef Safir::Dob::Internal::LeveledLock<boost::interprocess::interprocess_mutex,
                                                  REQUEST_OUT_QUEUE_LOCK_LEVEL,
                                                  NO_MASTER_LEVEL_REQUIRED> RequestOutQueueLock;
        mutable RequestOutQueueLock m_lock;
        typedef boost::interprocess::scoped_lock<RequestOutQueueLock> ScopedRequestOutQueueLock;

        size_t m_capacity;
        size_t m_size;

        typedef Containers<DistributionData>::list Requests;
        typedef std::pair<DistributionData,DistributionData> RequestsAndResponsesPair;
        typedef Containers<RequestsAndResponsesPair>::list RequestsAndResponses;

        void FinishDispatchRequests(Requests& toDispatch,
                                    Requests& dispatchedRequests,
                                    const size_t& numDispatched);
        void RemoveCurrentReference();
        void FinishDispatchResponses(RequestsAndResponses& queue,
                                     const size_t& numDispatched);

        Requests m_unhandledRequests;
        Requests m_dispatchedRequests;
        RequestsAndResponses m_handledRequests;

        Requests::iterator * m_currentlyDispatchingRequest;

        //statistics
        Typesystem::Int32 m_noTimeouts;

        Typesystem::Int32 m_noPushedRequests;  //number of requests successfully pushed onto an out-queue
        Typesystem::Int32 m_noOverflows; //number of overflows when trying to push
        Typesystem::Int32 m_noDispatchedRequests; //number of requests dispatched to dose_main
        Typesystem::Int32 m_noAttachedResponses; //number of responses sent attached by dose_main
        Typesystem::Int32 m_noDispatchedResponses; //number of responses dispatched to application

        AtomicUint32 m_simulateFull;

        friend void StatisticsCollector(RequestOutQueue&, void*);
    };
}
}
}

#endif

