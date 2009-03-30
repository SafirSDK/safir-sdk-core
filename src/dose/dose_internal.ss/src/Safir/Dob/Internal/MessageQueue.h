/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#ifndef __DOSE_MESSAGE_QUEUE_H__
#define __DOSE_MESSAGE_QUEUE_H__

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>
#include <Safir/Dob/Internal/ConsumerQueueContainer.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{

    /**
     * A queue for DistributionData containing messages (of message_tag type).
     */
    class DOSE_INTERNAL_API MessageQueue:
        public SharedMemoryObject,
        private boost::noncopyable
    {
    public:
        //Constructor, creates a queue with a capacity for given number of elements.
        explicit MessageQueue(const size_t capacity);

        //Returns false if queue is full
        bool push(const DistributionData & msg);

        typedef boost::function<void(const DistributionData & msg, bool & exitDispatch, bool & dontRemove)> DispatchFunc;
        typedef boost::function<void (void)> ActionFunc;

        void Dispatch(const DispatchFunc & dispatchFunc, const ActionFunc & postFullAction);

        //Checks if the queue is empty
        bool empty() const
        {
            boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);
            return (m_size == 0) && !m_simulateFull;
        }

        /**Checks if the queue is full. Will also return true if queue is set to simulate overflows. */
        bool full() const
        {
            boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);
            return m_size == m_capacity || m_simulateFull;
        }

        /** Returns the number of elements currently in the queue
        * If simulateFull is set it will return capacity instead, so that it appears that the queue is full
        */
        size_t size() const
        {
            boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);
            return m_simulateFull ? m_capacity : m_size;
        }

        /** Get the capacity of the queue. */
        size_t capacity() const
        {
            boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);
            return m_capacity;
        }

        /**
         * Enlarge the size of a queue.
         * If newSize is smaller than the current size the call is ignored.
         */
        void resize(const size_t newCapacity)
        {
            boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);
            m_capacity = std::max(m_capacity,newCapacity);
        }

        Typesystem::Int32 NumberOfPushes() const {return m_noPushed;}
        Typesystem::Int32 NumberOfOverflows() const {return m_noOverflows;}

        bool SimulateFull() const {return m_simulateFull;}
        void SimulateFull(const bool simulateFull) {m_simulateFull = simulateFull;}

    private:
        //Locking Policy:
        //This class uses a non-recursive lock, since there should be no
        //recursive locking (all callbacks from within Dispatch are
        //made with the lock unlocked).
        //Any attempts to take the lock recursively are to be regarded as
        //programming errors.
        mutable boost::interprocess::interprocess_mutex m_lock;

        size_t m_capacity;
        size_t m_size;

        typedef Containers<DistributionData>::list QueueData;

        void FinishDispatch(QueueData& queue, const size_t& numDispatched, bool& isNoLongerFull);

        QueueData m_data;

        Typesystem::Int32 m_noPushed;
        Typesystem::Int32 m_noOverflows;
        volatile bool m_simulateFull;

        friend void StatisticsCollector(MessageQueue&, void*);
    };

    typedef  ConsumerQueueContainer<MessageQueue> MessageQueueContainer;
    typedef MessageQueueContainer::QueuePtr MessageQueuePtr;
}
}
}
#endif
