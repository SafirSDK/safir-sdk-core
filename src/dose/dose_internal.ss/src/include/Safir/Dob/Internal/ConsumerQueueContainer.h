/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
*
* Created by: Anders Widén / stawi
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

#ifndef __DOSE_INTERNAL_CONSUMER_QUEUE_CONTAINER_H__
#define __DOSE_INTERNAL_CONSUMER_QUEUE_CONTAINER_H__

#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/LeveledLock.h>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>
#include <Safir/Dob/Internal/ConsumerId.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <boost/bind.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    // A word about the locking policy:
    // The intention is that an instance of this template class is a member of a connection object
    // wich means that adding, removal and traversing is performed by the same application thread and therefor
    // there is no real need for locks in the normal case. The locks exist merely to support traversing by
    // external debug/test applications (i.e Sate). We can accept the locking overhead because add/remove will
    // not be that frequent.
    template <class T>
    class ConsumerQueueContainer :
        public SharedMemoryObject
    {
    public:
        typedef typename SmartPointers<ConsumerQueueContainer<T> >::shared_ptr shared_ptr;
    private:
        class QueueDeleter :
            private my_deleter<T>
        {
        public:
            typedef typename my_deleter<T>::pointer pointer;

            QueueDeleter(const shared_ptr& _this,
                         const ConsumerId& consumer) : m_this(_this), m_consumer(consumer)
            {
                //std::wcout << "QueueDeleter constructor (this = " <<(void*)this <<")" << std::endl;
            }

            QueueDeleter(const QueueDeleter& other):
                m_this(other.m_this), m_consumer(other.m_consumer)
            {
                //std::wcout << "QueueDeleter copy constructor(this = " <<(void*)this <<")" << std::endl;
            }

            ~QueueDeleter()
            {
                //std::wcout << "~QueueDeleter destructor (m_this.use_count = " << m_this.use_count() <<", this = " <<(void*)this << ")" << std::endl;
                //explicitly drop the pointer, so that debugging becomes easier... (we've got a problem in this area, currently)
                m_this.reset();
            }

            void operator()(const pointer& p)
            {
                ENSURE(m_this != NULL, << "m_this was NULL when running the QueueDeleter::operator()");
                //std::wcout << "QueueDeleter operator() (this = " << (void*)this << ")"<<std::endl;
                // Remove the consumer entry ...
                m_this->RemoveQueue(m_consumer);

                // ... and then delete the queue itself.
                my_deleter<T>::operator()(p);

                m_this.reset(); //get rid of the pointer that we're not going to use any more!
            }

        private:

            QueueDeleter& operator=(const QueueDeleter&); //disallow assignment

            shared_ptr m_this;
            const ConsumerId m_consumer;
        };
    public:
        typedef typename boost::interprocess::shared_ptr
        <
            T,
            my_allocator<T>,
            QueueDeleter
        >
        QueuePtr;

        ConsumerQueueContainer()
        {

        }

        ~ConsumerQueueContainer()
        {
            //std::wcout << "ConsumerQueueContainer destructor" << std::endl;
            //explicitly clear the queue list, to simplify debugging.
            m_queues.clear();
        }

        //We need the _this parameter since enable_shared_from_this does not appear to work in the
        //current version of boost::interprocess.
        QueuePtr AddQueue(const shared_ptr& _this, const ConsumerId& consumer, const size_t capacity)
        {
            ScopedContainerLock lck(m_lock);

            typename Queues::const_iterator qIt = m_queues.find(consumer);

            if (qIt != m_queues.end())
            {
                // Queue already exists. Return a shared_ptr created from the stored weak_ptr
                QueuePtr qPtr = qIt->second.lock();
                if (qPtr != NULL)
                {
                    return qPtr;
                }
                //Oops! it's been removed. Pretend we didnt find it. And go on from here.
                m_queues.erase(qIt);
            }

            // Queue doesn't exist, create a queue and put the raw pointer to it in a shared pointer.
            QueuePtr queue(GetSharedMemory(). template construct<T>(boost::interprocess::anonymous_instance)(capacity),
                my_allocator<T>(),
                QueueDeleter(_this, consumer));

            // We actually store a weak pointer in the map, which means that destruction of the queue (when the last shared_ptr
            // goes out of scope) won't be blocked. The entry (containing the weak pointer) is erased by our own QueueDeleter which
            // is called when the last shared_ptr goes out of scope.
            QueueWeakPtr queueWeakPtr(queue);

            const bool success = m_queues.insert(std::make_pair(consumer, queueWeakPtr)).second;

            ENSURE(success, << "AddQueue: Failed to insert queue in map!" );

            return queue;
        }


        typedef boost::function<void(const ConsumerId& consumer, T& queue)> QueueFunc;

        void ForEach(const QueueFunc& queueFunc) const
        {

            TempQColl tmpQColl;

            {
                ScopedContainerLock lck(m_lock);// Hold lock while creating temporary copies of the shared_ptr:s

                for (typename Queues::const_iterator qIt = m_queues.begin(); qIt != m_queues.end(); ++qIt)
                {
                    QueuePtr qPtr = qIt->second.lock();
                    if (qPtr == NULL)
                    {
                        //Oops! It has been deleted. Skip it. It will be removed from this
                        //by the deleter as soon as we release the lock.
                    }
                    else
                    {
                        tmpQColl.push_back(TmpQData(qIt->first, qPtr));
                    }
                }
            } // lock released here

            for (typename TempQColl::iterator tmpQIt = tmpQColl.begin(); tmpQIt != tmpQColl.end(); ++tmpQIt)
            {
                queueFunc(tmpQIt->consumer, *tmpQIt->qPtr);
            }

        };

        void ForSpecific(const ConsumerId & consumer, const QueueFunc& queueFunc) const
        {
            QueuePtr tmp;

            {
                ScopedContainerLock lck(m_lock);// Hold lock while creating temporary copy of the shared_ptr
                typename Queues::const_iterator findIt = m_queues.find(consumer);
                tmp = findIt->second.lock();
            } // lock released here

            //check for "Oops!"-es. The weak pointer may have been removed!
            if (tmp != NULL)
            {
                queueFunc(consumer,*tmp);
            }
        };

    private:
        struct TmpQData
        {
            TmpQData(const ConsumerId& _consumer, const QueuePtr& _qPtr)
                : consumer(_consumer),
                  qPtr(_qPtr) {}
            
            TmpQData(const TmpQData& other)
                : consumer(other.consumer)
                , qPtr(other.qPtr)
            {

            }

            TmpQData& operator=(const TmpQData& other)
            {
                consumer=other.consumer;
                qPtr = other.qPtr;
                return *this;
            }

            ConsumerId consumer;
            QueuePtr  qPtr;
        };

        typedef std::vector<TmpQData> TempQColl;

        void RemoveQueue(const ConsumerId& consumer)
        {
            ScopedContainerLock lck(m_lock);
            m_queues.erase(consumer);
        }

        //Locking Policy:
        //This class uses a non-recursive lock.
        //There should be no recursive uses of this class' functions.
        //Any attempts to take the lock recursively are to be regarded as
        //programming errors.
        typedef Safir::Dob::Internal::LeveledLock<boost::interprocess::interprocess_mutex,
                                                  CONSUMER_QUEUE_CONTAINER_LOCK_LEVEL,
                                                  NO_MASTER_LEVEL_REQUIRED> ContainerLock;
        mutable ContainerLock m_lock;
        typedef boost::interprocess::scoped_lock<ContainerLock> ScopedContainerLock;

        typedef typename boost::interprocess::weak_ptr
        <
            T,
            my_allocator<T>,
            QueueDeleter
        >
        QueueWeakPtr;

        typedef typename PairContainers<ConsumerId, QueueWeakPtr>::map Queues;
        Queues m_queues;

    };
}
}
}

#endif
