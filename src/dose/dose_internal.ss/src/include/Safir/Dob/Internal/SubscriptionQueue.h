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

#ifndef __DOSE_SUBSCRIPTION_QUEUE_H__
#define __DOSE_SUBSCRIPTION_QUEUE_H__

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/Subscription.h>
#include <Safir/Dob/Internal/LeveledLock.h>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{

    /**
     * A queue for subscription ptrs.
     *
     */
    class DOSE_INTERNAL_API SubscriptionQueue:
        public SharedMemoryObject,
        private boost::noncopyable
    {
    public:

        // Implicitly generated Constructor and Destructor is ok.

        void push(const SubscriptionPtr& subscription);

        typedef boost::function<void(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove)> DispatchFunc;

        void Dispatch(const DispatchFunc& dispatchFunc);

        void clear(void);

    private:
        //Locking Policy:
        //This class uses a non-recursive lock, since there should be no
        //recursive locking (all callbacks from within Dispatch are
        //made with the lock unlocked).
        //Any attempts to take the lock recursively are to be regarded as
        //programming errors.
        typedef Safir::Dob::Internal::LeveledLock<boost::interprocess::interprocess_mutex,
                                                  SUBSCRIPTION_QUEUE_LOCK_LEVEL,
                                                  NO_MASTER_LEVEL_REQUIRED> SubscriptionQueueLock;
        mutable SubscriptionQueueLock m_lock;
        typedef boost::interprocess::scoped_lock<SubscriptionQueueLock> ScopedSubscriptionQueueLock;

        typedef Containers<SubscriptionPtr>::list Queue;

        void SpliceBack(Queue & queue);

        Queue m_queue;
    };

}
}
}
#endif
