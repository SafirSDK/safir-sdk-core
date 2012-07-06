/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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
#ifndef _dose_internal_handler_registration_h
#define _dose_internal_handler_registration_h

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/SubscriptionId.h>
#include <Safir/Dob/Internal/StateHolder.h>
#include <Safir/Dob/Internal/StateDeleter.h>
#include <Safir/Dob/Internal/RequestInQueue.h>
#include <Safir/Dob/Internal/ConsumerQueueContainer.h>
#include <Safir/Dob/Internal/SubscriptionOptions.h>
#include <Safir/Dob/Internal/LeveledLock.h>
#include <Safir/Dob/Internal/Atomic.h>
#include <boost/interprocess/sync/interprocess_mutex.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API State:
        public SharedMemoryObject
    {
    public:

        typedef ConsumerQueueContainer<RequestInQueue> RequestInQueueContainer;
        typedef RequestInQueueContainer::QueuePtr            RequestInQueuePtr;

        State();

        ~State();

        // markAsDirty: true => New subscriptions are marked as dirty, existing subscriptions are
        //                      marked as dirty only if markExistingAsDirty is true.
        //              false => no subscriptions are marked as dirty
        //
        void AddSubscription(const SubscriptionId&           subscriptionId,
                             const bool                      markAsDirty,
                             const bool                      markExistingAsDirty,
                             const SubscriptionOptionsPtr&   subscriptionOptions,
                             const StateSharedPtr&           sharedSelfPtr);

        void RemoveSubscription(const SubscriptionId&  subscriptionId);


        ConnectionPtr GetConnection() const;
        void SetConnection(const ConnectionPtr& connection);

        ConsumerId GetConsumer() const;
        void SetConsumer(const ConsumerId& consumer);

        RequestInQueuePtr GetOwnerRequestInQueue() const;
        void SetOwnerRequestInQueue(const RequestInQueuePtr& requestInQueue);
        void ResetOwnerRequestInQueue();

        DistributionData GetRealState() const;
        void SetRealState(const DistributionData& newRealState);

        DistributionData GetInjectionState() const;
        void SetInjectionState(const DistributionData& newInjectionState);

        bool IsReleased() const { return m_released != 0; }
        void SetReleased(bool released) { m_released = released ? 1 : 0; }

        // Make all subscription pointers held by this state weak.
        void ReleaseSubscribers();

    private:

        friend class StateContainer;

        // Locking Policy:
        // The state uses a non-recursive lock, since there should be no
        // recursive locking.
        // Any attempts to take the lock recursively are to be regarded as
        // programming errors.
        // In some scenarios a StateContainerRwLock is acquired while holding a StateLock and in other
        // scenarios a StateLock is acquired while holding a StateContainerRwLock. There are also
        // scenarios when an EntityState lock is acquired while holding a RegistrationState lock
        // (and vice versa). To avoid deadlocks a master lock at type level must be held in those cases.
        // The LeveledLock is therefor instantiated to enforce this restriction.
        typedef Safir::Dob::Internal::LeveledLock<boost::interprocess::interprocess_mutex,
                                                  STATE_LOCK_LEVEL,
                                                  TYPE_LOCK_LEVEL> StateLock;
        mutable StateLock m_lock;

        ConnectionPtr                           m_connection;
        ConsumerId                              m_consumer;
        RequestInQueuePtr                       m_ownerRequestInQueue;
        StateHolder                             m_realState;
        StateHolder                             m_injectionState;

        AtomicUint32 m_released;

        typedef PairContainers<SubscriptionId, UpgradeableSubscriptionPtr>::map Subscriptions;

        Subscriptions m_subscriptions;

        void KickSubscribers(const bool hasBeenDeleted);

        friend void StatisticsCollector(State&, void*);
    };
}
}
}
#endif

