/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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

#ifndef __DOSE_INTERNAL_STATE_CONTAINER_H__
#define __DOSE_INTERNAL_STATE_CONTAINER_H__

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/State.h>
#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/MetaSubscription.h>
#include <Safir/Dob/Internal/SubscriptionId.h>
#include <Safir/Dob/Internal/SubscriptionOptions.h>
#include <Safir/Dob/Internal/LeveledLock.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include <boost/interprocess/sync/interprocess_upgradable_mutex.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API StateContainer :
        public SharedMemoryObject
    {
    private:
        typedef UpgradeablePtr
        <
            State,
            StateWeakPtr,
            StateSharedPtr
        >
        UpgradeableStatePtr;
        typedef PairContainers<Dob::Typesystem::Int64, UpgradeableStatePtr>::map States;
    public:

        explicit StateContainer(const Typesystem::TypeId typeId);

        ~StateContainer();

        void Subscribe(const SubscriptionId&              subscriptionId,
                       const Dob::Typesystem::Int64       key,
                       const bool                         allKeys,
                       bool                               restartSubscription,
                       const SubscriptionOptionsPtr&      subscriptionOptions);

        void Unsubscribe(const SubscriptionId&              subscriptionId,
                         const Dob::Typesystem::Int64       key,
                         const bool                         allKeys);

        void UnsubscribeAll(const ConnectionPtr& connection);

        // Returns true if the given subscription is active
        bool HasSubscription(const SubscriptionId&   subscriptionId) const;

        typedef boost::function<void(const Dob::Typesystem::Int64 key,
                                     const StateSharedPtr& stateSharedPtr,
                                     bool& exitDispatch)> ForEachStateActionFunc;

        // Calls actionFunc for each existing state. Each state is locked during the callback.
        // The state 'Released' flag will be checked before and after the callback and if it
        // has changed from 'not released' to 'released' the state subscriptions will be released and
        // the container pointer will be downgraded.
        void ForEachState(const ForEachStateActionFunc& actionFunc, const bool includeReleasedStates);

        typedef boost::function<void(const Dob::Typesystem::Int64 key,
                                     const StateSharedPtr& stateSharedPtr)> ForSpecificStateActionFunc;

        // Calls actionFunc for the specific state. The state is locked during the callback.
        // The state 'Released' flag will be checked before and after the callback and if it
        // has changed from 'not released' to 'released' the state subscriptions will be released and
        // the container pointer will be downgraded.
        void ForSpecificState(const Dob::Typesystem::Int64 key,
                              const ForSpecificStateActionFunc& actionFunc,
                              const bool includeReleasedStates);

        // Returns a pointer, along with a lock management object, to the specific state. The state is locked until
        // the lock management object is destructed.
        LockedStateResult GetSpecificState(const Dob::Typesystem::Int64 key, const bool includeReleasedStates);

        // Calls actionFunc for the specific state. The state is locked during the callback.
        // If the state doesn't already exist a released state is created.
        // Subscriptions are added to the state according to the corresponding meta subscription.
        // The subscriptions are not marked as dirty.
        void ForSpecificStateAdd(const Dob::Typesystem::Int64 key, const ForSpecificStateActionFunc& actionFunc);

        void RemoveState(const Dob::Typesystem::Int64 key, const StateDeleter::pointer& pSharedMemoryObject);

        /** Note that this class CANNOT be stored in shared memory, since the iterator
            member is different size in debug and release builds (on msvc++) */
        class Iterator
        {
        public:
            /** Construct an "end" iterator.*/
            Iterator():m_entity(no_state_tag) {}

            Iterator& operator=(const Iterator& other) {
                m_state = other.m_state;
                m_stateReferenceHolder = other.m_stateReferenceHolder;
                m_underlyingIterator = other.m_underlyingIterator;
                m_entity = other.m_entity;
                return *this;
            }

            void Dereference(const char*& entityBlob, const char*& entityState) const;
            bool operator== (const Iterator& other) const
            {return m_entity==other.m_entity;}
        private:
            friend class StateContainer;

            StateSharedPtr m_state;
            mutable boost::shared_ptr<const char> m_stateReferenceHolder;
            States::iterator m_underlyingIterator;
            DistributionData m_entity;
        };

        const Iterator CreateStateIterator(bool& end) const;
        bool IncrementIterator(Iterator& iterator) const;

        // This method just  tries to acquire the container lock as a writer.
        // It can be used when testing for locked locks left by applications
        // not terminating in a proper way.
        bool CanAcquireContainerWriterLock(const boost::chrono::steady_clock::duration& lockTimeout);

    private:

        // Locking Policy:
        // The meta subscription lock is non-recursive, since there should be no
        // recursive locking.
        // Any attempts to take the lock recursively are to be regarded as
        // programming errors.
        typedef Safir::Dob::Internal::LeveledLock<boost::interprocess::interprocess_mutex,
                                                  STATE_CONTAINER_META_SUB_LOCK_LEVEL,
                                                  TYPE_LOCK_LEVEL> MetaSubLock;
        mutable MetaSubLock m_metaSubLock;
        typedef boost::interprocess::scoped_lock<MetaSubLock> ScopedMetaSubLock;

        // Locking Policy:
        // This lock is used to protect the state container. It is a readerWriter lock which allows
        // concurrent access to threads that just read the data but prevent concurrent access between
        // threads that read and threads that modify or between threads that modify. This kind of lock
        // is non-recursive so attempts to take the lock recursively are to be regarded as
        // programming errors.
        // In some scenarios a StateLock is acquired while holding a StateContainerRwLock and in other
        // scenarios a StateContainerRwLock is acquired while holding a StateLock. To avoid deadlocks
        // a master lock at type level must be held in those cases. The LeveledLock is therefor
        // instantiated to enforce this restriction.
        typedef Safir::Dob::Internal::LeveledLock<boost::interprocess::interprocess_upgradable_mutex,
                                                  STATE_CONTAINER_RW_LOCK_LEVEL,
                                                  TYPE_LOCK_LEVEL> StateContainerRwLock;
        mutable StateContainerRwLock m_stateReaderWriterlock;
        typedef boost::interprocess::scoped_lock<StateContainerRwLock> ScopedStateContainerRwLock;
        typedef boost::interprocess::sharable_lock<StateContainerRwLock> SharableStateContainerRwLock;

        Dob::Typesystem::TypeId m_typeId;

        mutable States m_states;

        typedef PairContainers<SubscriptionId, MetaSubscription>::map MetaSubscriptions;

        MetaSubscriptions m_metaSubscriptions;

        /*
         * Get a state as a reader
         *
         * @Note that the iterator returned from these methods is valid only as long as the returned
         * StateSharedPtr is kept.
         */
        StateSharedPtr GetState(const Dob::Typesystem::Int64 key, States::iterator& it);

        /*
         * Add a state as a writer
         *
         * @Note that the iterator returned from these methods is valid only as long as the returned
         * StateSharedPtr is kept.
         */
        StateSharedPtr AddState(const Dob::Typesystem::Int64 key, States::iterator& it);

        void UnsubscribeInternal(const MetaSubscriptions::iterator&             subIt,
                                 const Dob::Typesystem::Int64                   key,
                                 const bool                                     allKeys);

        void AddSubscription(const StateSharedPtr&               statePtr,
                             const SubscriptionId&               subscriptionId,
                             const bool                          restartSubscription,
                             const SubscriptionOptionsPtr&       subscriptionOptions);

        void RemoveSubscription(const StateSharedPtr&               statePtr,
                                const SubscriptionId&               subscriptionId);


        bool IncrementIteratorInternal(Iterator& iterator, std::vector<StateSharedPtr>& keepStates) const;

        // Note that the iterator returned from these methods is valid only as long as the returned
        // StateSharedPtr is kept.
        const StateSharedPtr GetFirstExistingState(const bool        includeReleasedStates,
                                                   States::iterator& it) const;
        const StateSharedPtr GetNextExistingState(const bool        includeReleasedStates,
                                                  States::iterator& it) const;
        const StateSharedPtr GetExistingState(const Dob::Typesystem::Int64    key,
                                              const bool                      includeReleasedStates,
                                              States::iterator&               it) const;

        friend void StatisticsCollector(StateContainer&, void*);
    };

    typedef boost::interprocess::offset_ptr<StateContainer> StateContainerPtr;
}
}
}

#endif
