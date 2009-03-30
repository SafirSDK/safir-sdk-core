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
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <boost/function.hpp>
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

        typedef boost::function<void(const Dob::Typesystem::Int64 key,
                                     const UpgradeableStateResult& statePtrResult)> ActionFunc;

        // Calls actionFunc for each existing state. Each state is locked during the callback.
        void ForEachState(const ActionFunc& actionFunc, const bool includeReleasedStates);

        // Calls actionFunc for the specific state. The state is locked during the callback.
        void ForSpecificState(const Dob::Typesystem::Int64 key, const ActionFunc& actionFunc, const bool includeReleasedStates);

        // Returns a pointer, along with a lock management object, to the specific state. The state is locked until
        // the lock management object is destructed.
        LockedStateResult GetSpecificState(const Dob::Typesystem::Int64 key, const bool includeReleasedStates);

        // Calls actionFunc for the specific state. The state is locked during the callback.
        // If the state doesn't already exist it is created. A downgraded state is revived.
        // Subscriptions is added to the state according to the corresponding meta subscription.
        // The subscriptions are not marked as dirty.
        void ForSpecificStateAdd(const Dob::Typesystem::Int64 key, const ActionFunc& actionFunc);

        typedef boost::function<void(const Dob::Typesystem::Int64 key,
                                     const UpgradeableStateResult& statePtrResult,
                                     bool& dontRelease,
                                     bool& exitDispatch)> ReleaseEachActionFunc;

        // Calls releaseActionFunc for each existing state. Each state is locked during the callback.
        void ReleaseEachState(const ReleaseEachActionFunc& releaseActionFunc);

        typedef boost::function<void(const Dob::Typesystem::Int64 key,
                                     const UpgradeableStateResult& statePtrResult,
                                     bool& dontRelease)> ReleaseSpecificActionFunc;

        // Calls releaseActionFunc for the specific state. The state is locked during the callback.
        void ReleaseSpecificState(const Dob::Typesystem::Int64 key, const ReleaseSpecificActionFunc& releaseActionFunc);

        // Calls releaseActionFunc for the specific state. The state is locked during the callback.
        // If the state doesn't already exist it is created. A downgraded state is revived.
        // Subscriptions is added to the state according to the corresponding meta subscription.
        // The subscriptions are not marked as dirty.
        void ForSpecificStateAddAndRelease(const Dob::Typesystem::Int64 key, const ReleaseSpecificActionFunc& releaseActionFunc);

        typedef boost::interprocess::offset_ptr<StateContainer> ThisPtr;
        static void RemoveState(ThisPtr _this, const Dob::Typesystem::Int64 key);

        /** Note that this class CANNOT be stored in shared memory, since the iterator
            member is different size in debug and release builds (on msvc++) */
        class Iterator
        {
        public:
            /** Construct an "end" iterator.*/
            Iterator():m_entity(no_state_tag) {}

            void Dereference(const char*& entityBlob, const char*& entityState) const;
            bool operator== (const Iterator& other) const
            {return m_entity==other.m_entity;}
        private:
            friend class StateContainer;
            //Iterator(const StateSharedPtr& state, /*const States::iterator& underlyingIterator, */const DistributionData& entity);

            StateSharedPtr m_state;
            mutable boost::shared_ptr<const char> m_stateReferenceHolder;
            States::iterator m_underlyingIterator;
            DistributionData m_entity;
        };

        const Iterator CreateStateIterator(bool& end) const;
        bool IncrementIterator(Iterator& iterator) const;

    private:

        // Locking Policy:
        // The meta subscription lock is non-recursive, since there should be no
        // recursive locking.
        // Any attempts to take the lock recursively are to be regarded as
        // programming errors.
        mutable boost::interprocess::interprocess_mutex         m_metaSubLock;

        // Locking Policy:
        // This lock is used to protect the state container. It is a readerWriter lock which allows
        // concurrent access to threads that just read the data but prevent concurrent access between
        // threads that read and threads that modify or between threads that modify. This kind of lock
        // is non-recursive so attempts to take the lock recursively are to be regarded as
        // programming errors.
        mutable boost::interprocess::interprocess_upgradable_mutex   m_stateReaderWriterlock;

        Dob::Typesystem::TypeId m_typeId;

        mutable States m_states;

        typedef PairContainers<SubscriptionId, MetaSubscription>::map MetaSubscriptions;

        MetaSubscriptions m_metaSubscriptions;

        typedef std::pair<UpgradeableStateResult, States::iterator> UpgradeableStateResultAndIter;
        typedef std::pair<StateSharedPtr, States::iterator> StateAndIter;

        // Get a state as a reader
        UpgradeableStateResultAndIter GetState(const Dob::Typesystem::Int64 key);

        // Add a state as a writer
        StateAndIter AddState(const Dob::Typesystem::Int64 key);

        void UnsubscribeInternal(const MetaSubscriptions::iterator&             subIt,
                                 const Dob::Typesystem::Int64                   key,
                                 const bool                                     allKeys);

        void NewStateAddSubscription(const std::pair<SubscriptionId, MetaSubscription>& sub,
                                     const Dob::Typesystem::Int64                       key,
                                     const StateSharedPtr&                              statePtr);


        void AddSubscription(const UpgradeableStateResult&       upgradeableStateResult,
                             const SubscriptionId&               subscriptionId,
                             const bool                          restartSubscription,
                             const SubscriptionOptionsPtr&       subscriptionOptions);

        void RemoveSubscription(const UpgradeableStateResult&       upgradeableStateResult,
                                const SubscriptionId&               subscriptionId);


        bool IncrementIteratorInternal(Iterator& iterator, std::vector<StateSharedPtr>& keepStates) const;

        // Note that the iterator returned from these methods is valid only as long as the returned
        // StateSharedPtr is kept.
        const UpgradeableStateResult GetFirstExistingState(const bool        includeReleasedStates,
                                                           States::iterator& it) const;
        const UpgradeableStateResult GetNextExistingState(const bool        includeReleasedStates,
                                                          States::iterator& it) const;
        const UpgradeableStateResult GetExistingState(const Dob::Typesystem::Int64    key,
                                                      const bool                      includeReleasedStates,
                                                      States::iterator&               it) const;

        friend void StatisticsCollector(StateContainer&, void*);
    };

    typedef boost::interprocess::offset_ptr<StateContainer> StateContainerPtr;
}
}
}

#endif
