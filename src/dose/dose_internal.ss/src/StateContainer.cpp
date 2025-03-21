/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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

#include <Safir/Dob/Internal/StateContainer.h>
#include <Safir/Dob/Internal/StateDeleter.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/interprocess/sync/scoped_lock.hpp>
#include <boost/interprocess/sync/sharable_lock.hpp>

using namespace std::placeholders;

namespace Safir
{
namespace Dob
{
namespace Internal
{
    StateContainer::StateContainer(const Typesystem::TypeId typeId)
        : m_typeId(typeId)
    {
    }


    StateContainer::~StateContainer()
    {
        lllout << "StateContainer DESTRUCTOR" << std::endl;
    }

    void StateContainer::Subscribe(const SubscriptionId&              subscriptionId,
                                   const Dob::Typesystem::Int64       key,
                                   const bool                         allKeys,
                                   bool                               restartSubscription,
                                   const SubscriptionOptionsPtr&      subscriptionOptions)
    {
        // Lock the meta subscriptions
        ScopedMetaSubLock lck(m_metaSubLock);

        // Check if a metasubscription for this subscritionId already exists.

        MetaSubscriptions::iterator subIt = m_metaSubscriptions.find(subscriptionId);

        if (subIt == m_metaSubscriptions.end())
        {
            // Have to create a new meta subscription

            MetaSubscription metaSub(subscriptionOptions);

            std::pair<MetaSubscriptions::iterator, bool> status;

            status = m_metaSubscriptions.insert(std::make_pair(subscriptionId, metaSub));

            ENSURE(status.second, << "StateContainer::Subscribe: Failed to insert a new meta subscription!" );

            subIt = status.first;

            subscriptionId.connectionConsumer.connection->AddSubscription(m_typeId, subscriptionId.subscriptionType);
        }

        // Set meta subscription state
        if (allKeys)
        {
            subIt->second.SubscribeAll();
        }
        else
        {
            subIt->second.Subscribe(key);
        }

        // Check all existing states that corresponds to this subscription (not the states that corresponds to the meta
        // subscription) and create subscription objects.
        // For an explanation regarding why we include released states here see the corresponding comment for ReadEntiy
        if (allKeys)
        {
            // Create a subscription for each existing state
            ForEachState
                ([this,&subscriptionId,restartSubscription,&options = subIt->second.GetSubscriptionOptions()]
                     (const auto /*key*/, const auto& stateSharedPtr, bool& /*exitDispatch*/)
                     {AddSubscription(stateSharedPtr,subscriptionId,restartSubscription,options);},
                 true);    // true => include released states
        }
        else
        {
            // Create a subscription for a single state
            ForSpecificState(key,
                             [this,&subscriptionId,restartSubscription,&options=subIt->second.GetSubscriptionOptions()]
                                 (const auto /*key*/, const auto& stateSharedPtr)
                                 {AddSubscription(stateSharedPtr, subscriptionId, restartSubscription, options);},
                             true); // true => include released states
        }
    }

    void StateContainer::Unsubscribe(const SubscriptionId&              subscriptionId,
                                     const Dob::Typesystem::Int64       key,
                                     const bool                         allKeys)
    {
        // Lock the meta subscriptions
        ScopedMetaSubLock lck(m_metaSubLock);

        MetaSubscriptions::iterator subIt = m_metaSubscriptions.find(subscriptionId);

        if (subIt == m_metaSubscriptions.end())
        {
            return;
        }

        UnsubscribeInternal(subIt, key, allKeys);
    }

    void StateContainer::UnsubscribeAll(const ConnectionPtr& connection)
    {
        // Lock the meta subscriptions
        ScopedMetaSubLock lck(m_metaSubLock);

        for (MetaSubscriptions::iterator subIt = m_metaSubscriptions.begin();
             subIt != m_metaSubscriptions.end();)
        {
            if (subIt->first.connectionConsumer.connection->Id() == connection->Id())
            {
                UnsubscribeInternal(subIt++,
                                    0,  // dummy
                                    true); // allKeys
            }
            else
            {
                ++subIt;
            }
        }
    }

    bool StateContainer::HasSubscription(const SubscriptionId&   subscriptionId) const
    {
        // Lock the meta subscriptions
        ScopedMetaSubLock lck(m_metaSubLock);

        return m_metaSubscriptions.find(subscriptionId) != m_metaSubscriptions.end();
    }

    void StateContainer::ForEachState(const ForEachStateActionFunc& actionFunc,
                                      const bool includeReleasedStates)
    {

        States::iterator it;

        StateSharedPtr statePtr = GetFirstExistingState(includeReleasedStates, it);

        while (statePtr != NULL)
        {
            {
                // lock state
                boost::interprocess::scoped_lock<State::StateLock> lck(statePtr->m_lock);

                bool savedReleaseStatus = statePtr->IsReleased();
                bool exitDispatch = false;

                // make callback
                actionFunc(it->first, statePtr, exitDispatch);

                if (savedReleaseStatus != statePtr->IsReleased())
                {
                    if (statePtr->IsReleased())
                    {
                        // First make the pointers that the state has to its subscriptions weak ones.
                        statePtr->ReleaseSubscribers();

                        // then make the pointer from this state container to the state a weak one
                        it->second.Downgrade();
                    }
                    else
                    {
                        ENSURE(false, << "Can't revive a released state using the ForEachState method.");
                    }
                }

                if (exitDispatch)
                {
                    break;
                }

            } // state released here

            statePtr = GetNextExistingState(includeReleasedStates, it);
        }
    }

    void StateContainer::ForSpecificState(const Dob::Typesystem::Int64 key,
                                          const ForSpecificStateActionFunc& actionFunc,
                                          const bool includeReleasedStates)
    {
        States::iterator it;

        StateSharedPtr statePtr = GetExistingState(key, includeReleasedStates, it);
        if (statePtr == NULL)
        {
            return;
        }

        // Lock state
        boost::interprocess::scoped_lock<State::StateLock> lck(statePtr->m_lock);

        bool savedReleaseStatus = statePtr->IsReleased();

        // make callback
        actionFunc(it->first, statePtr);

        if (savedReleaseStatus != statePtr->IsReleased())
        {
            // The state pointer in the container should be upgraded or downgraded
            if (statePtr->IsReleased())
            {
                // First make the pointers that the state has to its subscriptions weak ones.
                statePtr->ReleaseSubscribers();

                // then make the pointer from this state container to the state a weak one
                it->second.Downgrade();
            }
            else
            {
                ENSURE(false, << "Can't revive a released state using the ForEachState method.");
            }
        }
    }

    LockedStateResult StateContainer::GetSpecificState(const Dob::Typesystem::Int64 key, const bool includeReleasedStates)
    {
        States::iterator it;

        StateSharedPtr statePtr = GetExistingState(key,
                                                   includeReleasedStates,
                                                   it);
        if (statePtr == NULL)
        {
            // Return null pointer without a locked lock.
            return LockedStateResult(statePtr, SharedLock());
        }

        // Return pointer and the associated lock.
        return LockedStateResult(statePtr, SharedLock(statePtr->m_lock));
    }

    void StateContainer::ForSpecificStateAdd(const Dob::Typesystem::Int64 key,
                                             const ForSpecificStateActionFunc& actionFunc)
    {
        // Try to get an existing state or create a new one.
        States::iterator it;
        StateSharedPtr statePtr = GetState(key, it);

        // Lock state while in callback
        boost::interprocess::scoped_lock<State::StateLock> lck(statePtr->m_lock);

        bool savedReleaseStatus = statePtr->IsReleased();

        // make callback
        actionFunc(key, statePtr);

        if (statePtr->IsReleased())
        {
            statePtr->ReleaseSubscribers();
            it->second.Downgrade();
        }
        else if (savedReleaseStatus != statePtr->IsReleased())
        {
            it->second.Upgrade();
        }
    }

    void StateContainer::RemoveState(const Dob::Typesystem::Int64 key, const StateDeleter::pointer& pSharedMemoryObject)
    {
        // Get container writer lock
        ScopedStateContainerRwLock wlock(m_stateReaderWriterlock);

        States::iterator it = m_states.find(key);

        if (it != m_states.end() && it->second.IsDowngraded())
        {
            // New objects are created "released" and "Downgraded" for a short
            // while therefore we must compare pointers as well
            StateSharedPtr pState = it->second.GetIncludeWeak();
            if((pState == 0) || (pState.get() == pSharedMemoryObject))
            {
                m_states.erase(it);
            }
        }
    }

    StateSharedPtr StateContainer::GetState(const Dob::Typesystem::Int64 key, States::iterator& it)
    {
        // Get container reader lock
        SharableStateContainerRwLock rlock(m_stateReaderWriterlock);

        StateSharedPtr statePtr;
        it = m_states.find(key);

        bool addState = false;
        if (it != m_states.end())
        {
            // State found
            std::pair<StateSharedPtr, bool> UpgradeStatePtr = it->second.UpgradeAndGet();
            statePtr = UpgradeStatePtr.first;
            if (statePtr == NULL || statePtr->IsReleased() || UpgradeStatePtr.second)
            {
                // The state pointer is NULL (this can happen if the found pointer is a weak pointer that is NULL
                // but it hasn't yet been removed from the container) OR the state is released. In both cases
                // we must run the AddState method to get an fully elaborated state including
                // all subscribers.
                addState = true;
            }
        }
        else
        {
            addState = true;
        }

        if (addState)
        {
            rlock.unlock();  // Must release sharable lock here because an exclusive lock will be acquired
                             // by AddState.

            statePtr = AddState(key, it);
        }

        return statePtr;
    }

    StateSharedPtr StateContainer::AddState(const Dob::Typesystem::Int64 key, States::iterator& it)
    {
        // Get writer lock
        ScopedStateContainerRwLock wlock(m_stateReaderWriterlock);

        it = m_states.find(key);
        StateSharedPtr statePtr;

        if (it != m_states.end())
        {
            // State found
            statePtr = it->second.Get();

            if (statePtr == NULL)
            {
                // statePtr can be NULL even if a state was found in the container. This can happen if
                // the found pointer is a weak pointer that is NULL but it hasn't yet been removed from the
                // container (the oops case).
                m_states.erase(it);
                it = m_states.end();
            }
        }

        if (statePtr == NULL)
        {
            // State doesn't exist, create it.
            StateSharedPtr newStateSharedPtr(GetSharedMemory().construct<State>
                (boost::interprocess::anonymous_instance)(),
                 my_allocator<State>(),
                 StateDeleter(this, key));

            UpgradeableStatePtr upgradeableStatePtr(newStateSharedPtr);
            upgradeableStatePtr.Downgrade();  // A new state is 'released' so the pointer to the state from
                                              // the state container should be downgraded.

            std::pair<States::iterator, bool> insertRes =
                m_states.insert(std::make_pair(key, upgradeableStatePtr));

            ENSURE(insertRes.second, << "StateContainer::AddState: Failed to insert state in map!" );

            it = insertRes.first;

            statePtr = newStateSharedPtr;
        }

        // Lock the meta subscriptions
        ScopedMetaSubLock metaSubLock(m_metaSubLock);

        // Check each meta subscription and create subscriptions accordingly
        for(MetaSubscriptions::const_iterator metaIt = m_metaSubscriptions.begin(); metaIt != m_metaSubscriptions.end(); ++metaIt)
        {
            if (metaIt->second.IsSubscribed(key))
            {
                // Lock State
                boost::interprocess::scoped_lock<State::StateLock> stateLock(statePtr->m_lock);

                statePtr->AddSubscription(metaIt->first,
                                          false,                // don't mark as dirty
                                          false,
                                          metaIt->second.GetSubscriptionOptions(),
                                          statePtr);
            }
        }
        return statePtr;
    }

    void StateContainer::UnsubscribeInternal(const MetaSubscriptions::iterator&             subIt,
                                             const Dob::Typesystem::Int64                   key,
                                             const bool                                     allKeys)
    {
        // Set meta subscription state
        if (allKeys)
        {
            subIt->second.UnsubscribeAll();
        }
        else
        {
            subIt->second.Unsubscribe(key);
        }

        const SubscriptionId subscriptionId = subIt->first;

        if (!subIt->second.IsAnySubscribed())
        {
            // This consumer doesn't subscribe to anything, remove its metasubscription item.
            m_metaSubscriptions.erase(subIt);
        }

        // Check all existing states that corresponds to this subscription (not the states that corresponds to the meta
        // subscription) and remove subscription objects
        if (allKeys)
        {
            // Remove  a subscription for all handlers
            ForEachState([this, &subscriptionId]
                             (const auto /*key*/, const auto& stateSharedPtr, bool& /*exitDispatch*/)
                             {RemoveSubscription(stateSharedPtr,subscriptionId);},
                         true); // true => include released states. We must ensure that the subscribed flag in a subscription is set to
                                //         'not subscribed' also for released entities, otherwise a subscription in the dirty subscription
                                //         queue might get dispatched after an application has made an Unsubscribe.
        }
        else
        {
            // Remove a subscription for a single handler
            ForSpecificState(key,
                             [this,&subscriptionId](const auto /*key*/, const auto& stateSharedPtr)
                                 {RemoveSubscription(stateSharedPtr,subscriptionId);},
                             true); // true => include released states. We must ensure that the subscribed flag in a subscription is set to
                                    //         'not subscribed' also for released entities, otherwise a subscription in the dirty subscription
                                    //         queue might get dispatched after an application has made an Unsubscribe.
        }
    }

    void StateContainer::AddSubscription(const StateSharedPtr&          statePtr,
                                         const SubscriptionId&          subscriptionId,
                                         const bool                     restartSubscription,
                                         const SubscriptionOptionsPtr&  subscriptionOptions)
    {
        // First decide if released states should be discarded
        bool includeReleased = false;

        if (subscriptionId.subscriptionType == EntityRegistrationSubscription ||
            subscriptionId.subscriptionType == ServiceRegistrationSubscription)
        {
            //include released states for registration subscriptions, since dose_main needs them (see #726),
            //but dont include them for any other type of subscription, since they are not needed, and
            //the injectionsubscriptions get upset by them...
            includeReleased = true;
        }
        else if (subscriptionId.subscriptionType == EntitySubscription)
        {
            if (strstr(subscriptionId.connectionConsumer.connection->NameWithoutCounter(), "dose_pool_distribution") != NULL)
            {
                //include released states for entity subscriptions when doing pool distribution to
                // sync lamport clock on newly started node.
                includeReleased = true;
            }
        }

        if (statePtr->IsReleased() && !includeReleased)
        {
            return;
        }

        if (subscriptionId.subscriptionType == InjectionSubscription)
        {
            // Need to do some additional stuff when creating an injection subscription

            DistributionData realState = statePtr->GetRealState();
            DistributionData injectionState = statePtr->GetInjectionState();

            if ((!realState.IsNoState() && realState.GetHandlerId().GetRawValue() != subscriptionId.id) ||
                (!injectionState.IsNoState() && injectionState.GetHandlerId().GetRawValue() != subscriptionId.id))
            {
                // This state has another handlerId than the one setting up the injection subscription. Skip it!
                return;
            }

            // Save the instance so that we know when to send an OnInitialInjectionsDone when
            // dispatching subscriptions.
            Dob::Typesystem::InstanceId instanceId;
            if (!realState.IsNoState())
            {
                instanceId = realState.GetInstanceId();
            }
            else
            {
                ENSURE(!injectionState.IsNoState(), << "Found a state where both realState and injectionState are missing!");
                instanceId = injectionState.GetInstanceId();
            }
            subscriptionId.connectionConsumer.connection->AddInitialInjectionInstance(m_typeId,
                                                                                      Dob::Typesystem::HandlerId(subscriptionId.id),
                                                                                      instanceId);

        }

        statePtr->AddSubscription(subscriptionId,
                                  true,                 // mark new subscriptions as dirty  ...
                                  restartSubscription,  // ... and existing subscriptions if restart is true
                                  subscriptionOptions,
                                  statePtr);
    }

    void StateContainer::RemoveSubscription(const StateSharedPtr&   statePtr,
                                            const SubscriptionId&   subscriptionId)
    {
        statePtr->RemoveSubscription(subscriptionId);
    }

    void StateContainer::Iterator::Dereference(const char*& entityBlob, const char*& entityState) const
    {
        m_stateReferenceHolder.reset(m_entity.GetReference(),&DistributionData::DropReference);
        entityBlob = m_entity.GetBlob();
        entityState = m_stateReferenceHolder.get();
    }

    const StateContainer::Iterator
    StateContainer::CreateStateIterator(bool& end) const
    {
        end = false;
        Iterator iterator;

        iterator.m_state = GetFirstExistingState(true, iterator.m_underlyingIterator);
        if(iterator.m_state == NULL)
        {
            end = true;
            return Iterator();
        }
        else
        {
            // lock state
            boost::interprocess::scoped_lock<State::StateLock> lck(iterator.m_state->m_lock);
            if (!iterator.m_state->IsReleased())
            {
                iterator.m_entity = iterator.m_state->GetRealState();
                if (iterator.m_entity.IsCreated())
                {
                    return iterator;
                }
            }
        } // state lock released here

        // Try another state
        end = !IncrementIterator(iterator);
        return iterator;
    }

    bool
    StateContainer::IncrementIterator(Iterator& iterator) const
    {
        for(;;)
        {
            iterator.m_state = GetNextExistingState(true, iterator.m_underlyingIterator);
            if(iterator.m_state == NULL)
            {
                iterator = Iterator();
                return false;
            }
            else
            {
                // lock state
                boost::interprocess::scoped_lock<State::StateLock> lck(iterator.m_state->m_lock);
                if (!iterator.m_state->IsReleased())
                {
                    iterator.m_entity = iterator.m_state->GetRealState();
                    if (iterator.m_entity.IsCreated())
                    {
                        return true;
                    }
                }
            } // state lock released here
        }
    }

    const StateSharedPtr
    StateContainer::GetFirstExistingState(const bool includeReleasedStates, States::iterator& it) const
    {
        // Get container reader lock
        SharableStateContainerRwLock rlock(m_stateReaderWriterlock);

        StateSharedPtr statePtr;

        for (it = m_states.begin(); it != m_states.end(); ++it)
        {
            if (includeReleasedStates)
            {
                statePtr = it->second.GetIncludeWeak();
            }
            else
            {
                statePtr = it->second.Get();
            }

            if (statePtr != NULL)
            {
                break;
            }
        }
        return statePtr;
    }

    const StateSharedPtr
    StateContainer::GetNextExistingState(const bool includeReleasedStates, States::iterator& it) const
    {
        // Get container reader lock
        SharableStateContainerRwLock rlock(m_stateReaderWriterlock);

        StateSharedPtr statePtr;

        if (it == m_states.end())
        {
            return statePtr;
        }

        ++it;

        for (; it != m_states.end(); ++it)
        {
            if (includeReleasedStates)
            {
                statePtr = it->second.GetIncludeWeak();
            }
            else
            {
                statePtr = it->second.Get();
            }

            if (statePtr != NULL)
            {
                break;
            }
        }
        return statePtr;
    }

    const StateSharedPtr
    StateContainer::GetExistingState(const Dob::Typesystem::Int64    key,
                                     const bool                      includeReleasedStates,
                                     States::iterator&               it) const
    {
        // Get container reader lock
        SharableStateContainerRwLock rlock(m_stateReaderWriterlock);

        it = m_states.find(key);

        if (it == m_states.end())
        {
            return StateSharedPtr();
        }

        if (includeReleasedStates)
        {
            return it->second.GetIncludeWeak();
        }
        else
        {
            return it->second.Get();
        }
    }

    bool StateContainer::CanAcquireContainerWriterLock(const std::chrono::steady_clock::duration& lockTimeout)
    {
        // Get writer lock
        ScopedStateContainerRwLock wlock(m_stateReaderWriterlock,
                                         boost::interprocess::defer_lock);
        if (steady_try_lock_for(wlock,lockTimeout))
        {
            return true;
        }
        else
        {
            return false;
        }
    }
}
}
}
