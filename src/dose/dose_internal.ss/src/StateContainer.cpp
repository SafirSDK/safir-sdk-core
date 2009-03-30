/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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
#include <boost/bind.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>
#include <boost/interprocess/sync/sharable_lock.hpp>

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
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_metaSubLock);

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

            subscriptionId.connectionConsumer.connection->AddSubscription(m_typeId);
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
        // subscription) and create subscription objects
        if (allKeys)
        {
            // Create a subscription for each existing state
            ForEachState(boost::bind(&StateContainer::AddSubscription,
                                     this,
                                     _2,
                                     boost::cref(subscriptionId),
                                     restartSubscription,
                                     boost::cref(subIt->second.GetSubscriptionOptions())),
                          false); // false => don't include released states
        }
        else
        {
            // Create a subscription for a single state
            ForSpecificState(key,
                             boost::bind(&StateContainer::AddSubscription,
                                         this,
                                         _2,
                                         boost::cref(subscriptionId),
                                         restartSubscription,
                                         boost::cref(subIt->second.GetSubscriptionOptions())),
                             false); // false => don't include released states
        }
    }

    void StateContainer::Unsubscribe(const SubscriptionId&              subscriptionId,
                                     const Dob::Typesystem::Int64       key,
                                     const bool                         allKeys)
    {
        // Lock the meta subscriptions
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_metaSubLock);

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
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_metaSubLock);

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

    void StateContainer::ForEachState(const ActionFunc& actionFunc, const bool includeReleasedStates)
    {
        States::iterator it;

        UpgradeableStateResult upgradeableStateResult = GetFirstExistingState(includeReleasedStates, it);

        while (upgradeableStateResult.first != NULL)
        {
            {
                // Lock state
                boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(upgradeableStateResult.first->m_lock);

                actionFunc(it->first, upgradeableStateResult);

            } // state released here

            upgradeableStateResult = GetNextExistingState(includeReleasedStates, it);
        }
    }

    void StateContainer::ForSpecificState(const Dob::Typesystem::Int64 key, const ActionFunc& actionFunc, const bool includeReleasedStates)
    {
        States::iterator it;

        UpgradeableStateResult upgradeableStateResult = GetExistingState(key,
                                                                         includeReleasedStates,
                                                                         it);
        if (upgradeableStateResult.first == NULL)
        {
            return;
        }

        // Lock state
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(upgradeableStateResult.first->m_lock);

        actionFunc(it->first, upgradeableStateResult);
    }

    LockedStateResult StateContainer::GetSpecificState(const Dob::Typesystem::Int64 key, const bool includeReleasedStates)
    {
        States::iterator it;

        UpgradeableStateResult upgradeableStateResult = GetExistingState(key,
                                                                         includeReleasedStates,
                                                                         it);
        if (upgradeableStateResult.first == NULL)
        {
            // Return null pointer without a locked lock.
            return std::make_pair(upgradeableStateResult, SharedLock());
        }

        // Return pointer and the associated lock.
        return std::make_pair(upgradeableStateResult, SharedLock(upgradeableStateResult.first->m_lock));
    }

    void StateContainer::ForSpecificStateAdd(const Dob::Typesystem::Int64 key,
                                             const ActionFunc& actionFunc)
    {
        // Try to get an existing state or create a new one.
        UpgradeableStateResultAndIter state = GetState(key);

        // Get a better name
        UpgradeableStateResult& upgradeableStateResult = state.first;

        {
            // Lock state while in callback
            boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(upgradeableStateResult.first->m_lock);
            actionFunc(key, upgradeableStateResult);
        }
    }

    void StateContainer::ReleaseEachState(const ReleaseEachActionFunc& releaseActionFunc)
    {
        States::iterator it;

        UpgradeableStateResult upgradeableStateResult = GetFirstExistingState(false, // false => don't include already released states
                                                                              it);

        StateSharedPtr statePtr = upgradeableStateResult.first;

        while (statePtr != NULL)
        {
            {
                // Lock state
                boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(statePtr->m_lock);

                bool dontRelease;
                bool exitDispatch;

                releaseActionFunc(it->first, upgradeableStateResult, dontRelease, exitDispatch);

                if (!dontRelease)
                {
                    // Ok, the state should be released. First make the pointers that the state
                    // has to its subscriptions weak ones.
                    statePtr->ReleaseSubscribers();

                    // then make the pointer from this state container to the state a weak one
                    it->second.Downgrade();
                }

                if (exitDispatch)
                {
                    return;
                }

            } // state released here

            upgradeableStateResult = GetNextExistingState(false, it);
            statePtr = upgradeableStateResult.first;

        }
    }

    void StateContainer::ReleaseSpecificState(const Dob::Typesystem::Int64 key,
                                              const ReleaseSpecificActionFunc& releaseActionFunc)
    {
        States::iterator it;

        UpgradeableStateResult upgradeableStateResult = GetExistingState(key,
                                                                         false, // false => don't include already released states
                                                                         it);

        // Get a better name
        StateSharedPtr& statePtr = upgradeableStateResult.first;

        // don't need to check upgradedFromWeak since we are not concerned with already released states

        if (statePtr == NULL)
        {
            return;
        }

        // Lock state
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(statePtr->m_lock);

        bool dontRelease;

        releaseActionFunc(key, upgradeableStateResult, dontRelease);

        if (!dontRelease)
        {
            // Ok, the state should be released. First make the pointers that the state
            // has to its subscriptions weak ones.
            statePtr->ReleaseSubscribers();

            // then make the pointer from this state container to the state a weak one
            it->second.Downgrade();
        }
    }

    void StateContainer::ForSpecificStateAddAndRelease(const Dob::Typesystem::Int64 key, const ReleaseSpecificActionFunc& releaseActionFunc)
    {
        // Try to get an existing state or create a new one.
        UpgradeableStateResultAndIter state = GetState(key);

        // Get better names
        UpgradeableStateResult& upgradeableStateResult = state.first;
        States::iterator& it = state.second;

        // Get a better name
        StateSharedPtr& statePtr = upgradeableStateResult.first;

        // Lock state
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(statePtr->m_lock);

        bool dontRelease;

        releaseActionFunc(key, upgradeableStateResult, dontRelease);

        if (!dontRelease)
        {
            // Ok, the state should be released. First make the pointers that the state
            // has to its subscriptions weak ones.
            statePtr->ReleaseSubscribers();

            // then make the pointer from this state container to the state a weak one
            it->second.Downgrade();
        }
    }

    void StateContainer::RemoveState(ThisPtr _this, const Dob::Typesystem::Int64 key)
    {
        // Get container writer lock
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_upgradable_mutex> wlock(_this->m_stateReaderWriterlock);

        States::iterator it = _this->m_states.find(key);

        if (it != _this->m_states.end() && it->second.IsDowngraded())
        {
            _this->m_states.erase(it);
        }
    }

    StateContainer::UpgradeableStateResultAndIter
    StateContainer::GetState(const Dob::Typesystem::Int64 key)
    {
        // Get container reader lock
        boost::interprocess::sharable_lock<boost::interprocess::interprocess_upgradable_mutex> rlock(m_stateReaderWriterlock);

        UpgradeableStateResult result;

        States::iterator it = m_states.find(key);

        if (it != m_states.end())
        {
            // State found
            result = it->second.UpgradeAndGet();

            if (result.first == NULL || result.second)
            {
                // The state pointer is NULL (this can happen if the found pointer is a weak pointer that is NULL
                // but it hasn't yet been removed from the container) OR the state pointer is obtained from a weak
                // pointer. In both cases we must run the AddState method to get an fully elaborated state including
                // all subscribers.
                rlock.unlock();  // Must release sharable lock here because an exclusive lock will be acquired
                                        // by AddState.

                StateAndIter addRes = AddState(key);
                result.first = addRes.first;
                it = addRes.second;
            }
        }
        else
        {
            // No state found in container, create a new one.

            rlock.unlock();  // Must release sharable lock here because an exclusive lock will be acquired
                                    // by AddState.

            StateAndIter addRes = AddState(key);
            result.first = addRes.first;
            it = addRes.second;

            result.second = false;  // false => not upgraded from weak
        }

        return std::make_pair(result, it);
    }

    StateContainer::StateAndIter
    StateContainer::AddState(const Dob::Typesystem::Int64 key)
    {
        // Get writer lock
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_upgradable_mutex> wlock(m_stateReaderWriterlock);

        States::iterator it = m_states.find(key);

        StateSharedPtr retVal;

        if (it != m_states.end())
        {
            // State found
            retVal = it->second.Get();

            if (retVal == NULL)
            {
                // retVal can be NULL even if a state was found in the container. This can happen if
                // the found pointer is a weak pointer that is NULL but it hasn't yet been removed from the
                // container (the oops case).
                m_states.erase(it);
                it = m_states.end();
            }
        }

        if (retVal == NULL)
        {
            // State doesn't exist, create it.
            StateSharedPtr stateSharedPtr(GetSharedMemory().construct<State>
                (boost::interprocess::anonymous_instance)(),
                 my_allocator<State>(),
                 StateDeleter(this, key));

            UpgradeableStatePtr state(stateSharedPtr);

            std::pair<States::iterator, bool> insertRes = m_states.insert(std::make_pair(key, state));

            ENSURE(insertRes.second, << "StateContainer::AddState: Failed to insert state in map!" );

            it = insertRes.first;

            retVal = state.Get();
        }

        // Check each meta subscription and create subscriptions accordingly
        std::for_each(m_metaSubscriptions.begin(),
                      m_metaSubscriptions.end(),
                      boost::bind(&StateContainer::NewStateAddSubscription,
                                  this,
                                  _1,
                                  key,
                                  boost::cref(retVal)));

        return std::make_pair(retVal, it);
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
            ForEachState(boost::bind(&StateContainer::RemoveSubscription,
                                     this,
                                     _2,
                                     boost::cref(subscriptionId)),
                         true); // true => include released states. We must ensure that the subscribed flag in a subscription is set to
                                //         'not subscribed' also for released entities, otherwise a subscription in the dirty subscription
                                //         queue might get dispatched after an application has made an Unsubscribe.
        }
        else
        {
            // Remove a subscription for a single handler
            ForSpecificState(key,
                             boost::bind(&StateContainer::RemoveSubscription,
                                         this,
                                         _2,
                                         boost::cref(subscriptionId)),
                             true); // true => include released states. We must ensure that the subscribed flag in a subscription is set to
                                    //         'not subscribed' also for released entities, otherwise a subscription in the dirty subscription
                                    //         queue might get dispatched after an application has made an Unsubscribe.
        }
    }

    void StateContainer::NewStateAddSubscription(const std::pair<SubscriptionId, MetaSubscription>&     sub,
                                                 const Dob::Typesystem::Int64                           key,
                                                 const StateSharedPtr&                                  statePtr)
    {
        if (sub.second.IsSubscribed(key))
        {
            boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(statePtr->m_lock);

            statePtr->AddSubscription(sub.first,
                                      false,                // don't mark as dirty
                                      false,
                                      sub.second.GetSubscriptionOptions(),
                                      statePtr);
        }
    }

    void StateContainer::AddSubscription(const UpgradeableStateResult&  upgradeableStateResult,
                                         const SubscriptionId&          subscriptionId,
                                         const bool                     restartSubscription,
                                         const SubscriptionOptionsPtr&  subscriptionOptions)
    {
        StateSharedPtr statePtr = upgradeableStateResult.first;

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

    void StateContainer::RemoveSubscription(const UpgradeableStateResult&       upgradeableStateResult,
                                            const SubscriptionId&   subscriptionId)
    {
        StateSharedPtr statePtr = upgradeableStateResult.first;

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

        std::vector<StateSharedPtr> keepStates;

        {
            // Get container reader lock
            boost::interprocess::sharable_lock<boost::interprocess::interprocess_upgradable_mutex> rlock(m_stateReaderWriterlock);

            iterator.m_underlyingIterator = m_states.begin();
            if (iterator.m_underlyingIterator == m_states.end())
            {
                end = true;
                return Iterator();
            }

            iterator.m_state = iterator.m_underlyingIterator->second.Get();
            if (iterator.m_state != NULL)
            {
                iterator.m_entity = iterator.m_state->GetRealState();
            }

            //if it is downgraded m_entity will still be null, so we get into IncrementIterator below.

            if (!iterator.m_entity.IsCreated())
            {
                end = !IncrementIteratorInternal(iterator,keepStates);
                if (end)
                {
                    return Iterator();
                }
            }
            return iterator;
        }

        //keepStates will be released here, after reader lock has been released
    }

    bool
    StateContainer::IncrementIterator(Iterator& iterator) const
    {
        std::vector<StateSharedPtr> keepStates;

        {
            // Get container reader lock
            boost::interprocess::sharable_lock<boost::interprocess::interprocess_upgradable_mutex> rlock(m_stateReaderWriterlock);

            return IncrementIteratorInternal(iterator, keepStates);
        }

        //keepStates will be released here, after reader lock has been released
    }

    bool
    StateContainer::IncrementIteratorInternal(Iterator& iterator,
                                              std::vector<StateSharedPtr>& keepStates) const
    {
        for(;;)
        {
            //keep a reference to the last state, so that it can be released when we do not
            //have a reader lock.
            keepStates.push_back(iterator.m_state);

            ++iterator.m_underlyingIterator;
            if (iterator.m_underlyingIterator == m_states.end())
            {
                iterator = Iterator();
                return false;
            }

            iterator.m_state = iterator.m_underlyingIterator->second.Get();
            if (iterator.m_state == NULL)
            {
                continue;
            }

            iterator.m_entity = iterator.m_state->GetRealState();

            if (iterator.m_entity.IsCreated())
            {
                return true;
            }
        }
    }

    const UpgradeableStateResult
    StateContainer::GetFirstExistingState(const bool includeReleasedStates, States::iterator& it) const
    {
        // Get container reader lock
        boost::interprocess::sharable_lock<boost::interprocess::interprocess_upgradable_mutex> rlock(m_stateReaderWriterlock);

        UpgradeableStateResult upgradeableStateResult = std::make_pair(StateSharedPtr(),
                                                                       false);    // false => not obtained from weak

        for (it = m_states.begin(); it != m_states.end(); ++it)
        {
            if (includeReleasedStates)
            {
                upgradeableStateResult = it->second.GetIncludeWeak();
            }
            else
            {
                upgradeableStateResult.first = it->second.Get();
            }

            if (upgradeableStateResult.first != NULL)
            {
                break;
            }
        }
        return upgradeableStateResult;
    }

    const UpgradeableStateResult
    StateContainer::GetNextExistingState(const bool includeReleasedStates, States::iterator& it) const
    {
        // Get container reader lock
        boost::interprocess::sharable_lock<boost::interprocess::interprocess_upgradable_mutex> rlock(m_stateReaderWriterlock);

        UpgradeableStateResult upgradeableStateResult = std::make_pair(StateSharedPtr(),
                                                                       false);    // false => not obtained from weak

        if (it == m_states.end())
        {
            return upgradeableStateResult;
        }

        ++it;

        for (; it != m_states.end(); ++it)
        {
            if (includeReleasedStates)
            {
                upgradeableStateResult = it->second.GetIncludeWeak();
            }
            else
            {
                upgradeableStateResult.first = it->second.Get();
            }

            if (upgradeableStateResult.first != NULL)
            {
                break;
            }
        }
        return upgradeableStateResult;
    }

    const UpgradeableStateResult
    StateContainer::GetExistingState(const Dob::Typesystem::Int64    key,
                                     const bool                      includeReleasedStates,
                                     States::iterator&               it) const
    {
        // Get container reader lock
        boost::interprocess::sharable_lock<boost::interprocess::interprocess_upgradable_mutex> rlock(m_stateReaderWriterlock);

        UpgradeableStateResult upgradeableStateResult = std::make_pair(StateSharedPtr(),
                                                                       false);    // false => not obtained from weak

        it = m_states.find(key);

        if (it != m_states.end())
        {
            if (includeReleasedStates)
            {
                upgradeableStateResult = it->second.GetIncludeWeak();
            }
            else
            {
                upgradeableStateResult.first = it->second.Get();
            }
        }
        return upgradeableStateResult;
    }
}
}
}

