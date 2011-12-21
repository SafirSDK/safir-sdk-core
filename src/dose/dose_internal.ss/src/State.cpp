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

#include <Safir/Dob/Internal/State.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    State::State()
        : m_released(1)
    {
    }


    State::~State()
    {
    }

    void State::AddSubscription(const SubscriptionId&           subscriptionId,
                                const bool                      markAsDirty,
                                const bool                      markExistingAsDirty,
                                const SubscriptionOptionsPtr&   subscriptionOptions,
                                const StateSharedPtr&           sharedSelfPtr)
    {
        bool createNewSubscription = false;

        Subscriptions::iterator subIt = m_subscriptions.find(subscriptionId);

        if (subIt == m_subscriptions.end())
        {
            createNewSubscription = true;
        }
        else
        {
            // The subscription already exists
            SubscriptionPtr subPtr = subIt->second.UpgradeAndGet().first;
            if (subPtr != NULL)
            {
                subPtr->SetSubscribed(true);
            }
            else
            {
                // Oops, we received a null pointer. This can happen in rare cases
                // when this metod is called when the weak ptr is null but the entry in the map
                // has not yet been deleted.

                m_subscriptions.erase(subIt);

                createNewSubscription = true;
            }
        }

        if (createNewSubscription)
        {
            UpgradeableSubscriptionPtr sub(GetSharedMemory().construct<Subscription>(boost::interprocess::anonymous_instance)
                                           (subscriptionId,
                                            subscriptionOptions,
                                            sharedSelfPtr));

            std::pair<Subscriptions::iterator, bool> status;

            status = m_subscriptions.insert(std::make_pair(subscriptionId, sub));

            ENSURE(status.second, << "State::AddSubscription: Failed to insert subscription in map!" );

            subIt = status.first;
        }

        if (!markAsDirty)
        {
            return;
        }

        if (createNewSubscription || markExistingAsDirty)
        {
            // Reset last state
            SubscriptionPtr subPtr = subIt->second.Get();
            subPtr->SetLastRealState(DistributionData(no_state_tag));
            subPtr->SetLastInjectionState(DistributionData(no_state_tag));

            // Add to the subscribers dirty queue(only added if not already dirty)
            Subscription::AddToDirtySubscriptionQueue(subPtr);
        }

    }

    void State::RemoveSubscription(const SubscriptionId&    subscriptionId)
    {
        Subscriptions::iterator subIt = m_subscriptions.find(subscriptionId);

        if (subIt == m_subscriptions.end())
        {
            return;
        }

        UpgradeableSubscriptionPtr::SharedPtr subPtr = subIt->second.UpgradeAndGet().first;

        if (subPtr != NULL)
        {
            // Set the subscription to unsubscribed so it is not dispatched when accessing the subscription via
            // any remaining pointer in an applications dirty list.
            subPtr->SetSubscribed(false);
        }

        // Erase pointer from state to subscription
        m_subscriptions.erase(subIt);
    }

    ConnectionPtr State::GetConnection() const
    {
        return m_connection;
    }

    void State::SetConnection(const ConnectionPtr& connection)
    {
        m_connection = connection;
    }

    ConsumerId State::GetConsumer() const
    {
        return m_consumer;
    }

    void State::SetConsumer(const ConsumerId& consumer)
    {
        m_consumer = consumer;
    }

    State::RequestInQueuePtr State::GetOwnerRequestInQueue() const
    {
       return m_ownerRequestInQueue;
    }

    void State::SetOwnerRequestInQueue(const RequestInQueuePtr& requestInQueue)
    {
        m_ownerRequestInQueue = requestInQueue;
    }

    void State::ResetOwnerRequestInQueue()
    {
        m_ownerRequestInQueue.reset();
    }

    DistributionData State::GetRealState() const
    {
        return m_realState.GetState();
    }

    void State::SetRealState(const DistributionData& newRealState)
    {
        bool hasBeenDeleted = false;

        // An unregistration shall be indicated as 'hasBeenDeleted' to subscribers so that an unregister
        // followed by a register can be detected by registration susbcribers. For entity subscriptions
        // we have no requirements to detect intermediate deletes.
        if (newRealState.GetType() == DistributionData::RegistrationState)
        {
            DistributionData currRealState = m_realState.GetState();

            if (!currRealState.IsNoState() && currRealState.IsRegistered())
            {
                // Changing the registration state when the current state is Registered
                // shall always set the flag.
                // Reg->Unreg => Normal unregistration.
                // Reg->Reg => Overregistration.
                hasBeenDeleted = true;
            }
        }

        m_realState.SetState(newRealState);

        KickSubscribers(hasBeenDeleted);
    }

    DistributionData State::GetInjectionState() const
    {
        return m_injectionState.GetState();
    }

    void State::SetInjectionState(const DistributionData& newInjectionState)
    {
        m_injectionState.SetState(newInjectionState);

        KickSubscribers(false);
    }

    void State::ReleaseSubscribers()
    {
        for (Subscriptions::iterator subIt = m_subscriptions.begin();
             subIt != m_subscriptions.end();
             ++subIt)
        {
            subIt->second.Downgrade();  // Make the pointer to the subscription weak.
        }
    }
    
    void State::KickSubscribers(const bool hasBeenDeleted)
    {
        for (Subscriptions::iterator subIt = m_subscriptions.begin();
             subIt != m_subscriptions.end();
             ++subIt)
        {
            UpgradeableSubscriptionPtr::SharedPtr subPtr = subIt->second.UpgradeAndGet().first;

            if (subPtr == NULL)
            {
                // Oops, the weak ptr is not pointing to anything. This can happen in rare cases
                // when this metod is called when the weak ptr is null but the entry in the map
                // has not yet been deleted.
                continue;
            }

            if (hasBeenDeleted)
            {
                subPtr->HasBeenDeletedFlag().Set();
            }

            // Add to the subscribers dirty queue(only added if not already dirty)
            Subscription::AddToDirtySubscriptionQueue(subPtr);
        }
    }
}
}
}

