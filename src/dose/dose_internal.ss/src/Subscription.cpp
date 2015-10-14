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

#include <Safir/Dob/Internal/Subscription.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    void AddDirty(const UpgradeableSubscriptionPtr::SharedPtr& subPtr)
    {
        const ConnectionPtr& connection = subPtr->GetSubscriptionId().connectionConsumer.connection;

        connection->GetDirtySubscriptionQueue().push(subPtr);

        // Kick the subscriber application
        connection->SignalIn();
    }

    Subscription::Subscription(const SubscriptionId&          subsciptionId,
                               const SubscriptionOptionsPtr&  subscriptionOptions,
                               const StateSharedPtr&          state)
            : m_subscriptionId(subsciptionId),
              m_state(state),
              m_lastRealState(no_state_tag),
              m_lastInjectionState(no_state_tag),
              m_isSubscribed(true),
              m_isDirty(false),
              m_stateHasBeenDeleted(false),
              m_subscriptionOptions(subscriptionOptions)
    {
    }

    const SubscriptionId& Subscription::GetSubscriptionId() const
    {
        return m_subscriptionId;
    }

    const StateSharedPtr Subscription::GetState() const
    {
        return m_state;
    }

    const DistributionData Subscription::GetCurrentRealState() const
    {
        return m_state->GetRealState();
    }

    const DistributionData Subscription::GetLastRealState() const
    {
        return m_lastRealState;
    }

    void Subscription::SetLastRealState(const DistributionData& state)
    {
        m_lastRealState = state;
    }

    const DistributionData Subscription::GetCurrentInjectionState() const
    {
        return m_state->GetInjectionState();
    }

    const DistributionData Subscription::GetLastInjectionState() const
    {
        return m_lastInjectionState;
    }

    void Subscription::SetLastInjectionState(const DistributionData& state)
    {
        m_lastInjectionState = state;
    }

    SharedFlag& Subscription::DirtyFlag()
    {
        return m_isDirty;
    }

    SharedFlag& Subscription::HasBeenDeletedFlag()
    {
        return m_stateHasBeenDeleted;
    }

    bool Subscription::IsSubscribed() const
    {
        return m_isSubscribed;
    }

    void Subscription::SetSubscribed(bool subscribed)
    {
        m_isSubscribed = subscribed;
    }

    void Subscription::AddToDirtySubscriptionQueue(const SubscriptionPtr& subPtr)
    {
        subPtr->DirtyFlag().Set(boost::bind(AddDirty,boost::cref(subPtr)));
    }
}
}
}

