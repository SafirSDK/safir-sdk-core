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

#ifndef _dose_internal_subscription_h
#define _dose_internal_subscription_h

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/WrapAroundCounter.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/ConsumerId.h>
#include <Safir/Dob/Internal/SharedFlag.h>
#include <Safir/Dob/Internal/SubscriptionOptions.h>
#include <Safir/Dob/Internal/SubscriptionId.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API Subscription:
        public SharedMemoryObject
    {
    public:

        Subscription(const SubscriptionId&          subsciptionId,
                     const SubscriptionOptionsPtr&  subscriptionOptions,
                     const StateSharedPtr&          state);

        const SubscriptionId& GetSubscriptionId() const;

        const StateSharedPtr GetState() const;

        // State handling
        // A subscription keeps two state pointers. 'Current' which is the
        // current distribution state read from the object pool, and 'Last' which is
        // the last distribution state sent to the subscriber.

        const DistributionData GetCurrentRealState() const;
        const DistributionData GetLastRealState() const;

        void SetLastRealState(const DistributionData& state);

        const DistributionData GetCurrentInjectionState() const;
        const DistributionData GetLastInjectionState() const;

        void SetLastInjectionState(const DistributionData& state);

        SharedFlag& DirtyFlag();

        SharedFlag& HasBeenDeletedFlag();

        bool IsSubscribed() const;
        void SetSubscribed(bool subscribed);

        const SubscriptionOptionsPtr & GetSubscriptionOptions() const {return m_subscriptionOptions;}

        static void AddToDirtySubscriptionQueue(const SubscriptionPtr& subPtr);

    private:
        const Subscription& operator=(const Subscription& other);

        SubscriptionId        m_subscriptionId;
        StateSharedPtr        m_state;
        DistributionData      m_lastRealState;
        DistributionData      m_lastInjectionState;

        bool m_isSubscribed;
        SharedFlag m_isDirty;
        SharedFlag m_stateHasBeenDeleted;

        SubscriptionOptionsPtr m_subscriptionOptions;
    };
}
}
}
//#pragma warning (pop)

#endif

