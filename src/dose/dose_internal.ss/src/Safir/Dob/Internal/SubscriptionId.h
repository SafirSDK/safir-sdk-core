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

#ifndef _dose_internal_subscription_id_h
#define _dose_internal_subscription_id_h

#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/ConnectionConsumerPair.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    // Used to uniquely identify a subscripton.
    // The id member makes it possible for a connection/consumer to have several subscription.
    //
    struct SubscriptionId:
        public SharedMemoryObject
    {
        SubscriptionId(const ConnectionConsumerPair& _connectionConsumer,
                       const SubscriptionType        _subscriptionType,
                       const Dob::Typesystem::Int64 _id)
            : connectionConsumer(_connectionConsumer),
              subscriptionType(_subscriptionType),
              id(_id)
        {}

        ConnectionConsumerPair  connectionConsumer;
        SubscriptionType        subscriptionType;
        Dob::Typesystem::Int64  id;

        bool operator<(const SubscriptionId& other) const
        {
            if (connectionConsumer < other.connectionConsumer)
            {
                return true;
            }
            if (other.connectionConsumer < connectionConsumer)
            {
                return false;
            }

            if (subscriptionType < other.subscriptionType)
            {
                return true;
            }
            if (other.subscriptionType < subscriptionType)
            {
                return false;
            }

            if (id < other.id)
            {
                return true;
            }

            return false;
        }
    };

}
}
}
#endif

