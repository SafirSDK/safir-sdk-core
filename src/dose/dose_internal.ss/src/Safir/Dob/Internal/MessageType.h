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

#ifndef _dose_internal_mesage_type_h
#define _dose_internal_mesage_type_h

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/MetaSubscription.h>
#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Internal/MessageQueue.h>
#include <Safir/Dob/Internal/ConnectionConsumerPair.h>
#include <boost/interprocess/sync/interprocess_mutex.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API MessageType:
        public SharedMemoryObject
    {
    public:

        explicit MessageType(const Typesystem::TypeId typeId);

        void Subscribe(const ConnectionPtr&              connection,
                       const Dob::Typesystem::ChannelId& channelId,
                       const ConsumerId&                 consumer);

        void Unsubscribe(const ConnectionPtr&              connection,
                         const Dob::Typesystem::ChannelId& channelId,
                         const ConsumerId&                 consumer);

        void UnsubscribeAll(const ConnectionPtr& connection);

        // Distribute a message to the subscribers
        void DistributeMsg(const DistributionData& msg);

        Dob::Typesystem::TypeId GetTypeId() const {return m_typeId;}

        // Returns true if the given connection/consumer has any subscription for this type.
        bool HasSubscription(const ConnectionPtr&    connection,
                             const ConsumerId&       consumer) const;

        //Debug and statistics
        void DumpSubscriptions() const;

    private:
        Typesystem::TypeId m_typeId;

        //const methods may need to lock this
        mutable boost::interprocess::interprocess_mutex m_lock;

        struct ConsumerSubscription
        {
            ConsumerSubscription():subscriptionState(SubscriptionOptionsPtr(NULL)),consumerInQueue() {}
            MetaSubscription                   subscriptionState;
            MessageQueueContainer::QueuePtr    consumerInQueue;
        };

        typedef PairContainers<ConnectionConsumerPair, ConsumerSubscription>::map ConsumerSubscriptions;

        ConsumerSubscriptions m_subscriptions;

    };
}
}
}
#endif

