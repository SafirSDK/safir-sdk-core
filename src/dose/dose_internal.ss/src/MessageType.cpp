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

#include <Safir/Dob/Internal/MessageType.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Internal/ContextSharedTable.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    MessageType::MessageType(const Typesystem::TypeId typeId)
        : m_typeId(typeId)
    {
    }

    void MessageType::Subscribe(const ConnectionPtr&              connection,
                                const Dob::Typesystem::ChannelId& channelId,
                                const ConsumerId&                 consumer)
    {
        ScopedMessageTypeLock lck(m_lock);

        // Check if this connection/consumer already has a subscription

        ConnectionConsumerPair key(connection, consumer);

        ConsumerSubscriptions::iterator subIt = m_subscriptions.find(key);

        if (subIt == m_subscriptions.end())
        {
            // Have to create a new subscription

            ConsumerSubscription subscription;

            // Save smart pointer to consumer in-queue
            subscription.consumerInQueue = connection->AddMessageInQueue(consumer);

            std::pair<ConsumerSubscriptions::iterator, bool> status;

            status = m_subscriptions.insert(std::make_pair(key, subscription));

            ENSURE(status.second, << "Subscribe: Failed to insert a new message subscription!" );

            subIt = status.first;

            connection->AddSubscription(m_typeId, MessageSubscription);
        }

        // Set subscription state
        if (channelId == Typesystem::ChannelId::ALL_CHANNELS)
        {
            subIt->second.subscriptionState.SubscribeAll();
        }
        else
        {
            subIt->second.subscriptionState.Subscribe(channelId.GetRawValue());
        }
    }

    void MessageType::Unsubscribe(const ConnectionPtr&              connection,
                                  const Dob::Typesystem::ChannelId& channelId,
                                  const ConsumerId&                 consumer)
    {
        ScopedMessageTypeLock lck(m_lock);

        ConnectionConsumerPair key(connection, consumer);

        ConsumerSubscriptions::iterator subIt = m_subscriptions.find(key);

        if (subIt == m_subscriptions.end())
        {
            return;
        }

        if (channelId == Typesystem::ChannelId::ALL_CHANNELS)
        {
            subIt->second.subscriptionState.UnsubscribeAll();
        }
        else
        {
            subIt->second.subscriptionState.Unsubscribe(channelId.GetRawValue());
        }
        if (!subIt->second.subscriptionState.IsAnySubscribed())
        {
            // Nothing is subscribed by this consumer so we can remove
            // its subscription
            m_subscriptions.erase(subIt);
        }
    }

    void MessageType::UnsubscribeAll(const ConnectionPtr& connection)
    {
        ScopedMessageTypeLock lck(m_lock);

        for (ConsumerSubscriptions::iterator it = m_subscriptions.begin();
            it != m_subscriptions.end();)
        {
            if (it->first.connection->Id() == connection->Id())
            {
                it->second.subscriptionState.UnsubscribeAll();
                m_subscriptions.erase(it++);
            }
            else
            {
                ++it;
            }
        }
    }

    void MessageType::DistributeMsg(const DistributionData& msg)
    {
        ScopedMessageTypeLock lck(m_lock);

        for (ConsumerSubscriptions::iterator subIt = m_subscriptions.begin(); subIt != m_subscriptions.end(); ++subIt)
        {
            if ((msg.GetSenderId().m_contextId == subIt->first.connection->Id().m_contextId ||
                ContextSharedTable::Instance().IsContextShared(msg.GetTypeId())) &&                
                subIt->second.subscriptionState.IsSubscribed(msg.GetChannelId().GetRawValue()))
            {
                /*
                // Put the message in the in-queue ...
                if (subIt->second.consumerInQueue->push(msg))
                {
                    // ... and kick the application if it didn't overflow
                    subIt->first.connection->SignalIn();
                }
                */
                //An attempt at workaround for #696, we kick even if there is an overflow.
                subIt->second.consumerInQueue->push(msg);
                subIt->first.connection->SignalIn();
            }
        }
    }

    bool MessageType::HasSubscription(const ConnectionPtr&    connection,
                                      const ConsumerId&       consumer) const
    {
        ScopedMessageTypeLock lck(m_lock);

        ConnectionConsumerPair key(connection, consumer);

        return m_subscriptions.find(key) != m_subscriptions.end();
    }

    void MessageType::DumpSubscriptions() const
    {
    }



}
}
}

