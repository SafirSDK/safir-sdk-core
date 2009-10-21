/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Anders Widén/ stawi
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

#include "dose_consumer_references.h"
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/MessageTypes.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/Internal/ServiceTypes.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    void ConsumerReferences::AddDispatcherReference(const ConsumerId& consumer)
    {
        AddReference(consumer, m_dispatcherCounters);
    }

    void ConsumerReferences::MoveDispatcherReferencesToAttic(const DispatchThreadPtr& dispatchThread)
    {
        m_dispatcherCountersAttic.insert(std::make_pair(dispatchThread, m_dispatcherCounters));

        m_dispatcherCounters.clear();
    }

    void ConsumerReferences::DropAtticDispatcherReferences(const DispatchThreadPtr& dispatchThread,
                                                           const DropReferencesFunc&  dropReferencesFunc)
    {
        Attic::iterator threadIt = m_dispatcherCountersAttic.find(dispatchThread);

        if (threadIt == m_dispatcherCountersAttic.end())
        {
            // No such thread
            return;
        }

        // Drop dispather references for each consumer related to this thread.
        for (CounterMap::iterator consumerIt = threadIt->second.begin();
             consumerIt != threadIt->second.end();
             ++ consumerIt)
        {
            // make the callback
            dropReferencesFunc(consumerIt->first,       // ConsumerId
                               consumerIt->second);     // counter

        }

        // Remove this thread from attic.
        m_dispatcherCountersAttic.erase(threadIt);
    }

    bool ConsumerReferences::HasDispatcherReference(const ConsumerId& consumer) const
    {
        if (HasReference(consumer, m_dispatcherCounters))
        {
            return true;
        }
        else
        {
            // Check if there is a reference in the attic.
            for (Attic::const_iterator it = m_dispatcherCountersAttic.begin();
                 it != m_dispatcherCountersAttic.end();
                ++it)
            {
                if (HasReference(consumer, it->second))
                {
                    return true;
                }
            }
        }

        return false;
    }

    void ConsumerReferences::AddStopHandlerReference(const ConsumerId& consumer)
    {
        AddReference(consumer, m_stopHandlerCounters);
    }

    void ConsumerReferences::AddHandlerRegistrationReference(const Dob::Typesystem::TypeId      typeId,
                                                             const Dob::Typesystem::HandlerId&  handlerId,
                                                             const ConsumerId&                  consumer)
    {
        RegistrationKey key = boost::make_tuple(typeId, handlerId, consumer);

        RegistrationCounterMap::iterator findIt = m_registrationCounterMap.find(key);

        if (findIt == m_registrationCounterMap.end())
        {
            // Need to insert new entry into the map
            findIt = m_registrationCounterMap.insert(std::make_pair(key, 0)).first;
        }
        
        // Increment counter
        ++findIt->second;        
    }

    void ConsumerReferences::DropHandlerRegistrationReferences(const Dob::Typesystem::TypeId       typeId,
                                                               const Dob::Typesystem::HandlerId&   handlerId,
                                                               const DropReferencesFunc&           dropReferencesFunc)
    {
        for (RegistrationCounterMap::iterator it = m_registrationCounterMap.begin();
             it != m_registrationCounterMap.end();) //note the missing ++it, see below for explanation
        {
            // Since erase returns an iterator to the next element we handle the
            // iterator incrementation here rather than in the for statement
            if (typeId == it->first.get<0>() && handlerId == it->first.get<1>())
            {
                dropReferencesFunc(it->first.get<2>(),    // consumer
                                   it->second);         // counter

                RegistrationCounterMap::iterator eraseIt = it;
                ++it;
                m_registrationCounterMap.erase(eraseIt);
            }
            else
            {
                ++it;
            }
        }
    }

    bool ConsumerReferences::HasHandlerRegistrationReference(const Dob::Typesystem::TypeId      typeId,
                                                             const Dob::Typesystem::HandlerId&  handlerId,
                                                             const ConsumerId&                  consumer) const
    {
        RegistrationKey key = boost::make_tuple(typeId, handlerId, consumer);

        RegistrationCounterMap::const_iterator it = m_registrationCounterMap.find(key);

        if (it != m_registrationCounterMap.end() && it->second >0)
        {
            return true;
        }
        return false;

    }

    void ConsumerReferences::AddMessageSubscriptionReference(const ConsumerId& consumer)
    {
        AddReference(consumer, m_messageSubscriptionCounters);
    }

    void ConsumerReferences::DropMessageSubscriptionReferences(const ConnectionPtr&       connection,
                                                               const ConsumerId&          consumer,
                                                               const DropReferencesFunc&  dropReferencesFunc)
    {
        DropSubscriptionReferences(connection,
                                   consumer,
                                   dropReferencesFunc,
                                   MessageSubscription,
                                   m_messageSubscriptionCounters,
                                   boost::bind(&MessageTypes::HasSubscription,
                                               &MessageTypes::Instance(),
                                               _1, _2, _3));
    }

    bool ConsumerReferences::HasMessageSubscriptionReference(const ConsumerId& consumer) const
    {
        return HasReference(consumer, m_messageSubscriptionCounters);
    }

    void ConsumerReferences::AddEntitySubscriptionReference(const ConsumerId& consumer)
    {
        AddReference(consumer, m_entitySubscriptionCounters);
    }

    void ConsumerReferences::DropEntitySubscriptionReferences(const ConnectionPtr&       connection,
                                                              const ConsumerId&          consumer,
                                                              const DropReferencesFunc&  dropReferencesFunc)
    {
        DropSubscriptionReferences(connection,
                                   consumer,
                                   dropReferencesFunc,
                                   EntitySubscription,
                                   m_entitySubscriptionCounters,
                                   boost::bind(&EntityTypes::HasEntitySubscription,
                                               &EntityTypes::Instance(),
                                               _1, _2, _3));
    }

    bool ConsumerReferences::HasEntitySubscriptionReference(const ConsumerId& consumer) const
    {
        return HasReference(consumer, m_entitySubscriptionCounters);
    }

    void ConsumerReferences::AddEntityRegistrationSubscriptionReference(const ConsumerId& consumer)
    {
        AddReference(consumer, m_entityRegistrationSubscriptionCounters);
    }

    void ConsumerReferences::DropEntityRegistrationSubscriptionReferences(const ConnectionPtr&       connection,
                                                                          const ConsumerId&          consumer,
                                                                          const DropReferencesFunc&  dropReferencesFunc)
    {
        DropSubscriptionReferences(connection,
                                   consumer,
                                   dropReferencesFunc,
                                   EntityRegistrationSubscription,
                                   m_entityRegistrationSubscriptionCounters,
                                   boost::bind(&EntityTypes::HasRegistrationSubscription,
                                               &EntityTypes::Instance(),
                                               _1, _2, _3));
    }

    bool ConsumerReferences::HasEntityRegistrationSubscriptionReference(const ConsumerId& consumer) const
    {
        return HasReference(consumer, m_entityRegistrationSubscriptionCounters);
    }

    void ConsumerReferences::AddServiceRegistrationSubscriptionReference(const ConsumerId& consumer)
    {
        AddReference(consumer, m_serviceRegistrationSubscriptionCounters);
    }

    void ConsumerReferences::DropServiceRegistrationSubscriptionReferences(const ConnectionPtr&       connection,
                                                                           const ConsumerId&          consumer,
                                                                           const DropReferencesFunc&  dropReferencesFunc)
    {
        DropSubscriptionReferences(connection,
                                   consumer,
                                   dropReferencesFunc,
                                   ServiceRegistrationSubscription,
                                   m_serviceRegistrationSubscriptionCounters,
                                   boost::bind(&ServiceTypes::HasRegistrationSubscription,
                                               &ServiceTypes::Instance(),
                                               _1, _2, _3));
    }

    bool ConsumerReferences::HasServiceRegistrationSubscriptionReference(const ConsumerId& consumer) const
    {
        return HasReference(consumer, m_serviceRegistrationSubscriptionCounters);
    }

    void ConsumerReferences::DropAllReferences(const DropReferencesFunc& dropReferencesFunc)
    {
       DropReferences(m_dispatcherCounters, dropReferencesFunc);
       DropReferences(m_stopHandlerCounters, dropReferencesFunc);
       DropReferences(m_messageSubscriptionCounters, dropReferencesFunc);
       DropReferences(m_entitySubscriptionCounters, dropReferencesFunc);
       DropReferences(m_entityRegistrationSubscriptionCounters, dropReferencesFunc);
       DropReferences(m_serviceRegistrationSubscriptionCounters, dropReferencesFunc);
       DropReferences(m_registrationCounterMap, dropReferencesFunc);
    }

    void ConsumerReferences::AddReference(const ConsumerId& consumer, CounterMap& counterMap)
    {
        CounterMap::iterator findIt = counterMap.find(consumer);

        if (findIt == counterMap.end())
        {
            // Need to insert the consumer into the map
            findIt = counterMap.insert(std::make_pair(consumer, 0)).first;
        }
        
        // Increment counter
        ++findIt->second;
    }

    void ConsumerReferences::DropSubscriptionReferences(const ConnectionPtr&           connection,
                                                        const ConsumerId&              consumer,
                                                        const DropReferencesFunc&      dropReferencesFunc,
                                                        SubscriptionType               subscriptionType,
                                                        CounterMap&                    counterMap,
                                                        const HasSubscriptionFunc&     hasSubscriptionFunc)
    {
        CounterMap::iterator consumerIt = counterMap.find(consumer);

        if (consumerIt == counterMap.end())
        {
            // No such consumer
            return;
        }

        // Get the type ids for the given subscriptionType that this connection has subscribed for.
        // Note that this set contains all types (for the given subscriptionType) that the connection
        // has ever subscribed for, but it still is usefull for constraining the number of types
        // we have to check.
        Connection::TypesSet typeIds = connection->GetSubscriptions(subscriptionType);

        for (Connection::TypesSet::iterator typeIt = typeIds.begin();
             typeIt != typeIds.end();
             ++typeIt)
        {
            // Make callback to check wether or not the consumer has any subscriptions of
            // the given type
            if (hasSubscriptionFunc(connection, consumer, *typeIt))
            {
                // This consumer is used for a subscription so we must not drop any references.
                return;
            }
        }

        // Make the callback 
        dropReferencesFunc(consumer, consumerIt->second);

        // Now we can erase this consumer counter.
        counterMap.erase(consumerIt);
    }

    void ConsumerReferences::DropReferences(CounterMap&                    counterMap,
                                            const DropReferencesFunc&      dropReferencesFunc)
    {
        for (CounterMap::iterator it = counterMap.begin();
             it != counterMap.end();
             ++it)
        {
            dropReferencesFunc(it->first,   // ConsumerId
                               it->second); // counter
        }

        counterMap.clear();
    }

    void ConsumerReferences::DropReferences(RegistrationCounterMap&        counterMap,
                                            const DropReferencesFunc&      dropReferencesFunc)
    {
        for (RegistrationCounterMap::iterator it = counterMap.begin();
             it != counterMap.end();
             ++it)
        {
            dropReferencesFunc(it->first.get<2>(),  // ConsumerId
                               it->second);         // counter
        }

        counterMap.clear();
    }

    bool ConsumerReferences::HasReference(const ConsumerId&  consumer,
                                          const CounterMap&  counterMap)
    {
        CounterMap::const_iterator it = counterMap.find(consumer);

        if (it != counterMap.end() && it->second > 0)
        {
            return true;
        }
        return false;

    }
}
}
}
