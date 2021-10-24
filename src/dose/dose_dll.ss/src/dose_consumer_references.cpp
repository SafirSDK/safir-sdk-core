/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
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
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>

using namespace std::placeholders;

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

    bool ConsumerReferences::HasDispatcherReference(const ConsumerId& consumer) const
    {
        return HasReference(consumer, m_dispatcherCounters);
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

    void ConsumerReferences::DropAllHandlerRegistrationReferences(const Dob::Typesystem::TypeId       typeId,
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

    long ConsumerReferences::GetHandlerRegistrationReferenceCounter(const Dob::Typesystem::TypeId      typeId,
                                                                    const Dob::Typesystem::HandlerId&  handlerId,
                                                                    const ConsumerId&                  consumer) const
    {
        RegistrationKey key = boost::make_tuple(typeId, handlerId, consumer);

        RegistrationCounterMap::const_iterator it = m_registrationCounterMap.find(key);

        if (it != m_registrationCounterMap.end())
        {
            return it->second;
        }
        return 0;
    }

    void ConsumerReferences::DropHandlerRegistrationReferences(const Dob::Typesystem::TypeId       typeId,
                                                               const Dob::Typesystem::HandlerId&   handlerId,
                                                               const ConsumerId&                   consumer,
                                                               const long                          nbrOfReferences,
                                                               const DropReferencesFunc&           dropReferencesFunc)
     {
        RegistrationKey key = boost::make_tuple(typeId, handlerId, consumer);

        RegistrationCounterMap::iterator it = m_registrationCounterMap.find(key);

        if (it != m_registrationCounterMap.end())
        {
            ENSURE(it->second >= nbrOfReferences, <<
                   "The number of handler registration refrences to drop exceeds the actual number of references!");

            dropReferencesFunc(consumer,
                               nbrOfReferences);

            if (it->second == nbrOfReferences)
            {
                // No more references, erase from map.
                m_registrationCounterMap.erase(it);
            }
            else
            {
                it->second = it->second - nbrOfReferences;
            }
        }
        else if (nbrOfReferences > 0)
        {
            ENSURE(false, << "The number of handler registration refrences to drop exceeds the actual number of references!");
        }
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
                                   std::bind(&MessageTypes::HasSubscription,
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
                                   std::bind(&EntityTypes::HasEntitySubscription,
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
                                   std::bind(&EntityTypes::HasRegistrationSubscription,
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
                                   std::bind(&ServiceTypes::HasRegistrationSubscription,
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
