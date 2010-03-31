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

#include <Safir/Dob/Internal/ServiceType.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/State.h>
#include <Safir/Dob/InstanceIdPolicy.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/Internal/ContextSharedTable.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>


namespace Safir
{
namespace Dob
{
namespace Internal
{
    ServiceType::ServiceType(const Typesystem::TypeId typeId)
        : m_typeId(typeId),
          m_typeIsContextShared(ContextSharedTable::Instance().IsContextShared(typeId)),
          m_handlerRegistrations(Safir::Dob::NodeParameters::NumberOfContexts(), typeId),
          m_typeLocks(Safir::Dob::NodeParameters::NumberOfContexts())
    {
    }

    bool ServiceType::Register(const ConnectionPtr&                  connection,
                               const Dob::Typesystem::HandlerId&     handlerId,
                               const RegisterTime                    regTime,
                               const bool                            overrideRegistration,
                               const ConsumerId&                     consumer)
    {
        const ContextId context = connection->Id().m_contextId;

        if (m_typeIsContextShared && context != 0)
        {
            std::wostringstream ostr;
            ostr << "Service " << Typesystem::Operations::GetName(m_typeId) <<
                    ", which is ContextShared, can only be registered from context 0.";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);          
        }

        ScopedTypeLock lck(m_typeLocks[context]);

        return m_handlerRegistrations[context].Register(connection,
                                                        handlerId,
                                                        Dob::InstanceIdPolicy::RequestorDecidesInstanceId, // Dummy for services
                                                        false,           // false => no injection handler
                                                        regTime,
                                                        overrideRegistration,
                                                        consumer);
    }

    void ServiceType::Unregister(const ConnectionPtr&                connection,
                                 const Dob::Typesystem::HandlerId&   handlerId)
    {
        const ContextId context = connection->Id().m_contextId;

        if (m_typeIsContextShared && context != 0)
        {
            std::wostringstream ostr;
            ostr << "Service " << Typesystem::Operations::GetName(m_typeId) <<
                    ", which is ContextShared, can only be unregistered from context 0.";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);          
        }

        ScopedTypeLock lck(m_typeLocks[context]);

        if (handlerId == Dob::Typesystem::HandlerId::ALL_HANDLERS)
        {
            m_handlerRegistrations[context].UnregisterAll(connection);
        }
        else
        {
            m_handlerRegistrations[context].Unregister(connection, handlerId, RegisterTime());
        }
    }

    void ServiceType::UnregisterAll(const ConnectionPtr& connection)
    {
        const ContextId context = connection->Id().m_contextId;

        ScopedTypeLock lck(m_typeLocks[context]);

        m_handlerRegistrations[context].UnregisterAll(connection);
    }

    void ServiceType::RemoteSetRegistrationState(const ConnectionPtr& connection,
                                                 const DistributionData& registrationState)
    {
        ENSURE(!connection->IsLocal(), << "EntityType::RemoteSetRegistrationState can only be used by remote connections!");

        const ContextId context = connection->Id().m_contextId;

        ScopedTypeLock lck(m_typeLocks[context]);

        m_handlerRegistrations[context].RemoteSetRegistrationState(connection, registrationState);
    }

    void ServiceType::RemoteSetUnregistrationState(const DistributionData& registrationState)
    {
        const ContextId context = registrationState.GetSenderId().m_contextId;

        ScopedTypeLock lck(m_typeLocks[context]);

        m_handlerRegistrations[context].RemoteSetUnregistrationState(registrationState);
    }

    bool ServiceType::IsRegistered(const Dob::Typesystem::HandlerId& handlerId, const ContextId context) const
    {
        return m_handlerRegistrations[context].IsRegistered(handlerId);
    }

    const ConnectionConsumerPair
    ServiceType::GetRegisterer(const Dob::Typesystem::HandlerId& handlerId,
                               const ContextId context) const
    {
        return m_handlerRegistrations[context].GetRegisterer(handlerId);
    }


    void ServiceType::SubscribeRegistration(const SubscriptionId&                subscriptionId,
                                            const Dob::Typesystem::HandlerId&    handlerId,
                                            const bool                           restartSubscription,
                                            const SubscriptionOptionsPtr&        subscriptionOptions)
    {
        // If it is a ContextShared type we use context 0, otherwise we use the same context as the subscriber.
        ContextId context = 0;
        if (!m_typeIsContextShared)
        {
            context = subscriptionId.connectionConsumer.connection->Id().m_contextId;
        }

        ScopedTypeLock lck(m_typeLocks[context]);

        m_handlerRegistrations[context].Subscribe(subscriptionId,
                                                  handlerId,
                                                  restartSubscription,
                                                  subscriptionOptions);
    }

    void ServiceType::UnsubscribeRegistration(const SubscriptionId&              subscriptionId,
                                              const Dob::Typesystem::HandlerId&  handlerId)
    {
        // If it is a ContextShared type we use context 0, otherwise we use the same context as the subscriber.
        ContextId context = 0;
        if (!m_typeIsContextShared)
        {
            context = subscriptionId.connectionConsumer.connection->Id().m_contextId;
        }

        ScopedTypeLock lck(m_typeLocks[context]);

        m_handlerRegistrations[context].Unsubscribe(subscriptionId, handlerId);
    }

    void ServiceType::UnsubscribeRegistrationAll(const ConnectionPtr& connection)
    {
        // If it is a ContextShared type we use context 0, otherwise we use the same context as the subscriber.
        ContextId context = 0;
        if (!m_typeIsContextShared)
        {
            context = connection->Id().m_contextId;
        }

        ScopedTypeLock lck(m_typeLocks[context]);

        m_handlerRegistrations[context].UnsubscribeAll(connection);
    }

    bool ServiceType::HasRegistrationSubscription(const ConnectionPtr&    connection,
                                                  const ConsumerId&       consumer) const
    {
        // If it is a ContextShared type we use context 0, otherwise we use the same context as the subscriber.
        ContextId context = 0;
        if (!m_typeIsContextShared)
        {
            context = connection->Id().m_contextId;
        }

        return m_handlerRegistrations[context].HasSubscription(connection,
                                                               consumer);
    }
}
}
}

