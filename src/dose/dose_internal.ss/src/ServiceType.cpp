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
#include <Safir/Utilities/Internal/LowLevelLogger.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    ServiceType::ServiceType(const Typesystem::TypeId typeId)
        : m_typeId(typeId),
          m_handlerRegistrations(typeId)
    {
    }

    bool ServiceType::Register(const ConnectionPtr&                  connection,
                               const Dob::Typesystem::HandlerId&     handlerId,
                               const RegisterTime                    regTime,
                               const bool                            overrideRegistration,
                               const ConsumerId&                     consumer)
    {
        return m_handlerRegistrations.Register(connection,
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
        if (handlerId == Dob::Typesystem::HandlerId::ALL_HANDLERS)
        {
            m_handlerRegistrations.UnregisterAll(connection);
        }
        else
        {
            m_handlerRegistrations.Unregister(connection, handlerId, RegisterTime());
        }
    }

    void ServiceType::UnregisterAll(const ConnectionPtr& connection)
    {
        m_handlerRegistrations.UnregisterAll(connection);
    }

    void ServiceType::RemoteSetRegistrationState(const ConnectionPtr& connection,
                                                 const DistributionData& registrationState)
    {
        ENSURE(!connection->IsLocal(), << "EntityType::RemoteSetRegistrationState can only be used by remote connections!");

        m_handlerRegistrations.RemoteSetRegistrationState(connection, registrationState);
    }

    void ServiceType::RemoteSetUnregistrationState(const DistributionData& registrationState)
    {
        m_handlerRegistrations.RemoteSetUnregistrationState(registrationState);
    }

    bool ServiceType::IsRegistered(const Dob::Typesystem::HandlerId& handlerId) const
    {
        return m_handlerRegistrations.IsRegistered(handlerId);
    }

    const ConnectionConsumerPair
    ServiceType::GetRegisterer(const Dob::Typesystem::HandlerId& handlerId) const
    {
        return m_handlerRegistrations.GetRegisterer(handlerId);
    }


    void ServiceType::SubscribeRegistration(const SubscriptionId&                subscriptionId,
                                            const Dob::Typesystem::HandlerId&    handlerId,
                                            const bool                           restartSubscription,
                                            const SubscriptionOptionsPtr&        subscriptionOptions)
    {
        m_handlerRegistrations.Subscribe(subscriptionId,
                                         handlerId,
                                         restartSubscription,
                                         subscriptionOptions);
    }

    void ServiceType::UnsubscribeRegistration(const SubscriptionId&              subscriptionId,
                                              const Dob::Typesystem::HandlerId&  handlerId)
    {
        m_handlerRegistrations.Unsubscribe(subscriptionId, handlerId);
    }

    void ServiceType::UnsubscribeRegistrationAll(const ConnectionPtr& connection)
    {
        m_handlerRegistrations.UnsubscribeAll(connection);
    }

    bool ServiceType::HasRegistrationSubscription(const ConnectionPtr&    connection,
                                                  const ConsumerId&       consumer) const
    {
        return m_handlerRegistrations.HasSubscription(connection,
                                                      consumer);
    }
}
}
}

