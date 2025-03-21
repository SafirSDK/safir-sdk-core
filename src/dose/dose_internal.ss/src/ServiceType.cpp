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
    ServiceType::ServiceType(const Typesystem::TypeId typeId, const int64_t nodeId)
        : m_typeId(typeId),
          m_typeIsContextShared(ContextSharedTable::Instance().IsContextShared(typeId)),
          m_handlerRegistrations(Safir::Dob::NodeParameters::NumberOfContexts(), typeId, nodeId),
          m_typeLocks(Safir::Dob::NodeParameters::NumberOfContexts())
    {
    }

    bool ServiceType::Register(const ConnectionPtr&                  connection,
                               const Dob::Typesystem::HandlerId&     handlerId,
                               LamportClock&                         regClock,
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

        // Important to update the registration clock with the lock taken
        RegisterTime regTime = regClock.GetNewTimestamp();

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
            m_handlerRegistrations[context].UnregisterAll(connection,
                                                          true);     // true => explicit unregister
        }
        else
        {
            m_handlerRegistrations[context].Unregister(connection, handlerId);
        }
    }

    void ServiceType::UnregisterAll(const ConnectionPtr& connection, const bool explicitUnregister)
    {
        const ContextId context = connection->Id().m_contextId;

        ScopedTypeLock lck(m_typeLocks[context]);

        m_handlerRegistrations[context].UnregisterAll(connection, explicitUnregister);
    }

    void ServiceType::SetDetachFlagAll(const ConnectionPtr& connection, bool detached)
    {
        const ContextId context = connection->Id().m_contextId;

        ScopedTypeLock lck(m_typeLocks[context]);

        m_handlerRegistrations[context].SetDetachFlagAll(connection, detached);
    }

    void ServiceType::RemoteSetRegistrationState(const ConnectionPtr& connection,
                                                 const DistributionData& registrationState)
    {
        const ContextId context = registrationState.GetSenderId().m_contextId;

        ScopedTypeLock lck(m_typeLocks[context]);

        m_handlerRegistrations[context].RemoteSetRegistrationState(connection, registrationState);
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

    bool ServiceType::CanAcquireContainerWriterLock(const ContextId contextId,
                                                    const std::chrono::steady_clock::duration& lockTimeout)
    {
        ScopedTypeLock lck(m_typeLocks[contextId],
                           boost::interprocess::defer_lock);

        if (!steady_try_lock_for(lck, lockTimeout))
        {
            // Can't acquire the type level lock.
            return false;  // *** RETURN ***
        }

        return m_handlerRegistrations[contextId].CanAcquireContainerWriterLock(lockTimeout);
    }
}
}
}
