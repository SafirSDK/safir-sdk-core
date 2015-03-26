/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#include <Safir/Dob/Internal/ServiceTypes.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Service.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    ServiceTypes* ServiceTypes::m_instance = NULL;
    Safir::Utilities::Internal::AtomicUint32 ServiceTypes::m_isInitialized(0);

    ServiceTypes& ServiceTypes::Instance()
    {
        ENSURE(m_instance != NULL, << "ServiceTypes::Instance was called before Initialize!!!");
        return *m_instance;
    }

    ServiceTypes::ServiceTypes(private_constructor_t, const int64_t nodeId)
        : m_registrationClock(nodeId)
    {

    }

    void ServiceTypes::Initialize(const bool iAmDoseMain, const int64_t nodeId)
    {
        m_instance = GetSharedMemory().find_or_construct<ServiceTypes>("SERVICETYPES")(private_constructor_t(), nodeId);

        if (iAmDoseMain)
        {
            ENSURE (m_instance->m_serviceTypes.empty(),
                    << "Can't start dose_main. An application or another dose_main "
                    "instance is already started!");

            Dob::Typesystem::TypeIdVector tid = Dob::Typesystem::Operations::GetAllTypeIds();

            for (Dob::Typesystem::TypeIdVector::iterator it = tid.begin();
                 it != tid.end(); ++it)
            {
                if (Dob::Typesystem::Operations::IsClass(*it))
                {
                    if (Dob::Typesystem::Operations::IsOfType(*it, Safir::Dob::Service::ClassTypeId))
                    {
                        ServiceTypePtr serviceType = GetSharedMemory().construct<ServiceType>(boost::interprocess::anonymous_instance)(*it);
                        m_instance->m_serviceTypes.insert(std::make_pair(*it, serviceType));
                    }
                }
            }
        }

        m_isInitialized = 1;
    }

    bool ServiceTypes::IsInitialized()
    {
        return m_isInitialized != 0;
    }

    bool ServiceTypes::Register(const ConnectionPtr&                  connection,
                                const Dob::Typesystem::TypeId         typeId,
                                const Dob::Typesystem::HandlerId&     handlerId,
                                const bool                            overrideRegistration,
                                const ConsumerId&                     consumer)
    {
        return GetType(typeId).Register(connection,
                                        handlerId,
                                        m_registrationClock,
                                        overrideRegistration,
                                        consumer);
    }

    void ServiceTypes::Unregister(const ConnectionPtr&                connection,
                                  const Dob::Typesystem::TypeId       typeId,
                                  const Dob::Typesystem::HandlerId&   handlerId)
    {
        GetType(typeId).Unregister(connection,
                                   handlerId);
    }

    void ServiceTypes::UnregisterAll(const ConnectionPtr&           connection,
                                     const Dob::Typesystem::TypeId  typeId,
                                     const bool                     explicitUnregister)
    {
        GetType(typeId).UnregisterAll(connection, explicitUnregister);
    }

    void ServiceTypes::RemoteSetRegistrationState(const ConnectionPtr& connection,
                                                  const DistributionData& registrationState)
    {
        m_registrationClock.UpdateCurrentTimestamp(registrationState.GetRegistrationTime());

        GetType(registrationState.GetTypeId()).RemoteSetRegistrationState(connection,
                                                                          registrationState);
    }

    bool ServiceTypes::IsRegistered(const Dob::Typesystem::TypeId typeId,
                                    const Dob::Typesystem::HandlerId& handlerId,
                                    const ContextId                   contextId) const
    {
        return GetType(typeId).IsRegistered(handlerId, contextId);
    }


    const ConnectionConsumerPair
    ServiceTypes::GetRegisterer(const Dob::Typesystem::TypeId     typeId,
                                const Dob::Typesystem::HandlerId& handlerId,
                                const ContextId                   contextId) const
    {
        return GetType(typeId).GetRegisterer(handlerId, contextId);
    }


    void ServiceTypes::SubscribeRegistration(const SubscriptionId&                subscriptionId,
                                             const Dob::Typesystem::TypeId        typeId,
                                             const Dob::Typesystem::HandlerId&    handlerId,
                                             const bool                           includeSubclasses,
                                             const bool                           restartSubscription,
                                             const SubscriptionOptionsPtr&        subscriptionOptions)
    {
        Dob::Typesystem::TypeIdVector classTree;

        if (includeSubclasses)
        {
            classTree = Dob::Typesystem::Operations::GetClassTree(typeId);
        }
        else
        {
            classTree.push_back(typeId);
        }

        for (Dob::Typesystem::TypeIdVector::iterator it = classTree.begin();
             it != classTree.end();
             ++it)
        {
            GetType(*it).SubscribeRegistration(subscriptionId,
                                               handlerId,
                                               restartSubscription,
                                               subscriptionOptions);
        }
    }

    void ServiceTypes::UnsubscribeRegistration(const SubscriptionId&              subscriptionId,
                                               const Dob::Typesystem::TypeId      typeId,
                                               const Dob::Typesystem::HandlerId&  handlerId,
                                               const bool                         includeSubclasses)
    {
        Dob::Typesystem::TypeIdVector classTree;

        if (includeSubclasses)
        {
            classTree = Dob::Typesystem::Operations::GetClassTree(typeId);
        }
        else
        {
            classTree.push_back(typeId);
        }

        for (Dob::Typesystem::TypeIdVector::iterator it = classTree.begin();
             it != classTree.end();
             ++it)
        {
            GetType(*it).UnsubscribeRegistration(subscriptionId, handlerId);
        }
    }

    bool ServiceTypes::HasRegistrationSubscription(const ConnectionPtr&             connection,
                                                   const ConsumerId&                consumer,
                                                   const Dob::Typesystem::TypeId    typeId)
    {
        return GetType(typeId).HasRegistrationSubscription(connection, consumer);
    }

    void
    ServiceTypes::RegisterAcceptedPendingRegistrations(const ConnectionPtr & connection,
                                                       PendingRegistrationVector & prv,
                                                       bool & needKick)
    {
        PendingRegistrationVector pendingRegistrations = connection->GetPendingRegistrations();

        for (PendingRegistrationVector::iterator it = pendingRegistrations.begin();
             it != pendingRegistrations.end(); ++it)
        {
            if (Dob::Typesystem::Operations::IsOfType(it->typeId, Dob::Service::ClassTypeId) && it->accepted)
            {
                needKick = true;
                const bool registered = Register(connection,it->typeId, it->handlerId.GetHandlerId(), false, it->consumer);
                if (registered)
                {
                    prv.push_back(*it);
                    connection->RemoveAcceptedPendingOwnership(it->id);
                }
                else
                {
                    connection->RetryAcceptedPendingOwnership(it->id);
                }
            }
        }


    }

    void ServiceTypes::UnsubscribeRegistrationAll(const ConnectionPtr&           connection,
                                                  const Dob::Typesystem::TypeId  typeId)
    {
        GetType(typeId).UnsubscribeRegistrationAll(connection);
    }

    ServiceType& ServiceTypes::GetType(const Typesystem::TypeId typeId)
    {
        ServiceTypeTable::iterator findIt = m_serviceTypes.find(typeId);

        ENSURE(findIt != m_serviceTypes.end(), << "GetType failed to find the service type that was asked for!!! typeId = " <<
                                                  Dob::Typesystem::Operations::GetName(typeId));

        return *findIt->second;
    }

    bool ServiceTypes::CanAcquireContainerWriterLock(const Typesystem::TypeId             typeId,
                                                     const ContextId                      contextId,
                                                     const boost::chrono::steady_clock::duration& lockTimeout)
    {
        return GetType(typeId).CanAcquireContainerWriterLock(contextId, lockTimeout);
    }
}
}
}
