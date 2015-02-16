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

#include <Safir/Dob/Internal/EntityType.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/AccessDeniedException.h>
#include <Safir/Dob/GhostExistsException.h>
#include <Safir/Dob/NotFoundException.h>
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/Internal/LamportClocks.h>
#include <Safir/Dob/Internal/MonotonicClock.h>
#include <Safir/Dob/Internal/TimestampOperations.h>
#include <Safir/Dob/Internal/EndStates.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/Internal/ContextSharedTable.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Dob/ThisNodeParameters.h>

#include <boost/bind.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    EntityType::EntityType(const Typesystem::TypeId typeId)
        : m_typeId(typeId),
          m_typeIsContextShared(ContextSharedTable::Instance().IsContextShared(typeId)),
          m_injectionKind(InjectionKindTable::Instance().GetInjectionKind(typeId)),
          m_entityStates(Safir::Dob::NodeParameters::NumberOfContexts(), typeId),
          m_handlerRegistrations(Safir::Dob::NodeParameters::NumberOfContexts(), typeId),
          m_typeLocks(Safir::Dob::NodeParameters::NumberOfContexts())
    {
        // Set the correct state container pointer in each registration handler.
        for (ContextId context = 0; context < Safir::Dob::NodeParameters::NumberOfContexts(); ++context)
        {
            m_handlerRegistrations[context].SetStateContainer(&m_entityStates[context]);
        }
    }

    bool EntityType::Register(const ConnectionPtr&                  connection,
                              const Dob::Typesystem::HandlerId&     handlerId,
                              const InstanceIdPolicy::Enumeration   instanceIdPolicy,
                              const bool                            isInjectionHandler,
                              LamportClock&                         regClock,
                              const bool                            overrideRegistration,
                              const ConsumerId&                     consumer)
    {
        const ContextId context = connection->Id().m_contextId;

        if (m_typeIsContextShared && context != 0)
        {
            std::wostringstream ostr;
            ostr << "Entity " << Typesystem::Operations::GetName(m_typeId) <<
                    ", which is ContextShared, can only be registered from context 0.";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        bool result = true;
        {
            ScopedTypeLock lck(m_typeLocks[context]);

            // Important to update the registration clock with the lock taken
            RegisterTime regTime = regClock.GetNewTimestamp();

            result = m_handlerRegistrations[context].Register(connection,
                                                            handlerId,
                                                            instanceIdPolicy,
                                                            isInjectionHandler,
                                                            regTime,
                                                            overrideRegistration,
                                                            consumer);
        }

        CleanGhosts(handlerId, context);

        return result;
    }

    void EntityType::Unregister(const ConnectionPtr&                connection,
                                const Dob::Typesystem::HandlerId&   handlerId)
    {
        const ContextId context = connection->Id().m_contextId;

        if (m_typeIsContextShared && context != 0)
        {
            std::wostringstream ostr;
            ostr << "Entity " << Typesystem::Operations::GetName(m_typeId) <<
                    ", which is ContextShared, can only be unregistered from context 0.";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        {
            ScopedTypeLock lck(m_typeLocks[context]);

            if (handlerId == Dob::Typesystem::HandlerId::ALL_HANDLERS)
            {
                m_handlerRegistrations[context].UnregisterAll(connection,
                                                              true);  // true => explicit unregister
            }
            else
            {
                m_handlerRegistrations[context].Unregister(connection, handlerId);
            }
        }

        CleanGhosts(handlerId, context);
    }

    void EntityType::UnregisterAll(const ConnectionPtr& connection, const bool explicitUnregister)
    {
        const ContextId context = connection->Id().m_contextId;

        {
            ScopedTypeLock lck(m_typeLocks[context]);
            m_handlerRegistrations[context].UnregisterAll(connection, explicitUnregister);
        }

        CleanGhosts(Dob::Typesystem::HandlerId::ALL_HANDLERS, context);
    }

    void EntityType::RemoteSetRegistrationState(const ConnectionPtr& connection,
                                                const DistributionData& registrationState)
    {
        const ContextId context = registrationState.GetSenderId().m_contextId;
        const Safir::Dob::Typesystem::HandlerId handlerId = registrationState.GetHandlerId();
        {
            ScopedTypeLock lck(m_typeLocks[context]);

            RegisterTime regTime = registrationState.GetRegistrationTime();
            m_entityStates[context].ForEachState(boost::bind(&EntityType::RemoveGhost,
                                                                 this,
                                                                 _2,
                                                                 boost::cref(handlerId),
                                                                 boost::cref(regTime)),
                                                                false); // false => Do not include released states

            m_handlerRegistrations[context].RemoteSetRegistrationState(connection, registrationState);
        }
        CleanGhosts(handlerId, context);
    }

    bool EntityType::IsRegistered(const Dob::Typesystem::HandlerId& handlerId, const ContextId context) const
    {
        return m_handlerRegistrations[context].IsRegistered(handlerId);
    }

    const ConnectionConsumerPair
    EntityType::GetRegisterer(const Dob::Typesystem::HandlerId& handlerId, const ContextId context) const
    {
        return m_handlerRegistrations[context].GetRegisterer(handlerId);
    }

    bool
    EntityType::GetRegisterer(const Dob::Typesystem::HandlerId& handlerId,
                              const ContextId                   context,
                              ConnectionConsumerPair&           registerer,
                              InstanceIdPolicy::Enumeration&    instanceIdPolicy) const
    {
       return m_handlerRegistrations[context].GetRegisterer(handlerId, registerer, instanceIdPolicy);
    }

    void EntityType::SubscribeRegistration(const SubscriptionId&                subscriptionId,
                                           const Dob::Typesystem::HandlerId&    handlerId,
                                           const bool                           restartSubscription,
                                           const SubscriptionOptionsPtr&        subscriptionOptions)
    {
        // If it is a ContextShared type we use context 0, otherwise we use the same context as the subscriber.
        const ContextId context = m_typeIsContextShared ? 0 : subscriptionId.connectionConsumer.connection->Id().m_contextId;

        ScopedTypeLock lck(m_typeLocks[context]);
        m_handlerRegistrations[context].Subscribe(subscriptionId, handlerId, restartSubscription, subscriptionOptions);
    }

    void EntityType::UnsubscribeRegistration(const SubscriptionId&              subscriptionId,
                                             const Dob::Typesystem::HandlerId&  handlerId)
    {
        // If it is a ContextShared type we use context 0, otherwise we use the same context as the "unsubscriber".
        const ContextId context = m_typeIsContextShared ? 0 : subscriptionId.connectionConsumer.connection->Id().m_contextId;;

        ScopedTypeLock lck(m_typeLocks[context]);
        m_handlerRegistrations[context].Unsubscribe(subscriptionId, handlerId);
    }

    bool EntityType::HasRegistrationSubscription(const ConnectionPtr&    connection,
                                                 const ConsumerId&       consumer) const
    {
        // If it is a ContextShared type we use context 0, otherwise we use the same context as the "requestor".
        const ContextId context = m_typeIsContextShared ? 0 : connection->Id().m_contextId;

        return m_handlerRegistrations[context].HasSubscription(connection,
                                                               consumer);
    }

    void EntityType::SetEntity(const ConnectionPtr&                 connection,
                               const Dob::Typesystem::HandlerId&    handlerId,
                               const Dob::Typesystem::InstanceId&   instanceId,
                               const char* const                    blob,
                               const bool                           considerChangeFlags,
                               const bool                           initialInjection)
    {
        const ContextId context = connection->Id().m_contextId;

        if (m_typeIsContextShared && context != 0)
        {
            std::wostringstream ostr;
            ostr << "Entity " << Typesystem::Operations::GetName(m_typeId) <<
                    ", which is ContextShared, can only be set from context 0.";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        ScopedTypeLock lck(m_typeLocks[context]);

        if (!initialInjection)
        {
            // Add a state if it is not already present
            m_entityStates[context].ForSpecificStateAdd(instanceId.GetRawValue(),
                                                        boost::bind(&EntityType::SetEntityInternal,
                                                                    this,
                                                                    _2,
                                                                    boost::cref(connection),
                                                                    boost::cref(handlerId),
                                                                    boost::cref(instanceId),
                                                                    considerChangeFlags,
                                                                    blob));
        }
        else
        {
            // Add a state if it is not already present
            m_entityStates[context].ForSpecificStateAdd(instanceId.GetRawValue(),
                                                        boost::bind(&EntityType::SetInitalGhostInternal,
                                                                    this,
                                                                    _2,
                                                                    boost::cref(connection),
                                                                    boost::cref(handlerId),
                                                                    boost::cref(instanceId),
                                                                    blob));
        }
    }

    void EntityType::DeleteEntity(const ConnectionPtr&                  connection,
                                  const Dob::Typesystem::HandlerId&     handlerId,
                                  const Dob::Typesystem::InstanceId&    instanceId,
                                  const bool                            allInstances)
    {
        const ContextId context = connection->Id().m_contextId;

        if (m_typeIsContextShared && context != 0)
        {
            std::wostringstream ostr;
            ostr << "Entity " << Typesystem::Operations::GetName(m_typeId) <<
                    ", which is ContextShared, can only be deleted from context 0.";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        ScopedTypeLock lck(m_typeLocks[context]);

        if (allInstances)
        {
            m_entityStates[context].ForEachState(boost::bind(&EntityType::DeleteAllInstancesInternal,
                                                             this,
                                                             _2,
                                                             boost::cref(connection),
                                                             boost::cref(handlerId),
                                                             _3),
                                                 false); // no need to include already released states
        }
        else
        {
            m_entityStates[context].ForSpecificState(instanceId.GetRawValue(),
                                                     boost::bind(&EntityType::DeleteEntityInternal,
                                                                 this,
                                                                 _2,
                                                                 boost::cref(connection),
                                                                 boost::cref(handlerId),
                                                                 boost::cref(instanceId)),
                                                     false); // no need to include already released state
        }
    }

    void EntityType::InjectEntity(const ConnectionPtr&                 connection,
                                  const Dob::Typesystem::HandlerId&    handlerId,
                                  const Dob::Typesystem::InstanceId&   instanceId,
                                  const char* const                    blob,
                                  const Dob::Typesystem::Int64         timestamp)
    {
        ENSURE(blob != NULL, << "Trying to call InjectEntity with a NULL blob!");
        ENSURE(m_injectionKind == InjectionKind::Injectable,
               << "Trying to call InjectEntity with a non-injectable type");

        const ContextId context = connection->Id().m_contextId;

        if (m_typeIsContextShared && context != 0)
        {
            std::wostringstream ostr;
            ostr << "Entity " << Typesystem::Operations::GetName(m_typeId) <<
                    ", which is ContextShared, can only be injected from context 0.";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        ScopedTypeLock lck(m_typeLocks[context]);

        m_entityStates[context].ForSpecificStateAdd(instanceId.GetRawValue(),
                                                    boost::bind(&EntityType::InjectEntityInternal,
                                                                this,
                                                                _2,
                                                                connection,
                                                                boost::cref(handlerId),
                                                                boost::cref(instanceId),
                                                                blob,
                                                                timestamp));
    }

    void EntityType::InjectDeletedEntity(const ConnectionPtr&                 connection,
                                         const Dob::Typesystem::HandlerId&    handlerId,
                                         const Dob::Typesystem::InstanceId&   instanceId,
                                         const Dob::Typesystem::Int64         timestamp)
    {
        ENSURE(m_injectionKind == InjectionKind::Injectable,
               << "Trying to call InjectDeletedEntity with a non-injectable type");

        const ContextId context = connection->Id().m_contextId;

        if (m_typeIsContextShared && context != 0)
        {
            std::wostringstream ostr;
            ostr << "Entity " << Typesystem::Operations::GetName(m_typeId) <<
                    ", which is ContextShared, can only be deleteInjected from context 0.";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        ScopedTypeLock lck(m_typeLocks[context]);

        m_entityStates[context].ForSpecificStateAdd(instanceId.GetRawValue(),
                                                    boost::bind(&EntityType::InjectDeletedEntityInternal,
                                                                this,
                                                                _2,
                                                                connection,
                                                                boost::cref(handlerId),
                                                                boost::cref(instanceId),
                                                                timestamp));
    }

    void EntityType::AcceptInjection(const ConnectionPtr&       connection,
                                     DistributionData&          injectionState,
                                     const DistributionData&    originalInjectionState)
    {
        const ContextId context = connection->Id().m_contextId;

        if (m_typeIsContextShared)
        {
            ENSURE(context == 0, << "Entity " << Typesystem::Operations::GetName(m_typeId) <<
                    ", which is ContextShared, can only be accepted from context 0.");
        }

        ScopedTypeLock lck(m_typeLocks[context]);


        m_entityStates[context].ForSpecificState(injectionState.GetInstanceId().GetRawValue(),
                                                 boost::bind(&EntityType::AcceptInjectionInternal,
                                                             this,
                                                             _2,
                                                             boost::cref(connection),
                                                             boost::ref(injectionState),
                                                             boost::cref(originalInjectionState)),
                                                 false); // false => don't include released states;


    }

    void EntityType::SetInjection(const ConnectionPtr&                 connection,
                                  const Dob::Typesystem::HandlerId&    handlerId,
                                  const Dob::Typesystem::InstanceId&   instanceId,
                                  const bool                           considerChangeFlags,
                                  const char* const                    blob,
                                  const DistributionData&              originalInjectionState)
    {
        const ContextId context = connection->Id().m_contextId;

        if (m_typeIsContextShared && context != 0)
        {
            std::wostringstream ostr;
            ostr << "SetInjection for Entity " << Typesystem::Operations::GetName(m_typeId) <<
                    ", which is ContextShared, is not allowed from context " << context << "!";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        ScopedTypeLock lck(m_typeLocks[context]);

        m_entityStates[context].ForSpecificState(instanceId.GetRawValue(),
                                                 boost::bind(&EntityType::SetInjectionInternal,
                                                             this,
                                                             _2,
                                                             boost::cref(connection),
                                                             boost::cref(handlerId),
                                                             boost::cref(instanceId),
                                                             considerChangeFlags,
                                                             blob,
                                                             boost::cref(originalInjectionState)),
                                                 false); // false => don't include released states;
    }



    void EntityType::DeleteInjection(const ConnectionPtr&                 connection,
                                     const Dob::Typesystem::HandlerId&    handlerId,
                                     const Dob::Typesystem::InstanceId&   instanceId,
                                     const DistributionData&              originalInjectionState)
    {
        const ContextId context = connection->Id().m_contextId;

        if (m_typeIsContextShared && context != 0)
        {
            std::wostringstream ostr;
            ostr << "DeleteInjection for Entity " << Typesystem::Operations::GetName(m_typeId) <<
                    ", which is ContextShared, is not allowed from context 0.";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        ScopedTypeLock lck(m_typeLocks[context]);

        m_entityStates[context].ForSpecificState(instanceId.GetRawValue(),
                                                 boost::bind(&EntityType::DeleteInjectionInternal,
                                                             this,
                                                             _2,
                                                             boost::cref(connection),
                                                             boost::cref(handlerId),
                                                             boost::cref(instanceId),
                                                             boost::cref(originalInjectionState)),
                                                  false); // No need to include already released states
    }

    void EntityType::RemoteSetInjectionEntityState(const DistributionData& entityState)
    {

        const ContextId context = entityState.GetSenderId().m_contextId;

        if (m_typeIsContextShared)
        {
            ENSURE(context == 0, << "EntityType::RemoteSetInjectionEntityState. Received Entity " <<
                                    Typesystem::Operations::GetName(m_typeId) <<
                                    ", which is ContextShared, in context " << context);
        }

        ScopedTypeLock lck(m_typeLocks[context]);

        m_clock.UpdateCurrentTimestamp(entityState.GetCreationTime());

        m_entityStates[context].ForSpecificStateAdd(entityState.GetInstanceId().GetRawValue(),
                                                    boost::bind(&EntityType::RemoteSetInjectionEntityStateInternal,
                                                                this,
                                                                boost::cref(entityState),
                                                                _2));
    }

    void EntityType::RemoteSetDeleteEntityState(const DistributionData&   entityState)
    {
        const ContextId context = entityState.GetSenderId().m_contextId;

        if (m_typeIsContextShared)
        {
            ENSURE(context == 0, << "EntityType::RemoteSetDeleteEntityState. Received Entity " <<
                                    Typesystem::Operations::GetName(m_typeId) <<
                                    ", which is ContextShared, in context " << context);
        }

        {
            ScopedTypeLock lck(m_typeLocks[context]);

            m_clock.UpdateCurrentTimestamp(entityState.GetCreationTime());

            m_entityStates[context].ForSpecificStateAdd(entityState.GetInstanceId().GetRawValue(),
                                                        boost::bind(&EntityType::RemoteSetDeleteEntityStateInternal,
                                                                    this,
                                                                    boost::cref(entityState),
                                                                    _2));
        }

        bool isGhost = !entityState.IsNoState() && entityState.GetEntityStateKind()==DistributionData::Ghost;
        if (isGhost)
        {
            CleanGhosts(entityState.GetHandlerId(), context);
        }
    }

    void EntityType::RemoteSetRealEntityState(const ConnectionPtr&      connection,
                                              const DistributionData&   entityState)
    {
        const ContextId context = entityState.GetSenderId().m_contextId;

        if (m_typeIsContextShared)
        {
            ENSURE(context == 0, << "EntityType::RemoteSetRealEntityState. Received Entity " <<
                                    Typesystem::Operations::GetName(m_typeId) <<
                                    ", which is ContextShared, in context " << context);
        }

        {
            ScopedTypeLock lck(m_typeLocks[context]);

            m_clock.UpdateCurrentTimestamp(entityState.GetCreationTime());

            m_entityStates[context].ForSpecificStateAdd(entityState.GetInstanceId().GetRawValue(),
                                                        boost::bind(&EntityType::RemoteSetRealEntityStateInternal,
                                                                    this,
                                                                    boost::cref(connection),
                                                                    boost::cref(entityState),
                                                                    _2));
        }

        bool isGhost = !entityState.IsNoState() && entityState.GetEntityStateKind()==DistributionData::Ghost;
        if (isGhost)
        {
            CleanGhosts(entityState.GetHandlerId(), context);
        }
    }

    void EntityType::Subscribe(const SubscriptionId&                subscriptionId,
                               const Dob::Typesystem::InstanceId&   instanceId,
                               const bool                           allInstances,
                               const bool                           restartSubscription,
                               const SubscriptionOptionsPtr&        subscriptionOptions)
    {
        // If it is a ContextShared type we use context 0, otherwise we use the same context as the subscriber.
        const ContextId context = m_typeIsContextShared ? 0 : subscriptionId.connectionConsumer.connection->Id().m_contextId;

        ScopedTypeLock lck(m_typeLocks[context]);

        m_entityStates[context].Subscribe(subscriptionId,
                                          instanceId.GetRawValue(),
                                          allInstances,
                                          restartSubscription,
                                          subscriptionOptions);
    }

    void EntityType::Unsubscribe(const SubscriptionId&              subscriptionId,
                                 const Dob::Typesystem::InstanceId& instanceId,
                                 const bool                         allInstances)
    {
        // If it is a ContextShared type we use context 0, otherwise we use the same context as the subscriber.
        const ContextId context = m_typeIsContextShared ? 0 : subscriptionId.connectionConsumer.connection->Id().m_contextId;

        ScopedTypeLock lck(m_typeLocks[context]);

        m_entityStates[context].Unsubscribe(subscriptionId,
                                            instanceId.GetRawValue(),
                                            allInstances);
    }

    bool EntityType::HasEntitySubscription(const ConnectionPtr&    connection,
                                           const ConsumerId&       consumer) const
    {
        SubscriptionId subscriptionId(ConnectionConsumerPair(connection, consumer), EntitySubscription, 0);

        // If it is a ContextShared type we use context 0, otherwise we use the same context as the "requestor".
        const ContextId context = m_typeIsContextShared ? 0 : subscriptionId.connectionConsumer.connection->Id().m_contextId;

        return m_entityStates[context].HasSubscription(subscriptionId);
    }

    void EntityType::UnsubscribeAll(const ConnectionPtr& connection)
    {
        // If it is a ContextShared type we use context 0, otherwise we use the same context as the "unsubscriber".
        const ContextId context = m_typeIsContextShared ? 0 : connection->Id().m_contextId;

        ScopedTypeLock lck(m_typeLocks[context]);

        m_handlerRegistrations[context].UnsubscribeAll(connection);
        m_entityStates[context].UnsubscribeAll(connection);
    }

    const DistributionData EntityType::ReadEntity(const Dob::Typesystem::InstanceId& instanceId,
                                                  const ContextId readerContext) const
    {
        // If it is a ContextShared type we use context 0, otherwise we use the same context as the reader.
        const ContextId context = m_typeIsContextShared ? 0 : readerContext;

        DistributionData realState(no_state_tag);

        // ReadEntity is not protected by a type lock, so there might be an owner in here that
        // is about to create an entity but the entity is still in the released state. As a reader we don't want
        // to skip this entity but wait for it to be set to not released. (Of course, a released entity can also
        // be just released so that case must be handled properly.) All this explains why we include
        // released states here.
        m_entityStates[context].ForSpecificState(instanceId.GetRawValue(),
                                                 boost::bind(&EntityType::ReadEntityInternal,
                                                             this,
                                                             _2,
                                                             boost::ref(realState)),
                                                 true); // true => include released states.

        if (realState.IsNoState())
        {
            std::wostringstream ostr;
            ostr << "The entity instance " <<
                Typesystem::EntityId(m_typeId,instanceId)
                << " is not created";
            throw Safir::Dob::NotFoundException(ostr.str(),__WFILE__,__LINE__);
        }

        return realState;

    }

    void EntityType::GetMostRecentGhostRegTime(const StateSharedPtr&              statePtr,
                                               const Dob::Typesystem::HandlerId&  handlerId,
                                               RegisterTime&                      mostRecentRegisterTime) const
    {
        DistributionData realState = statePtr->GetRealState();

        if (!IsGhost(statePtr))
        {
            // Not a ghost
            return;
        }

        if (handlerId != realState.GetHandlerId())
        {
            // Ghost related to another handler
            return;
        }

        if (mostRecentRegisterTime < realState.GetRegistrationTime())
        {
            mostRecentRegisterTime = realState.GetRegistrationTime();
        }
    }

    void EntityType::RemoveGhost(const StateSharedPtr&              statePtr,
                                 const Dob::Typesystem::HandlerId&  handlerId,
                                 const RegisterTime&                mostRecentRegisterTime) const
    {
        DistributionData realState = statePtr->GetRealState();

        if (!IsGhost(statePtr))
        {
            // Not a ghost
            return;
        }

        if (handlerId != realState.GetHandlerId())
        {
            // Ghost related to another handler
            return;
        }

        if (realState.GetRegistrationTime() < mostRecentRegisterTime)
        {
            statePtr->SetReleased(true);
            DistributionData newRealState(no_state_tag);
            newRealState = realState.GetEntityStateCopy(false); //we dont want the blob, since this is a delete.

            //This is a ghost and has no owner. So probably it would be more correct to set explicitly deleted to false.
            //But this is a quick-fix to make DOPE react and delete this entity permanently without bigger changes.
            newRealState.SetExplicitlyDeleted(true);

            statePtr->SetRealState(newRealState);
        }
    }

    void EntityType::FindAllHandlers(const StateSharedPtr& statePtr, HandlerSet& handlers) const
    {
        bool gotIt=false;
        Dob::Typesystem::HandlerId handler;

        GetHandlerOfInstanceInternal(statePtr, handler, true, gotIt);
        if (gotIt)
        {
            handlers.insert(handler);
        }
    }

    void EntityType::CleanGhosts(const Dob::Typesystem::HandlerId&  handlerId,
                                 const ContextId                    context)
    {
        ScopedTypeLock lck(m_typeLocks[context]);

        HandlerSet handlers;
        if (handlerId==Safir::Dob::Typesystem::HandlerId::ALL_HANDLERS)
        {
            //We want to clean old ghost for all handlers. First find out which handlers exist.
            m_entityStates[context].ForEachState(boost::bind(&EntityType::FindAllHandlers, this, _2, boost::ref(handlers)), false);
        }
        else
        {
            //Clean ghost for a specific handler only.
            handlers.insert(handlerId);
        }

        //Go through all handlers, one by one, and clean old ghosts.
        for (HandlerSet::const_iterator it = handlers.begin(); it!=handlers.end(); ++it)
        {
            RegisterTime mostRecentRegisterTime;

            // Make a first pass to find the most recent registration time for a ghost.
            m_entityStates[context].ForEachState(boost::bind(&EntityType::GetMostRecentGhostRegTime,
                                                             this,
                                                             _2,
                                                             boost::cref(handlerId),
                                                             boost::ref(mostRecentRegisterTime)),
                                                 false); // false => Do not include released states

            // Make a second pass and remove (set the state to released) any ghost that is older
            // than the most recent registration time for any ghost.
            m_entityStates[context].ForEachState(boost::bind(&EntityType::RemoveGhost,
                                                                 this,
                                                                 _2,
                                                                 boost::cref(handlerId),
                                                                 boost::cref(mostRecentRegisterTime)),
                                                 false); // false => Do not include released states
        }
    }

    const Dob::Typesystem::HandlerId EntityType::GetHandlerOfInstance(const Dob::Typesystem::InstanceId& instanceId,
                                                                      const ContextId requestorContext) const
    {
        // If it is a ContextShared type we use context 0, otherwise we use the same context as the "requestor".
        const ContextId context = m_typeIsContextShared ? 0 : requestorContext;

        Dob::Typesystem::HandlerId handlerId;
        bool gotIt = false;

        // For an explanation regarding why we include released states here see the corresponding comment for ReadEntiy
        m_entityStates[context].ForSpecificState(instanceId.GetRawValue(),
                                                 boost::bind(&EntityType::GetHandlerOfInstanceInternal,
                                                             this,
                                                             _2,
                                                             boost::ref(handlerId),
                                                             false,
                                                             boost::ref(gotIt)),
                                                 true); // true => include released states

        if (!gotIt)
        {
            const Typesystem::EntityId eid(GetTypeId(),instanceId);
            std::wostringstream ostr;
            ostr << "Instance not found: " << eid;
            throw Safir::Dob::NotFoundException(ostr.str(),__WFILE__,__LINE__);
        }

        return handlerId;
    }

    bool EntityType::IsCreated(const Dob::Typesystem::InstanceId& instanceId, const ContextId requestorContext) const
    {
        // If it is a ContextShared type we use context 0, otherwise we use the same context as the "requestor".
        const ContextId context = m_typeIsContextShared ? 0 : requestorContext;

        bool isCreated = false;

        // For an explanation regarding why we include released states here see the corresponding comment for ReadEntiy
        m_entityStates[context].ForSpecificState(instanceId.GetRawValue(),
                                                 boost::bind(&EntityType::IsCreatedInternal,
                                                             this,
                                                             _2,
                                                             boost::ref(isCreated)),
                                                 true); // true => include released states

        return isCreated;
    }

    bool EntityType::IsOwner(const Dob::Typesystem::InstanceId& instanceId,
                             const Dob::Typesystem::HandlerId&  handlerId,
                             const ConnectionConsumerPair&      registerer) const
    {
        // If it is a ContextShared type we use context 0, otherwise we use the same context as the registerer.
        const ContextId context = m_typeIsContextShared ? 0 : registerer.connection->Id().m_contextId;

        bool isOwner = false;

        // For an explanation regarding why we include released states here see the corresponding comment for ReadEntiy
        m_entityStates[context].ForSpecificState(instanceId.GetRawValue(),
                                                 boost::bind(&EntityType::IsOwnerInternal,
                                                             this,
                                                             _2,
                                                             boost::cref(handlerId),
                                                             boost::cref(registerer),
                                                             boost::ref(isOwner)),
                                                 true); // true => include released states

        return isOwner;
    }

    void EntityType::SetEntityInternal(const StateSharedPtr&                statePtr,
                                       const ConnectionPtr&                 connection,
                                       const Dob::Typesystem::HandlerId&    handlerId,
                                       const Dob::Typesystem::InstanceId&   instanceId,
                                       const bool                           considerChangeFlags,
                                       const char* const                    blob)
    {
        ENSURE(blob != NULL, << "Trying to call a SetEntity with a NULL blob! connId = " << connection->Id());

        EntityAccessStatus accessStatus = VerifyEntityAccess(statePtr,
                                                             connection,
                                                             handlerId);
        switch (accessStatus)
        {
            case AccessOk:
            break;

            case HandlerRevoked:
            {
                // The handler doesn't know that its handler registration just has  been revoked,
                // so just let the connection think that the set succeeded.
                return;
            }
            break;

            case HandlerNotRegistered:
            {
                std::wostringstream ostr;
                ostr << "SetEntity called with handler that is not previously registered. (typeId = "
                     << Typesystem::Operations::GetName(m_typeId)
                     << ", connId = " << connection->Id()
                     << ", handlerId = " << handlerId << ")";
                throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);

            }
            break;

            case InstanceOwnedByOtherHandler:
            {
                std::wostringstream ostr;
                ostr << "SetEntity not allowed, instance owned by another handler. (typeId = "
                     << Typesystem::Operations::GetName(m_typeId)
                     << ", instanceId = " << instanceId
                     << ", handlerId = " << handlerId << ")";
                throw Safir::Dob::AccessDeniedException(ostr.str(),__WFILE__,__LINE__);

            }
            break;

            case InstanceIsGhost:
            {
                std::wostringstream ostr;
                ostr << "SetEntity not allowed, persistence exists. (typeId = "
                     << Typesystem::Operations::GetName(m_typeId)
                     << ", instanceId = " << instanceId
                     << ", handlerId = " << handlerId << ")";
                throw Safir::Dob::GhostExistsException(ostr.str(),__WFILE__,__LINE__);

            }
            break;
        }

        SetEntityLocal(statePtr,
                       connection,
                       handlerId,
                       instanceId,
                       considerChangeFlags,
                       blob,
                       DistributionData(no_state_tag));  // No injection state to consider in this case (is used to set correct top timestamps)
    }

    void EntityType::DeleteEntityInternal(const StateSharedPtr&                statePtr,
                                          const ConnectionPtr&                 connection,
                                          const Dob::Typesystem::HandlerId&    handlerId,
                                          const Dob::Typesystem::InstanceId&   instanceId)
    {
        EntityAccessStatus accessStatus = VerifyEntityAccess(statePtr,
                                                             connection,
                                                             handlerId);
        switch (accessStatus)
        {
            case AccessOk:
            break;

            case HandlerRevoked:
            {
                // The handler doesn't know that its handler registration just has been revoked,
                // so just let the connection think that the delete succeeded.
                return;
            }
            break;

            case HandlerNotRegistered:
            {
                std::wostringstream ostr;
                ostr << "DeleteEntity called with handler that is not previously registered. (typeId = "
                     << Typesystem::Operations::GetName(m_typeId)
                     << ", connId = " << connection->Id()
                     << ", handlerId = " << handlerId << ")";
                throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
            }
            break;

            case InstanceOwnedByOtherHandler:
            {
                std::wostringstream ostr;
                ostr << "DeleteEntity not allowed, instance owned by another handler. (typeId = "
                     << Typesystem::Operations::GetName(m_typeId)
                     << ", instanceId = " << instanceId
                     << ", handlerId = " << handlerId << ")";
                throw Safir::Dob::AccessDeniedException(ostr.str(),__WFILE__,__LINE__);
            }
            break;

            case InstanceIsGhost:
            {
                std::wostringstream ostr;
                ostr << "DeleteEntity not allowed, persistence exists. (typeId = "
                     << Typesystem::Operations::GetName(m_typeId)
                     << ", instanceId = " << instanceId
                     << ", handlerId = " << handlerId << ")";
                throw Safir::Dob::GhostExistsException(ostr.str(),__WFILE__,__LINE__);

            }
            break;
        }

        DeleteEntityLocal(statePtr,
                          connection->Id().m_contextId,
                          handlerId,
                          instanceId,
                          DistributionData(no_state_tag));  // No injection state to consider in this case (is used to set correct top timestamps)

    }

    void EntityType::DeleteAllInstancesInternal(const StateSharedPtr&               statePtr,
                                                const ConnectionPtr&                connection,
                                                const Dob::Typesystem::HandlerId&   handlerId,
                                                bool&                               exitDispatch)
    {
        exitDispatch = false;

        ConnectionConsumerPair owner;
        Typesystem::HandlerId  ownerHandlerId;
        bool hasOwner;

        GetOwner(statePtr, owner, ownerHandlerId, hasOwner);

        if ((hasOwner && (owner.connection->Id() != connection->Id() || ownerHandlerId != handlerId)) ||
            IsGhost(statePtr) ||
            !IsCreated(statePtr))
        {
            return;
        }

        DeleteEntityInternal(statePtr,
                             connection,
                             handlerId,
                             statePtr->GetRealState().GetInstanceId());

    }

    void EntityType::SetInitalGhostInternal(const StateSharedPtr&                statePtr,
                                            const ConnectionPtr&                 connection,
                                            const Dob::Typesystem::HandlerId&    handlerId,
                                            const Dob::Typesystem::InstanceId&   instanceId,
                                            const char* const                    blob)
    {
        ENSURE(blob != NULL, << "Trying to do a SetInitalGhostEntity with a NULL blob! connId = " << connection->Id());

        // Shouldn't check owner in this case

        DistributionData realState = statePtr->GetRealState();

        if (realState.IsNoState() || statePtr->IsReleased())
        {
            DistributionData newRealState =
                DistributionData(entity_state_tag,
                                 ConnectionId(ThisNodeParameters::NodeNumber(), connection->Id().m_contextId, -1), // Correct node number and context but no connection id for ghost states
                                 m_typeId,
                                 handlerId,
                                 LamportTimestamp(),             // an "old" registration time
                                 instanceId,
                                 m_clock.GetNewTimestamp(),      // creation time
                                 DistributionData::Ghost,
                                 false,                          // false => not explicitly deleted
                                 true,                           // true => source is permanent store
                                 blob);

            // Set the distribution state in the container state. All subscribers will be notified.
            statePtr->SetRealState(newRealState);

            statePtr->SetReleased(false);
        }
    }

    void EntityType::InjectEntityInternal(const StateSharedPtr&                statePtr,
                                          const ConnectionPtr&                 connection,
                                          const Dob::Typesystem::HandlerId&    handlerId,
                                          const Dob::Typesystem::InstanceId&   instanceId,
                                          const char* const                    blob,
                                          const Dob::Typesystem::Int64         timestamp)
    {
        DistributionData injectionState = statePtr->GetInjectionState();

        bool handlerIdOk = true;

        if (IsCreated(statePtr) || IsGhost(statePtr))
        {
            // In this case we can check that the injected handlerId corresponds to
            // the handler id in the existing or ghost instance.

            if (handlerId != statePtr->GetRealState().GetHandlerId())
            {
                handlerIdOk = false;
            }
        }
        else if (!injectionState.IsNoState() && handlerId != injectionState.GetHandlerId())
        {
            handlerIdOk = false;
        }

        if (!handlerIdOk)
        {
            std::wostringstream ostr;
            ostr << "Handler Id of the injector doesn't match the Handler Id used in this system. typeId = "
                 << Typesystem::Operations::GetName(m_typeId)
                 << " handlerId = " << handlerId << " instanceId = " << instanceId << ")";
            throw Safir::Dob::Typesystem::ConfigurationErrorException(ostr.str(),__WFILE__,__LINE__);
        }

        DistributionData newInjectionState(no_state_tag);

        if (injectionState.IsNoState())
        {
            // There is no existing injection state, create a new one
            newInjectionState = DistributionData(entity_state_tag,
                                                 // Dummy connectionId for injection states, but correct node number and context
                                                 ConnectionId(ThisNodeParameters::NodeNumber(), connection->Id().m_contextId, -1),
                                                 m_typeId,
                                                 handlerId,
                                                 LamportTimestamp(),            // dummy registration time for injection states
                                                 instanceId,
                                                 m_clock.GetNewTimestamp(),     // creation time
                                                 DistributionData::Injection,
                                                 false,                          // false => not explicitly deleted
                                                 false,                          // dummy (source is permanent store)
                                                 blob);

            // Set time for changed members and set timestamp for unchanged members to 0.
            TimestampOperations::SetTimestampForChangedMembers(newInjectionState,
                                                               timestamp,
                                                               true); // true => set timestamp for unchanged members to 0
            statePtr->SetReleased(false);
        }
        else
        {
            // There is an existing injection state

            ENSURE(!statePtr->IsReleased(), << "EntityType::InjectEntityInternal. "
                                           << "Found a released state with an existing injection!");

            // Create a copy based on timestamps
            const TimestampOperations::MergeResult mergeResult = TimestampOperations::Merge(injectionState,
                                                                                            blob,
                                                                                            timestamp);
            if (mergeResult.second == false)
            {
                // No merge has taken place
                return;
            }

            newInjectionState = mergeResult.first;

            // Dummy connectionId for injection states, but correct node number and context
            newInjectionState.SetSenderId(ConnectionId(ThisNodeParameters::NodeNumber(), connection->Id().m_contextId, -1));
            newInjectionState.SetHandlerId(handlerId);
        }

        const DistributionData& realState = statePtr->GetRealState();
        if (!realState.IsNoState() && TimestampOperations::HaveChanges(realState,newInjectionState))
        {
            // All the stuff in the new injection state is already in the real state. We can drop the new injection state.
            return;
        }


        // Set new injection state
        statePtr->SetInjectionState(newInjectionState);

        KickRegisterer(handlerId, connection->Id().m_contextId);
    }

    void EntityType::InjectDeletedEntityInternal(const StateSharedPtr&                statePtr,
                                                 const ConnectionPtr&                 connection,
                                                 const Dob::Typesystem::HandlerId&    handlerId,
                                                 const Dob::Typesystem::InstanceId&   instanceId,
                                                 const Dob::Typesystem::Int64         timestamp)
    {
        if (IsCreated(statePtr) || IsGhost(statePtr))
        {
            // In this case we can check that the injected handlerId corresponds to
            // the handler id in the existing or ghost instance.

            if (handlerId != statePtr->GetRealState().GetHandlerId())
            {
                std::wostringstream ostr;
                ostr << "Injected delete Handler Id doesn't match the Handler Id used in this system. typeId = "
                     << Typesystem::Operations::GetName(m_typeId)
                     << " handlerId = " << handlerId << " instanceId = " << instanceId << ")";
                throw Safir::Dob::Typesystem::ConfigurationErrorException(ostr.str(),__WFILE__,__LINE__);
            }
        }

        DistributionData injectionState = statePtr->GetInjectionState();
        DistributionData newInjectionState(no_state_tag);

        if (injectionState.IsNoState())
        {
            // There is no existing injection state, create a deleted state
            newInjectionState = DistributionData(entity_state_tag,
                                                 ConnectionId(ThisNodeParameters::NodeNumber(), connection->Id().m_contextId, -1),
                                                 m_typeId,
                                                 handlerId,
                                                 LamportTimestamp(),            // No registration time for injection states
                                                 instanceId,
                                                 m_clock.GetNewTimestamp(),     // creation time
                                                 DistributionData::Injection,
                                                 true,                           // true => explicitly deleted
                                                 false,                          // dummy (source is permanent store)
                                                 NULL);                          // no blob

            // Set top timestamp
            TimestampOperations::SetTimestampForAllMembers(newInjectionState,
                                                           timestamp);
        }
        else
        {
            // There is an existing injection state

            // Create a copy based on timestamps
            const TimestampOperations::MergeResult mergeResult =
                TimestampOperations::Merge(injectionState,
                                           NULL, // no blob
                                           timestamp);

            if (mergeResult.second == false)
            {
                // No merge has taken place
                return;
            }

            newInjectionState = mergeResult.first;

            // Dummy connectionId for injection states, but correct node number and context.
            newInjectionState.SetSenderId(ConnectionId(ThisNodeParameters::NodeNumber(), connection->Id().m_contextId, -1));
            newInjectionState.SetHandlerId(handlerId);
        }


        const DistributionData& realState = statePtr->GetRealState();
        if (!realState.IsNoState() && TimestampOperations::HaveChanges(realState,newInjectionState))
        {
            // All the stuff in the new injection state is already in the real state.
            // We can drop the new injection state.
            return;
        }

        // Set new injection state
        statePtr->SetInjectionState(newInjectionState);

        KickRegisterer(handlerId, connection->Id().m_contextId);

        // Ensure that the state will be kept in the container.
        statePtr->SetReleased(false);
    }

    void EntityType::AcceptInjectionInternal(const StateSharedPtr&          statePtr,
                                             const ConnectionPtr&           connection,
                                             DistributionData&              injectionState,
                                             const DistributionData&        originalInjectionState)
    {
        if (originalInjectionState == statePtr->GetInjectionState())
        {
            // No new injections has been done while we were dispatching the injection to the app.
            // In this case we can remove the injection state ...
            statePtr->SetInjectionState(DistributionData(no_state_tag));
        }

        Dob::Typesystem::HandlerId handlerId = injectionState.GetHandlerId();

        EntityAccessStatus accessStatus = VerifyEntityAccess(statePtr,
                                                             connection,
                                                             handlerId);

        switch (accessStatus)
        {
            case AccessOk:
            case InstanceIsGhost:
            break;

            case HandlerRevoked:
            case HandlerNotRegistered:
            case InstanceOwnedByOtherHandler:
            {
                // The handler doesn't know that its handler registration just has been revoked,
                // so just let the connection think that the accept succeeded.
                return;
            }
            break;
        }

        DistributionData realState = statePtr->GetRealState();

        {
            const ContextId context = connection->Id().m_contextId;

            // Get the registration state. The state will be locked within this scope.
            LockedStateResult lockedStateResult =
                m_handlerRegistrations[context].GetLockedRegistrationState(handlerId,
                                                                           false); // false => don't include released states

            // Get  a better name
            const StateSharedPtr& regStatePtr = lockedStateResult.first;

            if (regStatePtr == NULL || !regStatePtr->GetRealState().IsRegistered())
            {
                // The registerer must have been overregistered.
                return;
            }

            RegisterTime registrationTime = regStatePtr->GetRealState().GetRegistrationTime();

            if (!realState.IsCreated() || realState.GetRegistrationTime() < registrationTime)
            {
                // There is no existing real state or the owner has made a unregister followed by a register
                // in the OnInjected... calback. Create a new one.

                statePtr->SetConnection(connection);
                statePtr->SetConsumer(regStatePtr->GetConsumer());
                statePtr->SetOwnerRequestInQueue(connection->AddRequestInQueue(regStatePtr->GetConsumer()));

                // Set some data in the injection so it becomes a new state.
                injectionState.SetRegistrationTime(registrationTime);
                injectionState.ResetVersion();
                injectionState.SetCreationTime(m_clock.GetNewTimestamp());
            }
            else
            {
                // Update an existing state
                injectionState.IncrementVersion();
            }

            // Clear change flags before storing
            injectionState.SetChangeFlags(false);

            statePtr->SetRealState(injectionState);
        }   // Registration state is released here

        if (!injectionState.HasBlob())
        {
            // It's an accept of an injected delete

            if (statePtr->GetInjectionState().IsNoState())
            {
                // There is no unhandled injection state so the
                // state could be released
                statePtr->SetReleased(true);

                statePtr->SetConnection(ConnectionPtr());  // No connection since its a delete state
                statePtr->SetConsumer(ConsumerId(NULL, static_cast<short>(0))); //dummy consumer

                // The released end state must be saved "a while".
                EndStates::Instance().Add(statePtr);
            }
        }
    }

    void EntityType::SetInjectionInternal(const StateSharedPtr&                statePtr,
                                          const ConnectionPtr&                 connection,
                                          const Dob::Typesystem::HandlerId&    handlerId,
                                          const Dob::Typesystem::InstanceId&   instanceId,
                                          const bool                           considerChangeFlags,
                                          const char* const                    blob,
                                          const DistributionData&              originalInjectionState)
    {
        EntityAccessStatus accessStatus = VerifyEntityAccess(statePtr,
                                                             connection,
                                                             handlerId);
        switch (accessStatus)
        {
            case AccessOk:
            case InstanceIsGhost:
            break;

            case HandlerRevoked:
            {
                // The handler doesn't know that its handler registration just has  been revoked,
                // so just let the connection think that the set succeeded.
                return;
            }
            break;

            case HandlerNotRegistered:
            {
                std::wostringstream ostr;
                ostr << "SetEntity (in an OnInjected... callback) called with handler that is not previously registered. (typeId = "
                     << Typesystem::Operations::GetName(m_typeId)
                     << ", connId = " << connection->Id()
                     << ", handlerId = " << handlerId << ")";
                throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);

            }
            break;

            case InstanceOwnedByOtherHandler:
            {
                std::wostringstream ostr;
                ostr << "SetEntity (in an OnInjected... callback) not allowed, instance owned by another handler. (typeId = "
                     << Typesystem::Operations::GetName(m_typeId)
                     << ", instanceId = " << instanceId
                     << ", handlerId = " << handlerId << ")";
                throw Safir::Dob::AccessDeniedException(ostr.str(),__WFILE__,__LINE__);

            }
            break;
        }

        SetEntityLocal(statePtr,
                       connection,
                       handlerId,
                       instanceId,
                       considerChangeFlags,
                       blob,
                       originalInjectionState);  // The original injection state is used to get the top timestamps correct

        if (originalInjectionState == statePtr->GetInjectionState())
        {
            // No new injections has been done while we were dispatching the injection to the app.
            // In this case we can remove the injection state.
            statePtr->SetInjectionState(DistributionData(no_state_tag));
        }
    }

    void EntityType::DeleteInjectionInternal(const StateSharedPtr&                  statePtr,
                                             const ConnectionPtr&                   connection,
                                             const Dob::Typesystem::HandlerId&      handlerId,
                                             const Dob::Typesystem::InstanceId&     instanceId,
                                             const DistributionData&                originalInjectionState)
    {
        EntityAccessStatus accessStatus = VerifyEntityAccess(statePtr,
                                                             connection,
                                                             handlerId);
        switch (accessStatus)
        {
            case AccessOk:
            break;

            case HandlerRevoked:
            {
                // The handler doesn't know that its handler registration just has been revoked,
                // so just let the connection think that the delete succeeded.
                return;
            }
            break;

            case HandlerNotRegistered:
            {
                std::wostringstream ostr;
                ostr << "DeleteEntity (in an OnInjected... callback) called with handler that is not previously registered. (typeId = "
                     << Typesystem::Operations::GetName(m_typeId)
                     << ", connId = " << connection->Id()
                     << ", handlerId = " << handlerId << ")";
                throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
            }
            break;

            case InstanceOwnedByOtherHandler:
            {
                std::wostringstream ostr;
                ostr << "DeleteEntity (in an OnInjected... callback) not allowed, instance owned by another handler. (typeId = "
                     << Typesystem::Operations::GetName(m_typeId)
                     << ", instanceId = " << instanceId
                     << ", handlerId = " << handlerId << ")";
                throw Safir::Dob::AccessDeniedException(ostr.str(),__WFILE__,__LINE__);
            }
            break;

            case InstanceIsGhost:
            break;  // This is what we expect here
        }

        DeleteEntityLocal(statePtr,
                          connection->Id().m_contextId,
                          handlerId,
                          instanceId,
                          originalInjectionState); // is used to get correct top timestamps
    }

    void EntityType::RemoteSetInjectionEntityStateInternal(const DistributionData&        remoteEntity,
                                                           const StateSharedPtr&          statePtr)
    {
        DistributionData injectionState = statePtr->GetInjectionState();
        DistributionData newInjectionState(no_state_tag);

        if (injectionState.IsNoState())
        {
            // There is no existing injection state, create a new one
            newInjectionState = remoteEntity;
            statePtr->SetReleased(false);
        }
        else
        {
            // There is an existing injection state

            ENSURE(!statePtr->IsReleased(), << "EntityType::RemoteSetInjectionEntityStateInternal. "
                                            << "Found a released state with an existing injection!");

            if (injectionState.GetHandlerId() != remoteEntity.GetHandlerId())
            {
                std::wostringstream ostr;
                ostr << "Handler Id of the injection doesn't match the Handler Id of the existing injection. typeId = "
                     << Typesystem::Operations::GetName(m_typeId)
                     << " existing injection handlerId = " << injectionState.GetHandlerId()
                     << " new injection handlerId = " << remoteEntity.GetHandlerId()
                     << " instanceId = " << remoteEntity.GetInstanceId() << ")";
                throw Safir::Dob::Typesystem::ConfigurationErrorException(ostr.str(),__WFILE__,__LINE__);
            }

            // Create a copy based on timestamps
            const TimestampOperations::MergeResult mergeResult = TimestampOperations::Merge(injectionState,
                                                                                            remoteEntity);

            if (mergeResult.second == false)
            {
                // No merge has taken place
                return;
            }

            newInjectionState = mergeResult.first;
        }

        const DistributionData& realState = statePtr->GetRealState();
        if (!realState.IsNoState() && TimestampOperations::HaveChanges(realState,newInjectionState))
        {
            // All the stuff in the new injection state is already in the real state.
            // We can drop the new injection state.
            return;
        }

        statePtr->SetInjectionState(newInjectionState);

        KickRegisterer(newInjectionState.GetHandlerId(), remoteEntity.GetSenderId().m_contextId);
    }

    void EntityType::RemoteSetDeleteEntityStateInternal(const DistributionData&         remoteEntity,
                                                        const StateSharedPtr&           statePtr)
    {
        bool needToCheckRegistrationState = true;

        DistributionData localEntity = statePtr->GetRealState();

        if (!localEntity.IsNoState())
        {
            // There is an existing entity state

            if (remoteEntity.GetRegistrationTime() < localEntity.GetRegistrationTime())
            {
                lllog(3) << "Skipping remote delete state that belongs to an old registration" << std::endl;
                return;
            }
            else if (localEntity.GetRegistrationTime() < remoteEntity.GetRegistrationTime())
            {
                // The remote delete state belongs to a newer registration state. Skip it if
                // the local entity is not a ghost.
                if (localEntity.GetEntityStateKind() != DistributionData::Ghost)
                {
                    auto senderId = remoteEntity.GetSenderId();

                    SEND_SYSTEM_LOG(Alert,
                                    << "Got a delete entity state from sender  "
                                    << "(Id:" << senderId.m_id << " Node:" << senderId.m_node << " Ctx:"
                                    << senderId.m_contextId << ") with a reg time that is newer than"
                                    << " the current reg time");
                    throw std::logic_error("Received delete entity state for which the registration is unknown");
                }

                //if localEntity is a ghost, then just delete it the normal way.
            }
            else
            {
                // The existing local entity state (created or deleted) has the same registration time
                // as the remote entity state.
                if (!RemoteEntityStateIsAccepted(remoteEntity, localEntity))
                {
                    lllog(3) << "Discard remote delete state since the existing state is newer" << std::endl;
                    return;
                }

                if (localEntity.IsCreated())
                {
                    // The local entity is created and therefor we know that the registration time in the entity state corresponds
                    // to the active registration and we don't have to fetch the registration below.
                    needToCheckRegistrationState = false;
                }
            }
        }

        LockedStateResult lockedRegStateResult;

        if (needToCheckRegistrationState)
        {
            const ContextId context = remoteEntity.GetSenderId().m_contextId;

            lockedRegStateResult =
                m_handlerRegistrations[context].GetLockedRegistrationState(remoteEntity.GetHandlerId(),
                                                                           true); // true => include released states

            // Get  a better name
            const StateSharedPtr& regStatePtr = lockedRegStateResult.first;

            if (regStatePtr == NULL ||
                regStatePtr->GetRealState().IsNoState() ||
                !regStatePtr->GetRealState().IsRegistered() ||
                regStatePtr->GetRealState().GetRegistrationTime() < remoteEntity.GetRegistrationTime())
            {
                // There is no active registration or it is an old one. It's ok to set the state
            }
            else if (remoteEntity.GetRegistrationTime() < regStatePtr->GetRealState().GetRegistrationTime())
            {
                lllog(3) << "Skipping remote delete state that belongs to an old registration" << std::endl;
                return;
            }
        }

        //Check if we have an injection state that can be cleared
        const DistributionData injectionState = statePtr->GetInjectionState();

        if (injectionState.IsNoState())
        {
            //we can release the state since there is no injection
            statePtr->SetReleased(true);
        }
        else
        {
            // We have an injection state

            if (TimestampOperations::HaveChanges(remoteEntity, injectionState))
            {
                // The remote entity already has all what is in the injection state, so the injection state can be removed
                // and the state released.
                statePtr->SetInjectionState(DistributionData(no_state_tag));
                statePtr->SetReleased(true);
            }
        }

        // If we got this far everything is ok. Set the remote delete state.
        statePtr->SetConnection(ConnectionPtr());  // No connection since its a delete state
        statePtr->SetConsumer(ConsumerId(NULL, static_cast<short>(0))); //dummy consumer
        statePtr->SetRealState(remoteEntity);

        // This is an end state so it must be saved "a while".
        EndStates::Instance().Add(statePtr);
    }

    void EntityType::RemoteSetRealEntityStateInternal(const ConnectionPtr&           connection,
                                                      const DistributionData&        remoteEntity,
                                                      const StateSharedPtr&          statePtr)
    {
        bool needToCheckRegistrationState = true;

        if (!remoteEntity.IsNoState() &&
            remoteEntity.GetEntityStateKind()==DistributionData::Ghost &&
            remoteEntity.SourceIsPermanentStore())
        {
            needToCheckRegistrationState = false; //Ghosts from DOPE dont need a registration to be set.
        }

        DistributionData localEntity = statePtr->GetRealState();

        if (!localEntity.IsNoState())
        {
            // There is an existing entity state

            if (remoteEntity.GetRegistrationTime() < localEntity.GetRegistrationTime())
            {
                lllog(3) << "Skipping remote entity state since the existing local entity state "
                            "(created, deleted or ghost) belongs to a newer registration" << std::endl;
                return;
            }
            else if (localEntity.GetRegistrationTime() < remoteEntity.GetRegistrationTime())
            {
                // The existing local entity state (created, deleted or ghost) belongs to an older
                // registration wich means that we can accept this state.

                if (localEntity.IsCreated())
                {
                    // The entity is created and therefor we know that the registration time in the entity
                    // state corresponds to the active registration. Since this time is older than
                    // the received time we haven't got the registration yet.
                    auto senderId = remoteEntity.GetSenderId();

                    SEND_SYSTEM_LOG(Alert,
                                    << "Got an entity state from sender  "
                                    << "(Id:" << senderId.m_id << " Node:" << senderId.m_node << " Ctx:"
                                    << senderId.m_contextId << ") with a reg time that is newer than"
                                    << " the current reg time");
                    throw std::logic_error("Received entity state for which the registration is unknown");
                }
            }
            else
            {
                // The existing local entity state (created, deleted or ghost) has the same registration time
                // as the remote entity state.

                // Compare entity creation time and version
                if (!RemoteEntityStateIsAccepted(remoteEntity, localEntity))
                {
                    lllog(3) << "Discard remote entity state since the existing state is newer" << std::endl;
                    return;
                }

                if (localEntity.IsCreated())
                {
                    // The local entity is created and therefor we know that the registration time in the entity state corresponds
                    // to the active registration and we don't have to fetch the registration below.
                    needToCheckRegistrationState = false;
                }
            }
        }

        LockedStateResult lockedRegStateResult;

        if (needToCheckRegistrationState)
        {
            const ContextId context = remoteEntity.GetSenderId().m_contextId;

            lockedRegStateResult =
                m_handlerRegistrations[context].GetLockedRegistrationState(remoteEntity.GetHandlerId(),
                                                                           true); // true => include released states

            // Get  a better name
            const StateSharedPtr& regStatePtr = lockedRegStateResult.first;

            if (regStatePtr == NULL ||
                regStatePtr->GetRealState().IsNoState() ||
                regStatePtr->GetRealState().GetRegistrationTime() < remoteEntity.GetRegistrationTime())
            {
                // There is no registration state or it is an old one
                auto senderId = remoteEntity.GetSenderId();

                SEND_SYSTEM_LOG(Alert,
                                << "Got an entity state from sender  "
                                << "(Id:" << senderId.m_id << " Node:" << senderId.m_node << " Ctx:"
                                << senderId.m_contextId << ") with a reg time that is either not known or is "
                                << "newer than the current reg time");
                throw std::logic_error("Received entity state for which the registration is unknown or newer "
                                       "than the current one");
            }
            else if (remoteEntity.GetRegistrationTime() < regStatePtr->GetRealState().GetRegistrationTime())
            {
                // There is already a newer registration state
                lllog(3) << "Skipping remote entity state since it belongs to an old registration" << std::endl;
                return;
            }
        }

        //Check if we have an injection state that can be cleared
        const DistributionData injectionState = statePtr->GetInjectionState();
        if (!injectionState.IsNoState() && TimestampOperations::HaveChanges(remoteEntity, injectionState))
        {
            statePtr->SetInjectionState(DistributionData(no_state_tag));
        }

        // If we got this far everything is ok. Set the remote state.
        statePtr->SetConnection(connection);
        statePtr->SetConsumer(ConsumerId(NULL, static_cast<short>(0))); //dummy consumer for remote entities
        statePtr->SetRealState(remoteEntity);

        statePtr->SetReleased(false);
    }


    void EntityType::SetEntityLocal(const StateSharedPtr&                statePtr,
                                    const ConnectionPtr&                 connection,
                                    const Dob::Typesystem::HandlerId&    handlerId,
                                    const Dob::Typesystem::InstanceId&   instanceId,
                                    const bool                           considerChangeFlags,
                                    const char* const                    blob,
                                    const DistributionData&              injectionState)
    {
        DistributionData realState = statePtr->GetRealState();

        // Since this connection has been verified to be the owner (update case) with
        // the entity state locked, an overregistration of this instance can't take place and
        // it is therefor safe to get the registerer without a registration state lock.
        ConnectionConsumerPair          registerer;
        RegisterTime                    registrationTime;
        const ContextId context = connection->Id().m_contextId;
        m_handlerRegistrations[context].GetRegisterer(handlerId, registerer, registrationTime);

        DistributionData newRealState(no_state_tag);

        if (!realState.IsCreated() || statePtr->IsReleased())
        {
            // There is no existing real state. Create a new one.

            statePtr->SetConnection(connection);
            statePtr->SetConsumer(registerer.consumer);
            statePtr->SetOwnerRequestInQueue(connection->AddRequestInQueue(registerer.consumer));

            newRealState = DistributionData(entity_state_tag,
                                            connection->Id(),
                                            m_typeId,
                                            handlerId,
                                            registrationTime,
                                            instanceId,
                                            m_clock.GetNewTimestamp(),      // creation time
                                            DistributionData::Real,
                                            false,                          // false => not explicitly deleted
                                            false,                          // false => source is not permanent store
                                            blob);
        }
        else
        {
            // Update an existing state

            // Create a copy that includes the new blob
            newRealState = realState.GetEntityStateCopy(blob);

            newRealState.IncrementVersion();
            newRealState.SetExplicitlyDeleted(false);
            newRealState.ResetSourceIsPermanentStore();
        }

        // For an injectable type the timestamps (monotonic clock) must be set.
        if (m_injectionKind == InjectionKind::Injectable)
        {
            DistributionData timeBase(no_state_tag);

            if (injectionState.IsNoState())
            {
                // There is no injection state so base the timestamps on the  real state.
                timeBase = realState;
            }
            else
            {
                // Base the timestamps on the existing injection state
                timeBase = injectionState;
            }

            SetTimestamps(newRealState,
                          timeBase,
                          considerChangeFlags);
        }

        // Reset change flags in blob before storing
        newRealState.SetChangeFlags(false);

        // Set new state, subscribers will be notified
        statePtr->SetRealState(newRealState);

        statePtr->SetReleased(false);
    }

    void EntityType::DeleteEntityLocal(const StateSharedPtr&                statePtr,
                                       const ContextId                      context,
                                       const Dob::Typesystem::HandlerId&    handlerId,
                                       const Dob::Typesystem::InstanceId&   instanceId,
                                       const DistributionData&              injectionState)
    {
        statePtr->SetReleased(true);

        DistributionData realState = statePtr->GetRealState();

        if (realState.IsNoState() && injectionState.IsNoState())
        {
            return;
        }

        // Since this connection has been verified to be the owner with
        // the entity state locked, an overregistration of this instance can't take place and
        // it is therefor safe to get the registerer without a registration state lock.
        ConnectionConsumerPair          registerer;
        RegisterTime                    registrationTime;
        m_handlerRegistrations[context].GetRegisterer(handlerId, registerer, registrationTime);

        DistributionData newRealState(no_state_tag);

        if (!realState.IsCreated())
        {
            // There is no existing real state, create a new real state 'deleted'
            newRealState = DistributionData(entity_state_tag,
                                            // Correct node number and context but no connection id for delete states
                                            ConnectionId(ThisNodeParameters::NodeNumber(), registerer.connection->Id().m_contextId, -1),
                                            m_typeId,
                                            handlerId,
                                            registrationTime,
                                            instanceId,
                                            m_clock.GetNewTimestamp(),      // creation time
                                            DistributionData::Real,
                                            true,                          // deleted by owner
                                            false,                         // false => source is not permanent store
                                            NULL);                         // no blob
        }
        else
        {
            // Get a copy of the existing state without the blob
            newRealState = realState.GetEntityStateCopy(false);

            // Correct node number and context but no connection id for delete states
            newRealState.SetSenderId(ConnectionId(ThisNodeParameters::NodeNumber(), registerer.connection->Id().m_contextId, -1));
            newRealState.IncrementVersion();
            newRealState.SetExplicitlyDeleted(true);
            newRealState.SetEntityStateKind(DistributionData::Real);
            newRealState.ResetDecrementedFlag();
            newRealState.ResetSourceIsPermanentStore();
        }

        // For an injectable type the timestamps (monotonic clock) must be set.
        if (m_injectionKind == InjectionKind::Injectable)
        {
            DistributionData timeBase(no_state_tag);

            if (injectionState.IsNoState())
            {
                // There is no injection state so base the timestamps on the  real state.
                timeBase = realState;
            }
            else
            {
                // Base the timestamps on the existing injection state
                timeBase = injectionState;
            }

            SetTimestamps(newRealState,
                          timeBase,
                          false);   // false => No changeflags to consider when the new state is a 'deleted' state
        }

        // No owner for a delete state.
        statePtr->SetConnection(ConnectionPtr());
        statePtr->SetConsumer(ConsumerId(NULL, static_cast<short>(0)));

        // Release pointer to request in queue.
        statePtr->ResetOwnerRequestInQueue();

        // Set new state, subscribers will be notified
        statePtr->SetRealState(newRealState);

        // Figure out if the end state could be released

        if (!injectionState.IsNoState())
        {
            // This delete originates from an injection so check that
            // no new injections have been inserted while we were
            // dispatching the injection to the app.
            if (injectionState == statePtr->GetInjectionState())
            {
                // Clear the injection state.
                statePtr->SetInjectionState(DistributionData(no_state_tag));
            }

        }

        if (statePtr->GetInjectionState().IsNoState())
        {
            // There is no unhandled injection state.

            // The released end state must be saved "a while".
            EndStates::Instance().Add(statePtr);
        }
        else
        {
            // There is an unhandled injection state.
            statePtr->SetReleased(false);
        }
    }

    void EntityType::GetOwner(const StateSharedPtr&         statePtr,
                              ConnectionConsumerPair&       owner,
                              Dob::Typesystem::HandlerId&   handlerId,
                              bool&                         hasOwner) const
    {
        owner = ConnectionConsumerPair(statePtr->GetConnection(),statePtr->GetConsumer());

        hasOwner = true;

        if (owner.connection == NULL)
        {
            hasOwner = false;
            return;
        }

        DistributionData realState = statePtr->GetRealState();
        DistributionData injectionState = statePtr->GetInjectionState();

        if (!realState.IsNoState())
        {
            handlerId = realState.GetHandlerId();
            return;
        }
        if (!injectionState.IsNoState())
        {
            handlerId = injectionState.GetHandlerId();
            return;
        }

        ENSURE(false, << "Found a state with no real state and no injection state!");
    }

    EntityType::EntityAccessStatus EntityType::VerifyEntityAccess(const StateSharedPtr&                 statePtr,
                                                                  const ConnectionPtr&                  connection,
                                                                  const Dob::Typesystem::HandlerId&     handlerId) const
    {
        // Check if the handler has been revoked
        RegistrationVector revokedRegistrations = connection->GetRevokedRegistrations();

        for (RegistrationVector::iterator it = revokedRegistrations.begin();
             it != revokedRegistrations.end();
             ++it)
        {
            if (it->typeId == m_typeId && it->handlerId == handlerId)
            {
                return HandlerRevoked;
            }
        }

        const ContextId context = connection->Id().m_contextId;

        ConnectionConsumerPair  registerer = m_handlerRegistrations[context].GetRegisterer(handlerId);

        if (registerer.connection == NULL || registerer.connection->Id() != connection->Id())
        {
            return HandlerNotRegistered;
        }

        // Check if instance is owned by another handler
        ConnectionConsumerPair owner;
        Typesystem::HandlerId  ownerHandlerId;
        bool hasOwner;
        GetOwner(statePtr, owner, ownerHandlerId, hasOwner);

        if (hasOwner)
        {
            if (owner.connection->Id() != connection->Id() ||
                ownerHandlerId != handlerId)
            {
                return InstanceOwnedByOtherHandler;
            }
        }
        else if (IsGhost(statePtr))
        {
            return InstanceIsGhost;
        }

        return AccessOk;
    }

    bool EntityType::IsCreated(const StateSharedPtr& statePtr) const
    {
        DistributionData realState = statePtr->GetRealState();

        return !statePtr->IsReleased() &&
               !realState.IsNoState() &&
               realState.IsCreated();
    }

    bool EntityType::IsGhost(const StateSharedPtr& statePtr) const
    {
        DistributionData realState = statePtr->GetRealState();

        return !statePtr->IsReleased() &&
               !realState.IsNoState() &&
               realState.GetEntityStateKind() == DistributionData::Ghost;
    }

    void EntityType::ReadEntityInternal(const StateSharedPtr& statePtr, DistributionData& realState) const
    {
        if (!IsCreated(statePtr))
        {
            realState = DistributionData(no_state_tag);
            return;
        }

        realState = statePtr->GetRealState();
    }


    void EntityType::GetHandlerOfInstanceInternal(const StateSharedPtr& statePtr,
                                                  Dob::Typesystem::HandlerId& handlerId,
                                                  bool includeGhosts,
                                                  bool& gotIt) const
    {
        gotIt = true;
        DistributionData realState = statePtr->GetRealState();

        if (realState.IsNoState())
        {
            gotIt = false;
            return;
        }

        bool correctStateKind = (realState.GetEntityStateKind() == DistributionData::Real);
        if (!correctStateKind && includeGhosts)
        {
            correctStateKind = (realState.GetEntityStateKind() == DistributionData::Ghost);
        }

        if (statePtr->IsReleased() ||
            !realState.HasBlob() ||
            !correctStateKind)
        {
            gotIt = false;
            return;
        }

        handlerId = realState.GetHandlerId();
    }

    void EntityType::IsCreatedInternal(const StateSharedPtr& statePtr, bool& isCreated) const
    {
        isCreated = IsCreated(statePtr);
    }

    void EntityType::IsOwnerInternal(const StateSharedPtr&              statePtr,
                                     const Dob::Typesystem::HandlerId&  handlerId,
                                     const ConnectionConsumerPair&      registerer,
                                     bool&                              isOwner) const
    {
        isOwner = false;

        if (!IsCreated(statePtr))
        {
            return;
        }

        ConnectionConsumerPair owner;
        Typesystem::HandlerId  ownerHandlerId;
        bool hasOwner;
        GetOwner(statePtr, owner, ownerHandlerId, hasOwner);

        if (!hasOwner ||
            owner.connection->Id() != registerer.connection->Id() ||
            owner.consumer != registerer.consumer ||
            ownerHandlerId != handlerId)
        {
            return;
        }

        isOwner = true;
    }

    bool EntityType::RemoteEntityStateIsAccepted(const DistributionData& remoteState,
                                                 const DistributionData& localState) const
    {
        ENSURE(!remoteState.IsNoState() && !localState.IsNoState(), << "remoteState and/or local state is no state");


        if (remoteState.GetCreationTime() < localState.GetCreationTime())
        {
            // The remote state is older than the local state.
            return false;
        }
        else if (localState.GetCreationTime() < remoteState.GetCreationTime())
        {
            // The remote state is newer than the local state, no need to check the version.
            return true;
        }
        else
        {
            // The creation time is the same, check the version.
            if (localState.GetVersion() < remoteState.GetVersion())
            {
                return true;
            }
            else
            {
                // The remote state is older, or has the same version number. Skip it.
                return false;
            }
        }

    }

    void EntityType::SetTimestamps(DistributionData&       newState,
                                   const DistributionData& timeBase,
                                   const bool              considerChangeFlags) const
    {
        Typesystem::Int64 stateTopTimestamp = 0;

        bool resetUnchangedMembers = true;

        if (!timeBase.IsNoState())
        {
            stateTopTimestamp = timeBase.GetTopTimestamp();
            resetUnchangedMembers = false;
        }

        Typesystem::Int64 now = MonotonicClock::Get(stateTopTimestamp);

        if (considerChangeFlags)
        {
            TimestampOperations::SetTimestampForChangedMembers(newState,
                                                               now,
                                                               resetUnchangedMembers);
        }
        else
        {
            TimestampOperations::SetTimestampForAllMembers(newState,
                                                           now);
        }
    }

    void EntityType::KickRegisterer(const Dob::Typesystem::HandlerId& handlerId, const ContextId context) const
    {
        LockedStateResult lockedStateResult =
                m_handlerRegistrations[context].GetLockedRegistrationState(handlerId,
                                                                           false); // false => don't include released states

        // Get  a better name
        const StateSharedPtr& regStatePtr = lockedStateResult.first;

        if (regStatePtr == NULL)
        {
            return;
        }

        DistributionData regState = regStatePtr->GetRealState();

        if (regState.IsNoState() || !regStatePtr->GetRealState().IsRegistered())
        {
            return;
        }

        ConnectionPtr connection = regStatePtr->GetConnection();

        if (connection != NULL && connection->IsLocal())
        {
            connection->SignalIn();
        }
    }

    bool EntityType::CanAcquireContainerWriterLock(const ContextId contextId,
                                                   const boost::chrono::steady_clock::duration& lockTimeout)
    {
        bool okToAcquireLock = true;

        ScopedTypeLock lck(m_typeLocks[contextId],
                           boost::interprocess::defer_lock);

        if (!steady_try_lock_for(lck,lockTimeout))
        {
            // Can't acquire the type level lock.
            return false;  // *** RETURN ***
        }

        if (!m_handlerRegistrations[contextId].CanAcquireContainerWriterLock(lockTimeout))
        {
            okToAcquireLock = false;
        }

        if (!m_entityStates[contextId].CanAcquireContainerWriterLock(lockTimeout))
        {
            okToAcquireLock = false;
        }

        return okToAcquireLock;
    }
}
}
}
