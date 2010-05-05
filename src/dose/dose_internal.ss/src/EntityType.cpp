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

#include <Safir/Dob/Internal/EntityType.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/AccessDeniedException.h>
#include <Safir/Dob/NotFoundException.h>
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/Internal/LamportClocks.h>
#include <Safir/Dob/Internal/MonotonicClock.h>
#include <Safir/Dob/Internal/TimestampOperations.h>
#include <Safir/Dob/Internal/EndStates.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
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
          m_injectionKind(InjectionKindTable::Instance().GetInjectionKind(typeId)),
          m_entityStates(typeId),
          m_handlerRegistrations(typeId, &m_entityStates)
    {
    }

    bool EntityType::Register(const ConnectionPtr&                  connection,
                              const Dob::Typesystem::HandlerId&     handlerId,
                              const InstanceIdPolicy::Enumeration   instanceIdPolicy,
                              const bool                            isInjectionHandler,
                              const RegisterTime                    regTime,
                              const bool                            overrideRegistration,
                              const ConsumerId&                     consumer)
    {
        ScopedTypeLock lck(m_typeLock);

        return m_handlerRegistrations.Register(connection,
                                               handlerId,
                                               instanceIdPolicy,
                                               isInjectionHandler,
                                               regTime,
                                               overrideRegistration,
                                               consumer);
    }

    void EntityType::Unregister(const ConnectionPtr&                connection,
                                const Dob::Typesystem::HandlerId&   handlerId)
    {
        ScopedTypeLock lck(m_typeLock);

        if (handlerId == Dob::Typesystem::HandlerId::ALL_HANDLERS)
        {
            m_handlerRegistrations.UnregisterAll(connection);
        }
        else
        {
            m_handlerRegistrations.Unregister(connection, handlerId, RegisterTime());
        }
    }

    void EntityType::UnregisterAll(const ConnectionPtr& connection)
    {
        ScopedTypeLock lck(m_typeLock);

        m_handlerRegistrations.UnregisterAll(connection);
    }

    void EntityType::RemoteSetRegistrationState(const ConnectionPtr& connection,
                                                const DistributionData& registrationState)
    {
        ENSURE(!connection->IsLocal(), << "EntityType::RemoteSetRegistrationState can only be used by remote connections!");

        ScopedTypeLock lck(m_typeLock);

        m_handlerRegistrations.RemoteSetRegistrationState(connection, registrationState);

    }

    void EntityType::RemoteSetUnregistrationState(const DistributionData& registrationState)
    {
        ScopedTypeLock lck(m_typeLock);

        m_handlerRegistrations.RemoteSetUnregistrationState(registrationState);
    }

    bool EntityType::IsRegistered(const Dob::Typesystem::HandlerId& handlerId) const
    {
        return m_handlerRegistrations.IsRegistered(handlerId);
    }

    const ConnectionConsumerPair
    EntityType::GetRegisterer(const Dob::Typesystem::HandlerId& handlerId) const
    {
        return m_handlerRegistrations.GetRegisterer(handlerId);
    }

    bool
    EntityType::GetRegisterer(const Dob::Typesystem::HandlerId& handlerId,
                              ConnectionConsumerPair&           registerer,
                              InstanceIdPolicy::Enumeration&    instanceIdPolicy) const
    {
       return m_handlerRegistrations.GetRegisterer(handlerId, registerer, instanceIdPolicy);
    }

    void EntityType::SubscribeRegistration(const SubscriptionId&                subscriptionId,
                                           const Dob::Typesystem::HandlerId&    handlerId,
                                           const bool                           restartSubscription,
                                           const SubscriptionOptionsPtr&        subscriptionOptions)
    {
        lllout << "SubscribeRegistration for " << Typesystem::Operations::GetName(m_typeId) <<
            ", handlerId " << handlerId << std::endl;

        ScopedTypeLock lck(m_typeLock);
        m_handlerRegistrations.Subscribe(subscriptionId, handlerId, restartSubscription, subscriptionOptions);
    }

    void EntityType::UnsubscribeRegistration(const SubscriptionId&              subscriptionId,
                                             const Dob::Typesystem::HandlerId&  handlerId)
    {
        ScopedTypeLock lck(m_typeLock);
        m_handlerRegistrations.Unsubscribe(subscriptionId, handlerId);
    }

    bool EntityType::HasRegistrationSubscription(const ConnectionPtr&    connection,
                                                 const ConsumerId&       consumer) const
    {
        return m_handlerRegistrations.HasSubscription(connection,
                                                      consumer);
    }

    void EntityType::SetEntity(const ConnectionPtr&                 connection,
                               const Dob::Typesystem::HandlerId&    handlerId,
                               const Dob::Typesystem::InstanceId&   instanceId,
                               const char* const                    blob,
                               const bool                           considerChangeFlags,
                               const bool                           initialInjection)
    {
        ScopedTypeLock lck(m_typeLock);

        if (!initialInjection)
        {
            // Add a state if it is not already present
            m_entityStates.ForSpecificStateAdd(instanceId.GetRawValue(),
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
            m_entityStates.ForSpecificStateAdd(instanceId.GetRawValue(),
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
        ScopedTypeLock lck(m_typeLock);

        if (allInstances)
        {
            m_entityStates.ReleaseEachState(boost::bind(&EntityType::DeleteAllInstancesInternal,
                                                        this,
                                                        _2,
                                                        boost::cref(connection),
                                                        boost::cref(handlerId),
                                                        _3,
                                                        _4));
        }
        else
        {
            m_entityStates.ReleaseSpecificState(instanceId.GetRawValue(),
                                                boost::bind(&EntityType::DeleteEntityInternal,
                                                            this,
                                                            _2,
                                                            boost::cref(connection),
                                                            boost::cref(handlerId),
                                                            boost::cref(instanceId),
                                                            _3));
        }
    }

    void EntityType::InjectEntity(const ConnectionPtr&                 /*connection*/,  // AWI:Can be removed
                                  const Dob::Typesystem::HandlerId&    handlerId,
                                  const Dob::Typesystem::InstanceId&   instanceId,
                                  const char* const                    blob,
                                  const Dob::Typesystem::Int64         timestamp)
    {
        ENSURE(blob != NULL, << "Trying to call InjectEntity with a NULL blob!");
        ENSURE(m_injectionKind == InjectionKind::Injectable,
               << "Trying to call InjectEntity with a non-injectable type");

        ScopedTypeLock lck(m_typeLock);

        m_entityStates.ForSpecificStateAddAndRelease(instanceId.GetRawValue(),
                                                     boost::bind(&EntityType::InjectEntityInternal,
                                                                 this,
                                                                 _2,
                                                                 boost::cref(handlerId),
                                                                 boost::cref(instanceId),
                                                                 blob,
                                                                 timestamp,
                                                                 _3));
    }

    void EntityType::InjectDeletedEntity(const ConnectionPtr&                 /*connection*/,  // AWI: can be removed?
                                         const Dob::Typesystem::HandlerId&    handlerId,
                                         const Dob::Typesystem::InstanceId&   instanceId,
                                         const Dob::Typesystem::Int64         timestamp)
    {
        ENSURE(m_injectionKind == InjectionKind::Injectable,
               << "Trying to call InjectDeletedEntity with a non-injectable type");

        ScopedTypeLock lck(m_typeLock);

        m_entityStates.ForSpecificStateAddAndRelease(instanceId.GetRawValue(),
                                                     boost::bind(&EntityType::InjectDeletedEntityInternal,
                                                                 this,
                                                                 _2,
                                                                 boost::cref(handlerId),
                                                                 boost::cref(instanceId),
                                                                 timestamp,
                                                                 _3));
    }

    void EntityType::AcceptInjection(const ConnectionPtr&       connection,
                                     DistributionData&          injectionState,
                                     const DistributionData&    originalInjectionState)
    {
        ScopedTypeLock lck(m_typeLock);

        if (injectionState.HasBlob())
        {
            m_entityStates.ForSpecificState(injectionState.GetInstanceId().GetRawValue(),
                                            boost::bind(&EntityType::AcceptInjectionInternal,
                                                        this,
                                                        _2,
                                                        boost::cref(connection),
                                                        boost::ref(injectionState),
                                                        boost::cref(originalInjectionState)),
                                            false); // false => don't include released states;
        }
        else
        {
            m_entityStates.ReleaseSpecificState(injectionState.GetInstanceId().GetRawValue(),
                                                boost::bind(&EntityType::AcceptDeleteInjectionInternal,
                                                            this,
                                                            _2,
                                                            boost::cref(connection),
                                                            boost::ref(injectionState),
                                                            boost::cref(originalInjectionState),
                                                            _3));
        }
    }

    void EntityType::SetInjection(const ConnectionPtr&                 connection,
                                  const Dob::Typesystem::HandlerId&    handlerId,
                                  const Dob::Typesystem::InstanceId&   instanceId,
                                  const bool                           considerChangeFlags,
                                  const char* const                    blob,
                                  const DistributionData&              originalInjectionState)
    {
        ScopedTypeLock lck(m_typeLock);

        m_entityStates.ForSpecificState(instanceId.GetRawValue(),
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
        ScopedTypeLock lck(m_typeLock);

        m_entityStates.ReleaseSpecificState(instanceId.GetRawValue(),
                                            boost::bind(&EntityType::DeleteInjectionInternal,
                                                        this,
                                                        _2,
                                                        boost::cref(connection),
                                                        boost::cref(handlerId),
                                                        boost::cref(instanceId),
                                                        boost::cref(originalInjectionState),
                                                        _3));
    }

    void EntityType::RemoteSetGhostEntityState(const DistributionData& entityState)
    {
        ScopedTypeLock lck(m_typeLock);

        m_clock.UpdateCurrentTimestamp(entityState.GetCreationTime());

        m_entityStates.ForSpecificStateAdd(entityState.GetInstanceId().GetRawValue(),
                                           boost::bind(&EntityType::RemoteSetGhostEntityStateInternal,
                                                       this,
                                                       boost::cref(entityState),
                                                       _2));
    }

    void EntityType::RemoteSetInjectionEntityState(const DistributionData& entityState)
    {
        ScopedTypeLock lck(m_typeLock);

        m_clock.UpdateCurrentTimestamp(entityState.GetCreationTime());

        m_entityStates.ForSpecificStateAddAndRelease(entityState.GetInstanceId().GetRawValue(),
                                                     boost::bind(&EntityType::RemoteSetInjectionEntityStateInternal,
                                                                 this,
                                                                 boost::cref(entityState),
                                                                 _2,
                                                                 _3));
    }

    void EntityType::RemoteSetDeleteEntityState(const DistributionData&   entityState)
    {
        ScopedTypeLock lck(m_typeLock);

        m_clock.UpdateCurrentTimestamp(entityState.GetCreationTime());

        m_entityStates.ForSpecificStateAddAndRelease(entityState.GetInstanceId().GetRawValue(),
                                                     boost::bind(&EntityType::RemoteSetDeleteEntityStateInternal,
                                                                 this,
                                                                 boost::cref(entityState),
                                                                 _2,
                                                                 _3));
    }

    RemoteSetResult EntityType::RemoteSetRealEntityState(const ConnectionPtr&      connection,
                                                         const DistributionData&   entityState)
    {
        ScopedTypeLock lck(m_typeLock);

        m_clock.UpdateCurrentTimestamp(entityState.GetCreationTime());

        RemoteSetResult result;

        m_entityStates.ForSpecificStateAddAndRelease(entityState.GetInstanceId().GetRawValue(),
                                                     boost::bind(&EntityType::RemoteSetRealEntityStateInternal,
                                                                 this,
                                                                 boost::cref(connection),
                                                                 boost::cref(entityState),
                                                                 _2,
                                                                 _3,
                                                                 boost::ref(result)));

        return result;
    }

    void EntityType::Subscribe(const SubscriptionId&                subscriptionId,
                               const Dob::Typesystem::InstanceId&   instanceId,
                               const bool                           allInstances,
                               const bool                           restartSubscription,
                               const SubscriptionOptionsPtr&        subscriptionOptions)
    {
        ScopedTypeLock lck(m_typeLock);

        m_entityStates.Subscribe(subscriptionId,
                                 instanceId.GetRawValue(),
                                 allInstances,
                                 restartSubscription,
                                 subscriptionOptions);
    }

    void EntityType::Unsubscribe(const SubscriptionId&              subscriptionId,
                                 const Dob::Typesystem::InstanceId& instanceId,
                                 const bool                         allInstances)
    {
        ScopedTypeLock lck(m_typeLock);

        m_entityStates.Unsubscribe(subscriptionId,
                                   instanceId.GetRawValue(),
                                   allInstances);
    }

    bool EntityType::HasEntitySubscription(const ConnectionPtr&    connection,
                                           const ConsumerId&       consumer) const
    {
        SubscriptionId subscriptionId(ConnectionConsumerPair(connection, consumer), EntitySubscription, 0);

        return m_entityStates.HasSubscription(subscriptionId);
    }

    void EntityType::UnsubscribeAll(const ConnectionPtr& connection)
    {
        ScopedTypeLock lck(m_typeLock);

        m_handlerRegistrations.UnsubscribeAll(connection);
        m_entityStates.UnsubscribeAll(connection);
    }

    const DistributionData EntityType::ReadEntity(const Dob::Typesystem::InstanceId& instanceId) const
    {
        DistributionData realState(no_state_tag);

        m_entityStates.ForSpecificState(instanceId.GetRawValue(),
                                        boost::bind(&EntityType::ReadEntityInternal,
                                                    this,
                                                    _2,
                                                    boost::ref(realState)),
                                        false); // false => don't include released states

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

    const Dob::Typesystem::HandlerId EntityType::GetHandlerOfInstance(const Dob::Typesystem::InstanceId& instanceId) const
    {
        Dob::Typesystem::HandlerId handlerId;
        bool gotIt = false;

        m_entityStates.ForSpecificState(instanceId.GetRawValue(),
                                         boost::bind(&EntityType::GetHandlerOfInstanceInternal,
                                                     this,
                                                     _2,
                                                     boost::ref(handlerId),
                                                     boost::ref(gotIt)),
                                         false); // false => don't include released states

        if (!gotIt)
        {
            const Typesystem::EntityId eid(GetTypeId(),instanceId);
            std::wostringstream ostr;
            ostr << "Instance not found: " << eid;
            throw Safir::Dob::NotFoundException(ostr.str(),__WFILE__,__LINE__);
        }

        return handlerId;
    }

    bool EntityType::IsCreated(const Dob::Typesystem::InstanceId& instanceId) const
    {
        bool isCreated = false;

        m_entityStates.ForSpecificState(instanceId.GetRawValue(),
                                        boost::bind(&EntityType::IsCreatedInternal,
                                                    this,
                                                    _2,
                                                    boost::ref(isCreated)),
                                        false); // false => don't include released states

        return isCreated;
    }

    bool EntityType::IsOwner(const Dob::Typesystem::InstanceId& instanceId,
                             const Dob::Typesystem::HandlerId&  handlerId,
                             const ConnectionConsumerPair&      registerer) const
    {
        bool isOwner = false;

        m_entityStates.ForSpecificState(instanceId.GetRawValue(),
                                        boost::bind(&EntityType::IsOwnerInternal,
                                                    this,
                                                    _2,
                                                    boost::cref(handlerId),
                                                    boost::cref(registerer),
                                                    boost::ref(isOwner)),
                                        false); // false => don't include released states

        return isOwner;
    }

    void EntityType::SetEntityInternal(const UpgradeableStateResult&        upgradeableStateResult,
                                       const ConnectionPtr&                 connection,
                                       const Dob::Typesystem::HandlerId&    handlerId,
                                       const Dob::Typesystem::InstanceId&   instanceId,
                                       const bool                           considerChangeFlags,
                                       const char* const                    blob)
    {
        ENSURE(blob != NULL, << "Trying to call a SetEntity with a NULL blob! connId = " << connection->Id());

        const StateSharedPtr& statePtr = upgradeableStateResult.first;

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
                ostr << "SetEntity called with handler that is not previously registered. connId = " << connection->Id()
                     << " handlerId = " << handlerId << ")";
                throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);

            }
            break;

            case InstanceOwnedByOtherHandler:
            {
                std::wostringstream ostr;
                ostr << "SetEntity not allowed, instance owned by another handler. instanceId = " << instanceId
                     << " handlerId = " << handlerId << ")";
                throw Safir::Dob::AccessDeniedException(ostr.str(),__WFILE__,__LINE__);

            }
            break;

            case InstanceIsGhost:
            {
                std::wostringstream ostr;
                ostr << "SetEntity not allowed, persistence exists. instanceId = " << instanceId
                     << " handlerId = " << handlerId << ")";
                throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);

            }
            break;
        }

        SetEntityLocal(upgradeableStateResult,
                       connection,
                       handlerId,
                       instanceId,
                       considerChangeFlags,
                       blob,
                       DistributionData(no_state_tag));  // No injection state to consider in this case (is used to set correct top timestamps)
    }

    void EntityType::DeleteEntityInternal(const UpgradeableStateResult&        upgradeableStateResult,
                                          const ConnectionPtr&                 connection,
                                          const Dob::Typesystem::HandlerId&    handlerId,
                                          const Dob::Typesystem::InstanceId&   instanceId,
                                          StatePtrHandling&                    statePtrHandling)
    {
        const StateSharedPtr& statePtr = upgradeableStateResult.first;

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
                ostr << "DeleteEntity called with handler that is not previously registered. connId = " << connection->Id()
                     << " handlerId = " << handlerId << ")";
                throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
            }
            break;

            case InstanceOwnedByOtherHandler:
            {
                std::wostringstream ostr;
                ostr << "DeleteEntity not allowed, instance owned by another handler. instanceId = " << instanceId
                     << " handlerId = " << handlerId << ")";
                throw Safir::Dob::AccessDeniedException(ostr.str(),__WFILE__,__LINE__);
            }
            break;

            case InstanceIsGhost:
            {
                std::wostringstream ostr;
                ostr << "DeleteEntity not allowed, persistence exists. instanceId = " << instanceId
                     << " handlerId = " << handlerId << ")";
                throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);

            }
            break;
        }

        DeleteEntityLocal(statePtr,
                          handlerId,
                          instanceId,
                          DistributionData(no_state_tag),  // No injection state to consider in this case (is used to set correct top timestamps)
                          statePtrHandling);

        if (statePtrHandling == ReleasePtr)
        {
            if (statePtr->GetInjectionState().IsNoState())
            {
                // There is no unhandled injection state.

                // The released end state must be saved "a while".
                EndStates::Instance().Add(statePtr);
            }
            else
            {
                // There is an unhandled injection state.
                statePtrHandling = KeepPtr;
            }
        }
    }

    void EntityType::DeleteAllInstancesInternal(const UpgradeableStateResult&       upgradeableStateResult,
                                                const ConnectionPtr&                connection,
                                                const Dob::Typesystem::HandlerId&   handlerId,
                                                StatePtrHandling&                   statePtrHandling,
                                                bool&                               exitDispatch)
    {
        exitDispatch = false;

        ConnectionConsumerPair owner;
        Typesystem::HandlerId  ownerHandlerId;
        bool hasOwner;

        const StateSharedPtr& statePtr = upgradeableStateResult.first;

        GetOwner(statePtr, owner, ownerHandlerId, hasOwner);

        if ((hasOwner && (owner.connection->Id() != connection->Id() || ownerHandlerId != handlerId)) ||
            IsGhost(statePtr) ||
            !IsCreated(statePtr))
        {
            return;
        }

        DeleteEntityInternal(upgradeableStateResult,
                             connection,
                             handlerId,
                             statePtr->GetRealState().GetInstanceId(),
                             statePtrHandling);

    }

    void EntityType::SetInitalGhostInternal(const UpgradeableStateResult&        upgradeableStateResult,
                                            const ConnectionPtr&                 connection,
                                            const Dob::Typesystem::HandlerId&    handlerId,
                                            const Dob::Typesystem::InstanceId&   instanceId,
                                            const char* const                    blob)
    {
        ENSURE(blob != NULL, << "Trying to do a SetInitalGhostEntity with a NULL blob! connId = " << connection->Id());

        const StateSharedPtr& statePtr = upgradeableStateResult.first;
        const bool& isRevived = upgradeableStateResult.second;

        // Shouldn't check owner in this case

        DistributionData realState = statePtr->GetRealState();

        if (realState.IsNoState() || isRevived)
        {
            DistributionData newRealState =
                DistributionData(entity_state_tag,
                                 ConnectionId(ThisNodeParameters::NodeNumber(),-1), // Correct node number but no connection id for ghost states
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
        }
    }

    void EntityType::InjectEntityInternal(const UpgradeableStateResult&        upgradeableStateResult,
                                          const Dob::Typesystem::HandlerId&    handlerId,
                                          const Dob::Typesystem::InstanceId&   instanceId,
                                          const char* const                    blob,
                                          const Dob::Typesystem::Int64         timestamp,
                                          StatePtrHandling&                    statePtrHandling)
    {
        statePtrHandling = KeepPtr;

        const StateSharedPtr& statePtr = upgradeableStateResult.first;
        const bool& isRevived = upgradeableStateResult.second;

        DistributionData injectionState = statePtr->GetInjectionState();

        bool handlerIdOk = true;

        if (!isRevived && (IsCreated(statePtr) || IsGhost(statePtr)))
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

        if (injectionState.IsNoState() || isRevived)
        {
            // There is no existing injection state, create a new one
            newInjectionState = DistributionData(entity_state_tag,
                                                 ConnectionId(ThisNodeParameters::NodeNumber(),-1), // Dummy connectionId for injection states, but correct node number
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
        }
        else
        {
            // There is an existing injection state

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

            newInjectionState.SetSenderId(ConnectionId(ThisNodeParameters::NodeNumber(),-1)); // Dummy connectionId for injection states, but correct node number
            newInjectionState.SetHandlerId(handlerId);
        }

        const DistributionData& realState = statePtr->GetRealState();
        if (!realState.IsNoState() && TimestampOperations::HaveChanges(realState,newInjectionState))
        {
            // All the stuff in the new injection state is already in the real state. We can drop the new injection state.
            statePtrHandling = RestorePtr;
            return;
        }


        // Set new injection state
        statePtr->SetInjectionState(newInjectionState);

        KickRegisterer(handlerId);
    }

    void EntityType::InjectDeletedEntityInternal(const UpgradeableStateResult&        upgradeableStateResult,
                                                 const Dob::Typesystem::HandlerId&    handlerId,
                                                 const Dob::Typesystem::InstanceId&   instanceId,
                                                 const Dob::Typesystem::Int64         timestamp,
                                                 StatePtrHandling&                    statePtrHandling)
    {
        statePtrHandling = KeepPtr;

        const StateSharedPtr& statePtr = upgradeableStateResult.first;
        const bool& isRevived = upgradeableStateResult.second;

        if (!isRevived && (IsCreated(statePtr) || IsGhost(statePtr)))
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

        if (injectionState.IsNoState() || isRevived)
        {
            // There is no existing injection state, create a deleted state
            newInjectionState = DistributionData(entity_state_tag,
                                                 ConnectionId(ThisNodeParameters::NodeNumber(),-1), // Dummy connection for injection states
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
            const TimestampOperations::MergeResult mergeResult = TimestampOperations::Merge(injectionState,
                                                                                      NULL, // no blob
                                                                                      timestamp);

            if (mergeResult.second == false)
            {
                // No merge has taken place
                return;
            }

            newInjectionState = mergeResult.first;

            newInjectionState.SetSenderId(ConnectionId(ThisNodeParameters::NodeNumber(),-1)); // Dummy connectionId for injection states, but correct node number
            newInjectionState.SetHandlerId(handlerId);
        }


        const DistributionData& realState = statePtr->GetRealState();
        if (!realState.IsNoState() && TimestampOperations::HaveChanges(realState,newInjectionState))
        {
            // All the stuff in the new injection state is already in the real state. We can drop the new injection state.
            statePtrHandling = RestorePtr;
            return;
        }

        // Set new injection state
        statePtr->SetInjectionState(newInjectionState);

        KickRegisterer(handlerId);
    }

    void EntityType::AcceptInjectionInternal(const UpgradeableStateResult&  upgradeableStateResult,
                                             const ConnectionPtr&           connection,
                                             DistributionData&              injectionState,
                                             const DistributionData&        originalInjectionState)
    {
        const StateSharedPtr& statePtr = upgradeableStateResult.first;

        if (originalInjectionState == statePtr->GetInjectionState())
        {
            // No new injections has been done while we were dispatching the injection to the app.
            // In this case we can remove the injection state ...
            statePtr->SetInjectionState(DistributionData(no_state_tag));
        }

        CommonAcceptInjectionInternal(upgradeableStateResult, connection, injectionState);
    }

    void EntityType::AcceptDeleteInjectionInternal(const UpgradeableStateResult&    upgradeableStateResult,
                                                   const ConnectionPtr&             connection,
                                                   DistributionData&                injectionState,
                                                   const DistributionData&          originalInjectionState,
                                                   StatePtrHandling&                statePtrHandling)
    {
        statePtrHandling = KeepPtr;

        const StateSharedPtr& statePtr = upgradeableStateResult.first;

        if (originalInjectionState == statePtr->GetInjectionState())
        {
            // No new injections has been done while we were dispatching the injection to the app.
            // In this case we can remove the injection state ...
            statePtr->SetInjectionState(DistributionData(no_state_tag));

            // ... and release the injection state
            statePtrHandling = ReleasePtr;
        }

        CommonAcceptInjectionInternal(upgradeableStateResult, connection, injectionState);

        statePtr->SetConnection(NULL);  // No connection since its a delete state
        statePtr->SetConsumer(ConsumerId(NULL, static_cast<short>(0))); //dummy consumer

        // This is an end state so it must be saved "a while".
        EndStates::Instance().Add(statePtr);
    }

    void EntityType::CommonAcceptInjectionInternal(const UpgradeableStateResult&  upgradeableStateResult,
                                                   const ConnectionPtr&           connection,
                                                   DistributionData&              injectionState)
    {
        const StateSharedPtr& statePtr = upgradeableStateResult.first;

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
            // Get the registration state. The state will be locked within this scope.
            LockedStateResult lockedStateResult =
                m_handlerRegistrations.GetLockedRegistrationState(handlerId,
                                                                  false); // false => don't include released states

            // Get  a better name
            const StateSharedPtr& regStatePtr = lockedStateResult.first.first;

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
                injectionState.SetExplicitlyDeleted(false);
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
    }

    void EntityType::SetInjectionInternal(const UpgradeableStateResult&        upgradeableStateResult,
                                          const ConnectionPtr&                 connection,
                                          const Dob::Typesystem::HandlerId&    handlerId,
                                          const Dob::Typesystem::InstanceId&   instanceId,
                                          const bool                           considerChangeFlags,
                                          const char* const                    blob,
                                          const DistributionData&              originalInjectionState)
    {

        const StateSharedPtr& statePtr = upgradeableStateResult.first;

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
                ostr << "SetEntity (in an OnInjected... callback) called with handler that is not previously registered. connId = " << connection->Id()
                     << " handlerId = " << handlerId << ")";
                throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);

            }
            break;

            case InstanceOwnedByOtherHandler:
            {
                std::wostringstream ostr;
                ostr << "SetEntity (in an OnInjected... callback) not allowed, instance owned by another handler. instanceId = " << instanceId
                     << " handlerId = " << handlerId << ")";
                throw Safir::Dob::AccessDeniedException(ostr.str(),__WFILE__,__LINE__);

            }
            break;
        }

        SetEntityLocal(upgradeableStateResult,
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

    void EntityType::DeleteInjectionInternal(const UpgradeableStateResult&          upgradeableStateResult,
                                             const ConnectionPtr&                   connection,
                                             const Dob::Typesystem::HandlerId&      handlerId,
                                             const Dob::Typesystem::InstanceId&     instanceId,
                                             const DistributionData&                originalInjectionState,
                                             StatePtrHandling&                      statePtrHandling)
    {
        const StateSharedPtr& statePtr = upgradeableStateResult.first;

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
                ostr << "DeleteEntity (in an OnInjected... callback) called with handler that is not previously registered. connId = " << connection->Id()
                     << " handlerId = " << handlerId << ")";
                throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
            }
            break;

            case InstanceOwnedByOtherHandler:
            {
                std::wostringstream ostr;
                ostr << "DeleteEntity (in an OnInjected... callback) not allowed, instance owned by another handler. instanceId = " << instanceId
                     << " handlerId = " << handlerId << ")";
                throw Safir::Dob::AccessDeniedException(ostr.str(),__WFILE__,__LINE__);
            }
            break;

            case InstanceIsGhost:
            break;  // This is what we expect here
        }

        DeleteEntityLocal(statePtr,
                          handlerId,
                          instanceId,
                          originalInjectionState,   // is used to get correct top timestamps
                          statePtrHandling);

        if (statePtrHandling == ReleasePtr)
        {
            // We must also check that no new injections has been done while we were
            // dispatching the injection to the app, before the final descion to release
            // the state can be made
            if (originalInjectionState == statePtr->GetInjectionState())
            {
                // There is no unhandled injection state.
                statePtr->SetInjectionState(DistributionData(no_state_tag));

                // The released end state must be saved "a while".
                EndStates::Instance().Add(statePtr);
            }
            else
            {
                // There is an unhandled injection state.
                statePtrHandling = KeepPtr;
            }
        }
    }

    void EntityType::RemoteSetGhostEntityStateInternal(const DistributionData&        remoteEntity,
                                                       const UpgradeableStateResult&  upgradeableStateResult)
    {
        const StateSharedPtr& statePtr = upgradeableStateResult.first;
        const bool& isRevived = upgradeableStateResult.second;

        DistributionData localEntity = statePtr->GetRealState();

        if (!localEntity.IsNoState() && !isRevived)
        {
            // There is an existing local entity. Check if the received remote entity is newer.
            if (!RemoteEntityStateIsAccepted(remoteEntity, localEntity))
            {
                return;
            }
        }

        //Check if we have an injection state that can be cleared
        const DistributionData injectionState = statePtr->GetInjectionState();
        if (!injectionState.IsNoState() && TimestampOperations::HaveChanges(remoteEntity, injectionState))
        {
            statePtr->SetInjectionState(DistributionData(no_state_tag));
        }

        statePtr->SetConnection(NULL);  // No connection since its a ghost state
        statePtr->SetConsumer(ConsumerId(NULL, static_cast<short>(0))); //dummy consumer

        statePtr->SetRealState(remoteEntity);
    }

    void EntityType::RemoteSetInjectionEntityStateInternal(const DistributionData&        remoteEntity,
                                                           const UpgradeableStateResult&  upgradeableStateResult,
                                                           StatePtrHandling&              statePtrHandling)
    {
        statePtrHandling = KeepPtr;

        const StateSharedPtr& statePtr = upgradeableStateResult.first;
        const bool& isRevived = upgradeableStateResult.second;

        DistributionData injectionState = statePtr->GetInjectionState();
        DistributionData newInjectionState(no_state_tag);

        if (injectionState.IsNoState() || isRevived)
        {
            // There is no existing injection state, create a new one
            newInjectionState = remoteEntity;
        }
        else
        {
            // There is an existing injection state

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
            // All the stuff in the new injection state is already in the real state. We can drop the new injection state.
            statePtrHandling = RestorePtr;
            return;
        }

        statePtr->SetInjectionState(newInjectionState);

        KickRegisterer(newInjectionState.GetHandlerId());
    }

    void EntityType::RemoteSetDeleteEntityStateInternal(const DistributionData&         remoteEntity,
                                                        const UpgradeableStateResult&   upgradeableStateResult,
                                                        StatePtrHandling&               statePtrHandling)
    {
        bool needToCheckRegistrationState = true;

        const StateSharedPtr& statePtr = upgradeableStateResult.first;

        DistributionData localEntity = statePtr->GetRealState();

        if (!localEntity.IsNoState())
        {
            // There is an existing entity state

            if (remoteEntity.GetRegistrationTime() < localEntity.GetRegistrationTime())
            {
                // The existing local entity state (created or deleted) belongs to a newer registration. Skip the remote delete
                return;
            }
            else if (localEntity.GetRegistrationTime() < remoteEntity.GetRegistrationTime())
            {
                if (localEntity.IsCreated())
                {
                    ENSURE(false, << "Receive a remote deleted entity state when there is an existig created entity belonging to an old registration");
                }
            }
            else
            {
                // The existing local entity state (created or deleted) has the same registration time as the remote entity state.
                if (!RemoteEntityStateIsAccepted(remoteEntity, localEntity))
                {
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
            lockedRegStateResult =
                m_handlerRegistrations.GetLockedRegistrationState(remoteEntity.GetHandlerId(),
                                                                  true); // true => include released states

            // Get  a better name
            const StateSharedPtr& regStatePtr = lockedRegStateResult.first.first;

            if (regStatePtr == NULL ||
                regStatePtr->GetRealState().IsNoState() ||
                !regStatePtr->GetRealState().IsRegistered() ||
                regStatePtr->GetRealState().GetRegistrationTime() < remoteEntity.GetRegistrationTime())
            {
                // There is no active registration or it is an old one. It's ok to set the state
            }
            else if (remoteEntity.GetRegistrationTime() < regStatePtr->GetRealState().GetRegistrationTime())
            {
                // There is already a newer registration. Discard the remote state.

                if (localEntity.IsNoState())
                {
                    // Remember to get rid of the newly created state
                    statePtrHandling = ReleasePtr;
                }
                return;
            }
        }

        //Check if we have an injection state that can be cleared
        const DistributionData injectionState = statePtr->GetInjectionState();

        if (injectionState.IsNoState())
        {
            //we can release the state since there is no injection
            statePtrHandling = ReleasePtr;
        }
        else
        {
            // We have an injection state

            if (TimestampOperations::HaveChanges(remoteEntity, injectionState))
            {
                // The remote entity already has all what is in the injection state, so the injection state can be removed
                // and the state released.
                statePtr->SetInjectionState(DistributionData(no_state_tag));
                statePtrHandling = ReleasePtr;
            }
        }

        // If we got this far everything is ok. Set the remote delete state.
        statePtr->SetConnection(NULL);  // No connection since its a delete state
        statePtr->SetConsumer(ConsumerId(NULL, static_cast<short>(0))); //dummy consumer
        statePtr->SetRealState(remoteEntity);

        // This is an end state so it must be saved "a while".
        EndStates::Instance().Add(statePtr);
    }

    void EntityType::RemoteSetRealEntityStateInternal(const ConnectionPtr&           connection,
                                                      const DistributionData&        remoteEntity,
                                                      const UpgradeableStateResult&  upgradeableStateResult,
                                                      StatePtrHandling&              statePtrHandling,
                                                      RemoteSetResult&               remoteSetResult)
    {
        statePtrHandling = KeepPtr;
        remoteSetResult = RemoteSetAccepted;

        bool needToCheckRegistrationState = true;

        // get a better name
        const StateSharedPtr& statePtr = upgradeableStateResult.first;

        DistributionData localEntity = statePtr->GetRealState();

        if (!localEntity.IsNoState())
        {
            // There is an existing entity state

            if (remoteEntity.GetRegistrationTime() < localEntity.GetRegistrationTime())
            {
                // The existing local entity state (created or deleted) belongs to a newer registration. Skip the remote entity
                remoteSetResult = RemoteSetDiscarded;
                return;
            }
            else if (localEntity.GetRegistrationTime() < remoteEntity.GetRegistrationTime())
            {
                // The existing local entity state (created or deleted) belongs to an older registration wich means that
                // we can accept this state. But do we have the corresponding registration?

                if (localEntity.IsCreated())
                {
                    // The entity is created and therefor we know that the registration time in the entity state corresponds
                    // to the active registration. Since this time is older than the received time we haven't got the registration yet.
                    remoteSetResult = RemoteSetNeedRegistration;
                    return;
                }
            }
            else
            {
                // The existing local entity state (created or deleted) has the same registration time as the remote entity state.

                // Compare entity creation time and version
                if (!RemoteEntityStateIsAccepted(remoteEntity, localEntity))
                {
                    remoteSetResult = RemoteSetDiscarded;
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
            lockedRegStateResult =
                m_handlerRegistrations.GetLockedRegistrationState(remoteEntity.GetHandlerId(),
                                                                  true); // true => include released states

            // Get  a better name
            const StateSharedPtr& regStatePtr = lockedRegStateResult.first.first;

            if (regStatePtr == NULL ||
                regStatePtr->GetRealState().IsNoState() ||
                !regStatePtr->GetRealState().IsRegistered() ||
                regStatePtr->GetRealState().GetRegistrationTime() < remoteEntity.GetRegistrationTime())
            {
                // There is no active registration or it is an old one
                remoteSetResult = RemoteSetNeedRegistration;

                if (localEntity.IsNoState())
                {
                    // Remember to get rid of the newly created state
                    statePtrHandling = ReleasePtr;
                }
                return;
            }
            else if (remoteEntity.GetRegistrationTime() < regStatePtr->GetRealState().GetRegistrationTime())
            {
                // There is already a newer registration
                remoteSetResult = RemoteSetDiscarded;

                if (localEntity.IsNoState())
                {
                    // Remember to get rid of the newly created state
                    statePtrHandling = ReleasePtr;
                }
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

    }


    void EntityType::SetEntityLocal(const UpgradeableStateResult&        upgradeableStateResult,
                                    const ConnectionPtr&                 connection,
                                    const Dob::Typesystem::HandlerId&    handlerId,
                                    const Dob::Typesystem::InstanceId&   instanceId,
                                    const bool                           considerChangeFlags,
                                    const char* const                    blob,
                                    const DistributionData&              injectionState)
    {
        const StateSharedPtr& statePtr = upgradeableStateResult.first;
        const bool& isRevived = upgradeableStateResult.second;

        DistributionData realState = statePtr->GetRealState();

        // Since this connection has been verified to be the owner (update case) with
        // the entity state locked, an overregistration of this instance can't take place and
        // it is therefor safe to get the registerer without a registration state lock.
        ConnectionConsumerPair          registerer;
        RegisterTime                    registrationTime;
        m_handlerRegistrations.GetRegisterer(handlerId, registerer, registrationTime);

        DistributionData newRealState(no_state_tag);

        if (!realState.IsCreated() || isRevived)
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
    }

    void EntityType::DeleteEntityLocal(const StateSharedPtr&                statePtr,
                                       const Dob::Typesystem::HandlerId&    handlerId,
                                       const Dob::Typesystem::InstanceId&   instanceId,
                                       const DistributionData&              injectionState,
                                       StatePtrHandling&                    statePtrHandling)
    {
        statePtrHandling = ReleasePtr;

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
        m_handlerRegistrations.GetRegisterer(handlerId, registerer, registrationTime);

        DistributionData newRealState(no_state_tag);

        if (!realState.IsCreated())
        {
            // There is no existing real state, create a new real state 'deleted'
            newRealState = DistributionData(entity_state_tag,
                                            ConnectionId(ThisNodeParameters::NodeNumber(),-1), // Correct node number but no connection id for delete states
                                            m_typeId,
                                            handlerId,
                                            registrationTime,
                                            instanceId,
                                            m_clock.GetNewTimestamp(),      // creation time
                                            DistributionData::Real,
                                            true,                          // true => explicitly deleted
                                            false,                         // false => source is not permanent store
                                            NULL);                         // no blob
        }
        else
        {
            // Get a copy of the existing state without the blob
            newRealState = realState.GetEntityStateCopy(false);

            newRealState.SetSenderId(ConnectionId(ThisNodeParameters::NodeNumber(),-1));  // Correct node number but no connection id for delete states
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
        statePtr->SetConnection(NULL);
        statePtr->SetConsumer(ConsumerId(NULL, static_cast<short>(0)));

        // Release pointer to request in queue.
        statePtr->ResetOwnerRequestInQueue();

        // Set new state, subscribers will be notified
        statePtr->SetRealState(newRealState);
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

        ConnectionConsumerPair  registerer = m_handlerRegistrations.GetRegisterer(handlerId);

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
        return statePtr->GetRealState().IsCreated();
    }

    bool EntityType::IsGhost(const StateSharedPtr& statePtr) const
    {
        DistributionData realState = statePtr->GetRealState();

        return !realState.IsNoState() &&
               realState.GetEntityStateKind() == DistributionData::Ghost;
    }

    void EntityType::ReadEntityInternal(const UpgradeableStateResult& upgradeableStateResult, DistributionData& realState) const
    {
        const StateSharedPtr& statePtr = upgradeableStateResult.first;

        if (!IsCreated(statePtr))
        {
            realState = DistributionData(no_state_tag);
            return;
        }

        realState = statePtr->GetRealState();
    }


    void EntityType::GetHandlerOfInstanceInternal(const UpgradeableStateResult& upgradeableStateResult,
                                                  Dob::Typesystem::HandlerId& handlerId,
                                                  bool& gotIt) const
    {
        const StateSharedPtr& statePtr = upgradeableStateResult.first;

        gotIt = true;
        DistributionData realState = statePtr->GetRealState();

        if (!realState.HasBlob() ||
            realState.GetEntityStateKind() != DistributionData::Real)
        {
            gotIt = false;
            return;
        }

        handlerId = realState.GetHandlerId();
    }

    void EntityType::IsCreatedInternal(const UpgradeableStateResult& upgradeableStateResult, bool& isCreated) const
    {
        const StateSharedPtr& statePtr = upgradeableStateResult.first;

        isCreated = IsCreated(statePtr);
    }

    void EntityType::IsOwnerInternal(const UpgradeableStateResult&      upgradeableStateResult,
                                     const Dob::Typesystem::HandlerId&  handlerId,
                                     const ConnectionConsumerPair&      registerer,
                                     bool&                              isOwner) const
    {
        const StateSharedPtr& statePtr = upgradeableStateResult.first;

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

    void EntityType::KickRegisterer(const Dob::Typesystem::HandlerId& handlerId) const
    {
        LockedStateResult lockedStateResult =
                m_handlerRegistrations.GetLockedRegistrationState(handlerId,
                                                                  false); // false => don't include released states

        // Get  a better name
        const StateSharedPtr& regStatePtr = lockedStateResult.first.first;

        if (regStatePtr == NULL)
        {
            return;
        }

        DistributionData regState = regStatePtr->GetRealState();

        if (regState.IsNoState() || !regState.IsRegistered())
        {
            return;
        }

        ConnectionPtr connection = regStatePtr->GetConnection();

        if (connection != NULL && connection->IsLocal())
        {
            connection->SignalIn();
        }
    }
}
}
}
