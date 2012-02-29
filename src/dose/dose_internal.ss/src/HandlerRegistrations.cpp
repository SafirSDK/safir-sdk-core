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

#include <Safir/Dob/Internal/HandlerRegistrations.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/State.h>
#include <Safir/Dob/Internal/EndStates.h>
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Service.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    HandlerRegistrations::HandlerRegistrations(const Typesystem::TypeId typeId)
        : m_typeId(typeId),
          m_registrations(typeId),
          m_entityContainerPtr(NULL)
    {
        SetSubscriptionType();
    }

    void HandlerRegistrations::SetStateContainer(const StateContainerPtr& entityContainerPtr)
    {
        m_entityContainerPtr = entityContainerPtr;
    }

    void HandlerRegistrations::SetSubscriptionType()
    {
        if (Dob::Typesystem::Operations::IsOfType(m_typeId, Dob::Entity::ClassTypeId))
        {
            m_subscriptionType = EntityRegistrationSubscription;
        }
        else if (Dob::Typesystem::Operations::IsOfType(m_typeId, Dob::Service::ClassTypeId))
        {
            m_subscriptionType = ServiceRegistrationSubscription;
        }
        else
        {
            std::wostringstream ostr;
            ostr << "Type " << Typesystem::Operations::GetName(m_typeId)
                 << " is not an entity or service type";
            throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }
    }

    bool HandlerRegistrations::Register(const ConnectionPtr&                connection,
                                        const Dob::Typesystem::HandlerId&   handlerId,
                                        const InstanceIdPolicy::Enumeration instanceIdPolicy,
                                        const bool                          isInjectionHandler,
                                        const RegisterTime                  regTime,
                                        const bool                          overrideRegistration,
                                        const ConsumerId&                   consumer)
    {        
        bool regDone;

        ConnectionConsumerPair registerer(connection, consumer);

        // The input parameters are put in a struct because the current boost bind implementation can
        // handle only 9 parameters.
        RegisterInternalInput input;
        input.registerer = registerer;
        input.handlerId = handlerId;
        input.instanceIdPolicy = instanceIdPolicy;
        input.isInjectionHandler = isInjectionHandler;
        input.regTime = regTime;
        input.overrideRegistration = overrideRegistration;

        // Add a state if not already present
        m_registrations.ForSpecificStateAdd(handlerId.GetRawValue(),
                                            boost::bind(&HandlerRegistrations::RegisterInternal,
                                                        this,
                                                        _2,
                                                        boost::cref(input),
                                                        boost::ref(regDone)));

        return regDone;
    }

    void HandlerRegistrations::Unregister(const ConnectionPtr&                  connection,
                                          const Dob::Typesystem::HandlerId&     handlerId)
    {             
        m_registrations.ForSpecificStateAdd(handlerId.GetRawValue(),
                                            boost::bind(&HandlerRegistrations::UnregisterInternal,
                                                        this,
                                                        boost::cref(connection),
                                                        boost::cref(handlerId),
                                                        true,  // true => explicit unregistration
                                                        Safir::Dob::ThisNodeParameters::NodeNumber(),
                                                        connection->Id().m_contextId,
                                                        _2));
    }

    void HandlerRegistrations::RegisterInternal(const StateSharedPtr&         statePtr,
                                                const RegisterInternalInput&  inputPar,
                                                bool&                         registrationDone)
    {
        // Set up alias so the rest of the code is as easy to read as before.
        const ConnectionConsumerPair&           registerer = inputPar.registerer;
        const Dob::Typesystem::HandlerId&       handlerId = inputPar.handlerId;
        const InstanceIdPolicy::Enumeration&    instanceIdPolicy = inputPar.instanceIdPolicy;
        const bool                              isInjectionHandler = inputPar.isInjectionHandler;
        const RegisterTime&                     regTime = inputPar.regTime;
        const bool                              overrideRegistration = inputPar.overrideRegistration;

        registrationDone = false;

        // If this registration is on the revoked list we must act as if the application
        // is still the registerer
        RegistrationVector revoked = registerer.connection->GetRevokedRegistrations();
        for (RegistrationVector::iterator it = revoked.begin(); it != revoked.end(); ++it)
        {
            if (m_typeId == it->typeId && handlerId == it->handlerId)
            {
                if (registerer.consumer == it->consumer)
                {
                    // The connection and consumer is revoked. This is a NOP.
                    return;
                }
                else
                {
                    // The connection is registering using a different consumer. This is not allowed!
                    ENSURE(false,
                           << "Not allowed for a connection to register a handler that is already registered by the same connection! (revoked)");
                }
            }

        }

        DistributionData currentRegState = statePtr->GetRealState();
        ConnectionPtr currentRegisterer;
        ConsumerId currentConsumer;

        if (!currentRegState.IsNoState())
        {
            ENSURE(currentRegState.GetRegistrationTime() < regTime,
                   << "Local registration using a timestamp that is less or equal compared to the timestamp for the current registration");  
        }

        if (!currentRegState.IsNoState() && currentRegState.IsRegistered())
        {
            // There is an existing (older) registration

            if (registerer.connection->Id() == statePtr->GetConnection()->Id() &&
                registerer.consumer ==  statePtr->GetConsumer())
            {
                // It's the same connection and consumer making a registration for an already
                // registered handler.
                return;
            }

            if (!overrideRegistration)
            {
                return;
            }

            // We have an overregistration! Revoke the existing registerer.

            currentRegisterer = statePtr->GetConnection();
            currentConsumer = statePtr->GetConsumer();

            if (currentRegisterer->IsLocal())
            {
                ENSURE(registerer.connection->Id() != currentRegisterer->Id(),
                    << "Not allowed for a connection to register a handler that is already registered by the same connection!!!");
            }

            //Local register is overregistrering existing, forced revoke.
            RevokeRegisterer(false, currentRegisterer, currentConsumer, handlerId, currentRegState.GetRegistrationTime());
        }

        // If we got this far we have one of the following situations:
        // - No previous registration state OR
        // - We have unregistered a previous registerer (overregistration) OR
        // - The previous registration state is an unregistration
        //
        // In all these cases we should execute the new registration


        statePtr->SetConnection(registerer.connection);
        statePtr->SetConsumer(registerer.consumer);
        statePtr->SetOwnerRequestInQueue(registerer.connection->AddRequestInQueue(registerer.consumer));

        DistributionData newRegState(registration_state_tag,
                                     registerer.connection->Id(),
                                     m_typeId,
                                     handlerId,
                                     instanceIdPolicy,
                                     DistributionData::Registered,
                                     regTime);

        statePtr->SetRealState(newRegState);
        statePtr->SetReleased(false);

        if (currentRegisterer != NULL && currentRegisterer->IsLocal())
        {
            // Kick the current registerer so that any revoke will be dispatched
            currentRegisterer->SignalIn();
        }

        // If the handler id happens to be on the revoked list for the new registerer (unlikely) it must be removed.
        registerer.connection->RemoveRevokedRegistration(m_typeId, handlerId);

        registerer.connection->AddRegistration(m_typeId, handlerId, registerer.consumer);

        if (isInjectionHandler)
        {
            RegisterInjectionHandler(registerer.connection, handlerId, registerer.consumer);
        }

        registrationDone = true;
    }

    void HandlerRegistrations::UnregisterInternal(const ConnectionPtr&              connection,
                                                  const Dob::Typesystem::HandlerId& handlerId,
                                                  const bool                        explicitUnregister,
                                                  const NodeNumber                  nodeNumber,
                                                  const ContextId                   contextId,
                                                  const StateSharedPtr&             statePtr)
    {        
        DistributionData currentRegState = statePtr->GetRealState();

        if (connection == NULL ||
            currentRegState.IsNoState() ||
            !currentRegState.IsRegistered() ||
            connection->Id() != statePtr->GetConnection()->Id())
        {
            return;
        }        
        
        //Unregistration, not a forced revoke
        RevokeRegisterer( false,
                          statePtr->GetConnection(),
                         statePtr->GetConsumer(),
                         handlerId,
                         currentRegState.GetRegistrationTime());

        statePtr->SetConnection(ConnectionPtr());

        // Unregistration states are kept forever. This is necessary in order for the split/join
        // functionality to work.
        statePtr->SetReleased(false);

        DistributionData newRegState(registration_state_tag,
                                     ConnectionId(nodeNumber, contextId, -1),
                                     m_typeId,
                                     handlerId,
                                     InstanceIdPolicy::HandlerDecidesInstanceId, // Dummy for an unreg state
                                     explicitUnregister ? DistributionData::Unregistered : DistributionData::ImplicitUnregistered,
                                     currentRegState.GetRegistrationTime());

        statePtr->SetRealState(newRegState);
    }

    void HandlerRegistrations::RemoteRegistrationStateInternal(const ConnectionPtr&    connection,
                                                               const DistributionData& remoteRegistrationState,
                                                               const StateSharedPtr&   statePtr)
    {                   
        DistributionData currentRegState = statePtr->GetRealState();

        if (!NewRegStateIsAccepted(currentRegState, remoteRegistrationState))
        {
            return;
        }

        // We have an accepted registration state

        ConnectionPtr currentRegisterer;
        const Dob::Typesystem::HandlerId handlerId = remoteRegistrationState.GetHandlerId();
        bool isRemoteRegistration = remoteRegistrationState.IsRegistered();

        if (!currentRegState.IsNoState() && currentRegState.IsRegistered())
        {
            // There is an existing registration that must be revoked.            
            currentRegisterer = statePtr->GetConnection();
            ConsumerId currentConsumer = statePtr->GetConsumer();          
            
            if (isRemoteRegistration)
            {
                //Remote register is overregistrering existing, forced revoke
                bool forcedByRemote = connection!=NULL && !connection->IsLocal();
                RevokeRegisterer(forcedByRemote,
                                 currentRegisterer,
                                 currentConsumer,
                                 handlerId,
                                 currentRegState.GetRegistrationTime());
            }
            else //Remote unregistration
            {
                //this is a special case when we have a registered local owner but receives a 
                //unregistration from another node. This case can occur if another node overregisters
                //and then unregister immediately. Then its possible that we never got the register message
                //There must have been a newer registration on another node that we havent got, and now that
                //handler is unregistering again.
                RevokeRegisterer(true,
                                 currentRegisterer,
                                 currentConsumer,
                                 handlerId,
                                 currentRegState.GetRegistrationTime());

            }
        }
        
        if (isRemoteRegistration)
        {            
            statePtr->SetConnection(connection);
            connection->AddRegistration(m_typeId,
                                        handlerId,
                                        ConsumerId(NULL, static_cast<short>(0))); // Dummy consumer for registration state from external node
        }
        else
        {            
            statePtr->SetConnection(ConnectionPtr());
            const_cast<DistributionData&>(remoteRegistrationState).ResetSenderIdConnectionPart();
        }
        
        // All registration states are kept forever (even unregistration states) since this is necessary
        // in order for the split/join functionality to work correctly.
        statePtr->SetReleased(false);

        statePtr->SetRealState(remoteRegistrationState);

        if (currentRegisterer != NULL)
        {            
            // Kick the current register so that a revoke is dispatched
            currentRegisterer->SignalIn();
        }
    }

    void HandlerRegistrations::UnregisterAll(const ConnectionPtr& connection,
                                             const bool           explicitUnregister)
    {        
        m_registrations.ForEachState(boost::bind(&HandlerRegistrations::UnregisterAllInternal,
                                                 this,
                                                 boost::cref(connection),
                                                 explicitUnregister,
                                                 _2,
                                                 _3),
                                     false);  // no need to include already released states
    }

    void HandlerRegistrations::UnregisterAllInternal(const ConnectionPtr&           connection,
                                                     const bool                     explicitUnregister,
                                                     const StateSharedPtr&          statePtr,
                                                     bool&                          exitDispatch)
    {        
        exitDispatch = false;

        DistributionData currentRegState = statePtr->GetRealState();

        if (connection == NULL ||
            currentRegState.IsNoState() ||
            !currentRegState.IsRegistered() ||
            connection->Id() != statePtr->GetConnection()->Id())
        {
            return;
        }

        
        UnregisterInternal(connection,
                           statePtr->GetRealState().GetHandlerId(),
                           explicitUnregister,
                           statePtr->GetConnection()->Id().m_node,
                           statePtr->GetConnection()->Id().m_contextId,
                           statePtr);

    }

    void HandlerRegistrations::RemoteSetRegistrationState(const ConnectionPtr& connection,
                                                          const DistributionData& registrationState)
    {             
        Dob::Typesystem::HandlerId handlerId = registrationState.GetHandlerId();

        m_registrations.ForSpecificStateAdd(handlerId.GetRawValue(),
                                            boost::bind(&HandlerRegistrations::RemoteRegistrationStateInternal,
                                                        this,
                                                        boost::cref(connection),
                                                        boost::cref(registrationState),
                                                        _2));
    }

    bool HandlerRegistrations::IsRegistered(const Dob::Typesystem::HandlerId& handlerId) const
    {
        bool isRegistered = false;

        // For an explanation regarding why we include released states here see the corresponding
        // comment for EntityType::ReadEntiy
        m_registrations.ForSpecificState(handlerId.GetRawValue(),
                                         boost::bind(&HandlerRegistrations::IsRegisteredInternal,
                                                     this,
                                                     _2,
                                                     boost::ref(isRegistered)),
                                         true); // true => include released states);

        return isRegistered;
    }


    const ConnectionConsumerPair
    HandlerRegistrations::GetRegisterer(const Dob::Typesystem::HandlerId& handlerId) const
    {
        ConnectionConsumerPair registerer;
        InstanceIdPolicy::Enumeration instanceIdPolicy;  // Dummy in this case
        RegisterTime registrationTime;  // Dummy in this case

        // For an explanation regarding why we include released states here see the corresponding
        // comment for EntityType::ReadEntiy
        m_registrations.ForSpecificState(handlerId.GetRawValue(),
                                         boost::bind(&HandlerRegistrations::GetRegistererInternal,
                                                     this,
                                                     _2,
                                                     boost::ref(registerer),
                                                     boost::ref(instanceIdPolicy),
                                                     boost::ref(registrationTime)),
                                         true); // true => include released states
        return registerer;
    }

    bool
    HandlerRegistrations::GetRegisterer(const Dob::Typesystem::HandlerId& handlerId,
                                        ConnectionConsumerPair&           registerer,
                                        RegisterTime&                     registrationTime) const
    {
        InstanceIdPolicy::Enumeration instanceIdPolicy;  // Dummy in this case

        // For an explanation regarding why we include released states here see the corresponding
        // comment for EntityType::ReadEntiy
        m_registrations.ForSpecificState(handlerId.GetRawValue(),
                                         boost::bind(&HandlerRegistrations::GetRegistererInternal,
                                                     this,
                                                     _2,
                                                     boost::ref(registerer),
                                                     boost::ref(instanceIdPolicy),
                                                     boost::ref(registrationTime)),
                                         true); // true => include released states

        return registerer.connection != NULL;
    }

    bool
    HandlerRegistrations::GetRegisterer(const Dob::Typesystem::HandlerId& handlerId,
                                        ConnectionConsumerPair&           registerer,
                                        InstanceIdPolicy::Enumeration&    instanceIdPolicy) const
    {
        RegisterTime registrationTime; // Dummy in this case

        // For an explanation regarding why we include released states here see the corresponding
        // comment for EntityType::ReadEntiy
        m_registrations.ForSpecificState(handlerId.GetRawValue(),
                                         boost::bind(&HandlerRegistrations::GetRegistererInternal,
                                                     this,
                                                     _2,
                                                     boost::ref(registerer),
                                                     boost::ref(instanceIdPolicy),
                                                     boost::ref(registrationTime)),
                                         true); // true => include released states

        return registerer.connection != NULL;
    }

     const LockedStateResult
     HandlerRegistrations::GetLockedRegistrationState(const Dob::Typesystem::HandlerId& handlerId,
                                                      const bool                        includeReleasedStates) const
     {
        return m_registrations.GetSpecificState(handlerId.GetRawValue(), includeReleasedStates);
     }

    void HandlerRegistrations::Subscribe(const SubscriptionId&              subscriptionId,
                                         const Dob::Typesystem::HandlerId&  handlerId,
                                         bool                               restartSubscription,
                                         const SubscriptionOptionsPtr&      subscriptionOptions)
    {
        bool allHandlers = handlerId == Dob::Typesystem::HandlerId::ALL_HANDLERS;

        m_registrations.Subscribe(subscriptionId,
                                  handlerId.GetRawValue(),
                                  allHandlers,
                                  restartSubscription,
                                  subscriptionOptions);
    }

    void HandlerRegistrations::Unsubscribe(const SubscriptionId&                subscriptionId,
                                           const Dob::Typesystem::HandlerId&    handlerId)
    {
        bool allHandlers = handlerId == Dob::Typesystem::HandlerId::ALL_HANDLERS;

        m_registrations.Unsubscribe(subscriptionId,
                                    handlerId.GetRawValue(),
                                    allHandlers);
    }

    void HandlerRegistrations::UnsubscribeAll(const ConnectionPtr& connection)
    {
         m_registrations.UnsubscribeAll(connection);
    }

    bool HandlerRegistrations::HasSubscription(const ConnectionPtr&    connection,
                                               const ConsumerId&       consumer) const
    {
        SubscriptionId subscriptionId(ConnectionConsumerPair(connection, consumer), m_subscriptionType, 0);

        return m_registrations.HasSubscription(subscriptionId);     
    }

    void HandlerRegistrations::IsRegisteredInternal(const StateSharedPtr&   statePtr,
                                                    bool&                   isRegistered) const
    {
        isRegistered = IsRegisteredInternal(statePtr);
    }

    bool HandlerRegistrations::IsRegisteredInternal(const StateSharedPtr& statePtr) const
    {
        if (!statePtr || statePtr->IsReleased())
        {
            return false;
        }

        DistributionData realState = statePtr->GetRealState();

        return !realState.IsNoState() && realState.IsRegistered();
    }

    void HandlerRegistrations::GetRegistererInternal(const StateSharedPtr&          statePtr,
                                                     ConnectionConsumerPair&        registerer,
                                                     InstanceIdPolicy::Enumeration& instanceIdPolicy,
                                                     RegisterTime&                  registrationTime) const
    {
        if (IsRegisteredInternal(statePtr))
        {
            registerer.connection = statePtr->GetConnection();
            registerer.consumer = statePtr->GetConsumer();
            instanceIdPolicy = statePtr->GetRealState().GetInstanceIdPolicy();
            registrationTime = statePtr->GetRealState().GetRegistrationTime();
        }
        else
        {
            registerer.connection.reset();
            registerer.consumer = ConsumerId(NULL,0L);
        }
    } 

    void HandlerRegistrations::RevokeRegisterer(const bool                            forcedByRemoteHandler,
                                                const ConnectionPtr&                  connection,
                                                const ConsumerId&                     consumer,
                                                const Dob::Typesystem::HandlerId&     handlerId,
                                                const RegisterTime                    currentRegisterTime)
    {        
        ENSURE(connection != NULL, << "HandlerRegistrations::RevokeRegisterer called with a NULL connection!");    

        if (connection->IsLocal())
        {
            RemoveRegistration(connection, handlerId);
            ENSURE(consumer != ConsumerId(), << "RevokeRegisterer called with a \"NULL\" consumer!");
            connection->AddRevokedRegistration(m_typeId, handlerId, consumer);
        }

        if (m_entityContainerPtr != NULL)
        {
            // This is an unregistration for en entity type so delete (or set as ghost) all instances
            // owned by the current registerer.

            //If there is already a new handler and that handler resides on another node we simply delete the entities here.
            //The entities have been ghosted (if persistent) on the node where the new handler exists, and will be injected there.
            //If we keep ghosts here, we get strange behaviour after a split/join where old ghosted entities are injected again and the.                        
            m_entityContainerPtr->ForEachState(boost::bind(&HandlerRegistrations::RevokeEntity,
                                               this,
                                               forcedByRemoteHandler,
                                               _2,
                                               connection,
                                               handlerId,
                                               currentRegisterTime,                                               
                                               _3),
                                               false);  // No need to include already released states
        }
    }

    void HandlerRegistrations::RevokeEntity(const bool                           forcedByRemoteHandler,
                                            const StateSharedPtr&                statePtr,
                                            const ConnectionPtr&                 connection,
                                            const Dob::Typesystem::HandlerId&    handlerId,
                                            const RegisterTime                   currentRegisterTime,                                            
                                            bool&                                exitDispatch)
    {            
        if (forcedByRemoteHandler)
        {
            //Forced Revoke by handler that resides on another node
            //In that case we rely on that the other node already have the entities that is gonna be taken over.
            ForcedRevokeEntityByRemoteHandler(statePtr, connection, handlerId, currentRegisterTime, exitDispatch);                           
        }
        else
        {             
            //Unforced revoke, or forced revoke by handler on the same node
            if (statePtr->GetRealState().GetEntityStateKind() == DistributionData::Ghost)
            {
                UpdateGhost(statePtr, handlerId, currentRegisterTime, exitDispatch);
            }
            else
            {
                DeleteEntity(statePtr, connection, handlerId, false, exitDispatch);
            }                   
        }
    }

    void HandlerRegistrations::ForcedRevokeEntityByRemoteHandler(
                                                  const StateSharedPtr&                statePtr,
                                                  const ConnectionPtr&                 connection,
                                                  const Dob::Typesystem::HandlerId&    handlerId,
                                                  const RegisterTime                   currentRegisterTime,                          
                                                  bool&                                exitDispatch)
    {        
        //We delete all entities so that dope
        exitDispatch = false;

        if (statePtr->GetConnection() == NULL || connection->Id() != statePtr->GetConnection()->Id())
        {
            // This connection is not the owner.
            return;
        }

        if (!statePtr->GetRealState().IsCreated())
        {
            return;
        }

        DistributionData realState = statePtr->GetRealState();

        if (handlerId != Dob::Typesystem::HandlerId::ALL_HANDLERS &&
            realState.GetHandlerId() != handlerId)
        {
            // The unregistration is not for this handler.
            return;
        }
     
        ConnectionPtr nullConnection(static_cast<Connection*>(NULL));
        statePtr->SetConnection(nullConnection);
        statePtr->SetConsumer(ConsumerId(NULL, static_cast<short>(0)));

        DistributionData newRealState = realState.GetEntityStateCopy(false);        
        statePtr->SetReleased(true);
        newRealState.ResetSenderIdConnectionPart();
        newRealState.SetExplicitlyDeleted(true); //TODO: this is to make dope delete the entity, but we should figure a better way.
        newRealState.IncrementVersion();
             
        // Release pointer to request in queue. We must not have any shared pointers to request queues
        // when the connection (and therefore the queue container) is destructed.
        statePtr->ResetOwnerRequestInQueue();

        statePtr->SetRealState(newRealState);      

        // This is an end state so it must be saved "a while".
        EndStates::Instance().Add(statePtr);
    }    

    void HandlerRegistrations::DeleteEntity(const StateSharedPtr&                statePtr,
                                            const ConnectionPtr&                 connection,
                                            const Dob::Typesystem::HandlerId&    handlerId,
                                            const bool                           newOwnerExists,
                                            bool&                                exitDispatch)
    {
        exitDispatch = false;

        if (statePtr->GetConnection() == NULL || connection->Id() != statePtr->GetConnection()->Id())
        {
            // This connection is not the owner.
            return;
        }

        if (!statePtr->GetRealState().IsCreated())
        {
            return;
        }

        DistributionData realState = statePtr->GetRealState();

        if (handlerId != Dob::Typesystem::HandlerId::ALL_HANDLERS &&
            realState.GetHandlerId() != handlerId)
        {
            // The unregistration is not for this handler.
            return;
        }
     
        statePtr->SetConnection(ConnectionPtr());
        statePtr->SetConsumer(ConsumerId(NULL, static_cast<short>(0)));

        DistributionData newRealState(no_state_tag);       

        // Decide if the state should be saved as a ghost
        
        switch (InjectionKindTable::Instance().GetInjectionKind(m_typeId))
        {
            case InjectionKind::None:
            {
                newRealState = realState.GetEntityStateCopy(false);  // don't include blob
                // Set the state to released which will cause a drop of the reference from
                // the state container to this state.
                statePtr->SetReleased(true);
            }
            break;

            case InjectionKind::SynchronousVolatile:
            case InjectionKind::SynchronousPermanent:
            case InjectionKind::Injectable:
            {
                newRealState = realState.GetEntityStateCopy(true);  // include blob
                newRealState.SetEntityStateKind(DistributionData::Ghost);
                // Set the state to NOT released so that the reference from the state container is kept.
                statePtr->SetReleased(false);
            }
            break;
        }

        newRealState.ResetSenderIdConnectionPart();
        newRealState.SetExplicitlyDeleted(false);

        // Normally the entity version gets incremented for every new state, but, there is an
        // exception to this rule and this is when the deletion (or ghosting) of an entity is
        // caused by a remote node being marked as down. In this case we actually *decrements*
        // the version number so if the node joins again the version from the remote node will
        // override our version.
        if (!connection->IsLocal() && connection->NodeIsDown())
        {
            newRealState.DecrementVersion();
        }
        else
        {
            newRealState.IncrementVersion();
        }

        // Release pointer to request in queue. We must not have any shared pointers to request queues
        // when the connection (and therefore the queue container) is destructed.
        statePtr->ResetOwnerRequestInQueue();

        statePtr->SetRealState(newRealState);

        // This is an end state so it must be saved "a while".
        EndStates::Instance().Add(statePtr);
    }

    void HandlerRegistrations::UpdateGhost(const StateSharedPtr&                statePtr,
                                           const Dob::Typesystem::HandlerId&    handlerId,
                                           const RegisterTime                   currentRegisterTime,
                                           bool&                                exitDispatch)
    {
        exitDispatch = false;

        DistributionData realState = statePtr->GetRealState();

        if (handlerId == Dob::Typesystem::HandlerId::ALL_HANDLERS ||
            realState.GetHandlerId() != handlerId)
        {
            // Not for this handler.
            return;
        }

        if (!(realState.GetRegistrationTime() < currentRegisterTime))
        {
            // We are only concerned with ghosts that are "older" that the currentRegisterTime.
            return;
        }
     
        DistributionData newRealState(no_state_tag);

        newRealState = realState.GetEntityStateCopy(true);  // include blob

        // Any ghosts that haven't been injected yet can be lost forever
        // if there is an unregistration (forced or not). Since they have
        // a registartion time that is older than the current reg/unreg time
        // they will be deleted/discarded on all nodes. The solution is
        // to update their registration time to the current reg/unreg time.
        newRealState.SetRegistrationTime(currentRegisterTime);

        // Set the state to NOT released so that the reference from the state container is kept.
        statePtr->SetReleased(false);

        statePtr->SetRealState(newRealState);
    }

    void HandlerRegistrations::RegisterInjectionHandler(const ConnectionPtr&      connection,
                                                        const Dob::Typesystem::HandlerId&   handlerId,
                                                        const ConsumerId&                   consumer)
    {
        if (!connection->IsLocal())
        {
            return;
        }

        // Always add an empty (no instances) initial injection. If there are no existing instances
        // the subscribe call below won't add any instances but we still have a handle so we can
        // generate an OnInitialInjectionsDone callback when the app is dispatching.
        connection->AddEmptyInitialInjection(m_typeId,
                                             handlerId);
        connection->SignalIn();

        // Add an injection subscription
        SubscriptionId subscriptionId(ConnectionConsumerPair(connection, consumer),
                                      InjectionSubscription,
                                      handlerId.GetRawValue());

         m_entityContainerPtr->Subscribe(subscriptionId,
                                         0,
                                         true,                             // true => all instances
                                         true,                             // true => restart subscription
                                         SubscriptionOptionsPtr(NULL));    // options not used for injection subscription

        // Save the consumer, it is needed when unsubscribing for the injection subscriptions.
        connection->AddInjectionHandler(m_typeId, handlerId, consumer);
    }

    void HandlerRegistrations::RemoveRegistration(const ConnectionPtr& connection, 
        const Dob::Typesystem::HandlerId& handlerId)
    {
        if (connection->IsLocal())
        {
            RegistrationVector injectionHandlers = connection->GetInjectionHandlers();
            for (RegistrationVector::iterator it = injectionHandlers.begin(); it != injectionHandlers.end(); ++it)
            {
                if (it->typeId == m_typeId &&
                    (it->handlerId == handlerId || handlerId == Dob::Typesystem::HandlerId::ALL_HANDLERS))
                {
                    SubscriptionId subscriptionId(ConnectionConsumerPair(connection, it->consumer),
                        InjectionSubscription,
                        it->handlerId.GetRawValue());

                    m_entityContainerPtr->Unsubscribe(subscriptionId,
                        0,
                        true);          // true => all instances
                }
            }

        }

        connection->RemoveRegistration(m_typeId, handlerId);
    }

    bool HandlerRegistrations::NewRegStateIsAccepted(const DistributionData& currentRegState,
                                                     const DistributionData& newRegState)
    {
        if (currentRegState.IsNoState())
        {
            return true;
        }
        
        const LamportTimestamp currentRegTime = currentRegState.GetRegistrationTime();
        const LamportTimestamp newRegTime = newRegState.GetRegistrationTime();

        if (newRegTime < currentRegTime)
        {
            return false;
        }
        else if (currentRegTime < newRegTime)
        {
            return true;
        }

        // The registration time is the same, compare the state kind
        return newRegState.GetRegistrationStateKind() > currentRegState.GetRegistrationStateKind();
    }

    bool HandlerRegistrations::CanAcquireContainerWriterLock(const boost::posix_time::seconds& lockTimeout) const
    {
        return m_registrations.CanAcquireContainerWriterLock(lockTimeout);
    }
}
}
}
