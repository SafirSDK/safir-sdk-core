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
    }

    HandlerRegistrations::HandlerRegistrations(const Typesystem::TypeId typeId,
                                               const StateContainerPtr& entityContainerPtr)
        : m_typeId(typeId),
          m_registrations(typeId),
          m_entityContainerPtr(entityContainerPtr)
    {
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
                                          const Dob::Typesystem::HandlerId&     handlerId,
                                          const RegisterTime                    regTime)
    {
            m_registrations.ForSpecificStateAddAndRelease(handlerId.GetRawValue(),
                                                          boost::bind(&HandlerRegistrations::UnregisterInternal,
                                                                      this,
                                                                      boost::cref(connection),
                                                                      boost::cref(handlerId),
                                                                      regTime,
                                                                      Safir::Dob::ThisNodeParameters::NodeNumber(),
                                                                      _2,
                                                                      _3));
    }

    void HandlerRegistrations::RegisterInternal(const UpgradeableStateResult& upgradeableStateResult,
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

        const StateSharedPtr& statePtr = upgradeableStateResult.first;

        registrationDone = false;

        DistributionData currentRegState = statePtr->GetRealState();

        if (!currentRegState.IsNoState())
        {
            // There is an existing registration state

            if (regTime < currentRegState.GetRegistrationTime())
            {
                // The existing registration state (reg or unreg) is newer. Skip this registration.

                if (overrideRegistration)
                {
                    // We have an override registration that is rejected. This is a situation that must be
                    // handled with special care.
                    // Add this handler id to the revoked vector for the connection (even though the application
                    // never managed to register the handler). This means that a revoke will be dispatched soon
                    // after the return from this register call.
                    registerer.connection->AddRevokedRegistration(m_typeId, handlerId, registerer.consumer);
                    registerer.connection->SignalIn();

                    registrationDone = true;
                }

                return;
            }
            else if (currentRegState.GetRegistrationTime() < regTime)
            {
                // The existing registration state (reg or unreg) is older.

                if (currentRegState.IsRegistered())
                {
                    if (registerer.connection->Id() == statePtr->GetConnection()->Id() &&
                        registerer.consumer ==  statePtr->GetConsumer())
                    {
                        // It's the same connection and consumer making a registration for an already
                        // registered handler.
                        return;
                    }

                    // There is an existing old reg
                    if (!overrideRegistration)
                    {
                        return;
                    }

                    // We have an overregistration! Unregister the existing registerer.

                    const ConnectionPtr currentRegisterer = statePtr->GetConnection();
                    ConsumerId currentConsumer = statePtr->GetConsumer();

                    currentRegisterer->RemoveRegistration(m_typeId, handlerId);

                    if (m_entityContainerPtr != NULL)
                    {
                        // This is an unregistration for en entity type so delete (or set as ghost) all instances
                        // owned by the current registerer.
                        m_entityContainerPtr->ReleaseEachState(boost::bind(&HandlerRegistrations::DeleteEntity,
                                                                           this,
                                                                           _2,
                                                                           currentRegisterer,
                                                                           handlerId,
                                                                           _3,
                                                                           _4));
                    }

                    if (currentRegisterer->IsLocal())
                    {
                        ENSURE(registerer.connection->Id() != currentRegisterer->Id(),
                            << "Not allowed for a connection to overregister a handler that is already registered by the same connection!!!");

                        // If the current handler is an injection handler some additional work must be done
                        UnregisterInjectionHandler(currentRegisterer, handlerId);

                        currentRegisterer->AddRevokedRegistration(m_typeId, handlerId, currentConsumer);
                        currentRegisterer->SignalIn();
                    }
                }
            }
            else
            {
                // The registration times are the same. This is the same registration that we have or
                // it is a registration that has been unregistered
                return;
            }
        }

        // If we got this far we have one of the following situations:
        // - No previous registration state OR
        // - We have unregistered a previous registerer (overregistration) OR
        // - The previous registration state is an old unreg
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
                                     true,  // true => registered
                                     regTime);

        statePtr->SetRealState(newRegState);

        // If the handler id happens to be on the revoked list (unlikely) it must be removed.
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
                                                  const RegisterTime                registerTime,
                                                  const NodeNumber                  nodeNumber,
                                                  const UpgradeableStateResult&     upgradeableStateResult,
                                                  bool&                             dontRelease)
    {
        dontRelease = true;

        const StateSharedPtr& statePtr = upgradeableStateResult.first;

        DistributionData currentRegState = statePtr->GetRealState();

        if (connection != NULL && connection->IsLocal())
        {
            // A local connection that thinks it is the current registrerer. Is this so?
            if (currentRegState.IsNoState())
            {
                dontRelease = false;
                return;
            }
            else if(!currentRegState.IsRegistered() || connection->Id() != statePtr->GetConnection()->Id())
            {
                return;
            }
        }

        RegisterTime regTime = registerTime;

        if (!currentRegState.IsNoState())
        {
            // There is an existing registration state

            if (!(regTime != RegisterTime()))
            {
                // There is no supplied registration time so it must be an unregister by the local owner.
                // In this case we fetch the time from the current registration state.
                regTime = currentRegState.GetRegistrationTime();
            }

            if (regTime < currentRegState.GetRegistrationTime())
            {
                // The existing registration state (reg or unreg) is newer. Skip this unregistration.
                return;
            }
            else if (currentRegState.IsRegistered())
            {
                // The existing registration state is a reg which is older or has the same timestamp
                // as this unregistration

                ConnectionPtr currentRegisterer = statePtr->GetConnection();

                // If the handler is an injection handler some additional work must be done
                UnregisterInjectionHandler(currentRegisterer, handlerId);

                currentRegisterer->RemoveRegistration(m_typeId, handlerId);

                if (m_entityContainerPtr != NULL)
                {
                    // This is an unregistration for en entity type so delete (or set as ghost) all instances
                    // owned by the current registerer.
                    m_entityContainerPtr->ReleaseEachState(boost::bind(&HandlerRegistrations::DeleteEntity,
                                                                       this,
                                                                       _2,
                                                                       currentRegisterer,
                                                                       handlerId,
                                                                       _3,
                                                                       _4));
                }
            }
        }

        statePtr->SetConnection(NULL);

        DistributionData newRegState(registration_state_tag,
                                     ConnectionId(nodeNumber, -1),
                                     m_typeId,
                                     handlerId,
                                     InstanceIdPolicy::HandlerDecidesInstanceId, // Dummy for an unreg state
                                     false,  // false => unregistered
                                     regTime);

        statePtr->SetRealState(newRegState);

        // This is an end state so it must be saved "a while".
        EndStates::Instance().Add(statePtr);

        // Drop the reference from the state container to this state.
        dontRelease = false;
    }

    void HandlerRegistrations::UnregisterAll(const ConnectionPtr& connection)
    {
        m_registrations.ReleaseEachState(boost::bind(&HandlerRegistrations::UnregisterAllInternal,
                                                     this,
                                                     boost::cref(connection),
                                                     _2,
                                                     _3,
                                                     _4));
    }

    void HandlerRegistrations::UnregisterAllInternal(const ConnectionPtr&           connection,
                                                     const UpgradeableStateResult&  upgradeableStateResult,
                                                     bool&                          dontRelease,
                                                     bool&                          exitDispatch)
    {
        exitDispatch = false;

        const StateSharedPtr& statePtr = upgradeableStateResult.first;

        if (connection != NULL && connection->Id() == statePtr->GetConnection()->Id() &&
            !statePtr->GetRealState().IsNoState() && statePtr->GetRealState().IsRegistered())
        {
            UnregisterInternal(connection,
                               statePtr->GetRealState().GetHandlerId(),
                               RegisterTime(),
                               statePtr->GetConnection()->Id().m_node,
                               upgradeableStateResult,
                               dontRelease);
        }
        else
        {
            dontRelease = true;
        }
    }

    void HandlerRegistrations::RemoteSetRegistrationState(const ConnectionPtr& connection,
                                                          const DistributionData& registrationState)
    {
        Register(connection,
                 registrationState.GetHandlerId(),
                 registrationState.GetInstanceIdPolicy(),
                 false,     // isInjectionHandler dosn't matter for registrations received from an external node.
                 registrationState.GetRegistrationTime(),
                 true,     // override is always set to true for registrations received from an external node.
                 ConsumerId(NULL, static_cast<short>(0))); // dummy consumer for external connection
    }

    void HandlerRegistrations::RemoteSetUnregistrationState(const DistributionData& registrationState)
    {
        Dob::Typesystem::HandlerId handlerId = registrationState.GetHandlerId();

        m_registrations.ForSpecificStateAddAndRelease(handlerId.GetRawValue(),
                                                      boost::bind(&HandlerRegistrations::UnregisterInternal,
                                                                  this,
                                                                  ConnectionPtr(),
                                                                  boost::cref(handlerId),
                                                                  registrationState.GetRegistrationTime(),
                                                                  registrationState.GetSenderId().m_node,
                                                                  _2,
                                                                  _3));
    }

    bool HandlerRegistrations::IsRegistered(const Dob::Typesystem::HandlerId& handlerId) const
    {
        bool isRegistered = false;

        m_registrations.ForSpecificState(handlerId.GetRawValue(),
                                         boost::bind(&HandlerRegistrations::IsRegisteredInternal,
                                                     this,
                                                     _2,
                                                     boost::ref(isRegistered)),
                                         false); // false => don't include released states);

        return isRegistered;
    }


    const ConnectionConsumerPair
    HandlerRegistrations::GetRegisterer(const Dob::Typesystem::HandlerId& handlerId) const
    {
        ConnectionConsumerPair registerer;
        InstanceIdPolicy::Enumeration instanceIdPolicy;  // Dummy in this case
        RegisterTime registrationTime;  // Dummy in this case

        m_registrations.ForSpecificState(handlerId.GetRawValue(),
                                         boost::bind(&HandlerRegistrations::GetRegistererInternal,
                                                     this,
                                                     _2,
                                                     boost::ref(registerer),
                                                     boost::ref(instanceIdPolicy),
                                                     boost::ref(registrationTime)),
                                         false); // false => don't include released states
        return registerer;
    }

    bool
    HandlerRegistrations::GetRegisterer(const Dob::Typesystem::HandlerId& handlerId,
                                        ConnectionConsumerPair&           registerer,
                                        RegisterTime&                     registrationTime) const
    {
        InstanceIdPolicy::Enumeration instanceIdPolicy;  // Dummy in this case

        m_registrations.ForSpecificState(handlerId.GetRawValue(),
                                         boost::bind(&HandlerRegistrations::GetRegistererInternal,
                                                     this,
                                                     _2,
                                                     boost::ref(registerer),
                                                     boost::ref(instanceIdPolicy),
                                                     boost::ref(registrationTime)),
                                         false); // false => don't include released states

        return registerer.connection != NULL;
    }

    bool
    HandlerRegistrations::GetRegisterer(const Dob::Typesystem::HandlerId& handlerId,
                                        ConnectionConsumerPair&           registerer,
                                        InstanceIdPolicy::Enumeration&    instanceIdPolicy) const
    {
        RegisterTime registrationTime; // Dummy in this case

        m_registrations.ForSpecificState(handlerId.GetRawValue(),
                                         boost::bind(&HandlerRegistrations::GetRegistererInternal,
                                                     this,
                                                     _2,
                                                     boost::ref(registerer),
                                                     boost::ref(instanceIdPolicy),
                                                     boost::ref(registrationTime)),
                                         false); // false => don't include released states

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

    void HandlerRegistrations::IsRegisteredInternal(const UpgradeableStateResult&  upgradeableStateResult,
                                                    bool&                          isRegistered) const
    {
        const StateSharedPtr& statePtr = upgradeableStateResult.first;

        isRegistered = IsRegisteredInternal(statePtr);
    }

    bool HandlerRegistrations::IsRegisteredInternal(const StateSharedPtr& statePtr) const
    {
        if (!statePtr)
        {
            return false;
        }

        DistributionData realState = statePtr->GetRealState();

        return !realState.IsNoState() && realState.IsRegistered();
    }

    void HandlerRegistrations::GetRegistererInternal(const UpgradeableStateResult&  upgradeableStateResult,
                                                     ConnectionConsumerPair&        registerer,
                                                     InstanceIdPolicy::Enumeration& instanceIdPolicy,
                                                     RegisterTime&                  registrationTime) const
    {
        const StateSharedPtr& statePtr = upgradeableStateResult.first;

        if (IsRegisteredInternal(statePtr))
        {
            registerer.connection = statePtr->GetConnection();
            registerer.consumer = statePtr->GetConsumer();
            instanceIdPolicy = statePtr->GetRealState().GetInstanceIdPolicy();
            registrationTime = statePtr->GetRealState().GetRegistrationTime();
        }
        else
        {
            registerer.connection = NULL;
            registerer.consumer = ConsumerId(NULL,0L);
        }
    }

    void HandlerRegistrations::DeleteEntity(const UpgradeableStateResult&        upgradeableStateResult,
                                            const ConnectionPtr&                 connection,
                                            const Dob::Typesystem::HandlerId&    handlerId,
                                            bool&                                dontRelease,
                                            bool&                                exitDispatch)
    {
        dontRelease = true;
        exitDispatch = false;

        const StateSharedPtr& statePtr = upgradeableStateResult.first;

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

        statePtr->SetConnection(NULL);
        statePtr->SetConsumer(ConsumerId(NULL, static_cast<short>(0)));

        DistributionData newRealState(no_state_tag);

        // Decide if the state should be saved as a ghost

        switch (InjectionKindTable::Instance().GetInjectionKind(m_typeId))
        {
            case InjectionKind::None:
            {
                newRealState = realState.GetEntityStateCopy(false);  // don't include blob
                dontRelease = false;  // This state should not be kept
            }
            break;

            case InjectionKind::SynchronousVolatile:
            case InjectionKind::SynchronousPermanent:
            case InjectionKind::Injectable:
            {
                newRealState = realState.GetEntityStateCopy(true);  // include blob
                newRealState.SetEntityStateKind(DistributionData::Ghost);
            }
            break;
        }

        newRealState.ResetSenderIdConnectionPart();
        newRealState.IncrementVersion();
        newRealState.SetExplicitlyDeleted(false);

        // Release pointer to request in queue. We must not have any shared pointers to request queues
        // when the connection (and therefore the queue container) is destructed.
        statePtr->ResetOwnerRequestInQueue();

        statePtr->SetRealState(newRealState);

        // This is an end state so it must be saved "a while".
        EndStates::Instance().Add(statePtr);
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

    void HandlerRegistrations::UnregisterInjectionHandler(const ConnectionPtr&                connection,
                                                          const Dob::Typesystem::HandlerId&   handlerId)
    {
        if (!connection->IsLocal())
        {
            return;
        }

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

                connection->RemoveInjectionHandler(m_typeId, it->handlerId);
            }
        }

        // If the injection handler is unregistered (e.g. caused by an overregistration), the data that is used to determine
        // when an OnInitialInjectionsDone should be sent, must be cleared
        connection->RemoveInitialInjectionInstance(m_typeId,
                                                   handlerId,
                                                   Dob::Typesystem::InstanceId(),
                                                   true);  //true => all instances
    }
}
}
}
