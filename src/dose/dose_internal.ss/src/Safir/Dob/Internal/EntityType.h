/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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

#ifndef _dose_internal_entity_type_h
#define _dose_internal_entity_type_h

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/HandlerRegistrations.h>
#include <Safir/Dob/Internal/SubscriptionId.h>
#include <Safir/Dob/InjectionKind.h>
#include <Safir/Dob/InstanceIdPolicy.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API EntityType:
        public SharedMemoryObject
    {
    public:

        explicit EntityType(const Typesystem::TypeId typeId);

        Dob::Typesystem::TypeId GetTypeId() const {return m_typeId;}

        /**
         * @name Handler registrations
         */
        /** @{ */

        bool Register(const ConnectionPtr&                  connection,
                      const Dob::Typesystem::HandlerId&     handlerId,
                      const InstanceIdPolicy::Enumeration   instanceIdPolicy,
                      const bool                            isInjectionHandler,
                      const RegisterTime                    regTime,
                      const bool                            overrideRegistration,
                      const ConsumerId&                     consumer);

        void Unregister(const ConnectionPtr&                connection,
                        const Dob::Typesystem::HandlerId&   handlerId);

        void UnregisterAll(const ConnectionPtr& connection);

        /** New registration state from external node */
        void RemoteSetRegistrationState(const ConnectionPtr& connection,
                                        const DistributionData& registrationState);

        /** Unregistration (an end state) from external node */
        void RemoteSetUnregistrationState(const DistributionData& registrationState);

        bool IsRegistered(const Dob::Typesystem::HandlerId& handlerId) const;

        const ConnectionConsumerPair GetRegisterer(const Dob::Typesystem::HandlerId& handlerId) const;

        bool GetRegisterer(const Dob::Typesystem::HandlerId& handlerId,
                           ConnectionConsumerPair&           registerer,
                           InstanceIdPolicy::Enumeration&    instanceIdPolicy) const;

        /** @} */

        /**
         * @name Handler registration subscriptions
         */
        /** @{ */
        void SubscribeRegistration(const SubscriptionId&                subscriptionId,
                                   const Dob::Typesystem::HandlerId&    handlerId,
                                   const bool                           restartSubscription,
                                   const SubscriptionOptionsPtr&        subscriptionOptions);

        void UnsubscribeRegistration(const SubscriptionId&              subscriptionId,
                                     const Dob::Typesystem::HandlerId&  handlerId);

        // Returns true if the given connection/consumer has any entity handler registration
        // subscription for this type.
        bool HasRegistrationSubscription(const ConnectionPtr&    connection,
                                         const ConsumerId&       consumer) const;

        /** @} */

        /**
         * @name Entity state
         */
        /** @{ */

        void SetEntity(const ConnectionPtr&                 connection,
                       const Dob::Typesystem::HandlerId&    handlerId,
                       const Dob::Typesystem::InstanceId&   instanceId,
                       const char* const                    blob,
                       const bool                           considerChangeFlags,
                       const bool                           initialInjection);

        void DeleteEntity(const ConnectionPtr&                  connection,
                          const Dob::Typesystem::HandlerId&     handlerId,
                          const Dob::Typesystem::InstanceId&    instanceId,
                          const bool                            allInstances);
        /** @} */

        /**
         * @name Entity injection
         */
        /** @{ */
        void InjectEntity(const ConnectionPtr&                 connection,
                          const Dob::Typesystem::HandlerId&    handlerId,
                          const Dob::Typesystem::InstanceId&   instanceId,
                          const char* const                    blob,
                          const Dob::Typesystem::Int64         timestamp);

        void InjectDeletedEntity(const ConnectionPtr&                 connection,
                                 const Dob::Typesystem::HandlerId&    handlerId,
                                 const Dob::Typesystem::InstanceId&   instanceId,
                                 const Dob::Typesystem::Int64         timestamp);

        /** @} */

        /**
         * @name Entity injection acceptance
         */
        /** @{ */
        void AcceptInjection(const ConnectionPtr&       connection,
                             DistributionData&          injectionState,
                             const DistributionData&    originalInjectionState);
        /** @} */

        /**
         * @name Methods to be used when application overrides the received injection
         */
        /** @{ */
        void SetInjection(const ConnectionPtr&                 connection,
                          const Dob::Typesystem::HandlerId&    handlerId,
                          const Dob::Typesystem::InstanceId&   instanceId,
                          const bool                           considerChangeFlags,
                          const char* const                    blob,
                          const DistributionData&              originalInjectionState);

        void DeleteInjection(const ConnectionPtr&                 connection,
                             const Dob::Typesystem::HandlerId&    handlerId,
                             const Dob::Typesystem::InstanceId&   instanceId,
                             const DistributionData&              originalInjectionState);
        /** @} */

        /**
         * @name Methods to be used to set states received from an external node
         */
        /** @{ */

        /** Set a ghost from external node. */
        void RemoteSetGhostEntityState(const DistributionData& entityState);

        /** Set an injection from external node. */
        void RemoteSetInjectionEntityState(const DistributionData& entityState);

        /** Set a delete (an end state) from external node */
        void RemoteSetDeleteEntityState(const DistributionData&   entityState);

        /** Set a state (that is not a ghost, injection or delete state) from external node. */
        RemoteSetResult RemoteSetRealEntityState(const ConnectionPtr&      connection,
                                                 const DistributionData&   entityState);

        /** @} */

        /**
         * @name Entity state subscriptions
         */
        /** @{ */

        void Subscribe(const SubscriptionId&                subscriptionId,
                       const Dob::Typesystem::InstanceId&   instanceId,
                       const bool                           allInstances,
                       const bool                           restartSubscription,
                       const SubscriptionOptionsPtr&        subscriptionOptions);

        void Unsubscribe(const SubscriptionId&              subscriptionId,
                         const Dob::Typesystem::InstanceId& instanceId,
                         const bool                         allInstances);

        // Returns true if the given connection/consumer has any entity subscription for this type.
        bool HasEntitySubscription(const ConnectionPtr&    connection,
                                   const ConsumerId&       consumer) const;

        /** @} */

        // Removes all existing registration and entity subscriptions (all consumers) for the given connection.
        void UnsubscribeAll(const ConnectionPtr& connection);

        const DistributionData ReadEntity(const Dob::Typesystem::InstanceId& instanceId) const;

        //throws NotFoundException if no such instance
        const Dob::Typesystem::HandlerId GetHandlerOfInstance(const Dob::Typesystem::InstanceId& instanceId) const;

        bool IsCreated(const Dob::Typesystem::InstanceId& instanceId) const;

        bool IsOwner(const Dob::Typesystem::InstanceId& instanceId,
                     const Dob::Typesystem::HandlerId&  handlerId,
                     const ConnectionConsumerPair&      registerer) const;

        const StateContainer::Iterator CreateEntityIterator(bool& end) const
        {return m_entityStates.CreateStateIterator(end);}

        bool IncrementIterator(StateContainer::Iterator& iterator) const
        {return m_entityStates.IncrementIterator(iterator);}

    private:
        Typesystem::TypeId            m_typeId;
        InjectionKind::Enumeration    m_injectionKind;
        LamportClock                  m_clock;

        mutable StateContainer m_entityStates;

        HandlerRegistrations m_handlerRegistrations;

        void SetEntityInternal(const UpgradeableStateResult&        upgradeableStateResult,
                               const ConnectionPtr&                 connection,
                               const Dob::Typesystem::HandlerId&    handlerId,
                               const Dob::Typesystem::InstanceId&   instanceId,
                               const bool                           considerChangeFlags,
                               const char* const                    blob);

        void DeleteEntityInternal(const UpgradeableStateResult&        upgradeableStateResult,
                                  const ConnectionPtr&                 connection,
                                  const Dob::Typesystem::HandlerId&    handlerId,
                                  const Dob::Typesystem::InstanceId&   instanceId,
                                  bool&                                dontRelease);

        void DeleteAllInstancesInternal(const UpgradeableStateResult&       upgradeableStateResult,
                                        const ConnectionPtr&                connection,
                                        const Dob::Typesystem::HandlerId&   handlerId,
                                        bool&                               dontRelease,
                                        bool&                               exitDispatch);

        void SetInitalGhostInternal(const UpgradeableStateResult&        upgradeableStateResult,
                                    const ConnectionPtr&                 connection,
                                    const Dob::Typesystem::HandlerId&    handlerId,
                                    const Dob::Typesystem::InstanceId&   instanceId,
                                    const char* const                    blob);

        void InjectEntityInternal(const UpgradeableStateResult&        upgradeableStateResult,
                                  const Dob::Typesystem::HandlerId&    handlerId,
                                  const Dob::Typesystem::InstanceId&   instanceId,
                                  const char* const                    blob,
                                  const Dob::Typesystem::Int64         timestamp,
                                  bool&                                dontRelease);

        void InjectDeletedEntityInternal(const UpgradeableStateResult&        upgradeableStateResult,
                                         const Dob::Typesystem::HandlerId&    handlerId,
                                         const Dob::Typesystem::InstanceId&   instanceId,
                                         const Dob::Typesystem::Int64         timestamp,
                                         bool&                                dontRelease);

        void AcceptInjectionInternal(const UpgradeableStateResult&  upgradeableStateResult,
                                     const ConnectionPtr&           connection,
                                     DistributionData&              injectionState,
                                     const DistributionData&        originalInjectionState);

        void AcceptDeleteInjectionInternal(const UpgradeableStateResult&  upgradeableStateResult,
                                           const ConnectionPtr&           connection,
                                           DistributionData&              injectionState,
                                           const DistributionData&        originalInjectionState,
                                           bool&                          dontRelease);

        void CommonAcceptInjectionInternal(const UpgradeableStateResult&  upgradeableStateResult,
                                           const ConnectionPtr&           connection,
                                           DistributionData&              injectionState);

        void SetInjectionInternal(const UpgradeableStateResult&        upgradeableStateResult,
                                  const ConnectionPtr&                 connection,
                                  const Dob::Typesystem::HandlerId&    handlerId,
                                  const Dob::Typesystem::InstanceId&   instanceId,
                                  const bool                           considerChangeFlags,
                                  const char* const                    blob,
                                  const DistributionData&              originalInjectionState);

        void DeleteInjectionInternal(const UpgradeableStateResult&        upgradeableStateResult,
                                     const ConnectionPtr&                 connection,
                                     const Dob::Typesystem::HandlerId&    handlerId,
                                     const Dob::Typesystem::InstanceId&   instanceId,
                                     const DistributionData&              originalInjectionState,
                                     bool&                                dontRelease);

        void RemoteSetGhostEntityStateInternal(const DistributionData&        remoteEntity,
                                               const UpgradeableStateResult&  upgradeableStateResult);

        void RemoteSetInjectionEntityStateInternal(const DistributionData&        remoteEntity,
                                                   const UpgradeableStateResult&  upgradeableStateResult,
                                                   bool&                          dontRelease);

        void RemoteSetDeleteEntityStateInternal(const DistributionData&         remoteEntity,
                                                const UpgradeableStateResult&   upgradeableStateResult,
                                                bool&                           dontRelease);

        void RemoteSetRealEntityStateInternal(const ConnectionPtr&           connection,
                                              const DistributionData&        remoteEntity,
                                              const UpgradeableStateResult&  upgradeableStateResult,
                                              bool&                          dontRelease,
                                              RemoteSetResult&               remoteSetResult);

        void SetEntityLocal(const UpgradeableStateResult&        upgradeableStateResult,
                            const ConnectionPtr&                 connection,
                            const Dob::Typesystem::HandlerId&    handlerId,
                            const Dob::Typesystem::InstanceId&   instanceId,
                            const bool                           considerChangeFlags,
                            const char* const                    blob,
                            const DistributionData&              injectionState);

        void DeleteEntityLocal(const StateSharedPtr&                statePtr,
                               const Dob::Typesystem::HandlerId&    handlerId,
                               const Dob::Typesystem::InstanceId&   instanceId,
                               const DistributionData&              injectionState,
                               bool&                                dontRelease);

        // A state always has an owner. (Checks any real or injected state for a handler id.)
        void GetOwner(const StateSharedPtr&       statePtr,
                      ConnectionConsumerPair&     owner,
                      Dob::Typesystem::HandlerId& handlerId,
                      bool&                       hasOwner) const;

        enum EntityAccessStatus
        {
            AccessOk,
            HandlerRevoked,
            HandlerNotRegistered,
            InstanceOwnedByOtherHandler,
            InstanceIsGhost
        };

        EntityAccessStatus VerifyEntityAccess(const StateSharedPtr&               statePtr,
                                              const ConnectionPtr&                connection,
                                              const Dob::Typesystem::HandlerId&   handlerId) const;

        bool IsCreated(const StateSharedPtr& statePtr) const;
        bool IsGhost(const StateSharedPtr& statePtr) const;

        void ReadEntityInternal(const UpgradeableStateResult& upgradeableStateResult, DistributionData& realState) const;

        void GetHandlerOfInstanceInternal(const UpgradeableStateResult& upgradeableStateResult,
                                          Dob::Typesystem::HandlerId& handlerId,
                                          bool& gotIt) const;

        void IsCreatedInternal(const UpgradeableStateResult& upgradeableStateResult, bool& isCreated) const;

        void IsOwnerInternal(const UpgradeableStateResult&      upgradeableStateResult,
                             const Dob::Typesystem::HandlerId&  handlerId,
                             const ConnectionConsumerPair&      registerer,
                             bool&                              isOwner) const;

        bool RemoteEntityStateIsAccepted(const DistributionData& remoteState,
                                         const DistributionData& localState) const;

        void SetTimestamps(DistributionData&       newState,
                           const DistributionData& timeBase,
                           const bool              considerChangeFlags) const;

        void KickRegisterer(const Dob::Typesystem::HandlerId& handlerId) const;

        friend void StatisticsCollector(EntityType&, void*);
    };
}
}
}
#endif

