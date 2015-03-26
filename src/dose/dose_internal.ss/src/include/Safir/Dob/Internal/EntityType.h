/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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
#include <Safir/Dob/Internal/LeveledLock.h>
#include <Safir/Dob/Internal/ShmArray.h>

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

        EntityType(const Typesystem::TypeId typeId, const int64_t nodeId);

        Dob::Typesystem::TypeId GetTypeId() const {return m_typeId;}

        /**
         * @name Handler registrations
         */
        /** @{ */

        bool Register(const ConnectionPtr&                  connection,
                      const Dob::Typesystem::HandlerId&     handlerId,
                      const InstanceIdPolicy::Enumeration   instanceIdPolicy,
                      const bool                            isInjectionHandler,
                      LamportClock&                         regClock,
                      const bool                            overrideRegistration,
                      const ConsumerId&                     consumer);

        void Unregister(const ConnectionPtr&                connection,
                        const Dob::Typesystem::HandlerId&   handlerId);

        void UnregisterAll(const ConnectionPtr& connection,
                           const bool           explicitUnregister);

        /** New registration state from external node */
        void RemoteSetRegistrationState(const ConnectionPtr& connection,
                                        const DistributionData& registrationState);

        bool IsRegistered(const Dob::Typesystem::HandlerId& handlerId, const ContextId context) const;

        const ConnectionConsumerPair GetRegisterer(const Dob::Typesystem::HandlerId& handlerId,
                                                   const ContextId context) const;

        bool GetRegisterer(const Dob::Typesystem::HandlerId& handlerId,
                           const ContextId                   context,
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

        /** Set an injection from external node. */
        void RemoteSetInjectionEntityState(const DistributionData& entityState);

        /** Set a delete (an end state) from external node */
        void RemoteSetDeleteEntityState(const DistributionData&   entityState);

        /** Set a state (that is not an injection or delete state) from external node. */
        void RemoteSetRealEntityState(const ConnectionPtr&      connection,
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

        const DistributionData ReadEntity(const Dob::Typesystem::InstanceId& instanceId,
                                          const ContextId readerContext) const;

        void CleanGhosts(const Dob::Typesystem::HandlerId&  handlerId,
                         const ContextId                    context);

        //throws NotFoundException if no such instance
        const Dob::Typesystem::HandlerId GetHandlerOfInstance(const Dob::Typesystem::InstanceId& instanceId,
                                                              const ContextId requestorContext) const;

        bool IsCreated(const Dob::Typesystem::InstanceId& instanceId, const ContextId requestorContext) const;

        bool IsOwner(const Dob::Typesystem::InstanceId& instanceId,
                     const Dob::Typesystem::HandlerId&  handlerId,
                     const ConnectionConsumerPair&      registerer) const;

        const StateContainer::Iterator CreateEntityIterator(const ContextId context, bool& end) const
        {return m_entityStates[context].CreateStateIterator(end);}

        bool IncrementIterator(StateContainer::Iterator& iterator, const ContextId context) const
        {return m_entityStates[context].IncrementIterator(iterator);}

        bool CanAcquireContainerWriterLock(const ContextId contextId,
                                           const boost::chrono::steady_clock::duration& lockTimeout);

    private:
        Typesystem::TypeId            m_typeId;
        bool                          m_typeIsContextShared;
        InjectionKind::Enumeration    m_injectionKind;
        LamportClock                  m_clock;

        typedef ShmArray<StateContainer> StateContainerVector;
        mutable StateContainerVector m_entityStates;

        typedef ShmArray<HandlerRegistrations> HandlerRegistrationVector;
        HandlerRegistrationVector m_handlerRegistrations;

        // Locking Policy:
        // The state uses a non-recursive lock, since there should be no recursive locking.
        // Any attempts to take the lock recursively are to be regarded as programming errors.
        // The TypeLock is needed because there are operations that take both a EntityState lock and
        // a RegistrationState lock, and since these locks are taken in different order by different
        // operations, there is a risk for a deadlock. The reason why we have locks at both type-level and
        // state-level is that we want subscribers to fetch data from single states withot locking
        // the whole type.
        typedef Safir::Dob::Internal::LeveledLock<boost::interprocess::interprocess_mutex,
                                                  TYPE_LOCK_LEVEL, NO_MASTER_LEVEL_REQUIRED> TypeLock;

        typedef ShmArray<TypeLock> TypeLockVector;
        TypeLockVector m_typeLocks;

        typedef boost::interprocess::scoped_lock<TypeLock> ScopedTypeLock;

        void SetEntityInternal(const StateSharedPtr&                statePtr,
                               const ConnectionPtr&                 connection,
                               const Dob::Typesystem::HandlerId&    handlerId,
                               const Dob::Typesystem::InstanceId&   instanceId,
                               const bool                           considerChangeFlags,
                               const char* const                    blob);

        void DeleteEntityInternal(const StateSharedPtr&                statePtr,
                                  const ConnectionPtr&                 connection,
                                  const Dob::Typesystem::HandlerId&    handlerId,
                                  const Dob::Typesystem::InstanceId&   instanceId);

        void DeleteAllInstancesInternal(const StateSharedPtr&               statePtr,
                                        const ConnectionPtr&                connection,
                                        const Dob::Typesystem::HandlerId&   handlerId,
                                        bool&                               exitDispatch);

        void SetInitalGhostInternal(const StateSharedPtr&                statePtr,
                                    const ConnectionPtr&                 connection,
                                    const Dob::Typesystem::HandlerId&    handlerId,
                                    const Dob::Typesystem::InstanceId&   instanceId,
                                    const char* const                    blob);

        void InjectEntityInternal(const StateSharedPtr&                statePtr,
                                  const ConnectionPtr&                 connection,
                                  const Dob::Typesystem::HandlerId&    handlerId,
                                  const Dob::Typesystem::InstanceId&   instanceId,
                                  const char* const                    blob,
                                  const Dob::Typesystem::Int64         timestamp);

        void InjectDeletedEntityInternal(const StateSharedPtr&                statePtr,
                                         const ConnectionPtr&                 connection,
                                         const Dob::Typesystem::HandlerId&    handlerId,
                                         const Dob::Typesystem::InstanceId&   instanceId,
                                         const Dob::Typesystem::Int64         timestamp);

        void AcceptInjectionInternal(const StateSharedPtr&          statePtr,
                                     const ConnectionPtr&           connection,
                                     DistributionData&              injectionState,
                                     const DistributionData&        originalInjectionState);

        void SetInjectionInternal(const StateSharedPtr&                statePtr,
                                  const ConnectionPtr&                 connection,
                                  const Dob::Typesystem::HandlerId&    handlerId,
                                  const Dob::Typesystem::InstanceId&   instanceId,
                                  const bool                           considerChangeFlags,
                                  const char* const                    blob,
                                  const DistributionData&              originalInjectionState);

        void DeleteInjectionInternal(const StateSharedPtr&                statePtr,
                                     const ConnectionPtr&                 connection,
                                     const Dob::Typesystem::HandlerId&    handlerId,
                                     const Dob::Typesystem::InstanceId&   instanceId,
                                     const DistributionData&              originalInjectionState);

        void RemoteSetInjectionEntityStateInternal(const DistributionData&        remoteEntity,
                                                   const StateSharedPtr&          statePtr);

        void RemoteSetDeleteEntityStateInternal(const DistributionData&     remoteEntity,
                                                const StateSharedPtr&       statePtr);

        void RemoteSetRealEntityStateInternal(const ConnectionPtr&           connection,
                                              const DistributionData&        remoteEntity,
                                              const StateSharedPtr&          statePtr);

        void SetEntityLocal(const StateSharedPtr&                statePtr,
                            const ConnectionPtr&                 connection,
                            const Dob::Typesystem::HandlerId&    handlerId,
                            const Dob::Typesystem::InstanceId&   instanceId,
                            const bool                           considerChangeFlags,
                            const char* const                    blob,
                            const DistributionData&              injectionState);

        void DeleteEntityLocal(const StateSharedPtr&                statePtr,
                               const ContextId                      context,
                               const Dob::Typesystem::HandlerId&    handlerId,
                               const Dob::Typesystem::InstanceId&   instanceId,
                               const DistributionData&              injectionState);

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

        void ReadEntityInternal(const StateSharedPtr& statePtr, DistributionData& realState) const;

        void GetHandlerOfInstanceInternal(const StateSharedPtr&       statePtr,
                                          Dob::Typesystem::HandlerId& handlerId,
                                          bool includeGhosts,
                                          bool& gotIt) const;

        void IsCreatedInternal(const StateSharedPtr& statePtr, bool& isCreated) const;

        void IsOwnerInternal(const StateSharedPtr&              statePtr,
                             const Dob::Typesystem::HandlerId&  handlerId,
                             const ConnectionConsumerPair&      registerer,
                             bool&                              isOwner) const;

        bool RemoteEntityStateIsAccepted(const DistributionData& remoteState,
                                         const DistributionData& localState) const;

        void SetTimestamps(DistributionData&       newState,
                           const DistributionData& timeBase,
                           const bool              considerChangeFlags) const;

        void KickRegisterer(const Dob::Typesystem::HandlerId& handlerId, const ContextId context) const;

        void GetMostRecentGhostRegTime(const StateSharedPtr&              statePtr,
                                       const Dob::Typesystem::HandlerId&  handlerId,
                                       RegisterTime&                      mostRecentRegisterTime) const;

        void RemoveGhost(const StateSharedPtr&              statePtr,
                         const Dob::Typesystem::HandlerId&  handlerId,
                         const RegisterTime&                mostRecentRegisterTime) const;

        typedef std::set<Dob::Typesystem::HandlerId> HandlerSet;
        void FindAllHandlers(const StateSharedPtr& statePtr, HandlerSet& handlers) const;

        friend void StatisticsCollector(EntityType&, void*);
    };
}
}
}
#endif
