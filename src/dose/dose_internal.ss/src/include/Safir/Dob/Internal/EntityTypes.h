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

#ifndef _dose_internal_entity_types_h
#define _dose_internal_entity_types_h

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Internal/EntityType.h>
#include <Safir/Dob/Internal/ConsumerId.h>
#include <Safir/Dob/Internal/SubscriptionId.h>
#include <Safir/Dob/Internal/LamportClocks.h>
#include <Safir/Dob/Internal/PendingRegistration.h>
#include <Safir/Dob/Internal/SubscriptionOptions.h>
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/Atomic.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    enum IsOwnerResult
    {
        IsInstanceOwner, //I (my connection/consumer/handler) am the owner and the instance exists
        InstanceDoesNotExist, //The instance does not exist
        IsNotInstanceOwner, //I am not the owner of the instance
        IsNotRegisterer //I am not the registerer of that handler
    };

    class DOSE_INTERNAL_API EntityTypes:
        public SharedMemoryObject,
        private boost::noncopyable
    {
    private:
        //This is to make sure that only Instance can call the constructor even though the constructor
        //itself has to be public (limitation of boost::interprocess)
        struct private_constructor_t {};

        typedef PairContainers<Typesystem::TypeId, EntityTypePtr>::map EntityTypeTable;
    public:
        static void Initialize(const bool iAmDoseMain = false);
        static bool IsInitialized();

        static EntityTypes& Instance();

        /**
         * @name Entity handler registrations
         */
        /** @{ */

        bool Register(const ConnectionPtr&                  connection,
                      const Dob::Typesystem::TypeId         typeId,
                      const Dob::Typesystem::HandlerId&     handlerId,
                      const InstanceIdPolicy::Enumeration   instanceIdPolicy,
                      const bool                            isInjectionHandler,
                      const bool                            overrideRegistration,
                      const ConsumerId&                     consumer);

        void Unregister(const ConnectionPtr&                connection,
                        const Dob::Typesystem::TypeId       typeId,
                        const Dob::Typesystem::HandlerId&   handlerId);

        // Unregisters all existing registrations for the given connection and type.
        void UnregisterAll(const ConnectionPtr&           connection,
                           const Dob::Typesystem::TypeId  typeId,
                           const bool                     explicitUnregister);

        /** New registration state from external node */
        void RemoteSetRegistrationState(const ConnectionPtr& connection,
                                        const DistributionData& registrationState);

        bool IsRegistered(const Dob::Typesystem::TypeId     typeId,
                          const Dob::Typesystem::HandlerId& handlerId,
                          const ContextId                   contextId) const;

        /**
         * Find the connection/consumer that has registered the given handler
         * If the handler is not registered this method returns (NULL,(NULL,0))
         */
        const ConnectionConsumerPair GetRegisterer(const Dob::Typesystem::TypeId     typeId,
                                                   const Dob::Typesystem::HandlerId& handlerId,
                                                   const ContextId                   contextId) const;

        /**
         * Find the connection/consumer that has registered the given handler.
         * This method also returns the instance id policy (valid only for entity handlers)
         */
        void GetRegisterer(const Dob::Typesystem::TypeId     typeId,
                           const Dob::Typesystem::HandlerId& handlerId,
                           const ContextId                   contextId,
                           ConnectionConsumerPair&           registerer,
                           InstanceIdPolicy::Enumeration&    instanceIdPolicy) const;

        //throws NotFoundException if no such instance
        const Dob::Typesystem::HandlerId GetHandlerOfInstance(const Dob::Typesystem::EntityId& entityId, const ContextId requestorContext);

        bool IsOwner(const Dob::Typesystem::EntityId& entityId,
                     const Dob::Typesystem::HandlerId& handlerId,
                     const ConnectionConsumerPair& registerer) const;

        //also removes them from the PendingOwnerships shared vector.
        void RegisterAcceptedPendingRegistrations(const ConnectionPtr& connection,
                                                  PendingRegistrationVector& prv,
                                                  bool& needKick);


        /** @} */

        /**
         * @name Handler registration subscriptions
         */
        /** @{ */

        void SubscribeRegistration(const SubscriptionId&                subscriptionId,
                                   const Dob::Typesystem::TypeId        typeId,
                                   const Dob::Typesystem::HandlerId&    handlerId,
                                   const bool                           includeSubclasses,
                                   const bool                           restartSubscription,
                                   const SubscriptionOptionsPtr&        subscriptionOptions);

        void UnsubscribeRegistration(const SubscriptionId&              subscriptionId,
                                     const Dob::Typesystem::TypeId      typeId,
                                     const Dob::Typesystem::HandlerId&  handlerId,
                                     const bool                         includeSubclasses);

        // Returns true if the given connection/consumer has any registration subscription for the given type.
        bool HasRegistrationSubscription(const ConnectionPtr&           connection,
                                         const ConsumerId&              consumer,
                                         const Dob::Typesystem::TypeId  typeId);

        /** @} */

        /**
         * @name Entity state
         */
        /** @{ */

        void SetEntity(const ConnectionPtr&                 connection,
                       const Dob::Typesystem::HandlerId&    handlerId,
                       const Dob::Typesystem::EntityId&     entityId,
                       const char* const                    blob,
                       const bool                           considerChangeFlags,
                       const bool                           initialInjection);

        void DeleteEntity(const ConnectionPtr&              connection,
                          const Dob::Typesystem::HandlerId& handlerId,
                          const Dob::Typesystem::EntityId&  entityId,
                          const bool                        allInstances);

        bool IsCreated(const Dob::Typesystem::EntityId&  entityId, const ContextId requestorContext) const;
        /** @} */

        /**
         * @name Entity injection
         */
        /** @{ */
        void InjectEntity(const ConnectionPtr&                 connection,
                          const Dob::Typesystem::HandlerId&    handlerId,
                          const Dob::Typesystem::EntityId&     entityId,
                          const char* const                    blob,
                          const Dob::Typesystem::Int64         timestamp);

        void InjectDeletedEntity(const ConnectionPtr&                 connection,
                                 const Dob::Typesystem::HandlerId&    handlerId,
                                 const Dob::Typesystem::EntityId&     entityId,
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
                          const Dob::Typesystem::EntityId&     entityId,
                          const bool                           considerChangeFlags,
                          const char* const                    blob,
                          const DistributionData&              originalInjectionState);

        void DeleteInjection(const ConnectionPtr&                 connection,
                             const Dob::Typesystem::HandlerId&    handlerId,
                             const Dob::Typesystem::EntityId&     entityId,
                             const DistributionData&              originalInjectionState);
        /** @} */


        /**
         * @name Methods to be used to set states received from an external node
         */
        /** @{ */

        /** Set an injection from external node. */
        void RemoteSetInjectionEntityState(const DistributionData& entityState);

        /** Set a delete (an end state) from external node */
        RemoteSetResult RemoteSetDeleteEntityState(const DistributionData&   entityState);

        /** Set a state (that is not an injection or delete state) from external node. */
        RemoteSetResult RemoteSetRealEntityState(const ConnectionPtr&      connection,
                                                 const DistributionData&   entityState);

        /** @} */

        /**
         * @name Entity state subscriptions
         */
        /** @{ */

        void Subscribe(const SubscriptionId&                subscriptionId,
                       const Dob::Typesystem::EntityId&     entityId,
                       const bool                           allInstances,
                       const bool                           includeSubclasses,
                       const bool                           restartSubscription,
                       const SubscriptionOptionsPtr&        subscriptionOptions);

        void Unsubscribe(const SubscriptionId&              subscriptionId,
                         const Dob::Typesystem::EntityId&   entityId,
                         const bool                         allInstances,
                         const bool                         includeSubclasses);

       // Returns true if the given connection/consumer has any entity subscription for the given type.
        bool HasEntitySubscription(const ConnectionPtr&           connection,
                                   const ConsumerId&              consumer,
                                   const Dob::Typesystem::TypeId  typeId);

        /** @} */

        // Removes all existing registration and entity subscriptions (all consumers) for the given connection and type.
        void UnsubscribeAll(const ConnectionPtr&           connection,
                            const Dob::Typesystem::TypeId  typeId);

        /**
         * @name Read entity
         */
        /** @{ */

        const DistributionData ReadEntity(const Dob::Typesystem::EntityId& entityId, const ContextId readerContext) const;

        /** @} */

        /**
         * @name Clean ghosts
         */
        /** @{ */

        // Do a cleanup of ghosts for all types, handler ids and contexts.
        // The cleanup will remove any ghost that has a registration time that is older than the most recent registration time
        // for any of the ghosts.
        void CleanGhosts();

        // Do a cleanup of ghosts for the given typeI id, handler id and context.
        void CleanGhosts(const Dob::Typesystem::TypeId      typeId,
                         const Dob::Typesystem::HandlerId&  handlerId,
                         const ContextId                    context);

        /** @} */

        /**
         * @name Iterators
         */
        /** @{ */

        /** Note that this class CANNOT be stored in shared memory, since the iterator
            member is different size in debug and release builds (on msvc++) */
        class DOSE_INTERNAL_API EntityIterator
        {
        public:
            /** Construct an "end" iterator.*/
            //Iterator();
//TODO: this class must be possible to copy correctly!
            void Dereference(const char*& entityBlob, const char*& entityState) const;
            bool operator== (const EntityIterator& other) const
            {return m_stateContainerIterator==other.m_stateContainerIterator;}

        private:
            friend class EntityTypes;

            ContextId m_connectionContext;  // The context of the connection that initiated the iteration
            TypeIdVector m_remainingTypes;
            StateContainer::Iterator m_stateContainerIterator;
            EntityTypeTable::const_iterator m_currentType;
            ContextId m_currentContext;  // The context we use for the current type
        };

        const EntityIterator CreateEntityIterator(const Typesystem::TypeId typeId,
                                                  const ContextId connectionContext,  // context in which the iterating connection is running
                                                  const bool includeSubclasses,
                                                  bool& end) const;

        //returns false if iterator has reached end.
        bool IncrementIterator(EntityIterator& iterator) const;

        /** @} */

        bool IsInitialSetAllowed() const {return m_allowInitialSet;}
        void DisallowInitialSet() {m_allowInitialSet = false;}

        // This method tries to acquire and release the container lock for the handler registration container
        // and the entity container in all contexts.
        bool CanAcquireContainerWriterLock(const Typesystem::TypeId typeId,
                                           const ContextId contextId,
                                           const boost::chrono::steady_clock::duration& lockTimeout);

        //Debug and statistics
        void DumpRegistrationSubscriptions() const;
        void DumpRegisteredHandlers() const;

        //The constructor and destructor have to be public for the boost::interprocess internals to be able to call
        //them, but we can make the constructor "fake-private" by making it require a private type as argument.
        explicit EntityTypes(private_constructor_t);

    private:
        //Move the iterator to the next type if it is not currently iterating over a type.
        void MoveIteratorToNextType(EntityTypes::EntityIterator& iterator, bool& end) const;

        EntityType& GetType(const Typesystem::TypeId typeId);

        const EntityType& GetType(const Typesystem::TypeId typeId) const
        {return const_cast<EntityTypes&>(*this).GetType(typeId);} //just call the nonconst version

        EntityTypeTable m_entityTypes;

        bool m_iAmDoseMain;
        static EntityTypes* m_instance;
        static AtomicUint32 m_isInitialized;

        LamportClock m_registrationClock;

        bool m_allowInitialSet;

        friend void StatisticsCollector(EntityTypes&, void*);

    };
}
}
}
#endif
