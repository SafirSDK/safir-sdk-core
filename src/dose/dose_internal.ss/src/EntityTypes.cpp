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

#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/ContextSharedTable.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/NodeParameters.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    EntityTypes* EntityTypes::m_instance = NULL;
    Safir::Utilities::Internal::AtomicUint32 EntityTypes::m_isInitialized(0);

    EntityTypes& EntityTypes::Instance()
    {
        ENSURE(m_instance != NULL, << "EntityTypes::Instance was called before Initialize!!!");
        return *m_instance;
    }

    EntityTypes::EntityTypes(private_constructor_t, const int64_t nodeId)
        : m_registrationClock(nodeId)
        , m_allowInitialSet(true)
    {
        ENSURE(nodeId != 0, << "EntityTypes must be constructed with valid nodeId")
    }

    void EntityTypes::Initialize(const bool iAmDoseMain, const int64_t nodeId)
    {
        m_instance = GetSharedMemory().find_or_construct<EntityTypes>("EntityTypes")(private_constructor_t(), nodeId);

        if (iAmDoseMain)
        {
            ENSURE (m_instance->m_entityTypes.empty(),
                    << "Can't start dose_main. An application or another dose_main "
                    "instance is already started!");

            Dob::Typesystem::TypeIdVector tid = Dob::Typesystem::Operations::GetAllTypeIds();

            for (Dob::Typesystem::TypeIdVector::iterator it = tid.begin();
                 it != tid.end(); ++it)
            {
                if (Dob::Typesystem::Operations::IsClass(*it))
                {
                    if (Dob::Typesystem::Operations::IsOfType(*it, Safir::Dob::Entity::ClassTypeId))
                    {
                        EntityTypePtr entityType =
                            GetSharedMemory().construct<EntityType>
                            (boost::interprocess::anonymous_instance)(*it, nodeId);
                        m_instance->m_entityTypes.insert(std::make_pair(*it, entityType));
                    }
                }
            }
        }

        m_isInitialized = 1;
    }

    bool EntityTypes::IsInitialized()
    {
        return m_isInitialized != 0;
    }

    bool EntityTypes::Register(const ConnectionPtr&                connection,
                               const Dob::Typesystem::TypeId       typeId,
                               const Dob::Typesystem::HandlerId&   handlerId,
                               const InstanceIdPolicy::Enumeration instanceIdPolicy,
                               const bool                          isInjectionHandler,
                               const bool                          overrideRegistration,
                               const ConsumerId&                   consumer)
    {
        return GetType(typeId).Register(connection,
                                        handlerId,
                                        instanceIdPolicy,
                                        isInjectionHandler,
                                        m_registrationClock,
                                        overrideRegistration,
                                        consumer);
    }

    void EntityTypes::Unregister(const ConnectionPtr&                connection,
                                 const Dob::Typesystem::TypeId       typeId,
                                 const Dob::Typesystem::HandlerId&   handlerId)
    {
        GetType(typeId).Unregister(connection,
                                   handlerId);
    }

    void EntityTypes::UnregisterAll(const ConnectionPtr&           connection,
                                    const Dob::Typesystem::TypeId  typeId,
                                    const bool                     explicitUnregister)
    {
        GetType(typeId).UnregisterAll(connection, explicitUnregister);
    }

    void EntityTypes::RemoteSetRegistrationState(const ConnectionPtr& connection,
                                                 const DistributionData& registrationState)
    {
        m_registrationClock.UpdateCurrentTimestamp(registrationState.GetRegistrationTime());

        GetType(registrationState.GetTypeId()).RemoteSetRegistrationState(connection, registrationState);
    }

    bool EntityTypes::IsRegistered(const Dob::Typesystem::TypeId        typeId,
                                   const Dob::Typesystem::HandlerId&    handlerId,
                                   const ContextId                      contextId) const
    {
        return GetType(typeId).IsRegistered(handlerId, contextId);
    }

    void
    EntityTypes::RegisterAcceptedPendingRegistrations(const ConnectionPtr& connection,
                                                      PendingRegistrationVector& prv,
                                                      bool& needKick)
    {
        PendingRegistrationVector pendingRegistrations = connection->GetPendingRegistrations();

        for (PendingRegistrationVector::iterator it = pendingRegistrations.begin();
             it != pendingRegistrations.end(); ++it)
        {
            if (Dob::Typesystem::Operations::IsOfType(it->typeId, Dob::Entity::ClassTypeId) && it->accepted)
            {
                needKick = true;
                const bool registered = Register(connection,
                                                 it->typeId,
                                                 it->handlerId.GetHandlerId(),
                                                 it->instanceIdPolicy,
                                                 it->isInjectionHandler,
                                                 false,
                                                 it->consumer);
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

    const ConnectionConsumerPair
    EntityTypes::GetRegisterer(const Dob::Typesystem::TypeId     typeId,
                               const Dob::Typesystem::HandlerId& handlerId,
                               const ContextId                   contextId) const
    {
        return GetType(typeId).GetRegisterer(handlerId, contextId);
    }

    void
    EntityTypes::GetRegisterer(const Dob::Typesystem::TypeId     typeId,
                               const Dob::Typesystem::HandlerId& handlerId,
                               const ContextId                   contextId,
                               ConnectionConsumerPair&           registerer,
                               InstanceIdPolicy::Enumeration&    instanceIdPolicy) const
    {
        GetType(typeId).GetRegisterer(handlerId, contextId, registerer, instanceIdPolicy);
    }

    bool
    EntityTypes::IsOwner(const Dob::Typesystem::EntityId& entityId,
                         const Dob::Typesystem::HandlerId& handlerId,
                         const ConnectionConsumerPair& registerer) const
    {
        return GetType(entityId.GetTypeId()).IsOwner(entityId.GetInstanceId(), handlerId, registerer);
    }

    const Dob::Typesystem::HandlerId
    EntityTypes::GetHandlerOfInstance(const Dob::Typesystem::EntityId& entityId, const ContextId requestorContext)
    {
        return GetType(entityId.GetTypeId()).GetHandlerOfInstance(entityId.GetInstanceId(), requestorContext);
    }

    void EntityTypes::SubscribeRegistration(const SubscriptionId&                subscriptionId,
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
            GetType(*it).SubscribeRegistration(subscriptionId, handlerId, restartSubscription, subscriptionOptions);
        }
    }

    void EntityTypes::UnsubscribeRegistration(const SubscriptionId&              subscriptionId,
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

    bool EntityTypes::HasRegistrationSubscription(const ConnectionPtr&             connection,
                                                  const ConsumerId&                consumer,
                                                  const Dob::Typesystem::TypeId    typeId)
    {
        return GetType(typeId).HasRegistrationSubscription(connection, consumer);
    }

    void EntityTypes::SetEntity(const ConnectionPtr&                 connection,
                                const Dob::Typesystem::HandlerId&    handlerId,
                                const Dob::Typesystem::EntityId&     entityId,
                                const char* const                    blob,
                                const bool                           considerChangeFlags,
                                const bool                           initialInjection)
    {
        //if it is an initial set and we've been told not to allow any more
        //initial sets, ignore the call.
        if (initialInjection && !m_allowInitialSet)
        {
            return;
        }

        GetType(entityId.GetTypeId()).SetEntity(connection, handlerId, entityId.GetInstanceId(), blob, considerChangeFlags, initialInjection);
    }

    void EntityTypes::DeleteEntity(const ConnectionPtr&              connection,
                                   const Dob::Typesystem::HandlerId& handlerId,
                                   const Dob::Typesystem::EntityId&  entityId,
                                   const bool                        allInstances)
    {
        GetType(entityId.GetTypeId()).DeleteEntity(connection, handlerId, entityId.GetInstanceId(), allInstances);
    }

    void EntityTypes::InjectEntity(const ConnectionPtr&                 connection,
                                   const Dob::Typesystem::HandlerId&    handlerId,
                                   const Dob::Typesystem::EntityId&     entityId,
                                   const char* const                    blob,
                                   const Dob::Typesystem::Int64         timestamp)
    {
        GetType(entityId.GetTypeId()).InjectEntity(connection, handlerId, entityId.GetInstanceId(), blob, timestamp);
    }

    void EntityTypes::InjectDeletedEntity(const ConnectionPtr&                 connection,
                                          const Dob::Typesystem::HandlerId&    handlerId,
                                          const Dob::Typesystem::EntityId&     entityId,
                                          const Dob::Typesystem::Int64         timestamp)
    {
        GetType(entityId.GetTypeId()).InjectDeletedEntity(connection, handlerId, entityId.GetInstanceId(), timestamp);
    }

    void EntityTypes::AcceptInjection(const ConnectionPtr&       connection,
                                      DistributionData&          injectionState,
                                      const DistributionData&    originalInjectionState)
    {
        GetType(injectionState.GetTypeId()).AcceptInjection(connection, injectionState, originalInjectionState);
    }

    void EntityTypes::SetInjection(const ConnectionPtr&                 connection,
                                   const Dob::Typesystem::HandlerId&    handlerId,
                                   const Dob::Typesystem::EntityId&     entityId,
                                   const bool                           considerChangeFlags,
                                   const char* const                    blob,
                                   const DistributionData&              originalInjectionState)
    {
        GetType(entityId.GetTypeId()).SetInjection(connection,
                                                   handlerId,
                                                   entityId.GetInstanceId(),
                                                   considerChangeFlags,
                                                   blob,
                                                   originalInjectionState);
    }

    void EntityTypes::DeleteInjection(const ConnectionPtr&                 connection,
                                      const Dob::Typesystem::HandlerId&    handlerId,
                                      const Dob::Typesystem::EntityId&     entityId,
                                      const DistributionData&              originalInjectionState)
    {
        GetType(entityId.GetTypeId()).DeleteInjection(connection, handlerId, entityId.GetInstanceId(), originalInjectionState);
    }

    bool EntityTypes::IsCreated(const Dob::Typesystem::EntityId&  entityId, const ContextId requestorContext) const
    {
        return GetType(entityId.GetTypeId()).IsCreated(entityId.GetInstanceId(), requestorContext);
    }

    void EntityTypes::RemoteSetInjectionEntityState(const DistributionData& entityState)
    {
        m_registrationClock.UpdateCurrentTimestamp(entityState.GetRegistrationTime());
        GetType(entityState.GetTypeId()).RemoteSetInjectionEntityState(entityState);
    }

    void EntityTypes::RemoteSetDeleteEntityState(const DistributionData&    entityState)
    {
        m_registrationClock.UpdateCurrentTimestamp(entityState.GetRegistrationTime());
        GetType(entityState.GetTypeId()).RemoteSetDeleteEntityState(entityState);
    }

    void EntityTypes::RemoteSetRealEntityState(const ConnectionPtr&      connection,
                                          const DistributionData&   entityState)
    {
        m_registrationClock.UpdateCurrentTimestamp(entityState.GetRegistrationTime());
        GetType(entityState.GetTypeId()).RemoteSetRealEntityState(connection, entityState);
    }

    void EntityTypes::Subscribe(const SubscriptionId&                subscriptionId,
                                const Dob::Typesystem::EntityId&     entityId,
                                const bool                           allInstances,
                                const bool                           includeSubclasses,
                                const bool                           restartSubscription,
                                const SubscriptionOptionsPtr&        subscriptionOptions)
    {
        Dob::Typesystem::TypeIdVector classTree;

        if (includeSubclasses)
        {
            classTree = Dob::Typesystem::Operations::GetClassTree(entityId.GetTypeId());
        }
        else
        {
            classTree.push_back(entityId.GetTypeId());
        }

        for (Dob::Typesystem::TypeIdVector::iterator it = classTree.begin();
             it != classTree.end();
             ++it)
        {
            GetType(*it).Subscribe(subscriptionId, entityId.GetInstanceId(), allInstances, restartSubscription, subscriptionOptions);
        }
    }

    void EntityTypes::Unsubscribe(const SubscriptionId&              subscriptionId,
                                  const Dob::Typesystem::EntityId&   entityId,
                                  const bool                         allInstances,
                                  const bool                         includeSubclasses)
    {
        Dob::Typesystem::TypeIdVector classTree;

        if (includeSubclasses)
        {
            classTree = Dob::Typesystem::Operations::GetClassTree(entityId.GetTypeId());
        }
        else
        {
            classTree.push_back(entityId.GetTypeId());
        }

        for (Dob::Typesystem::TypeIdVector::iterator it = classTree.begin();
             it != classTree.end();
             ++it)
        {
            GetType(*it).Unsubscribe(subscriptionId, entityId.GetInstanceId(), allInstances);
        }
    }

    void EntityTypes::UnsubscribeAll(const ConnectionPtr&           connection,
                                     const Dob::Typesystem::TypeId  typeId)
    {
        GetType(typeId).UnsubscribeAll(connection);
    }

    bool EntityTypes::HasEntitySubscription(const ConnectionPtr&             connection,
                                            const ConsumerId&                consumer,
                                            const Dob::Typesystem::TypeId    typeId)
    {
        return GetType(typeId).HasEntitySubscription(connection, consumer);
    }

    const DistributionData EntityTypes::ReadEntity(const Dob::Typesystem::EntityId& entityId, const ContextId readerContext) const
    {
        return GetType(entityId.GetTypeId()).ReadEntity(entityId.GetInstanceId(), readerContext);
    }

    void EntityTypes::CleanGhosts()
    {
        // Get all entity type ids
        Dob::Typesystem::TypeIdVector classTree = Dob::Typesystem::Operations::GetClassTree(Safir::Dob::Entity::ClassTypeId);

        unsigned int nbrOfContexts = Safir::Dob::NodeParameters::NumberOfContexts();

        for (Dob::Typesystem::TypeIdVector::iterator it = classTree.begin();
             it != classTree.end();
             ++it)
        {
            for (unsigned int context = 0; context < nbrOfContexts; ++context)
            {
                CleanGhosts(*it, Dob::Typesystem::HandlerId::ALL_HANDLERS, context);
            }
        }
    }

    void EntityTypes::CleanGhosts(const Dob::Typesystem::TypeId      typeId,
                                  const Dob::Typesystem::HandlerId&  handlerId,
                                  const ContextId                    context)
    {
        GetType(typeId).CleanGhosts(handlerId, context);
    }

    EntityType& EntityTypes::GetType(const Typesystem::TypeId typeId)
    {
        EntityTypeTable::iterator findIt = m_entityTypes.find(typeId);
        ENSURE(findIt != m_entityTypes.end(), << "GetType failed to find the entitytype that was asked for!!! typeId = " <<
                                                  Dob::Typesystem::Operations::GetName(typeId));

        return *findIt->second;
    }


    void EntityTypes::EntityIterator::Dereference(const char*& entityBlob, const char*& entityState) const
    {
        m_stateContainerIterator.Dereference(entityBlob,entityState);
    }



    const EntityTypes::EntityIterator
    EntityTypes::CreateEntityIterator(const Typesystem::TypeId typeId,
                                      const ContextId connectionContext,
                                      const bool includeSubclasses,
                                      bool& end) const
    {
        EntityIterator iterator;

        iterator.m_connectionContext = connectionContext;

        if (includeSubclasses)
        {
            iterator.m_remainingTypes = Dob::Typesystem::Operations::GetClassTree(typeId);
        }
        else
        {
            iterator.m_remainingTypes.push_back(typeId);
        }

        iterator.m_currentType = m_entityTypes.end();

        end = !IncrementIterator(iterator);
        return iterator;
    }

    void EntityTypes::MoveIteratorToNextType(EntityTypes::EntityIterator& iterator, bool& end) const
    {
        while (iterator.m_currentType == m_entityTypes.end())
        {
            if (iterator.m_remainingTypes.empty())
            {
                end = true;
                return;
            }
            iterator.m_currentType = m_entityTypes.find(iterator.m_remainingTypes.back());
            iterator.m_remainingTypes.pop_back();
            iterator.m_currentContext =
                ContextSharedTable::Instance().IsContextShared(iterator.m_currentType->first) ? 0 : iterator.m_connectionContext;
            bool myEnd;
            iterator.m_stateContainerIterator =
                iterator.m_currentType->second->CreateEntityIterator(iterator.m_currentContext, myEnd);

            //if there were no states, force another round in this loop.
            if (myEnd)
            {
                iterator.m_currentType = m_entityTypes.end();
            }
        }
        end = false;
    }

    bool
    EntityTypes::IncrementIterator(EntityTypes::EntityIterator& iterator) const
    {
        bool end;
        if (iterator.m_currentType == m_entityTypes.end())
        {
            MoveIteratorToNextType(iterator, end);
            if (end)
            {
                return false;
            }
        }
        else
        {
            end = !iterator.m_currentType->second->IncrementIterator(iterator.m_stateContainerIterator, iterator.m_currentContext);
            if (end)
            {
                iterator.m_currentType = m_entityTypes.end();
                MoveIteratorToNextType(iterator, end); //this only moves the iterator if it needs to be moved
                if (end)
                {
                    return false;
                }
            }
        }
        return true;
    }

    bool EntityTypes::CanAcquireContainerWriterLock(const Typesystem::TypeId             typeId,
                                                    const ContextId                      contextId,
                                                    const boost::chrono::steady_clock::duration&    lockTimeout)
    {
        return GetType(typeId).CanAcquireContainerWriterLock(contextId, lockTimeout);
    }
}
}
}
