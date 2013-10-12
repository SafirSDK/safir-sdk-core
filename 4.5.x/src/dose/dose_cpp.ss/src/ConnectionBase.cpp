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

#include <Safir/Dob/ConnectionBase.h>
#include "Callbacks.h"
#include "EntityProxyImpl.h"

#include "ResponseSenderImpl.h"
#include <Safir/Dob/Internal/Interface.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/NotFoundException.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>


namespace Safir
{
namespace Dob
{
    //-------------------------------------------------------------
    // Constructors / Destructors
    //-------------------------------------------------------------
    ConnectionBase::ConnectionBase()
    {

    }


    ConnectionBase::~ConnectionBase()
    {

    }

    void ConnectionBase::RegisterEntityHandler(const Safir::Dob::Typesystem::TypeId      typeId,
                                               const Dob::Typesystem::HandlerId&         handlerId,
                                               const Dob::InstanceIdPolicy::Enumeration  instanceIdPolicy,
                                               Dob::EntityHandler* const                 entityHandler) const
    {
        bool success;

        DoseC_RegisterEntityHandler(GetControllerId(),
                                    typeId,
                                    handlerId.GetRawValue(),
                                    handlerId.Utf8String().c_str(),
                                    instanceIdPolicy,
                                    true,  // override registration
                                    false, //not an injectionHandler
                                    DOSE_LANGUAGE_CPP,
                                    static_cast<Internal::ConsumerBase* const>(entityHandler),
                                    success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }


    void ConnectionBase::RegisterEntityHandlerInjection(const Safir::Dob::Typesystem::TypeId      typeId,
                                                        const Dob::Typesystem::HandlerId&         handlerId,
                                                        const Dob::InstanceIdPolicy::Enumeration  instanceIdPolicy,
                                                        Dob::EntityHandlerInjection* const        entityHandlerInjection) const
    {
        bool success;

        DoseC_RegisterEntityHandler(GetControllerId(),
                                    typeId,
                                    handlerId.GetRawValue(),
                                    handlerId.Utf8String().c_str(),
                                    instanceIdPolicy,
                                    true,  // override registration
                                    true, //an injectionHandler
                                    DOSE_LANGUAGE_CPP,
                                    static_cast<Internal::ConsumerBase* const>(entityHandlerInjection),
                                    success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }


    void ConnectionBase::RegisterEntityHandlerPending(const Safir::Dob::Typesystem::TypeId      typeId,
                                                      const Dob::Typesystem::HandlerId&         handlerId,
                                                      const Dob::InstanceIdPolicy::Enumeration  instanceIdPolicy,
                                                      Dob::EntityHandlerPending* const          entityHandlerPending) const
    {
        bool success;

        DoseC_RegisterEntityHandler(GetControllerId(),
                                    typeId,
                                    handlerId.GetRawValue(),
                                    handlerId.Utf8String().c_str(),
                                    instanceIdPolicy,
                                    false,  // pending registration
                                    true, //an injectionHandler
                                    DOSE_LANGUAGE_CPP,
                                    static_cast<Internal::ConsumerBase* const>(entityHandlerPending),
                                    success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }


    void ConnectionBase::RegisterServiceHandler(const Safir::Dob::Typesystem::TypeId      typeId,
                                                const Dob::Typesystem::HandlerId&         handlerId,
                                                Dob::ServiceHandler* const                serviceHandler) const
    {
        bool success;

        DoseC_RegisterServiceHandler(GetControllerId(),
                                     typeId,
                                     handlerId.GetRawValue(),
                                     handlerId.Utf8String().c_str(),
                                     true,  // override registration
                                     DOSE_LANGUAGE_CPP,
                                     static_cast<Internal::ConsumerBase* const>(serviceHandler),
                                     success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void ConnectionBase::RegisterServiceHandlerPending(const Safir::Dob::Typesystem::TypeId      typeId,
                                                       const Dob::Typesystem::HandlerId&         handlerId,
                                                       Dob::ServiceHandlerPending* const         serviceHandlerPending) const
    {
        bool success;

        DoseC_RegisterServiceHandler(GetControllerId(),
                                     typeId,
                                     handlerId.GetRawValue(),
                                     handlerId.Utf8String().c_str(),
                                     false,  // pending registration
                                     DOSE_LANGUAGE_CPP,
                                     static_cast<Internal::ConsumerBase* const>(serviceHandlerPending),
                                     success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }


    void ConnectionBase::UnregisterHandler(const Dob::Typesystem::TypeId        typeId,
                                           const Dob::Typesystem::HandlerId&    handlerId) const
    {
        bool success;
        DoseC_UnregisterHandler(GetControllerId(),
                                typeId,
                                handlerId.GetRawValue(),
                                handlerId.Utf8String().c_str(),
                                success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }


    //-------------------------------
    // Subscription methods
    //-------------------------------

    //Message
    void ConnectionBase::SubscribeMessage(const Dob::Typesystem::TypeId     typeId,
                                          const Dob::Typesystem::ChannelId& channelId,
                                          Dob::MessageSubscriber* const     messageSubscriber) const
    {
        SubscribeMessage(typeId,channelId,true,messageSubscriber);
    }

    void ConnectionBase::SubscribeMessage(const Dob::Typesystem::TypeId     typeId,
                                          const Dob::Typesystem::ChannelId& channelId,
                                          const bool                        includeSubclasses,
                                          Dob::MessageSubscriber* const     messageSubscriber) const
    {
        bool success;
        DoseC_SubscribeMessage(GetControllerId(),
                               typeId,
                               channelId.GetRawValue(),
                               channelId.Utf8String().c_str(),
                               includeSubclasses,
                               DOSE_LANGUAGE_CPP,
                               static_cast<Internal::ConsumerBase* const>(messageSubscriber),
                               success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void ConnectionBase::UnsubscribeMessage(const Dob::Typesystem::TypeId     typeId,
                                            const Dob::Typesystem::ChannelId& channelId,
                                            Dob::MessageSubscriber* const     messageSubscriber) const
    {
        UnsubscribeMessage(typeId,channelId,true,messageSubscriber);
    }

    void ConnectionBase::UnsubscribeMessage(const Dob::Typesystem::TypeId     typeId,
                                            const Dob::Typesystem::ChannelId& channelId,
                                            const bool                        includeSubclasses,
                                            Dob::MessageSubscriber* const     messageSubscriber) const
    {
        bool success;
        DoseC_UnsubscribeMessage(GetControllerId(),
                                 typeId,
                                 channelId.GetRawValue(),
                                 channelId.Utf8String().c_str(),
                                 includeSubclasses,
                                 DOSE_LANGUAGE_CPP,
                                 static_cast<Internal::ConsumerBase* const>(messageSubscriber),
                                 success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }


    //Entity

    void ConnectionBase::SubscribeEntity(const Dob::Typesystem::TypeId      typeId,
                                         Dob::EntitySubscriber* const       entitySubscriber) const
    {
        SubscribeEntity(typeId,
                        true, //includeUpdates
                        true, //includeSubclasses
                        true, //restartSubscription
                        entitySubscriber);
    }


    void ConnectionBase::SubscribeEntity(const Dob::Typesystem::TypeId      typeId,
                                         const bool                         includeUpdates,
                                         const bool                         includeSubclasses,
                                         const bool                         restartSubscription,
                                         Dob::EntitySubscriber* const       entitySubscriber) const
    {
        bool success;
        DoseC_SubscribeEntity(GetControllerId(),
                              typeId,
                              0,
                              "",
                              true, //allInstances,
                              includeUpdates,
                              includeSubclasses,
                              restartSubscription,
                              DOSE_LANGUAGE_CPP,
                              static_cast<Internal::ConsumerBase* const>(entitySubscriber),
                              success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }


    void ConnectionBase::SubscribeEntity(const Dob::Typesystem::EntityId&    entityId,
                                         const bool                          includeUpdates,
                                         const bool                          restartSubscription,
                                         Dob::EntitySubscriber* const        entitySubscriber) const
    {
        bool success;
        DoseC_SubscribeEntity(GetControllerId(),
                              entityId.GetTypeId(),
                              entityId.GetInstanceId().GetRawValue(),
                              entityId.GetInstanceId().Utf8String().c_str(),
                              false, //allInstances,
                              includeUpdates,
                              false, //includeSubclasses
                              restartSubscription,
                              DOSE_LANGUAGE_CPP,
                              static_cast<Internal::ConsumerBase* const>(entitySubscriber),
                              success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void ConnectionBase::UnsubscribeEntity(const Dob::Typesystem::TypeId        typeId,
                                           Dob::EntitySubscriber* const         entitySubscriber) const
    {
        UnsubscribeEntity(typeId,
                          true, //includeSubclasses,
                          entitySubscriber);
    }


    void ConnectionBase::UnsubscribeEntity(const Dob::Typesystem::TypeId        typeId,
                                           const bool                           includeSubclasses,
                                           Dob::EntitySubscriber* const         entitySubscriber) const
    {
        bool success;
        DoseC_UnsubscribeEntity(GetControllerId(),
                                typeId,
                                0,
                                "",
                                true, //allInstances,
                                includeSubclasses,
                                DOSE_LANGUAGE_CPP,
                                static_cast<Internal::ConsumerBase* const>(entitySubscriber),
                                success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }


    void ConnectionBase::UnsubscribeEntity(const Dob::Typesystem::EntityId& entityId,
                                           Dob::EntitySubscriber* const     entitySubscriber) const
    {
        bool success;
        DoseC_UnsubscribeEntity(GetControllerId(),
                                entityId.GetTypeId(),
                                entityId.GetInstanceId().GetRawValue(),
                                entityId.GetInstanceId().Utf8String().c_str(),
                                false, //allInstances,
                                false, //includeSubclasses
                                DOSE_LANGUAGE_CPP,
                                static_cast<Internal::ConsumerBase* const>(entitySubscriber),
                                success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void ConnectionBase::SubscribeRegistration(const Dob::Typesystem::TypeId            typeId,
                                               const Dob::Typesystem::HandlerId&        handlerId,
                                               const bool                               includeSubclasses,
                                               const bool                               restartSubscription,
                                               Dob::RegistrationSubscriber* const       registrationSubscriber) const
    {
        bool success;

        DoseC_SubscribeRegistration(GetControllerId(),
                                    typeId,
                                    handlerId.GetRawValue(),
                                    handlerId.Utf8String().c_str(),
                                    includeSubclasses,
                                    restartSubscription,
                                    DOSE_LANGUAGE_CPP,
                                    static_cast<Internal::ConsumerBase* const>(registrationSubscriber),
                                    success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void ConnectionBase::UnsubscribeRegistration(const Dob::Typesystem::TypeId      typeId,
                                                 const Dob::Typesystem::HandlerId&  handlerId,
                                                 const bool                         includeSubclasses,
                                                 Dob::RegistrationSubscriber* const registrationSubscriber) const
    {
        bool success;

        DoseC_UnsubscribeRegistration(GetControllerId(),
                                      typeId,
                                      handlerId.GetRawValue(),
                                      handlerId.Utf8String().c_str(),
                                      includeSubclasses,
                                      DOSE_LANGUAGE_CPP,
                                      static_cast<Internal::ConsumerBase* const>(registrationSubscriber),
                                      success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void ConnectionBase::ExitDispatch() const
    {
        bool success;
        DoseC_ExitDispatch(GetControllerId(), success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    //----------------------
    // Messages
    //----------------------
    void ConnectionBase::Send(const Dob::MessagePtr & message,
                              const Dob::Typesystem::ChannelId& channelId,
                              MessageSender * const messageSender) const
    {
        bool success;

        Typesystem::BinarySerialization bin;
        Typesystem::Serialization::ToBinary(message,bin);
        //TODO: serialize straight to shared memory
        DoseC_SendMessage(GetControllerId(),
                          &bin[0],
                          channelId.GetRawValue(),
                          channelId.Utf8String().c_str(),
                          DOSE_LANGUAGE_CPP,
                          static_cast<Internal::ConsumerBase* const>(messageSender),
                          success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }


    RequestId
    ConnectionBase::ServiceRequest(const ServicePtr& request,
                                   const Dob::Typesystem::HandlerId& handlerId,
                                   Requestor* const requestor) const
    {
        bool success;

        Typesystem::BinarySerialization bin;
        Typesystem::Serialization::ToBinary(request,bin);
        //TODO: serialize straight to shared memory

        RequestId reqId;
        DoseC_ServiceRequest(GetControllerId(),
                             &bin[0],
                             handlerId.GetRawValue(),
                             handlerId.Utf8String().c_str(),
                             DOSE_LANGUAGE_CPP,
                             static_cast<Internal::ConsumerBase* const>(requestor),
                             reqId,
                             success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return reqId;
    }

    //Entities
    Dob::RequestId ConnectionBase::CreateRequest(const Dob::EntityPtr&               request,
                                                 const Dob::Typesystem::HandlerId&   handlerId,
                                                 Dob::Requestor* const               requestor) const
    {
        bool success;

        Typesystem::BinarySerialization bin;
        Typesystem::Serialization::ToBinary(request,bin);
        //TODO: serialize straight to shared memory

        Dob::RequestId reqId;
        DoseC_CreateRequest(GetControllerId(),
                            &bin[0],
                            false,
                            0,
                            "",
                            handlerId.GetRawValue(),
                            handlerId.Utf8String().c_str(),
                            DOSE_LANGUAGE_CPP,
                            static_cast<Internal::ConsumerBase* const>(requestor),
                            reqId,
                            success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return reqId;
    }

    Dob::RequestId ConnectionBase::CreateRequest(const Dob::EntityPtr&              request,
                                                 const Dob::Typesystem::InstanceId& instanceId,
                                                 const Dob::Typesystem::HandlerId&  handlerId,
                                                 Dob::Requestor* const              requestor) const
    {
        bool success;

        Typesystem::BinarySerialization bin;
        Typesystem::Serialization::ToBinary(request,bin);
        //TODO: serialize straight to shared memory

        Dob::RequestId reqId;
        DoseC_CreateRequest(GetControllerId(),
                            &bin[0],
                            true,
                            instanceId.GetRawValue(),
                            instanceId.Utf8String().c_str(),
                            handlerId.GetRawValue(),
                            handlerId.Utf8String().c_str(),
                            DOSE_LANGUAGE_CPP,
                            static_cast<Internal::ConsumerBase* const>(requestor),
                            reqId,
                            success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return reqId;
    }

    Dob::RequestId ConnectionBase::UpdateRequest(const Dob::EntityPtr&              request,
                                                 const Dob::Typesystem::InstanceId& instanceId,
                                                 Dob::Requestor* const              requestor) const
    {
        bool success;

        Typesystem::BinarySerialization bin;
        Typesystem::Serialization::ToBinary(request,bin);
        //TODO: serialize straight to shared memory

        Dob::RequestId reqId;
        DoseC_UpdateRequest(GetControllerId(),
                            &bin[0],
                            instanceId.GetRawValue(),
                            instanceId.Utf8String().c_str(),
                            DOSE_LANGUAGE_CPP,
                            static_cast<Internal::ConsumerBase* const>(requestor),
                            reqId,
                            success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return reqId;
    }

    Dob::RequestId ConnectionBase::DeleteRequest(const Dob::Typesystem::EntityId& entityId,
                                                 Dob::Requestor* const            requestor) const
    {
        bool success;

        Dob::RequestId reqId;
        DoseC_DeleteRequest(GetControllerId(),
                            entityId.GetTypeId(),
                            entityId.GetInstanceId().GetRawValue(),
                            entityId.GetInstanceId().Utf8String().c_str(),
                            DOSE_LANGUAGE_CPP,
                            static_cast<Internal::ConsumerBase* const>(requestor),
                            reqId,
                            success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return reqId;
    }

    //----------------------
    // Entities
    //----------------------

    void ConnectionBase::SetChanges(const Dob::EntityPtr&              entity,
                                    const Dob::Typesystem::InstanceId& instanceId,
                                    const Dob::Typesystem::HandlerId&  handlerId) const
    {
        const Dob::Typesystem::EntityId eid(entity->GetTypeId(),instanceId);
        if (IsCreated(eid))
        {
            const Dob::EntityProxy entityProxy = Read(Dob::Typesystem::EntityId(entity->GetTypeId(),instanceId));

            EntityPtr merged = entityProxy.GetEntity();
            Typesystem::Utilities::MergeChanges(merged,entity);

            Set(merged,
                instanceId,
                handlerId,
                true);     // true => consider change flags
        }
        else
        {
            Set(entity,
                instanceId,
                handlerId,
                false);     // false => don't consider change flags
        }
    }

    void ConnectionBase::SetAll(const Dob::EntityPtr&              entity,
                                const Dob::Typesystem::InstanceId& instanceId,
                                const Dob::Typesystem::HandlerId&  handlerId) const
    {
        Set(entity,
            instanceId,
            handlerId,
            false);     // false => don't consider change flags
    }

    // private method
    void ConnectionBase::Set(const Dob::EntityPtr&              entity,
                             const Dob::Typesystem::InstanceId& instanceId,
                             const Dob::Typesystem::HandlerId&  handlerId,
                             const bool                         considerChangeFlags) const
    {
        bool success;

        Typesystem::BinarySerialization bin;
        Typesystem::Serialization::ToBinary(entity, bin);
        //TODO: serialize straight to shared memory
        DoseC_SetEntity(GetControllerId(),
                        &bin[0],
                        instanceId.GetRawValue(),
                        instanceId.Utf8String().c_str(),
                        handlerId.GetRawValue(),
                        handlerId.Utf8String().c_str(),
                        considerChangeFlags,
                        false,   // false => this is not an initial injection
                        success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void ConnectionBase::Delete(const Dob::Typesystem::EntityId&    entityId,
                                const Dob::Typesystem::HandlerId&   handlerId ) const
    {
        bool success;
        DoseC_DeleteEntity(GetControllerId(),
                           entityId.GetTypeId(),
                           entityId.GetInstanceId().GetRawValue(),
                           entityId.GetInstanceId().Utf8String().c_str(),
                           false,                        // false => not all instances
                           handlerId.GetRawValue(),
                           handlerId.Utf8String().c_str(),
                           success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void ConnectionBase::DeleteAllInstances(const Dob::Typesystem::TypeId       typeId,
                                            const Dob::Typesystem::HandlerId&   handlerId) const
    {
        bool success;
        DoseC_DeleteEntity(GetControllerId(),
                           typeId,
                           0,
                           "",
                           true,                        // true => all instances
                           handlerId.GetRawValue(),
                           handlerId.Utf8String().c_str(),
                           success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    Dob::EntityIterator
    ConnectionBase::GetEntityIterator(const Dob::Typesystem::TypeId     typeId,
                                      const bool                        includeSubclasses) const
    {
        return EntityIterator(GetControllerId(), typeId, includeSubclasses);
    }

    const Dob::EntityProxy ConnectionBase::Read(const Dob::Typesystem::EntityId& entityId) const
    {
        bool success;
        const char* currentBlob;
        const char* currentState;
        DoseC_ReadEntity(GetControllerId(),
                         entityId.GetTypeId(),
                         entityId.GetInstanceId().GetRawValue(),
                         entityId.GetInstanceId().Utf8String().c_str(),
                         currentBlob,
                         currentState,
                         success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }

        return Dob::EntityProxy(new Safir::Dob::Internal::EntityProxyImpl(currentBlob,
                                                                          currentState,
                                                                          NULL,
                                                                          NULL,
                                                                          false, //addReference
                                                                          false)); //timestampDiff
    }

    bool ConnectionBase::IsCreated(const Dob::Typesystem::EntityId& entityId) const
    {
        bool success;
        bool isCreated;
        DoseC_IsCreated(GetControllerId(),
                        entityId.GetTypeId(),
                        entityId.GetInstanceId().GetRawValue(),
                        entityId.GetInstanceId().Utf8String().c_str(),
                        isCreated,
                        success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return isCreated;
    }


    Dob::Typesystem::Int64
    ConnectionBase::GetNumberOfInstances(const Dob::Typesystem::TypeId typeId,
                                         const Dob::Typesystem::HandlerId& handlerId,
                                         const bool includeSubclasses) const
    {
        bool success;
        Typesystem::Int64 numInstances;
        DoseC_GetNumberOfInstances(GetControllerId(),
                                   typeId,
                                   handlerId.GetRawValue(),
                                   handlerId.Utf8String().c_str(),
                                   includeSubclasses,
                                   numInstances,
                                   success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return numInstances;
    }



   Dob::InstanceIdPolicy::Enumeration ConnectionBase::GetInstanceIdPolicy(const Dob::Typesystem::TypeId typeId,
                                                          const Dob::Typesystem::HandlerId& handlerId) const
   {
        bool success;
        Dob::Typesystem::EnumerationValue instanceIdPolicy;
        DoseC_GetInstanceIdPolicy(GetControllerId(),
                                  typeId,
                                  handlerId.GetRawValue(),
                                  instanceIdPolicy,
                                  success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return static_cast<Dob::InstanceIdPolicy::Enumeration>(instanceIdPolicy);

   }

}
}
