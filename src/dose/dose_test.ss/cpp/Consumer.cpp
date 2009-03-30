/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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

#include <DoseTest/RootService.h>
#include <DoseTest/RootEntity.h>
#include <DoseTest/RootMessage.h>
#include <Safir/Dob/ConnectionAspectPostpone.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/EntityIdResponse.h>
#include <Safir/Dob/NotFoundException.h>
#include <Safir/Dob/AccessDeniedException.h>

#include <Safir/Dob/ConnectionAspectInjector.h>
#include <DoseTest/SuccessfulCreate.h>
#include <DoseTest/SuccessfulDelete.h>
#include <DoseTest/SuccessfulUpdate.h>
#include <DoseTest/SuccessfulService.h>
#include <DoseTest/LastInjectionTimestamp.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include "Consumer.h"
#include <iostream>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/Typesystem/Members.h>
#include <boost/bind.hpp>
#include "Logger.h"
#include <cctype>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>

const std::wstring PREFIX = L"Consumer ";

Consumer::Consumer(int consumerNumber,
                   const std::wstring & connectionName,
                   const std::wstring & instance):
    m_consumerNumber(consumerNumber),
    m_callbackActions(Safir::Dob::CallbackId::Size())
{
    m_connection.Attach(connectionName,instance);
}


void Consumer::AddCallbackAction(DoseTest::ActionPtr action)
{
    m_callbackActions[action->ActionCallback().GetVal()].push_back(action);
}

void Consumer::ExecuteCallbackActions(const Safir::Dob::CallbackId::Enumeration callback)
{
    for (Actions::iterator it = m_callbackActions[callback].begin();
         it != m_callbackActions[callback].end(); ++it)
    {
        ExecuteAction(*it);
    }
}

void Consumer::OnMessage(const Safir::Dob::MessageProxy messageProxy)
{
    m_connection.ExitDispatch();
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnMessage);

    DoseTest::RootMessagePtr msg = boost::dynamic_pointer_cast<DoseTest::RootMessage>(messageProxy.GetMessage());
    lout << PREFIX << m_consumerNumber << ": "
         << "OnMessage:" << std::endl
         << "  Type       = " << Safir::Dob::Typesystem::Operations::GetName(messageProxy.GetTypeId()) << std::endl
         << "  ChannelId  = " << messageProxy.GetChannelId() << std::endl
         << "  Sender     = " << messageProxy.GetSenderConnectionInfo() << std::endl
         << "  ChannelId  = " << messageProxy.GetChannelIdWithStringRepresentation() << std::endl
         << "  Message    = " << Safir::Dob::Typesystem::Serialization::ToXml(messageProxy.GetBlob()) << std::endl
         << std::endl;
    lout << std::endl;
}

void Consumer::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
{
    m_connection.ExitDispatch();
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnNewEntity);

    lout << PREFIX << m_consumerNumber << ": "
         << "OnNewEntity:" << std::endl
         << "  EntityId  = " << entityProxy.GetEntityId() << std::endl
         << "  Owner     = " << entityProxy.GetOwner() << std::endl
         << "  OwnerConn = " << entityProxy.GetOwnerConnectionInfo() << std::endl
         << "  OwnerStr  = " << entityProxy.GetOwnerWithStringRepresentation() << std::endl
         << "  Entity    = " << Safir::Dob::Typesystem::Serialization::ToXml(entityProxy.GetBlob()) << std::endl
         << "  Changed top-level members: " << std::endl;

    Safir::Dob::EntityPtr entity = entityProxy.GetEntityWithChangeInfo();

    for (int i = 0;
         i < Safir::Dob::Typesystem::Members::GetNumberOfMembers(entityProxy.GetTypeId());
         ++i)
    {
        if (entity->GetMember(i, 0).IsChanged())
        {
            lout << "    "
                 << Safir::Dob::Typesystem::Members::GetName(entityProxy.GetTypeId(), i)
                 << std::endl;
        }
    }
    lout << std::endl;
}

void
Consumer::OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
{
    m_connection.ExitDispatch();
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnUpdatedEntity);

    lout << PREFIX << m_consumerNumber << ": "
         << "OnUpdatedEntity:" << std::endl
         << "  EntityId  = " << entityProxy.GetEntityId() << std::endl
         << "  Owner     = " << entityProxy.GetOwner() << std::endl
         << "  OwnerConn = " << entityProxy.GetOwnerConnectionInfo() << std::endl
         << "  OwnerStr  = " << entityProxy.GetOwnerWithStringRepresentation() << std::endl
         << "  Entity    = " << Safir::Dob::Typesystem::Serialization::ToXml(entityProxy.GetBlob()) << std::endl
         << "  Previous  = " << Safir::Dob::Typesystem::Serialization::ToXml(entityProxy.GetPrevious().GetBlob()) << std::endl
         << "  Changed top-level members: " << std::endl;

    Safir::Dob::EntityPtr entity = entityProxy.GetEntityWithChangeInfo();

    for (int i = 0;
         i < Safir::Dob::Typesystem::Members::GetNumberOfMembers(entityProxy.GetTypeId());
         ++i)
    {
        if (entity->GetMember(i, 0).IsChanged())
        {
            lout << "    "
                 << Safir::Dob::Typesystem::Members::GetName(entityProxy.GetTypeId(), i)
                 << std::endl;
        }
    }
    lout << std::endl;
}

void
Consumer::OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool deletedByOwner)
{
    m_connection.ExitDispatch();
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnDeletedEntity);

    lout << PREFIX << m_consumerNumber << ": "
         << "OnDeletedEntity:" << std::endl
         << "  EntityId       = " << entityProxy.GetEntityId() << std::endl
         << "  deletedByOwner = " << std::boolalpha << deletedByOwner << std::endl
         << "  Owner          = " << entityProxy.GetOwner() << std::endl
         << "  OwnerConn = " << entityProxy.GetOwnerConnectionInfo() << std::endl
         << "  OwnerStr  = " << entityProxy.GetOwnerWithStringRepresentation() << std::endl
         << "  Previous  = " << Safir::Dob::Typesystem::Serialization::ToXml(entityProxy.GetPrevious().GetBlob()) << std::endl;
    lout << std::endl;
}

void Consumer::OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy entityProxy)
{
    m_connection.ExitDispatch();
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnInjectedNewEntity);

    lout << PREFIX << m_consumerNumber << ": "
         << "OnInjectedNewEntity:" << std::endl
         << "  EntityId  = " << entityProxy.GetEntityId() << std::endl
         << "  Injection = " << Safir::Dob::Typesystem::Serialization::ToXml(entityProxy.GetInjectionBlob()) << std::endl
         << "  Changed top-level members: " << std::endl;

    Safir::Dob::EntityPtr entity = entityProxy.GetInjection();

    for (int i = 0;
         i < Safir::Dob::Typesystem::Members::GetNumberOfMembers(entityProxy.GetTypeId());
         ++i)
    {
        if (entity->GetMember(i, 0).IsChanged())
        {
            lout << "    "
                 << Safir::Dob::Typesystem::Members::GetName(entityProxy.GetTypeId(), i)
                 << std::endl;
        }
    }
    lout << std::endl;
}

void
Consumer::OnInjectedUpdatedEntity(const Safir::Dob::InjectedEntityProxy entityProxy)
{
    m_connection.ExitDispatch();
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnInjectedUpdatedEntity);

    lout << PREFIX << m_consumerNumber << ": "
         << "OnInjectedUpdatedEntity:" << std::endl
         << "  EntityId  = " << entityProxy.GetEntityId() << std::endl
         << "  Injection = " << Safir::Dob::Typesystem::Serialization::ToXml(entityProxy.GetInjectionBlob()) << std::endl
         << "  Current   = " << Safir::Dob::Typesystem::Serialization::ToXml(entityProxy.GetCurrent()) << std::endl
         << "  Changed top-level members: " << std::endl;

    Safir::Dob::EntityPtr entity = entityProxy.GetInjection();

    for (int i = 0;
         i < Safir::Dob::Typesystem::Members::GetNumberOfMembers(entityProxy.GetTypeId());
         ++i)
    {
        if (entity->GetMember(i, 0).IsChanged())
        {
            lout << "    "
                 << Safir::Dob::Typesystem::Members::GetName(entityProxy.GetTypeId(), i)
                 << std::endl;
        }
    }
    lout << std::endl;
}

void
Consumer::OnInjectedDeletedEntity(const Safir::Dob::InjectedEntityProxy entityProxy)
{
    m_connection.ExitDispatch();
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnInjectedDeletedEntity);

    lout << PREFIX << m_consumerNumber << ": "
         << "OnInjectedDeletedEntity:" << std::endl
         << "  EntityId       = " << entityProxy.GetEntityId() << std::endl
         << "  Current  = " << Safir::Dob::Typesystem::Serialization::ToXml(entityProxy.GetCurrent()) << std::endl;
    lout << std::endl;
}

void
Consumer::OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId        typeId,
                                  const Safir::Dob::Typesystem::HandlerId&     handlerId)
{
    m_connection.ExitDispatch();
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnInitialInjectionsDone);

    lout << PREFIX << m_consumerNumber << ": "
         << "OnInitialInjectionsDone:" << std::endl
         << "  Type      = " << Safir::Dob::Typesystem::Operations::GetName(typeId) << std::endl
         << "  HandlerId = " << handlerId << std::endl;
    lout << std::endl;
}

void
Consumer::OnRegistered(const Safir::Dob::Typesystem::TypeId      typeId,
                       const Safir::Dob::Typesystem::HandlerId&  handlerId)
{
    m_connection.ExitDispatch();
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnRegistered);

    lout << PREFIX << m_consumerNumber << ": "
         << "OnRegistered:" << std::endl
         << "  Type      = " << Safir::Dob::Typesystem::Operations::GetName(typeId) << std::endl
         << "  HandlerId = " << handlerId << std::endl;
    lout << std::endl;
}

void
Consumer::OnUnregistered(const Safir::Dob::Typesystem::TypeId      typeId,
                         const Safir::Dob::Typesystem::HandlerId&  handlerId)
{
    m_connection.ExitDispatch();
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnUnregistered);

    lout << PREFIX << m_consumerNumber << ": "
         << "OnUnregistered:" << std::endl
         << "  Type      = " << Safir::Dob::Typesystem::Operations::GetName(typeId) << std::endl
         << "  HandlerId = " << handlerId << std::endl;
    lout << std::endl;
}

void Consumer::OnNotMessageOverflow()
{
    m_connection.ExitDispatch();
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnNotMessageOverflow);

    lout << PREFIX << m_consumerNumber << ": "
         << "OnNotMessageOverflow" << std::endl;
    lout << std::endl;
}

void Consumer::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                     const Safir::Dob::Typesystem::HandlerId& handlerId)
{
    m_connection.ExitDispatch();
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnRevokedRegistration);

    lout << PREFIX << m_consumerNumber << ": "
         << "OnRevokedRegistration:" << std::endl
         << "  Type      = " << Safir::Dob::Typesystem::Operations::GetName(typeId) << std::endl
         << "  HandlerId = " << handlerId << std::endl;
    lout << std::endl;
}

void Consumer::OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                       const Safir::Dob::Typesystem::HandlerId& handlerId)
{
    m_connection.ExitDispatch();
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnCompletedRegistration);

     lout << PREFIX << m_consumerNumber << ": "
         << "OnCompletedRegistration:" << std::endl
         << "  Type      = " << Safir::Dob::Typesystem::Operations::GetName(typeId) << std::endl
         << "  HandlerId = " << handlerId << std::endl;
    lout << std::endl;
}

void Consumer::OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                                Safir::Dob::ResponseSenderPtr responseSender)
{
    m_connection.ExitDispatch();
    m_responseSender = responseSender;
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnServiceRequest);

    lout << PREFIX << m_consumerNumber << ": "
         << "OnServiceRequest: " << std::endl
         << "  Type       = " << Safir::Dob::Typesystem::Operations::GetName(serviceRequestProxy.GetTypeId()) << std::endl
         << "  Sender     = " << serviceRequestProxy.GetSenderConnectionInfo() << std::endl
         << "  Handler    = " << serviceRequestProxy.GetReceivingHandlerId() << std::endl
         << "  HandlerStr = " << serviceRequestProxy.GetReceiverWithStringRepresentation() << std::endl
         << "  Request    = " << Safir::Dob::Typesystem::Serialization::ToXml(serviceRequestProxy.GetBlob()) << std::endl;
    lout << std::endl;

    if (!responseSender->IsDone())
    {
        DoseTest::SuccessfulServicePtr resp = DoseTest::SuccessfulService::Create();
        resp->Info().SetVal(L"AutoResponse");
        responseSender->Send(resp);
    }
}


void
Consumer::OnCreateRequest(const Safir::Dob::EntityRequestProxy entityProxy,
                          Safir::Dob::ResponseSenderPtr rs)
{
    m_connection.ExitDispatch();
    m_responseSender = rs;
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnCreateRequest);

    InstanceIdPolicyMap::iterator it =
        m_instanceIdPolicyMap.find(boost::make_tuple(entityProxy.GetTypeId(), entityProxy.GetReceivingHandlerId()));

    ENSURE(it != m_instanceIdPolicyMap.end(), << "Didn't find a corresponding item in m_instanceIdPolicyMap!");

    if (it->second.first == Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId)
    {
        lout << PREFIX << m_consumerNumber << ": "
             << "OnCreateRequest (Handler decides instance id): " << std::endl
             << "  Type       = " << entityProxy.GetTypeId() << std::endl
             << "  Sender     = " << entityProxy.GetSenderConnectionInfo() << std::endl
             << "  Handler    = " << entityProxy.GetReceivingHandlerId() << std::endl
             << "  HandlerStr = " << entityProxy.GetReceiverWithStringRepresentation() << std::endl
             << "  Request    = " << Safir::Dob::Typesystem::Serialization::ToXml(entityProxy.GetBlob()) << std::endl;
        lout << std::endl;

        m_connection.SetAll(entityProxy.GetRequest(),
                            Safir::Dob::Typesystem::InstanceId(it->second.second),
                            entityProxy.GetReceivingHandlerId());

        lout << PREFIX << m_consumerNumber << ": "
             << "Handler created instance " << it->second.second << std::endl;


        Safir::Dob::EntityIdResponsePtr resp = Safir::Dob::EntityIdResponse::Create();
        resp->Assigned().SetVal(Safir::Dob::Typesystem::EntityId(entityProxy.GetTypeId(),
                                                                 Safir::Dob::Typesystem::InstanceId(it->second.second)));
        rs->Send(resp);
        ++(it->second.second);
    }
    else
    {
        lout << PREFIX << m_consumerNumber << ": "
             << "OnCreateRequest (Requestor decides instance id): " << std::endl
             << "  Entity     = " << entityProxy.GetEntityId() << std::endl
             << "  Sender     = " << entityProxy.GetSenderConnectionInfo() << std::endl
             << "  Handler    = " << entityProxy.GetReceivingHandlerId() << std::endl
             << "  HandlerStr = " << entityProxy.GetReceiverWithStringRepresentation() << std::endl
             << "  Request    = " << Safir::Dob::Typesystem::Serialization::ToXml(entityProxy.GetBlob()) << std::endl;
        lout << std::endl;

        m_connection.SetAll(entityProxy.GetRequest(),
                            entityProxy.GetInstanceId(),
                            entityProxy.GetReceivingHandlerId());
    }




    if (!rs->IsDone())
    {
        DoseTest::SuccessfulCreatePtr resp = DoseTest::SuccessfulCreate::Create();
        resp->Info().SetVal(L"AutoResponse");
        rs->Send(resp);
    }
}

void
Consumer::OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityProxy,
                          Safir::Dob::ResponseSenderPtr rs)
{
    m_connection.ExitDispatch();
    m_responseSender = rs;
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnUpdateRequest);

    lout << PREFIX << m_consumerNumber << ": "
         << "OnUpdateRequest: " << std::endl
         << "  Entity     = " << entityProxy.GetEntityId() << std::endl
         << "  Sender     = " << entityProxy.GetSenderConnectionInfo() << std::endl
         << "  Handler    = " << entityProxy.GetReceivingHandlerId() << std::endl
         << "  HandlerStr = " << entityProxy.GetReceiverWithStringRepresentation() << std::endl
         << "  Request    = " << Safir::Dob::Typesystem::Serialization::ToXml(entityProxy.GetBlob()) << std::endl;
    lout << std::endl;

    m_connection.SetChanges(entityProxy.GetRequest(),entityProxy.GetInstanceId(),entityProxy.GetReceivingHandlerId());

    if (!rs->IsDone())
    {
        DoseTest::SuccessfulUpdatePtr resp = DoseTest::SuccessfulUpdate::Create();
        resp->Info().SetVal(L"AutoResponse");
        rs->Send(resp);
    }
}

void
Consumer::OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityProxy,
                          Safir::Dob::ResponseSenderPtr rs)
{
    m_connection.ExitDispatch();
    m_responseSender = rs;
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnDeleteRequest);

    lout << PREFIX << m_consumerNumber << ": "
         << "OnDeleteRequest: " << std::endl
         << "  Entity     = " << entityProxy.GetEntityId() << std::endl
         << "  Sender     = " << entityProxy.GetSenderConnectionInfo() << std::endl
         << "  Handler    = " << entityProxy.GetReceivingHandlerId() << std::endl
         << "  HandlerStr = " << entityProxy.GetReceiverWithStringRepresentation() << std::endl;

    lout << std::endl;

    m_connection.Delete(entityProxy.GetEntityId(),entityProxy.GetReceivingHandlerId());

    if (!rs->IsDone())
    {
        DoseTest::SuccessfulDeletePtr resp = DoseTest::SuccessfulDelete::Create();
        resp->Info().SetVal(L"AutoResponse");
        rs->Send(resp);
    }
}

void Consumer::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
{
    m_connection.ExitDispatch();
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnResponse);

    lout << PREFIX << m_consumerNumber
         << ": "<< "OnResponse:" << std::endl
         << "  Type       = " << Safir::Dob::Typesystem::Operations::GetName(responseProxy.GetTypeId()) << std::endl
         << "  IsSuccess  = " << std::boolalpha << responseProxy.IsSuccess() << std::endl
         << "  Sender     = " << responseProxy.GetResponseSenderConnectionInfo() << std::endl
         << "  Response   = " << Safir::Dob::Typesystem::Serialization::ToXml(responseProxy.GetBlob()) << std::endl
         << "  Request    = ";
    try
    {
        lout << Safir::Dob::Typesystem::Serialization::ToXml(responseProxy.GetRequest()) << std::endl;
    }
    catch (const Safir::Dob::Typesystem::SoftwareViolationException&)
    {
        const Safir::Dob::Typesystem::EntityId eid (responseProxy.GetRequestTypeId(),responseProxy.GetRequestInstanceId());
        lout << "DeleteRequest on " << eid << std::endl;
    }

    lout << std::endl;
}

void Consumer::OnNotRequestOverflow()
{
    m_connection.ExitDispatch();
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnNotRequestOverflow);
    lout << PREFIX << m_consumerNumber << ": " << "OnNotRequestOverflow" << std::endl;
}

std::wstring ToUpper(const std::wstring & str)
{
    std::wstring result;
    for (std::wstring::const_iterator it = str.begin(); it != str.end(); ++it)
    {
        result.push_back(static_cast<wchar_t>(std::toupper(*it)));
    }
    return result;
}



Safir::Dob::Typesystem::Int64 Consumer::GetTimestamp(const DoseTest::ActionPtr& action)
{
    static const Safir::Dob::Typesystem::EntityId entityId
        (DoseTest::LastInjectionTimestamp::ClassTypeId,
         Safir::Dob::Typesystem::InstanceId(DoseTest::LastInjectionTimestamp::ClassTypeId));

    const Safir::Dob::Typesystem::Int64 delta = action->TimestampDelta();
    const Safir::Dob::EntityProxy ep = m_connection.Read(entityId);

    DoseTest::LastInjectionTimestampPtr ent = boost::static_pointer_cast<DoseTest::LastInjectionTimestamp>(ep.GetEntity());

    const Safir::Dob::Typesystem::Int64 newVal = ent->Timestamp() + delta;

    ent->Timestamp() = newVal;

    m_connection.UpdateRequest(ent,entityId.GetInstanceId(),&m_timestampRequestor);

    return newVal;
}

void Consumer::ExecuteAction(DoseTest::ActionPtr action)
{
    try
    {
        //only becomes true if RepeatUntilOverflow is true
        bool repeat = !action->RepeatUntilOverflow().IsNull() && action->RepeatUntilOverflow().GetVal();

        do //while repeat
        {
            try
            {
                switch (action->ActionType().GetVal())
                {
                case DoseTest::ActionEnum::SendResponse:
                    {
                        Safir::Dob::ResponsePtr ptr=boost::static_pointer_cast<Safir::Dob::Response>(action->Object().GetPtr());
                        m_responseSender->Send(ptr);
                    }
                    break;
                case DoseTest::ActionEnum::DiscardResponseSender:
                    {
                        m_responseSender->Discard();
                    }
                    break;
                case DoseTest::ActionEnum::RegisterEntityHandler:
                    {
                        m_connection.RegisterEntityHandler(action->TypeId(),
                                                           action->Handler(),
                                                           action->InstanceIdPolicy(),
                                                           this);
                        // Save instance id policy
                        PolicyKey policyKey(action->TypeId(), action->Handler());

                        m_instanceIdPolicyMap[policyKey] = std::make_pair(action->InstanceIdPolicy(),
                                                                          action->Handler().GetVal().GetRawValue());

                    }
                    break;
                case DoseTest::ActionEnum::RegisterEntityHandlerInjection:
                    {
                        m_connection.RegisterEntityHandlerInjection(action->TypeId(),
                                                                    action->Handler(),
                                                                    action->InstanceIdPolicy(),
                                                                    this);
                        // Save instance id policy
                        PolicyKey policyKey(action->TypeId(), action->Handler());

                        m_instanceIdPolicyMap[policyKey] = std::make_pair(action->InstanceIdPolicy(),
                                                                          action->Handler().GetVal().GetRawValue());

                    }
                    break;
                case DoseTest::ActionEnum::RegisterEntityHandlerPending:
                    {
                        m_connection.RegisterEntityHandlerPending(action->TypeId(),
                                                                  action->Handler(),
                                                                  action->InstanceIdPolicy(),
                                                                  this);
                        // Save instance id policy
                        PolicyKey policyKey(action->TypeId(), action->Handler());

                        m_instanceIdPolicyMap[policyKey] = std::make_pair(action->InstanceIdPolicy(),
                                                                          action->Handler().GetVal().GetRawValue());
                    }
                    break;
                case DoseTest::ActionEnum::RegisterServiceHandler:
                    {
                        m_connection.RegisterServiceHandler(action->TypeId(),
                                                            action->Handler(),
                                                            this);

                    }
                    break;
                case DoseTest::ActionEnum::RegisterServiceHandlerPending:
                    {
                        m_connection.RegisterServiceHandlerPending(action->TypeId(),
                                                                   action->Handler(),
                                                                   this);

                    }
                    break;
                case DoseTest::ActionEnum::UnregisterHandler:
                    {
                        m_connection.UnregisterHandler(action->TypeId(),
                                                       action->Handler());

                    }
                    break;

                case DoseTest::ActionEnum::SubscribeMessage:
                    {
                        m_connection.SubscribeMessage(action->TypeId(),
                                                      action->Channel(),
                                                      action->IncludeSubclasses(),
                                                      this);

                    }
                    break;
                case DoseTest::ActionEnum::UnsubscribeMessage:
                    {
                        m_connection.UnsubscribeMessage(action->TypeId(),
                                                        action->Channel(),
                                                        action->IncludeSubclasses(),
                                                        this);
                    }
                    break;
                case DoseTest::ActionEnum::SubscribeEntity:
                    {
                        if (!action->TypeId().IsNull())
                        {
                            m_connection.SubscribeEntity(action->TypeId(),
                                                         action->IncludeUpdates(),
                                                         action->IncludeSubclasses(),
                                                         action->RestartSubscription(),
                                                         this);
                        }
                        else
                        {
                            m_connection.SubscribeEntity(action->EntityId(),
                                                         action->IncludeUpdates(),
                                                         action->RestartSubscription(),
                                                         this);
                        }
                    }
                    break;
                case DoseTest::ActionEnum::InjectorSubscribeEntity:
                    {
                        Safir::Dob::ConnectionAspectInjector(m_connection).SubscribeEntity
                            (action->TypeId(),
                             action->IncludeUpdates(),
                             action->IncludeSubclasses(),
                             action->RestartSubscription(),
                             action->WantsGhostDelete(),
                             action->WantsLastState(),
                             action->DoesntWantSourceIsPermanentStore(),
                             action->WantsAllStateChanges(),
                             action->TimestampChangeInfo(),
                             this);
                    }
                    break;
                case DoseTest::ActionEnum::UnsubscribeEntity:
                    {
                        if (action->TypeId().IsNull())
                        {
                            m_connection.UnsubscribeEntity(action->EntityId(),
                                                           this);
                        }
                        else
                        {
                            m_connection.UnsubscribeEntity(action->TypeId(),
                                                           action->IncludeSubclasses(),
                                                           this);
                        }
                    }
                    break;
                case DoseTest::ActionEnum::SubscribeRegistration:
                    {
                        m_connection.SubscribeRegistration(action->TypeId(),
                                                           action->Handler(),
                                                           action->IncludeSubclasses(),
                                                           action->RestartSubscription(),
                                                           this);
                    }
                    break;
                case DoseTest::ActionEnum::UnsubscribeRegistration:
                    {
                        m_connection.UnsubscribeRegistration(action->TypeId(),
                                                             action->Handler(),
                                                             action->IncludeSubclasses(),
                                                             this);
                    }
                    break;
                case DoseTest::ActionEnum::SendMessage:
                    {
                        m_connection.Send(boost::static_pointer_cast<Safir::Dob::Message>(action->Object().GetPtr()),
                                          action->Channel(),
                                          this);
                    }
                    break;
                case DoseTest::ActionEnum::ServiceRequest:
                    {
                        m_latestRequestId = m_connection.ServiceRequest
                            (boost::static_pointer_cast<Safir::Dob::Service>(action->Object().GetPtr()),
                             action->Handler(),
                             this);
                    }
                    break;

                case DoseTest::ActionEnum::CreateRequest:
                    {
                        if (!action->Instance().IsNull())
                        {
                            m_latestRequestId = m_connection.CreateRequest
                                (boost::static_pointer_cast<Safir::Dob::Entity>(action->Object().GetPtr()),
                                 action->Instance(),
                                 action->Handler(),
                                 this);
                        }
                        else
                        {
                            m_latestRequestId = m_connection.CreateRequest
                                (boost::static_pointer_cast<Safir::Dob::Entity>(action->Object().GetPtr()),
                                 action->Handler(),
                                 this);
                        }
                    }
                    break;
                case DoseTest::ActionEnum::UpdateRequest:
                    {
                        m_latestRequestId = m_connection.UpdateRequest
                            (boost::static_pointer_cast<Safir::Dob::Entity>(action->Object().GetPtr()),
                             action->Instance(),
                             this);
                    }
                    break;
                case DoseTest::ActionEnum::DeleteRequest:
                    {
                        m_latestRequestId = m_connection.DeleteRequest(action->EntityId().GetVal(),this);
                    }
                    break;
                case DoseTest::ActionEnum::SetAll:
                    {
                        m_connection.SetAll(boost::static_pointer_cast<Safir::Dob::Entity>(action->Object().GetPtr()),
                                            action->Instance(),
                                            action->Handler());
                    }
                    break;
                case DoseTest::ActionEnum::InitialSet:
                    {
                        Safir::Dob::ConnectionAspectInjector(m_connection).InitialSet
                            (boost::static_pointer_cast<Safir::Dob::Entity>(action->Object().GetPtr()),
                             action->Instance(),
                             action->Handler());
                    }
                    break;
                case DoseTest::ActionEnum::SetChanges:
                    {
                        m_connection.SetChanges(boost::static_pointer_cast<Safir::Dob::Entity>(action->Object().GetPtr()),
                                                action->Instance(),
                                                action->Handler());
                    }
                    break;
                case DoseTest::ActionEnum::InjectChanges:
                    {
                        Safir::Dob::ConnectionAspectInjector(m_connection).InjectChanges
                            (boost::static_pointer_cast<Safir::Dob::Entity>(action->Object().GetPtr()),
                             action->Instance(),
                             GetTimestamp(action),
                             action->Handler());
                    }
                    break;
                case DoseTest::ActionEnum::Delete:
                    {
                        m_connection.Delete(action->EntityId().GetVal(),
                                            action->Handler());
                    }
                    break;
                case DoseTest::ActionEnum::InjectDelete:
                    {
                        Safir::Dob::ConnectionAspectInjector(m_connection).InjectDelete(action->EntityId().GetVal(),
                                                                                        GetTimestamp(action),
                                                                                        action->Handler());
                    }
                    break;
                case DoseTest::ActionEnum::Postpone:
                    {
                        Safir::Dob::ConnectionAspectPostpone(m_connection).Postpone(action->RedispatchCurrent().GetVal());
                    }
                    break;
                case DoseTest::ActionEnum::ResumePostponed:
                    {
                        Safir::Dob::ConnectionAspectPostpone(m_connection).ResumePostponed();
                    }
                    break;
                case DoseTest::ActionEnum::IncompleteInjectionState:
                    {
                        Safir::Dob::ConnectionAspectPostpone(m_connection).IncompleteInjectionState();
                    }
                    break;
                case DoseTest::ActionEnum::DeleteAllInstances:
                    {
                        m_connection.DeleteAllInstances(action->TypeId().GetVal(),
                                                        action->Handler());
                    }
                    break;

                case DoseTest::ActionEnum::GetEntityIterator:
                    {
                        lout << PREFIX << m_consumerNumber << ": "
                             << "Iterating over entities of type "
                             << Safir::Dob::Typesystem::Operations::GetName(action->TypeId())
                             <<  ":" << std::endl;
                        Safir::Dob::EntityIterator it = m_connection.GetEntityIterator(action->TypeId(), action->IncludeSubclasses());
                        while ( it != Safir::Dob::EntityIterator())
                        {
                            const Safir::Dob::EntityProxy& entityProxy = *it;
                            lout << "  EntityId  = " << entityProxy.GetEntityId() << ":" << std::endl
                                 << "     Owner     = " << entityProxy.GetOwner() << std::endl
                                 << "     OwnerConn = " << entityProxy.GetOwnerConnectionInfo() << std::endl
                                 << "     OwnerStr  = " << entityProxy.GetOwnerWithStringRepresentation() << std::endl
                                 << "     Entity    = " << Safir::Dob::Typesystem::Serialization::ToXml(entityProxy.GetBlob()) << std::endl;

                            ++it;
                        }
                        lout << std::endl;

                    }
                    break;

                case DoseTest::ActionEnum::Read:
                    {
                        lout << PREFIX << m_consumerNumber << ": "
                             << "Read entity "
                             << action->EntityId().GetVal()
                             << ":" << std::endl;

                        const Safir::Dob::EntityProxy entityProxy = m_connection.Read(action->EntityId());
                        lout << "  EntityId  = " << entityProxy.GetEntityId() << ":" << std::endl
                             << "  Owner     = " << entityProxy.GetOwner() << std::endl
                             << "  OwnerConn = " << entityProxy.GetOwnerConnectionInfo() << std::endl
                             << "  OwnerStr  = " << entityProxy.GetOwnerWithStringRepresentation() << std::endl
                             << "  Entity    = " << Safir::Dob::Typesystem::Serialization::ToXml(entityProxy.GetBlob()) << std::endl;

                        lout << std::endl;
                    }
                    break;

                case DoseTest::ActionEnum::SimulateOverflows:
                    {
                        lout << PREFIX << m_consumerNumber << ": "
                             << "SimulateOverflows("
                             << std::boolalpha
                             << action->InQueues() << ", "
                             << action->OutQueues() << ")" << std::endl;
                        Safir::Dob::ConnectionAspectMisc(m_connection).SimulateOverflows(action->InQueues(),action->OutQueues());
                    }
                    break;

                case DoseTest::ActionEnum::IsCreated:
                    {
                        lout << PREFIX << m_consumerNumber << ": "
                            << "The instance "
                            << action->EntityId().GetVal()
                            << " is "
                            << (m_connection.IsCreated(action->EntityId().GetVal())?"":"not ")
                            << "created."
                            << std::endl;
                    }
                    break;

                case DoseTest::ActionEnum::GetNumberOfInstances:
                    {
                        lout << PREFIX << m_consumerNumber << ": "
                            << "GetNumberOfInstances (type = "
                            << Safir::Dob::Typesystem::Operations::GetName(action->TypeId())
                            << ", handler = " << action->Handler()
                            << ", includeSubclasses = " << std::boolalpha << action->IncludeSubclasses()
                            << "): "
                            << m_connection.GetNumberOfInstances(action->TypeId(),
                                                                 action->Handler(),
                                                                 action->IncludeSubclasses())
                            << std::endl;
                    }
                    break;

                case DoseTest::ActionEnum::GetQueueCapacity:
                    {
                        lout << PREFIX << m_consumerNumber << ": "
                             << "The capacity of "
                             << Safir::Dob::ConnectionQueueId::ToString(action->ConnectionQueueId().GetVal())
                             << " is "
                             << Safir::Dob::ConnectionAspectMisc(m_connection).GetQueueCapacity(action->ConnectionQueueId().GetVal())
                             << std::endl;
                    }
                    break;

                case DoseTest::ActionEnum::GetQueueSize:
                    {
                        lout << PREFIX << m_consumerNumber << ": "
                             << "The size of "
                             << Safir::Dob::ConnectionQueueId::ToString(action->ConnectionQueueId().GetVal())
                             << " is "
                             << Safir::Dob::ConnectionAspectMisc(m_connection).GetQueueSize(action->ConnectionQueueId().GetVal())
                             << std::endl;
                    }
                    break;

                case DoseTest::ActionEnum::ResetCallbackActions:
                    {
                        lout << PREFIX << m_consumerNumber << ": ResetCallbackActions"<<std::endl;
                        std::for_each(m_callbackActions.begin(),m_callbackActions.end(),boost::bind(&Actions::clear,_1));
                    }
                    break;

                default:
                    {
                        lout << PREFIX << m_consumerNumber << ": "
                            << "No handler defined for action "
                            << DoseTest::ActionEnum::ToString(action->ActionType().GetVal())
                            << std::endl;
                    }
                }
            }
            catch (const Safir::Dob::OverflowException &)
            {
                lout << "Caught Overflow exception" << std::endl;
                repeat = false;
            }
        }
        while (repeat);
    }
    catch (const Safir::Dob::Typesystem::Exception & exc)
    {
        lout << "Caught Exception in ExecuteAction: " << exc.GetName() << std::endl;
        std::wcout << "Exception info: " << exc.GetExceptionInfo() << std::endl;
    }
    catch (const Safir::Dob::Typesystem::FundamentalException & exc)
    {
        lout << "Caught FundamentalException in ExecuteAction: " << exc.GetName() << std::endl;
        std::wcout << "Exception info: " << exc.GetExceptionInfo() << std::endl;
    }
}
