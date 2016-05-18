/******************************************************************************
*
* Copyright Saab AB, 2016 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#include <boost/lexical_cast.hpp>
#include "DobConnection.h"
#include "JsonHelpers.h"
#include "JsonRpcNotification.h"
#include "JsonRpcResponse.h"
#include "Methods.h"

DobConnection::DobConnection::DobConnection(boost::asio::io_service &ioService, boost::function<void(const std::string&)> send)
    :m_con()
    ,m_dispatcher(m_con, ioService)
    ,m_wsSend(send)
{

}

std::string DobConnection::ToJson(const sd::EntityProxy& proxy) const
{
    std::ostringstream os;
    os<<"{"<<proxy.GetInstanceId()<<","<<SAFIR_WS_OBJ("entity",ts::Internal::ToJson(proxy.GetBlob()))<<"}";
    return std::move(os.str());
}

std::string DobConnection::ToJson(const sd::MessageProxy& proxy) const
{
    std::ostringstream os;
    os<<"{"<<proxy.GetChannelId()<<","<<SAFIR_WS_OBJ("message",ts::Internal::ToJson(proxy.GetBlob()))<<"}";
    return std::move(os.str());
}

std::string DobConnection::ToJson(const sd::ResponseProxy& proxy) const
{
    std::ostringstream os;
    os<<"{"<<SAFIR_WS_BOOL("isSuccess",proxy.IsSuccess())<<","<<SAFIR_WS_OBJ("response", ts::Internal::ToJson(proxy.GetBlob()))<<"}";
    return std::move(os.str());
}

std::string DobConnection::ToJson(const sd::ServiceRequestProxy& proxy) const
{
    std::ostringstream os;
    os<<"{"<<proxy.GetReceivingHandlerId()<<","<<SAFIR_WS_OBJ("request",ts::Internal::ToJson(proxy.GetBlob()))<<"}";
    return std::move(os.str());
}

std::string DobConnection::ToJson(const Safir::Dob::EntityRequestProxy &proxy) const
{
    std::ostringstream os;
    os<<"{"<<proxy.GetReceivingHandlerId()<<",";
    if (m_con.GetInstanceIdPolicy(proxy.GetTypeId(), proxy.GetReceivingHandlerId())==sd::InstanceIdPolicy::RequestorDecidesInstanceId)
    {
        os<<proxy.GetInstanceId()<<",";
    }
    os<<SAFIR_WS_OBJ("request",ts::Internal::ToJson(proxy.GetBlob()))<<"}";
    return std::move(os.str());
}

std::string DobConnection::ToJson(const Safir::Dob::InjectedEntityProxy &proxy) const
{
    std::ostringstream os;
    os<<"{"<<proxy.GetInstanceId()<<","<<SAFIR_WS_OBJ("entity",ts::Internal::ToJson(proxy.GetInjectionBlob()))<<"}";
    return std::move(os.str());
}

std::string DobConnection::ToJson(Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handler) const
{
    std::ostringstream os;
    os<<"{"<<SAFIR_WS_STR("typeId", GetName(typeId))<<","<<handler<<"}";
    return std::move(os.str());
}

//------------------------------------------------------
// DOB events
//------------------------------------------------------
// EntityHandler interface
void DobConnection::OnCreateRequest(const sd::EntityRequestProxy entityRequestProxy, sd::ResponseSenderPtr responseSender)
{
    //TODO
}
void DobConnection::OnUpdateRequest(const sd::EntityRequestProxy entityRequestProxy, sd::ResponseSenderPtr responseSender)
{
    //TODO
}
void DobConnection::OnDeleteRequest(const sd::EntityRequestProxy entityRequestProxy, sd::ResponseSenderPtr responseSender)
{
    //TODO
}
//ServiceHandler interface
void DobConnection::OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    //TODO
}


void DobConnection::OnRevokedRegistration(const sd::Typesystem::TypeId typeId, const sd::Typesystem::HandlerId &handlerId)
{
    auto notification=JsonRpcNotification::Json(Methods::OnRevokedRegistration, ToJson(typeId, handlerId));
    m_wsSend(notification);
}
void DobConnection::OnCompletedRegistration(const sd::Typesystem::TypeId typeId, const sd::Typesystem::HandlerId& handlerId)
{
    auto notification=JsonRpcNotification::Json(Methods::OnCompletedRegistration, ToJson(typeId, handlerId));
    m_wsSend(notification);
}

//Injection interface
void DobConnection::OnInjectedNewEntity(const sd::InjectedEntityProxy injectedEntityProxy)
{
    auto notification=JsonRpcNotification::Json(Methods::OnInjectedNewEntity, ToJson(injectedEntityProxy));
    m_wsSend(notification);
}
void DobConnection::OnInjectedUpdatedEntity(const sd::InjectedEntityProxy injectedEntityProxy)
{
    auto notification=JsonRpcNotification::Json(Methods::OnInjectedUpdatedEntity, ToJson(injectedEntityProxy));
    m_wsSend(notification);
}
void DobConnection::OnInjectedDeletedEntity(const sd::InjectedEntityProxy injectedEntityProxy)
{
    auto notification=JsonRpcNotification::Json(Methods::OnInjectedDeletedEntity, ToJson(injectedEntityProxy));
    m_wsSend(notification);
}
void DobConnection::OnInitialInjectionsDone(const sd::Typesystem::TypeId typeId, const sd::Typesystem::HandlerId& handlerId)
{
    auto notification=JsonRpcNotification::Json(Methods::OnInjectedDeletedEntity, ToJson(typeId, handlerId));
    m_wsSend(notification);
}

//EntitySubscriber interface
void DobConnection::OnNewEntity(const sd::EntityProxy entityProxy)
{
    auto notification=JsonRpcNotification::Json(Methods::OnNewEntity, ToJson(entityProxy));
    m_wsSend(notification);
}
void DobConnection::OnUpdatedEntity(const sd::EntityProxy entityProxy)
{
    auto notification=JsonRpcNotification::Json(Methods::OnUpdatedEntity, ToJson(entityProxy));
    m_wsSend(notification);
}
void DobConnection::OnDeletedEntity(const sd::EntityProxy entityProxy, const bool)
{
    auto notification=JsonRpcNotification::Json(Methods::OnDeletedEntity, ToJson(entityProxy));
    m_wsSend(notification);
}

//RegistrationSubscriber interface
void DobConnection::OnRegistered(const ts::TypeId typeId, const ts::HandlerId&  handlerId)
{
    auto notification=JsonRpcNotification::Json(Methods::OnRegistered, ToJson(typeId, handlerId));
    m_wsSend(notification);
}

void DobConnection::OnUnregistered(const ts::TypeId typeId, const Safir::Dob::Typesystem::HandlerId&  handlerId)
{
    auto notification=JsonRpcNotification::Json(Methods::OnUnregistered, ToJson(typeId, handlerId));
    m_wsSend(notification);
}

//Requestor interface
void DobConnection::OnResponse(const sd::ResponseProxy responseProxy)
{
    auto response=JsonRpcResponse::Json(m_reqIdMapper.Get(responseProxy.GetRequestId()), ToJson(responseProxy));
    m_wsSend(response);

}
void DobConnection::OnNotRequestOverflow()
{
    auto notification=JsonRpcNotification::Empty(Methods::OnNotRequestOverflow);
    m_wsSend(notification);
}

//MessageSender interface
void DobConnection::OnNotMessageOverflow()
{
    auto notification=JsonRpcNotification::Empty(Methods::OnNotMessageOverflow);
    m_wsSend(notification);
}

//MessageSubscriber interface
void DobConnection::OnMessage(const sd::MessageProxy messageProxy)
{
    std::cout<<"OnMessage - "<<messageProxy.GetChannelId()<<std::endl;
    auto notification=JsonRpcNotification::Json(Methods::OnMessage, ToJson(messageProxy));
    m_wsSend(notification);
}
