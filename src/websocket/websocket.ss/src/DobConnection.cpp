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
#include "JsonRpcRequest.h"
#include "JsonRpcNotification.h"
#include "JsonRpcResponse.h"
#include "Methods.h"

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable:4355)
#endif


DobConnection::DobConnection(boost::asio::io_context::strand& strand, std::function<void(const std::string&)> send)
    :m_con()
    ,m_dispatcher(m_con, strand)
    ,m_wsSend(send)
    ,m_responseSenderStore(static_cast<size_t>(sd::QueueParameters::QueueRules(0)->RequestInQueueCapacity().IsNull() ? 10
                                               : sd::QueueParameters::QueueRules(0)->RequestInQueueCapacity().GetVal()))
    ,m_proxyToJson([this](ts::TypeId t){return GetName(t);},
                   [this](ts::TypeId t, const ts::HandlerId& h){return m_con.GetInstanceIdPolicy(t,h);})
{

}

#ifdef _MSC_VER
#pragma warning(pop)
#endif


//------------------------------------------------------
// DOB events
//------------------------------------------------------
// EntityHandler interface
void DobConnection::OnCreateRequest(const sd::EntityRequestProxy entityRequestProxy, sd::ResponseSenderPtr responseSender)
{
    lllog(5)<<"WS: OnCreateRequest"<<std::endl;
    auto id=m_responseSenderStore.Add(responseSender);
    auto req=JsonRpcRequest::Json(Methods::OnCreateRequest, m_proxyToJson.ToJson(entityRequestProxy, ProxyToJson::CreateReqType), JsonRpcId(id));
    m_wsSend(req);
}

void DobConnection::OnUpdateRequest(const sd::EntityRequestProxy entityRequestProxy, sd::ResponseSenderPtr responseSender)
{
    lllog(5)<<"WS: OnUpdateRequest"<<std::endl;
    auto id=m_responseSenderStore.Add(responseSender);
    auto req=JsonRpcRequest::Json(Methods::OnUpdateRequest, m_proxyToJson.ToJson(entityRequestProxy, ProxyToJson::UpdateReqType), JsonRpcId(id));
    m_wsSend(req);
}
void DobConnection::OnDeleteRequest(const sd::EntityRequestProxy entityRequestProxy, sd::ResponseSenderPtr responseSender)
{
    lllog(5)<<"WS: OnDeleteRequest"<<std::endl;
    auto id=m_responseSenderStore.Add(responseSender);
    auto req=JsonRpcRequest::Json(Methods::OnDeleteRequest, m_proxyToJson.ToJson(entityRequestProxy, ProxyToJson::DeleteReqType), JsonRpcId(id));
    m_wsSend(req);
}
//ServiceHandler interface
void DobConnection::OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    lllog(5)<<"WS: OnServiceRequest"<<std::endl;
    auto id=m_responseSenderStore.Add(responseSender);
    auto req=JsonRpcRequest::Json(Methods::OnServiceRequest, m_proxyToJson.ToJson(serviceRequestProxy), JsonRpcId(id));
    m_wsSend(req);
}


void DobConnection::OnRevokedRegistration(const sd::Typesystem::TypeId typeId, const sd::Typesystem::HandlerId &handlerId)
{
    lllog(5)<<"WS: OnRevokedRegistration"<<std::endl;
    auto notification=JsonRpcNotification::Json(Methods::OnRevokedRegistration, m_proxyToJson.ToJson(typeId, handlerId));
    m_wsSend(notification);
}
void DobConnection::OnCompletedRegistration(const sd::Typesystem::TypeId typeId, const sd::Typesystem::HandlerId& handlerId)
{
    lllog(5)<<"WS: OnCompletedRegistration"<<std::endl;
    auto notification=JsonRpcNotification::Json(Methods::OnCompletedRegistration, m_proxyToJson.ToJson(typeId, handlerId));
    m_wsSend(notification);
}

//Injection interface
void DobConnection::OnInjectedNewEntity(const sd::InjectedEntityProxy injectedEntityProxy)
{
    lllog(5)<<"WS: OnInjectedNewEntity"<<std::endl;
    auto notification=JsonRpcNotification::Json(Methods::OnInjectedNewEntity, m_proxyToJson.ToJson(injectedEntityProxy));
    m_wsSend(notification);
}
void DobConnection::OnInjectedUpdatedEntity(const sd::InjectedEntityProxy injectedEntityProxy)
{
    lllog(5)<<"WS: OnInjectedUpdatedEntity"<<std::endl;
    auto notification=JsonRpcNotification::Json(Methods::OnInjectedUpdatedEntity, m_proxyToJson.ToJson(injectedEntityProxy));
    m_wsSend(notification);
}
void DobConnection::OnInjectedDeletedEntity(const sd::InjectedEntityProxy injectedEntityProxy)
{
    lllog(5)<<"WS: OnInjectedDeletedEntity"<<std::endl;
    auto notification=JsonRpcNotification::Json(Methods::OnInjectedDeletedEntity, m_proxyToJson.ToJson(injectedEntityProxy));
    m_wsSend(notification);
}
void DobConnection::OnInitialInjectionsDone(const sd::Typesystem::TypeId typeId, const sd::Typesystem::HandlerId& handlerId)
{
    lllog(5)<<"WS: OnInitialInjectionsDone"<<std::endl;
    auto notification=JsonRpcNotification::Json(Methods::OnInitialInjectionsDone, m_proxyToJson.ToJson(typeId, handlerId));
    m_wsSend(notification);
}

//EntitySubscriber interface
void DobConnection::OnNewEntity(const sd::EntityProxy entityProxy)
{
    lllog(5)<<"WS: OnNewEntity"<<std::endl;
    auto notification=JsonRpcNotification::Json(Methods::OnNewEntity, m_proxyToJson.ToJson(entityProxy));
    m_wsSend(notification);
}
void DobConnection::OnUpdatedEntity(const sd::EntityProxy entityProxy)
{
    lllog(5)<<"WS: OnUpdatedEntity"<<std::endl;
    auto notification=JsonRpcNotification::Json(Methods::OnUpdatedEntity, m_proxyToJson.ToJson(entityProxy));
    m_wsSend(notification);
}
void DobConnection::OnDeletedEntity(const sd::EntityProxy entityProxy, const bool)
{
    lllog(5)<<"WS: OnDeletedEntity"<<std::endl;
    auto notification=JsonRpcNotification::Json(Methods::OnDeletedEntity, m_proxyToJson.ToJson(entityProxy, true));
    m_wsSend(notification);
}

//RegistrationSubscriber interface
void DobConnection::OnRegistered(const ts::TypeId typeId, const ts::HandlerId&  handlerId)
{
    lllog(5)<<"WS: OnRegistered"<<std::endl;
    auto notification=JsonRpcNotification::Json(Methods::OnRegistered, m_proxyToJson.ToJson(typeId, handlerId));
    m_wsSend(notification);
}

void DobConnection::OnUnregistered(const ts::TypeId typeId, const Safir::Dob::Typesystem::HandlerId&  handlerId)
{
    lllog(5)<<"WS: OnUnregistered"<<std::endl;
    auto notification=JsonRpcNotification::Json(Methods::OnUnregistered, m_proxyToJson.ToJson(typeId, handlerId));
    m_wsSend(notification);
}

//Requestor interface
void DobConnection::OnResponse(const sd::ResponseProxy responseProxy)
{
    lllog(5)<<"WS: OnResponse"<<std::endl;
    auto id=m_reqIdMapper.Get(responseProxy.GetRequestId());
    if (!id.IsNull())
    {
        m_wsSend(JsonRpcResponse::Json(id, m_proxyToJson.ToJson(responseProxy)));
    }
}
void DobConnection::OnNotRequestOverflow()
{
    lllog(5)<<"WS: OnNotRequestOverflow"<<std::endl;
    auto notification=JsonRpcNotification::Empty(Methods::OnNotRequestOverflow);
    m_wsSend(notification);
}

//MessageSender interface
void DobConnection::OnNotMessageOverflow()
{
    lllog(5)<<"WS: OnNotMessageOverflow"<<std::endl;
    auto notification=JsonRpcNotification::Empty(Methods::OnNotMessageOverflow);
    m_wsSend(notification);
}

//MessageSubscriber interface
void DobConnection::OnMessage(const sd::MessageProxy messageProxy)
{
    lllog(5)<<"WS: OnMessage"<<std::endl;
    auto notification=JsonRpcNotification::Json(Methods::OnMessage, m_proxyToJson.ToJson(messageProxy));
    m_wsSend(notification);
}
