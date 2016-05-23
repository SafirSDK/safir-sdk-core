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
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/ToolSupport/Serialization.h>
#include <Safir/Websocket/Parameters.h>
#include "RemoteClient.h"
#include "CommandValidator.h"
#include "Typesystem.h"
#include "Methods.h"
#include "JsonRpcRequest.h"

RemoteClient::RemoteClient(WsServer& server,
                           boost::asio::io_service& ioService,
                           websocketpp::connection_hdl& connectionHandle,
                           std::function<void(const RemoteClient*)> onClose)
    :m_server(server)
    ,m_strand(ioService)
    ,m_connectionHandle(connectionHandle)
    ,m_connection(m_server.get_con_from_hdl(connectionHandle))
    ,m_onConnectionClosed(onClose)
    ,m_dob(m_strand, [=](const std::string& msg){SendToClient(msg);})
    ,m_pingHandler(ioService, static_cast<int>(Safir::Websocket::Parameters::PingInterval()), [=]{m_connection->ping("");})
{
    m_connection->set_close_handler(m_strand.wrap([=](websocketpp::connection_hdl hdl){OnClose(hdl);}));
    m_connection->set_message_handler(m_strand.wrap([=](websocketpp::connection_hdl hdl, WsMessage msg){OnMessage(hdl, msg);}));

    m_pingHandler.Start();
}

void RemoteClient::Close()
{
    //called from websocket server
    m_strand.dispatch([=]
    {
        m_pingHandler.Stop();
        m_dob.Close();
        m_server.close(m_connectionHandle, websocketpp::close::status::normal, "onStopOrder");
    });
}

std::string RemoteClient::ToString() const
{
    return m_connection->get_remote_endpoint();
}

//------------------------------------------------------
// Websocket events
//------------------------------------------------------
void RemoteClient::OnClose(websocketpp::connection_hdl hdl)
{
    //client closed connection
    lllog(5)<<"RemoteClient.OnClose"<<std::endl;
    m_pingHandler.Stop();
    m_dob.Close();
    m_onConnectionClosed(this);
}

void RemoteClient::OnMessage(websocketpp::connection_hdl hdl, WsMessage msg)
{
    try
    {        
        auto payload=msg->get_payload();
        lllog(5)<<"RemoteClient.OnMessage "<<payload.c_str()<<std::endl;

        JsonRpcRequest req(payload);
        try
        {
            req.Validate();
        }
        catch (const RequestErrorException& e)
        {
            SendToClient(JsonRpcResponse::Error(req.Id(), e.Code(), e.Message()));
            return;
        }

        WsDispatch(req);
    }
    catch (const RequestErrorException& e)
    {
        SendToClient(JsonRpcResponse::Error(JsonRpcId(), e.Code(), e.Message()));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::Error(JsonRpcId(), RequestErrorException::InternalError, e.what()));
    }
    catch (...)
    {
        SendToClient(JsonRpcResponse::Error(JsonRpcId(), RequestErrorException::InternalError, "Unexpected error in safir_websocket"));
    }
}

//------------------------------------------------------
// Websocket client commands
//------------------------------------------------------
void RemoteClient::WsDispatch(const JsonRpcRequest& req)
{
    if (req.IsResponse())
    {
        //if the received data is a JsonRpc-response, is must
        //be a response to a Safir-request (Service, or Entity CRUD)
       WsResponse(req);
    }
    else if (req.Method()==Methods::SetEntity)
    {
        WsSetEntity(req);
    }
    else if (req.Method()==Methods::SetEntityChanges)
    {
        WsSetEntityChanges(req);
    }
    else if (req.Method()==Methods::CreateRequest)
    {
        WsCreateRequest(req);
    }
    else if (req.Method()==Methods::UpdateRequest)
    {
        WsUpdateRequest(req);
    }
    else if (req.Method()==Methods::DeleteRequest)
    {
        WsDeleteRequest(req);
    }
    else if (req.Method()==Methods::ServiceRequest)
    {
        WsServiceRequest(req);
    }
    else if (req.Method()==Methods::SendMessage)
    {
        WsSendMessage(req);
    }
    else if (req.Method()==Methods::ReadEntity)
    {
        WsReadEntity(req);
    }
    else if (req.Method()==Methods::DeleteEntity)
    {
        WsDeleteEntity(req);
    }
    else if (req.Method()==Methods::DeleteAllInstances)
    {
        WsDeleteAllInstances(req);
    }
    else if (req.Method()==Methods::SubscribeMessage)
    {
        WsSubscribeMessage(req);
    }    
    else if (req.Method()==Methods::UnsubscribeMessage)
    {
        WsUnsubscribeMessage(req);
    }
    else if (req.Method()==Methods::SubscribeEntity)
    {
        WsSubscribeEntity(req);
    }
    else if (req.Method()==Methods::RegisterEntityHandler)
    {
        WsRegisterEntityHandler(req);
    }
    else if (req.Method()==Methods::RegisterServiceHandler)
    {
        WsRegisterServiceHandler(req);
    }
    else if (req.Method()==Methods::UnregisterHandler)
    {
        WsUnregisterHandler(req);
    }
    else if (req.Method()==Methods::SubscribeRegistration)
    {
        WsSubscribeRegistration(req);
    }
    else if (req.Method()==Methods::UnsubscribeRegistration)
    {
        WsUnsubscribeRegistration(req);
    }
    else if (req.Method()==Methods::IsCreated)
    {
        WsIsCreated(req);
    }
    else if (req.Method()==Methods::GetNumberOfInstances)
    {
        WsGetNumberOfInstances(req);
    }
    else if (req.Method()==Methods::GetAllInstanceIds)
    {
        WsGetAllInstanceIds(req);
    }    
    else if (req.Method()==Methods::Ping)
    {
        WsPing(req);
    }
    else if (req.Method()==Methods::Open)
    {
        WsOpen(req);
    }
    else if (req.Method()==Methods::Close)
    {
        WsClose(req);
    }
    else if (req.Method()==Methods::IsOpen)
    {
        WsIsOpen(req);
    }
    else if (req.Method()==Methods::GetInstanceIdPolicy)
    {
        WsGetInstanceIdPolicy(req);
    }
    else if (req.Method()==Methods::GetTypeHierarchy)
    {
        WsGetTypeHierarchy(req);
    }
    else
    {
        lllog(5)<<"Received command that is not supported: "<<req.Method().c_str()<<std::endl;
        SEND_SYSTEM_LOG(Error, <<"Received command that is not supported: "<<req.Method().c_str()<<std::endl);
        auto errorMsg=JsonRpcResponse::Error(req.Id(), RequestErrorException::MethodNotFound, "Command is not supported. "+req.Method());
        SendToClient(errorMsg);
    }
}

void RemoteClient::WsResponse(const JsonRpcRequest &req)
{
    try
    {
        CommandValidator::ValidateResponse(req);
        sd::ResponsePtr response=ToObject<sd::Response>(req.Result());        
        m_dob.SendResponse(response, req.Id().Int());
    }
    catch (const std::exception& e)
    {
        //There is no way to respond to a response, just log the error.
        //A future improvement could be to send a notification for this kind of errors
        SEND_SYSTEM_LOG(Error, <<"Failed to send response: "<<e.what()<<std::endl);
        lllog(5)<<"Failed to send response: "<<e.what()<<std::endl;
    }
}

void RemoteClient::WsPing(const JsonRpcRequest &req)
{
    SendToClient(JsonRpcResponse::String(req.Id(), "pong"));
}

void RemoteClient::WsOpen(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateOpen(req);
        auto context=req.HasContext() ?  req.Context() : 0;
        m_dob.Open(Wstr(req.ConnectionName()), context);
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsClose(const JsonRpcRequest& req)
{
    try
    {
        m_dob.Close();
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsIsOpen(const JsonRpcRequest &req)
{
    try
    {
        SendToClient(JsonRpcResponse::Bool(req.Id(), m_dob.IsOpen()));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsGetTypeHierarchy(const JsonRpcRequest& req)
{
    try
    {
        SendToClient(JsonRpcResponse::Json(req.Id(), Typesystem::GetTypeHierarchy()));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::Error(req.Id(), RequestErrorException::InternalError, e.what()));
    }
}

void RemoteClient::WsSubscribeMessage(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateSubscribeMessage(req);
        auto channel=req.HasChannelId() ? req.ChannelId() : ts::ChannelId::ALL_CHANNELS;
        auto includeSub=req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
        m_dob.SubscribeMessage(req.TypeId(), channel, includeSub);
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsSendMessage(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateSendMessage(req);
        auto channel=req.HasChannelId() ? req.ChannelId() : ts::ChannelId();
        sd::MessagePtr message=ToObject<sd::Message>(req.Message());
        m_dob.SendMessage(message, channel);
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsUnsubscribeMessage(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateUnsubscribeMessage(req);
        auto channel=req.HasChannelId() ? req.ChannelId() : ts::ChannelId::ALL_CHANNELS;
        auto includeUpdates=req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
        m_dob.UnsubscribeMessage(req.TypeId(), channel, includeUpdates);
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsSubscribeEntity(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateSubscribeEntity(req);
        auto includeUpdates=req.HasIncludeUpdates() ? req.IncludeUpdates() : true;
        auto restartSub=req.HasRestartSubscription() ? req.RestartSubscription() : true;

        if (req.HasInstanceId())
        {
            auto entityId=ts::EntityId(req.TypeId(), req.InstanceId());
            m_dob.SubscribeEntity(entityId, includeUpdates, restartSub);
        }
        else
        {
            auto includeSubclasses=req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
            m_dob.SubscribeEntity(req.TypeId(), includeUpdates, includeSubclasses, restartSub);
        }

        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsUnsubscribeEntity(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateUnsubscribeEntity(req);

        if (req.HasInstanceId())
        {
            auto entityId=ts::EntityId(req.TypeId(), req.InstanceId());
            m_dob.UnsubscribeEntity(entityId);
        }
        else
        {
            auto includeSubclasses=req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
            m_dob.UnsubscribeEntity(req.TypeId(), includeSubclasses);
        }

        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsRegisterEntityHandler(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateRegisterEntityHandler(req);
        auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
        auto instPolicy=req.HasInstanceIdPolicy() ? req.InstanceIdPolicy() : sd::InstanceIdPolicy::RequestorDecidesInstanceId;
        auto injectionHandler=req.HasInjectionHandler() ? req.InjectionHandler() : false;
        auto pendingReg=req.HasPending() ? req.Pending() : false;
        m_dob.RegisterEntity(req.TypeId(), handler, instPolicy, injectionHandler, pendingReg);
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {        
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsRegisterServiceHandler(const JsonRpcRequest &req)
{
    try
    {
        CommandValidator::ValidateRegisterServiceHandler(req);
        auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
        auto pendingReg=req.HasPending() ? req.Pending() : false;
        m_dob.RegisterService(req.TypeId(), handler, pendingReg);
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsUnregisterHandler(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateUnregisterHandler(req);
        auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId::ALL_HANDLERS;
        m_dob.UnregisterHandler(req.TypeId(), handler);
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsSubscribeRegistration(const JsonRpcRequest &req)
{
    try
    {
        CommandValidator::ValidateSubscribeRegistration(req);
        auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId::ALL_HANDLERS;
        auto inclSub=req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
        auto restartSub=req.HasRestartSubscription() ? req.RestartSubscription() : true;
        m_dob.SubscribeRegistration(req.TypeId(), handler, inclSub, restartSub);
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }

}

void RemoteClient::WsUnsubscribeRegistration(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateUnsubscribeRegistration(req);
        auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId::ALL_HANDLERS;
        auto inclSub=req.IncludeSubclasses() ? req.IncludeSubclasses() : true;
        m_dob.UnsubscribeRegistration(req.TypeId(), handler, inclSub);
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsCreateRequest(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateCreateRequest(req);
        auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
        sd::EntityPtr entity=ToObject<sd::Entity>(req.Entity());
        if (req.HasInstanceId())
        {
            m_dob.CreateRequest(entity, req.InstanceId(), handler, req.Id());
        }
        else
        {
            m_dob.CreateRequest(entity, handler, req.Id());
        }
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsUpdateRequest(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateUpdateRequest(req);
        auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
        sd::EntityPtr entity=ToObject<sd::Entity>(req.Entity());
        m_dob.UpdateRequest(entity, req.InstanceId(), handler, req.Id());
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsDeleteRequest(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateDeleteRequest(req);
        m_dob.DeleteRequest(req.TypeId(), req.InstanceId(), req.Id());
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsServiceRequest(const JsonRpcRequest &req)
{
    try
    {
        CommandValidator::ValidateServiceRequest(req);
        auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
        sd::ServicePtr service=ToObject<sd::Service>(req.Request());
        m_dob.ServiceRequest(service, handler, req.Id());
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsSetEntityChanges(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateSetEntityChanges(req);
        sd::EntityPtr entity=ToObject<sd::Entity>(req.Entity());
        auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
        m_dob.SetChanges(entity, req.InstanceId(), handler);
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsSetEntity(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateSetEntity(req);
        sd::EntityPtr entity=ToObject<sd::Entity>(req.Entity());
        auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
        m_dob.SetAll(entity, req.InstanceId(), handler);
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsDeleteEntity(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateDeleteEntity(req);
        auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
        m_dob.Delete(req.TypeId(), req.InstanceId(), handler);
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsDeleteAllInstances(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateDeleteAllInstances(req);
        auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
        m_dob.DeleteAllInstances(req.TypeId(), handler);
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsReadEntity(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateReadEntity(req);
        auto entityProxy=m_dob.Read(req.TypeId(), req.InstanceId());
        SendToClient(JsonRpcResponse::Json(req.Id(), entityProxy));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsIsCreated(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateIsCreated(req);
        auto isCreated=m_dob.IsCreated(req.TypeId(), req.InstanceId());
        SendToClient(JsonRpcResponse::Bool(req.Id(), isCreated));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsGetNumberOfInstances(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateGetNumberOfInstances(req);
        auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId::ALL_HANDLERS;
        auto inclSub=req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
        ts::Int64 num=m_dob.GetNumberOfInstances(req.TypeId(), handler, inclSub);
        SendToClient(JsonRpcResponse::Int(req.Id(), num));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsGetInstanceIdPolicy(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateGetInstanceIdPolicy(req);
        auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
        auto policy=m_dob.GetInstanceIdPolicy(req.TypeId(), handler);
        SendToClient(JsonRpcResponse::String(req.Id(), policy));

    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsGetAllInstanceIds(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateGetAllInstanceIds(req);
        auto ids=m_dob.GetAllInstanceIds(req.TypeId());
        SendToClient(JsonRpcResponse::UnquotedArray(req.Id(), ids));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::String(req.Id(), e.what()));
    }
}
