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
    ,m_connectionHandle(connectionHandle)
    ,m_connection(m_server.get_con_from_hdl(connectionHandle))
    ,m_onConnectionClosed(onClose)
    ,m_dob(ioService, [=](const std::string& msg){m_connection->send(msg);}, [=]{Close();})
{
    m_connection->set_close_handler([=](websocketpp::connection_hdl hdl){OnClose(hdl);});
    m_connection->set_message_handler([=](websocketpp::connection_hdl hdl, WsMessage msg){OnMessage(hdl, msg);});
}

void RemoteClient::Close()
{
    m_server.close(m_connectionHandle, websocketpp::close::status::going_away, "");
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
    std::cout<<"RemoteClient OnClose"<<std::endl;
    m_dob.Close();
    m_onConnectionClosed(this);
}

void RemoteClient::OnMessage(websocketpp::connection_hdl hdl, WsMessage msg)
{
    try
    {
        std::cout<<"RemoteClient OnMessage "<<msg->get_payload()<<std::endl;
        auto payload=msg->get_payload();
        JsonRpcRequest req(payload);
        try
        {
            req.Validate();
        }
        catch (const RequestErrorException& e)
        {
            m_connection->send(JsonRpcResponse::Error(req.Id(), e.Code(), e.Message()));
            return;
        }

        WsDispatch(req);
    }
    catch (const RequestErrorException& e)
    {
        m_connection->send(JsonRpcResponse::Error(JsonRpcId(), e.Code(), e.Message()));
    }
    catch (const std::exception& e)
    {
        m_connection->send(JsonRpcResponse::Error(JsonRpcId(), RequestErrorException::InternalError, e.what()));
    }
    catch (...)
    {
        m_connection->send(JsonRpcResponse::Error(JsonRpcId(), RequestErrorException::InternalError, "Unexpected error in safir_websocket"));
    }
}

//------------------------------------------------------
// Websocket client commands
//------------------------------------------------------
void RemoteClient::WsDispatch(const JsonRpcRequest& req)
{
    if (req.Method()==Methods::GetTypeHierarchy)
    {
        WsGetTypeHierarchy(req);
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
    else if (req.Method()==Methods::SubscribeMessage)
    {
        WsSubscribeMessage(req);
    }
    else if (req.Method()==Methods::SendMessage)
    {
        WsSendMessage(req);
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
    else if (req.Method()==Methods::UnregisterHandler)
    {
        WsUnregisterHandler(req);
    }
    else
    {
        std::cout<<"Received command that is not supported "<<std::endl;
        auto errorMsg=JsonRpcResponse::Error(req.Id(), RequestErrorException::MethodNotFound, "Command is not supported. "+req.Method());
        m_connection->send(errorMsg);
    }
}

void RemoteClient::WsOpen(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateOpen(req);
        auto context=req.HasContext() ?  req.Context() : 0;
        m_dob.Open(Wstr(req.ConnectionName()), context);
        m_connection->send(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        m_connection->send(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsClose(const JsonRpcRequest& req)
{
    try
    {
        m_dob.Close();
        m_connection->send(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        m_connection->send(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsIsOpen(const JsonRpcRequest &req)
{
    try
    {
        m_connection->send(JsonRpcResponse::Bool(req.Id(), m_dob.IsOpen()));
    }
    catch (const std::exception& e)
    {
        m_connection->send(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsGetTypeHierarchy(const JsonRpcRequest& req)
{
    try
    {
        m_connection->send(JsonRpcResponse::Json(req.Id(), Typesystem::GetTypeHierarchy()));
    }
    catch (const std::exception& e)
    {
        m_connection->send(JsonRpcResponse::Error(req.Id(), RequestErrorException::InternalError, e.what()));
    }
}

void RemoteClient::WsSubscribeMessage(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateSubscribeMessage(req);
        auto channel=req.HasChannelId() ? req.ChannelId() : Safir::Dob::Typesystem::ChannelId();
        auto includeSub=req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
        m_dob.SubscribeMessage(req.TypeId(), channel, includeSub);
        m_connection->send(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        m_connection->send(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsSendMessage(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateSendMessage(req);
        auto channel=req.HasChannelId() ? req.ChannelId() : Safir::Dob::Typesystem::ChannelId();
        sd::MessagePtr message=ToObject<sd::Message>(req.Message());
        m_dob.SendMessage(message, channel);
        m_connection->send(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        m_connection->send(JsonRpcResponse::String(req.Id(), e.what()));
    }

}

void RemoteClient::WsUnsubscribeMessage(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateUnsubscribeMessage(req);
        auto channel=req.HasChannelId() ? req.ChannelId() : Safir::Dob::Typesystem::ChannelId();
        auto includeUpdates=req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
        m_dob.UnsubscribeMessage(req.TypeId(), channel, includeUpdates);
        m_connection->send(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        m_connection->send(JsonRpcResponse::String(req.Id(), e.what()));
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

        m_connection->send(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        m_connection->send(JsonRpcResponse::String(req.Id(), e.what()));
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

        m_connection->send(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        m_connection->send(JsonRpcResponse::String(req.Id(), e.what()));
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
        m_connection->send(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        m_connection->send(JsonRpcResponse::String(req.Id(), e.what()));
    }
}

void RemoteClient::WsUnregisterHandler(const JsonRpcRequest& req)
{
    try
    {
        CommandValidator::ValidateUnregisterHandler(req);
        auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
        m_dob.UnregisterHandler(req.TypeId(), handler);
        m_connection->send(JsonRpcResponse::String(req.Id(), "OK"));
    }
    catch (const std::exception& e)
    {
        m_connection->send(JsonRpcResponse::String(req.Id(), e.what()));
    }
}
