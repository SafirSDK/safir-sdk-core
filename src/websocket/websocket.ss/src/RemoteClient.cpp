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
    std::cout<<"RemoteClient OnMessage "<<msg->get_payload()<<std::endl;
    std::string payload=msg->get_payload();
    JsonRpcRequest dummy(payload);

    payload.insert(1, "\"_DouType\":\"Safir.Websocket.Send\",");

    auto cmd = ToCommand(payload);
    if (cmd)
    {
        WsDispatch(cmd);
    }
    else
    {
        std::cout<<"Received data from "<<ToString()<<" that could not be understood"<<std::endl<<msg->get_payload()<<std::endl;
        m_connection->send(ResponseFactory::Error(Methods::GeneralError, L"I dont understand what you mean!"));
    }
}

//------------------------------------------------------
// Websocket client commands
//------------------------------------------------------
void RemoteClient::WsDispatch(const ws::SendPtr& cmd)
{
    if (cmd->Command().IsNull())
    {
        std::cout<<"Received command that is not supported "<<std::endl;
        auto msg=ResponseFactory::Error(Methods::GeneralError, L"Command is not supported. "+cmd->Command().GetVal(), ReqId(cmd));
        m_connection->send(msg);
    }
    else if (cmd->Command()==Methods::GetTypeHierarchy)
    {
        WsGetTypeHierarchy(cmd);
    }
    else if (cmd->Command()==Methods::Open)
    {
        WsOpen(cmd);
    }
    else if (cmd->Command()==Methods::Close)
    {
        WsClose(cmd);
    }
    else if (cmd->Command()==Methods::SubscribeMessage)
    {
        WsSubscribeMessage(cmd);
    }
    else if (cmd->Command()==Methods::SendMessage)
    {
        WsSendMessage(cmd);
    }
    else if (cmd->Command()==Methods::UnsubscribeMessage)
    {
        WsUnsubscribeMessage(cmd);
    }
    else if (cmd->Command()==Methods::SubscribeEntity)
    {
        WsSubscribeEntity(cmd);
    }
    else if (cmd->Command()==Methods::RegisterEntityHandler)
    {
        WsRegisterEntityHandler(cmd);
    }
    else if (cmd->Command()==Methods::UnregisterHandler)
    {
        WsUnregisterHandler(cmd);
    }
    else
    {
        std::cout<<"Received command that is not supported "<<std::endl;
        auto msg=ResponseFactory::Error(Methods::GeneralError, L"Command is not supported. "+cmd->Command().GetVal(), ReqId(cmd));
        m_connection->send(msg);
    }
}

void RemoteClient::WsOpen(const ws::SendPtr& cmd)
{
    try
    {
        CommandValidator::ValidateOpen(cmd);
        auto context=cmd->Context().IsNull() ? 0 : cmd->Context().GetVal();
        m_dob.Open(cmd->ConnectionName().GetVal(), context);
        Confirm(Methods::Open, cmd);
    }
    catch (const std::exception& e)
    {
        m_connection->send(ResponseFactory::Error(Methods::Open, Wstr(e.what()), ReqId(cmd)));
    }
}

void RemoteClient::WsClose(const ws::SendPtr &cmd)
{
    try
    {
        m_dob.Close();
        Confirm(Methods::Close, cmd);
    }
    catch (const std::exception& e)
    {
        m_connection->send(ResponseFactory::Error(Methods::Close, Wstr(e.what()), ReqId(cmd)));
    }

}

void RemoteClient::WsGetTypeHierarchy(const ws::SendPtr& cmd)
{
    try
    {
        m_connection->send(Typesystem::GetTypeHierarchy());
    }
    catch (const std::exception& e)
    {
        m_connection->send(ResponseFactory::Error(Methods::GetTypeHierarchy, Wstr(e.what()), ReqId(cmd)));
    }
}

void RemoteClient::WsSubscribeMessage(const ws::SendPtr& cmd)
{
    try
    {
        CommandValidator::ValidateSubscribeMessage(cmd);
        auto channel=cmd->Channel().IsNull() ? Safir::Dob::Typesystem::ChannelId() : cmd->Channel().GetVal();
        auto includeSub=cmd->IncludeSubclasses().IsNull() ? true : cmd->IncludeSubclasses().GetVal();
        m_dob.SubscribeMessage(cmd->Type().GetVal(), channel, includeSub);
        Confirm(Methods::SubscribeMessage, cmd);
    }
    catch (const std::exception& e)
    {
        m_connection->send(ResponseFactory::Error(Methods::SubscribeMessage, Wstr(e.what()), ReqId(cmd)));
    }
}

void RemoteClient::WsSendMessage(const Safir::Websocket::SendPtr &cmd)
{
    try
    {
        CommandValidator::ValidateSendMessage(cmd);
        auto channel=cmd->Channel().IsNull() ? Safir::Dob::Typesystem::ChannelId() : cmd->Channel().GetVal();
        m_dob.SendMessage(cmd->Message().GetPtr(), channel);
        Confirm(Methods::SendMessage, cmd);
    }
    catch (const std::exception& e)
    {
        m_connection->send(ResponseFactory::Error(Methods::SendMessage, Wstr(e.what()), ReqId(cmd)));
    }

}

void RemoteClient::WsUnsubscribeMessage(const Safir::Websocket::SendPtr &cmd)
{
    try
    {
        CommandValidator::ValidateUnsubscribeMessage(cmd);
        auto channel=cmd->Channel().IsNull() ? Safir::Dob::Typesystem::ChannelId() : cmd->Channel().GetVal();
        auto inclSub = cmd->IncludeSubclasses().IsNull() ? true : cmd->IncludeSubclasses().GetVal();
        m_dob.UnsubscribeMessage(cmd->Type().GetVal(), channel, inclSub);
        Confirm(Methods::UnsubscribeMessage, cmd);
    }
    catch (const std::exception& e)
    {
        m_connection->send(ResponseFactory::Error(Methods::UnsubscribeMessage, Wstr(e.what()), ReqId(cmd)));
    }
}

void RemoteClient::WsSubscribeEntity(const Safir::Websocket::SendPtr &cmd)
{
    try
    {
        CommandValidator::ValidateSubscribeEntity(cmd);
        auto includeUpdates=cmd->IncludeUpdates().IsNull() ? true : cmd->IncludeUpdates().GetVal();
        auto restartSub=cmd->RestartSubscription().IsNull() ? true : cmd->RestartSubscription().GetVal();

        if (cmd->Instance().IsNull())
        {
            auto includeSubclasses=cmd->IncludeSubclasses().IsNull() ? true : cmd->IncludeSubclasses().GetVal();
            m_dob.SubscribeEntity(cmd->Type().GetVal(), includeUpdates, includeSubclasses, restartSub);
        }
        else
        {
            auto entityId=ts::EntityId(cmd->Type().GetVal(), cmd->Instance().GetVal());
            m_dob.SubscribeEntity(entityId, includeUpdates, restartSub);
        }

        Confirm(Methods::SubscribeEntity, cmd);
    }
    catch (const std::exception& e)
    {
        m_connection->send(ResponseFactory::Error(Methods::SubscribeEntity, Wstr(e.what()), ReqId(cmd)));
    }
}

void RemoteClient::WsUnsubscribeEntity(const Safir::Websocket::SendPtr &cmd)
{
    try
    {
        CommandValidator::ValidateUnsubscribeEntity(cmd);

        if (cmd->Instance().IsNull())
        {
            auto includeSubclasses=cmd->IncludeSubclasses().IsNull() ? true : cmd->IncludeSubclasses().GetVal();
            m_dob.UnsubscribeEntity(cmd->Type().GetVal(), includeSubclasses);
        }
        else
        {
            auto entityId=ts::EntityId(cmd->Type().GetVal(), cmd->Instance().GetVal());
            m_dob.UnsubscribeEntity(entityId);
        }

        Confirm(Methods::UnsubscribeEntity, cmd);
    }
    catch (const std::exception& e)
    {
        m_connection->send(ResponseFactory::Error(Methods::UnsubscribeEntity, Wstr(e.what()), ReqId(cmd)));
    }
}

void RemoteClient::WsRegisterEntityHandler(const Safir::Websocket::SendPtr &cmd)
{
    try
    {
        CommandValidator::ValidateRegisterEntityHandler(cmd);
        auto handler=cmd->Handler().IsNull() ? ts::HandlerId() : cmd->Handler().GetVal();
        auto instPolicy=cmd->InstancePolicy().IsNull() ? sd::InstanceIdPolicy::RequestorDecidesInstanceId : cmd->InstancePolicy().GetVal();
        auto injectionHandler=cmd->InjectionHandler().IsNull() ? false : cmd->InjectionHandler().GetVal();
        auto pendingReg=cmd->Pending().IsNull() ? false : cmd->Pending().GetVal();
        m_dob.RegisterEntity(cmd->Type().GetVal(), handler, instPolicy, injectionHandler, pendingReg);
        Confirm(Methods::SubscribeMessage, cmd);
    }
    catch (const std::exception& e)
    {
        m_connection->send(ResponseFactory::Error(Methods::RegisterEntityHandler, Wstr(e.what()), ReqId(cmd)));
    }
}

void RemoteClient::WsUnregisterHandler(const Safir::Websocket::SendPtr &cmd)
{
    try
    {
        CommandValidator::ValidateUnregisterHandler(cmd);
        auto handler=cmd->Handler().IsNull() ? ts::HandlerId() : cmd->Handler().GetVal();
        m_dob.UnregisterHandler(cmd->Type().GetVal(), handler);
        Confirm(Methods::SubscribeMessage, cmd);
    }
    catch (const std::exception& e)
    {
        m_connection->send(ResponseFactory::Error(Methods::UnregisterHandler, Wstr(e.what()), ReqId(cmd)));
    }
}
