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
#pragma once

#include <functional>
#include <boost/optional.hpp>
#include <websocketpp/config/asio_no_tls.hpp>
#include <websocketpp/server.hpp>
#include <Safir/Dob/Connection.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Websocket/Send.h>
#include <Safir/Websocket/Receive.h>
#include "ResponseFactory.h"
#include "DobConnection.h"
#include "JsonHelpers.h"

namespace sd = Safir::Dob;
namespace ts = Safir::Dob::Typesystem;

class RemoteClient
{
public:
    typedef websocketpp::server<websocketpp::config::asio> WsServer;
    typedef websocketpp::server<websocketpp::config::asio>::connection_ptr WsConnection;
    typedef websocketpp::server<websocketpp::config::asio>::message_ptr WsMessage;

    RemoteClient(WsServer& server,
                 boost::asio::io_service& ioService,
                 websocketpp::connection_hdl& connectionHandle,
                 std::function<void(const RemoteClient*)> onClose);

    void Close();

    std::string ToString() const;

private:
    WsServer& m_server;
    websocketpp::connection_hdl m_connectionHandle;
    WsConnection m_connection;
    std::function<void(const RemoteClient*)> m_onConnectionClosed;
    DobConnection m_dob;

    //websocket events
    //-----------------
    void OnClose(websocketpp::connection_hdl hdl);
    void OnMessage(websocketpp::connection_hdl hdl, WsMessage msg);

    // handle client commands
    //------------------------
    void WsDispatch(const ws::SendPtr& cmd);
    void WsOpen(const ws::SendPtr& cmd);
    void WsClose(const ws::SendPtr& cmd);
    void WsGetTypeHierarchy(const ws::SendPtr& cmd);
    void WsSubscribeMessage(const ws::SendPtr& cmd);
    void WsSendMessage(const ws::SendPtr& cmd);
    void WsUnsubscribeMessage(const ws::SendPtr& cmd);

    void WsSubscribeEntity(const ws::SendPtr& cmd);
    void WsUnsubscribeEntity(const ws::SendPtr& cmd);
    void WsRegisterEntityHandler(const ws::SendPtr& cmd);
    void WsUnregisterHandler(const ws::SendPtr& cmd);

    //---------------helpers---------------------

    inline ws::SendPtr ToCommand(const std::string& payload)
    {
        try
        {
            auto obj=ts::Serialization::ToObjectFromJson(ts::Utilities::ToWstring(payload));
            if (obj->GetTypeId()==ws::Send::ClassTypeId)
            {
                ws::SendPtr cmd=boost::dynamic_pointer_cast<ws::Send>(obj);
                return cmd;
            }
        }
        catch (...)
        {
            std::cout<<"Failed to parse JSON message. Expected a Command message."<<std::endl;
        }

        ws::SendPtr nullVal;
        return nullVal;
    }

    inline boost::optional<int> ReqId(const ws::SendPtr& c)
    {
        return c->Id().IsNull() ? boost::optional<int>() : boost::optional<int>(c->Id().GetVal());
    }

    inline std::wstring Wstr(const std::string& s) {return ts::Utilities::ToWstring(s);}
    inline std::string Str(const std::wstring& s) {return ts::Utilities::ToUtf8(s);}
    inline void Confirm(const std::wstring& command, const ws::SendPtr& cmd)
    {
        if (!cmd->Id().IsNull())
            m_connection->send(ResponseFactory::Success(command, cmd->Id().GetVal()));
    }

    //-------------------------------------------
    /**
      Open(name, context)
      Close()

      RegHandler
      RegHandlerInj
      RegHandlerPend
      UnregHandler

      SubscribeMessage(type, channel, includeSubclasses)
      UnsubMessage
      SubEnt
      UnsubEnt

      SubReg
      UnsubReg

      Request crud
      SendMessage(message, channel)

      SetAll
      SetChanges
      Delete
      DeleteAll
      Read

      //----------
      Open
      Close

      RegHandler
      UnregHandler

      Subscribe
      Unsubscribe

      Request
      SendMessage

      SetEntity
      ReadEntity

      ReadParameter


      */
};
