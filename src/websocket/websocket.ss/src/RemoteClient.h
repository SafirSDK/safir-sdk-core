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
#include <Safir/Dob/Typesystem/Internal/InternalOperations.h>
#include <Safir/Websocket/Send.h>
#include <Safir/Websocket/Receive.h>
#include "JsonRpcRequest.h"
#include "JsonRpcResponse.h"
#include "ResponseFactory.h"
#include "DobConnection.h"
#include "JsonHelpers.h"
#include "PingHandler.h"

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
    PingHandler m_pingHandler;

    inline void SendToClient(const std::string& msg)
    {
        m_connection->send(msg);
        m_pingHandler.Update();
    }

    //websocket events
    //-----------------
    void OnClose(websocketpp::connection_hdl hdl);
    void OnMessage(websocketpp::connection_hdl hdl, WsMessage msg);

    // handle client commands
    //------------------------
    void WsDispatch(const JsonRpcRequest& req);
    void WsResponse(const JsonRpcRequest& req);
    void WsPing(const JsonRpcRequest& req);
    void WsOpen(const JsonRpcRequest& req);
    void WsClose(const JsonRpcRequest& req);
    void WsIsOpen(const JsonRpcRequest& req);
    void WsGetTypeHierarchy(const JsonRpcRequest& req);
    void WsSubscribeMessage(const JsonRpcRequest& req);
    void WsSendMessage(const JsonRpcRequest& req);
    void WsUnsubscribeMessage(const JsonRpcRequest& req);
    void WsSubscribeEntity(const JsonRpcRequest& req);
    void WsUnsubscribeEntity(const JsonRpcRequest& req);
    void WsRegisterEntityHandler(const JsonRpcRequest& req);
    void WsUnregisterHandler(const JsonRpcRequest& req);

    void WsSubscribeRegistration(const JsonRpcRequest& req);
    void WsUnsubscribeRegistration(const JsonRpcRequest& req);
    void WsCreateRequest(const JsonRpcRequest& req);
    void WsUpdateRequest(const JsonRpcRequest& req);
    void WsDeleteRequest(const JsonRpcRequest& req);
    void WsServiceRequest(const JsonRpcRequest& req);
    void WsSetEntityChanges(const JsonRpcRequest& req);
    void WsSetEntity(const JsonRpcRequest& req);
    void WsDeleteEntity(const JsonRpcRequest& req);
    void WsDeleteAllInstances(const JsonRpcRequest& req);
    void WsReadEntity(const JsonRpcRequest& req);
    void WsIsCreated(const JsonRpcRequest& req);
    void WsGetNumberOfInstances(const JsonRpcRequest& req);
    void WsGetAllInstanceIds(const JsonRpcRequest& req);
    void WsGetInstanceIdPolicy(const JsonRpcRequest& req);

    //---------------helpers--------------------

    inline std::wstring Wstr(const std::string& s) const {return ts::Utilities::ToWstring(s);}
    inline std::string Str(const std::wstring& s) const {return ts::Utilities::ToUtf8(s);}

    template <class T>
    boost::shared_ptr<T> ToObject(const std::string& json) const
    {
        try
        {
            auto obj=ts::Internal::ToObjectFromJson(json);
            auto ptr=boost::dynamic_pointer_cast<T>(obj);
            if (ptr)
            {
                return ptr;
            }
        }
        catch (...)
        {
            throw std::invalid_argument("The JSON serialized Safir.Dob.Object (Entity/Message/Service/Response) could not be parsed.");
        }

        throw std::invalid_argument("The JSON serialized Safir.Dob.Object is not of correct type (Entity/Message/Service/Response).");
    }
};
