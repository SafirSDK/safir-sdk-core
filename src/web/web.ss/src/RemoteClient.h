/******************************************************************************
*
* Copyright Saab AB, 2026 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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

#include <deque>
#include <functional>
#include <memory>
#include <string>
#include <boost/asio/ip/tcp.hpp>
#include <boost/beast/websocket.hpp>
#include <boost/beast/core/flat_buffer.hpp>
#include <boost/beast/http.hpp>
#include <Safir/Dob/Connection.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include <Safir/Dob/Typesystem/Internal/InternalOperations.h>
#include "PingHandler.h"
#include "DobConnection.h"
#include "JsonRpcRequest.h"
#include "JsonRpcResponse.h"

class DobConnectionRegistry;

namespace sd = Safir::Dob;
namespace ts = Safir::Dob::Typesystem;

class RemoteClient : public std::enable_shared_from_this<RemoteClient>
{
public:
    typedef boost::asio::ip::tcp::socket TcpSocket;
    typedef boost::beast::websocket::stream<TcpSocket> WsStream;

    RemoteClient(boost::asio::io_context& io,
                 TcpSocket socket,
                 std::shared_ptr<DobConnectionRegistry> dobConnectionRegistry,
                 std::function<void(const RemoteClient*)> onClose);

    void Start(boost::beast::http::request<boost::beast::http::string_body> handshakeRequest,
               std::function<void(bool)> onStarted);

    void Close();

    std::string ToString() const;

private:
    // no copy
    RemoteClient(const RemoteClient&) = delete;
    RemoteClient& operator=(const RemoteClient&) = delete;

    WsStream m_stream;
    std::shared_ptr<boost::asio::io_context::strand> m_strand;
    std::shared_ptr<DobConnectionRegistry> m_dobConnectionRegistry;
    std::function<void(const RemoteClient*)> m_onConnectionClosed;
    std::shared_ptr<DobConnection> m_dobConnection;
    std::shared_ptr<PingHandler> m_pingHandler;
    std::string m_connectionName;
    bool m_enableTypeSystem;
    boost::beast::flat_buffer m_readBuffer;
    std::deque<std::string> m_writeQueue;
    bool m_isWriting;
    bool m_isClosed;
    bool m_isOpened;
    
    void SendToClient(const std::string& msg);
    void SendPing();
    void NotifyClosed();
    void LogError(const char* context, const boost::system::error_code& ec);
    void DoRead();
    void DoWrite();
    void CloseInternal();
    void RemoveDobConnectionFromRegistry();

    // handle client commands
    //------------------------
    void WsDispatch(const JsonRpcRequest& req);
    void WsResponse(const JsonRpcRequest& req);
    void WsPing(const JsonRpcRequest& req);
    void WsOpen(const JsonRpcRequest& req);
    void WsClose(const JsonRpcRequest& req);
    void WsIsOpen(const JsonRpcRequest& req);
    void WsGetTypeHierarchy(const JsonRpcRequest& req);
    void WsGetVersion(const JsonRpcRequest& req);
    void WsSubscribeMessage(const JsonRpcRequest& req);
    void WsSendMessage(const JsonRpcRequest& req);
    void WsUnsubscribeMessage(const JsonRpcRequest& req);
    void WsSubscribeEntity(const JsonRpcRequest& req);
    void WsUnsubscribeEntity(const JsonRpcRequest& req);
    void WsRegisterEntityHandler(const JsonRpcRequest& req);
    void WsRegisterServiceHandler(const JsonRpcRequest& req);
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
};
