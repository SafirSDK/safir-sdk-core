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


#include <set>
#include <boost/asio.hpp>
#include <boost/asio/signal_set.hpp>
#include "RemoteClient.h"

//websocketpp stuff is already included in RemoteClient.h, and to avoid duplicating
//all the msvc warning stuff we depend on that.

class WebsocketServer : public sd::StopHandler
{
public:
    WebsocketServer(boost::asio::io_context& io);

    void Run();
    void Terminate();

private:
    typedef websocketpp::server<websocketpp::config::asio> WsServer;
    typedef WsServer::connection_ptr WsConnection;
    WsServer m_server;
    boost::asio::io_context& m_io;
    boost::asio::io_context::strand m_connectionsStrand;
    boost::asio::executor_work_guard<boost::asio::io_context::executor_type> m_work;
    std::set<std::shared_ptr<RemoteClient> > m_connections;
    boost::asio::signal_set m_signals;

    //own DOB connection
    sd::Connection m_dobConnection;
    Safir::Utilities::AsioDispatcher m_dobDispatcher;

    void OnConnectionOpen(const std::shared_ptr<RemoteClient>& con);
    void OnConnectionClosed(const RemoteClient* con);

    void OnStopOrder() override;

    //debug
    void PrintConnections() const;
};

