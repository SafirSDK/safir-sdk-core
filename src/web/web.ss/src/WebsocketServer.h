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


#include <set>
#include <boost/asio.hpp>
#include <boost/asio/signal_set.hpp>
#include <boost/asio/ip/tcp.hpp>
#include "RemoteClient.h"

class DobConnectionRegistry;

class WebsocketServer : public sd::StopHandler
{
public:
    WebsocketServer(boost::asio::io_context& io,
                    const std::shared_ptr<DobConnectionRegistry>& dobConnectionRegistry);

    void Run();
    void Terminate();

private:
    typedef boost::asio::ip::tcp::acceptor TcpAcceptor;
    TcpAcceptor m_acceptor;
    boost::asio::io_context& m_io;
    std::shared_ptr<DobConnectionRegistry> m_dobConnectionRegistry;
    boost::asio::io_context::strand m_connectionsStrand;
    boost::asio::executor_work_guard<boost::asio::io_context::executor_type> m_work;
    std::set<std::shared_ptr<RemoteClient> > m_connections;
    boost::asio::signal_set m_signals;
    bool m_isTerminating;

    //own DOB connection
    sd::Connection m_dobConnection;
    Safir::Utilities::AsioDispatcher m_dobDispatcher;

    void StartAccept();
    void OnConnectionOpen(const std::shared_ptr<RemoteClient>& con);
    void OnConnectionClosed(const RemoteClient* con);

    void OnStopOrder() override;

    //debug
    void PrintConnections() const;
};

