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
#include <signal.h>
#include <Safir/Websocket/Parameters.h>
#include "WebsocketServer.h"
#include "IpAddressHelper.h"

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4100)
#endif

#include <boost/thread.hpp>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

namespace ws = Safir::Websocket;

WebsocketServer::WebsocketServer(boost::asio::io_context& io)
    :m_server()
    ,m_io(io)
    ,m_connectionsStrand(m_io)
    ,m_work(boost::asio::make_work_guard(m_io))
    ,m_connections()
    ,m_signals(m_io)
    ,m_dobConnection()
    ,m_dobDispatcher(m_dobConnection, m_io)
{
    m_server.clear_access_channels(websocketpp::log::alevel::all);
    m_server.clear_error_channels(websocketpp::log::alevel::all);
#if defined (_WIN32)
    m_signals.add(SIGABRT);
    m_signals.add(SIGBREAK);
    m_signals.add(SIGINT);
    m_signals.add(SIGTERM);
#else
    m_signals.add(SIGQUIT);
    m_signals.add(SIGINT);
    m_signals.add(SIGTERM);
#endif
}

void WebsocketServer::Run()
{
    m_signals.async_wait([this](const boost::system::error_code&, int /*signal*/){Terminate();});

    lllog(5)<<"WS: Wait for DOB to let us open a connection..."<<std::endl;
    m_dobConnection.Open(L"safir_websocket", L"", 0, this, &m_dobDispatcher);

    // Initialize ASIO
    m_server.init_asio(&m_io);

    //try to disable all logging from websocketpp, seems like info is still logging
    m_server.set_access_channels(websocketpp::log::alevel::none);
    m_server.set_reuse_addr(true);

    m_server.set_open_handler([this](websocketpp::connection_hdl hdl)
    {
        auto con=std::make_shared<RemoteClient>(m_server, m_io, hdl, [this](const RemoteClient* con){OnConnectionClosed(con);});
        OnConnectionOpen(con);
        lllog(5)<<"WS: Server: new connection added: "<<con->ToString().c_str()<<std::endl;
    });

    std::string ip="";
    unsigned short port=0;
    if (!IpAddressHelper::SplitAddress(ts::Utilities::ToUtf8(ws::Parameters::ServerEndpoint()), ip, port))
    {
        lllog(5)<<"WS: ServerEndpoint from configuration could not be parsed as a valid ip address and port. Expected format is <ip>:<port>"<<std::endl;
        SEND_SYSTEM_LOG(Error, <<"ServerEndpoint from configuration could not be parsed as a valid ip address and port. Expected format is <ip>:<port>"<<std::endl);
        std::wcout<<L"ServerEndpoint from configuration could not be parsed as a valid ip address and port. Expected format is <ip>:<port>"<<std::endl;
        return;
    }

    boost::asio::ip::tcp::endpoint serverTcpEndpoint;
    try
    {
        serverTcpEndpoint=IpAddressHelper::CreateEndpoint(ip, port);
    }
    catch (const std::exception& e)
    {
        lllog(5)<<"WS: Could not create server endpoint. "<<e.what()<<std::endl;
        SEND_SYSTEM_LOG(Error, <<"Could not create server endpoint. "<<e.what()<<std::endl);
        std::wcout<<L"Could not create server endpoint. "<<e.what()<<std::endl;
        return;
    }

    m_server.listen(serverTcpEndpoint);

    // Start the server accept loop
    m_server.start_accept();

    lllog(5)<<"WS: Running ws server on "<<serverTcpEndpoint.address().to_string().c_str()<<":"<<serverTcpEndpoint.port()<<std::endl;
    std::wcout<<L"Running ws server on "<<serverTcpEndpoint.address().to_string().c_str()<<L":"<<serverTcpEndpoint.port()<<std::endl;
}

void WebsocketServer::Terminate()
{
    lllog(5)<<"WS: safir_websocket is starting to shut down..."<<std::endl;
    //stop accepting new connections
    m_server.stop_listening();

    //close this dob connection
    if (m_dobConnection.IsOpen())
    {
        m_dobConnection.Close();
    }

    boost::asio::post(m_connectionsStrand, [this]
    {
        //close all existing connections
        for (auto it = m_connections.begin(); it != m_connections.end(); ++it)
        {
            (*it)->Close();
        }
    });

    m_work.reset();

    //give a couple of seconds to send pending messages and nice shutdown messages
    std::shared_ptr<boost::asio::steady_timer> shutDownTimer=std::make_shared<boost::asio::steady_timer>(m_io);
    shutDownTimer->expires_after(std::chrono::seconds(3));
    shutDownTimer->async_wait([this, shutDownTimer](const boost::system::error_code&)
    {
        boost::asio::post(m_connectionsStrand, [this]{m_server.stop();});
    });

    lllog(5)<<"WS: all connections closed..."<<std::endl;
}

void WebsocketServer::OnConnectionOpen(const std::shared_ptr<RemoteClient>& con)
{
    boost::asio::post(m_connectionsStrand, [this, con]
    {
        m_connections.insert(con);
        PrintConnections();
    });
}

void WebsocketServer::OnConnectionClosed(const RemoteClient* con)
{
    //vs2010 cannot handle nested lambdas, so we need to create the find_if predicate here.
    std::function<bool(const std::shared_ptr<RemoteClient>&)> pred =
        [con](const std::shared_ptr<RemoteClient>& p) {return p.get()==con;};

    boost::asio::post(m_connectionsStrand, [this, pred, con]
    {
        auto it=std::find_if(m_connections.begin(), m_connections.end(), pred);

        if (it!=m_connections.end())
        {
            lllog(5)<<"WS: Connection closed: "<<con->ToString().c_str()<<std::endl;
            m_connections.erase(it);

            if (m_connections.empty())
            {
                lllog(5)<<"WS: Last connection removed"<<std::endl;
            }
        }
        else
        {
            lllog(5)<<"WS: Closed connection was not found."<<std::endl;
        }

    });
}

void WebsocketServer::OnStopOrder()
{
    lllog(5)<<"WS: WebsocketServer got StopOrder. All connected client will be disconnected."<<std::endl;
    Terminate();
}

void WebsocketServer::PrintConnections() const
{
    if (Safir::Utilities::Internal::Internal::LowLevelLogger::Instance().LogLevel() >= 5)
    {
        lllog(5)<<"WS: ----- Connections -----"<<std::endl;
        for (auto it = m_connections.begin(); it != m_connections.end(); ++it)
        {
            lllog(5)<<(*it)->ToString().c_str()<<std::endl;
        }
    }
}

