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
#include <signal.h>
#include <chrono>
#include <Safir/Web/Parameters.h>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include "ApiServer.h"
#include "IpAddressHelper.h"
#include "RestServer.h"
#include "DobConnectionRegistry.h"

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4100)
#endif

#include <boost/thread.hpp>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

namespace ws = Safir::Web;
namespace http = boost::beast::http;
namespace beast = boost::beast;

ApiServer::ApiServer(boost::asio::io_context& io,
                     const std::shared_ptr<DobConnectionRegistry>& dobConnectionRegistry)
    : m_acceptor(io)
    , m_io(io)
    , m_dobConnectionRegistry(dobConnectionRegistry)
    , m_connectionsStrand(m_io)
    , m_work(boost::asio::make_work_guard(m_io))
    , m_connections()
    , m_signals(m_io)
    , m_isTerminating(false)
    , m_dobConnection()
    , m_dobDispatcher(m_dobConnection, m_io)
    , m_tracer(L"ApiServer")
{
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

void ApiServer::Run()
{
    m_signals.async_wait([this](const boost::system::error_code&, int /*signal*/) { Terminate(); });

    lllog(5)<<"API: Wait for DOB to let us open a connection..."<<std::endl;
    m_dobConnection.Open(L"safir_web", L"", 0, this, &m_dobDispatcher);

    // Start tracer backdoor to be able to enable/disable tracing via DOB commands
    Safir::Application::TracerBackdoor::Start(m_dobConnection);

    std::string ip="";
    unsigned short port=0;
    if (!IpAddressHelper::SplitAddress(ts::Utilities::ToUtf8(ws::Parameters::ApiEndpoint()), ip, port))
    {
        lllog(5)<<"API: ApiEndpoint from configuration could not be parsed as a valid ip address and port. Expected format is <ip>:<port>"<<std::endl;
        SEND_SYSTEM_LOG(Error, <<"ApiEndpoint from configuration could not be parsed as a valid ip address and port. Expected format is <ip>:<port>"<<std::endl);
        std::wcout<<L"ApiEndpoint from configuration could not be parsed as a valid ip address and port. Expected format is <ip>:<port>"<<std::endl;
        return;
    }

    boost::asio::ip::tcp::endpoint serverTcpEndpoint;
    try
    {
        serverTcpEndpoint=IpAddressHelper::CreateEndpoint(ip, port);
    }
    catch (const std::exception& e)
    {
        lllog(5)<<"API: Could not create server endpoint. "<<e.what()<<std::endl;
        SEND_SYSTEM_LOG(Error, <<"Could not create server endpoint. "<<e.what()<<std::endl);
        std::wcout<<L"Could not create server endpoint. "<<e.what()<<std::endl;
        return;
    }

    boost::system::error_code ec;
    m_acceptor.open(serverTcpEndpoint.protocol(), ec);
    if (ec)
    {
        lllog(5) << "API: Could not open server acceptor. " << ec << std::endl;
        SEND_SYSTEM_LOG(Error, << "Could not open server acceptor. " << ec << std::endl);
        return;
    }

    m_acceptor.set_option(boost::asio::ip::tcp::acceptor::reuse_address(true), ec);
    if (ec)
    {
        lllog(5) << "API: Could not set reuse_address on server acceptor. " << ec << std::endl;
        SEND_SYSTEM_LOG(Error, << "Could not set reuse_address on server acceptor. " << ec << std::endl);
        return;
    }

    m_acceptor.bind(serverTcpEndpoint, ec);
    if (ec)
    {
        lllog(5) << "API: Could not bind server acceptor. " << ec << std::endl;
        SEND_SYSTEM_LOG(Error, << "Could not bind server acceptor. " << ec << std::endl);
        return;
    }

    m_acceptor.listen(boost::asio::socket_base::max_listen_connections, ec);
    if (ec)
    {
        lllog(5) << "API: Could not listen on server acceptor. " << ec << std::endl;
        SEND_SYSTEM_LOG(Error, << "Could not listen on server acceptor. " << ec << std::endl);
        return;
    }

    StartAccept();

    lllog(5)<<"API: Running server on "<<serverTcpEndpoint.address().to_string().c_str()<<":"<<serverTcpEndpoint.port()<<std::endl;
    std::wcout<<L"Running API server on "<<serverTcpEndpoint.address().to_string().c_str()<<L":"<<serverTcpEndpoint.port()<<std::endl;
    m_tracer << L"Listening on " << ts::Utilities::ToWstring(serverTcpEndpoint.address().to_string())
             << L":" << serverTcpEndpoint.port() << std::endl;
}

void ApiServer::Terminate()
{
    if (m_isTerminating)
    {
        return;
    }

    m_isTerminating = true;
    m_tracer << L"Shutting down" << std::endl;
    lllog(5)<<"API: safir_web is starting to shut down..."<<std::endl;

    boost::system::error_code signalsEc;
    m_signals.cancel(signalsEc);

    boost::system::error_code ec;
    m_acceptor.cancel(ec);
    m_acceptor.close(ec);

    //close this dob connection
    if (m_dobConnection.IsOpen())
    {
        Safir::Application::TracerBackdoor::Stop();
        m_dobConnection.Close();
    }

    boost::asio::post(m_connectionsStrand, [this]
    {
        //close all existing connections
        for (const auto& c : m_connections)
        {
            c->Close();
        }
    });

    m_work.reset();

    //give a couple of seconds to send pending messages and nice shutdown messages
    std::shared_ptr<boost::asio::steady_timer> shutDownTimer=std::make_shared<boost::asio::steady_timer>(m_io);
    shutDownTimer->expires_after(std::chrono::milliseconds(500));
    shutDownTimer->async_wait([this, shutDownTimer](const boost::system::error_code&)
    {
        lllog(5)<<"API: all connections closed..."<<std::endl;
        m_io.stop();
    });
}

void ApiServer::StartAccept()
{
    m_acceptor.async_accept([this](const boost::system::error_code& ec, boost::asio::ip::tcp::socket socket)
    {
        if (m_isTerminating)
        {
            return;
        }

        if (ec)
        {
            if (ec != boost::asio::error::operation_aborted)
            {
                lllog(5) << "API: Error while accepting connection: " << ec << std::endl;
                SEND_SYSTEM_LOG(Error, << "API: Error while accepting connection: " << ec << std::endl);
            }
        }
        else
        {
            // Read the initial HTTP request to distinguish REST from WebSocket upgrade.
            auto stream = std::make_shared<beast::tcp_stream>(std::move(socket));
            auto buffer = std::make_shared<beast::flat_buffer>();
            auto parser = std::make_shared<http::request_parser<http::string_body>>();
            parser->body_limit(5 * 1024 * 1024);
            stream->expires_after(std::chrono::seconds(30));

            http::async_read(*stream, *buffer, *parser,
                [this, stream, buffer, parser](const boost::system::error_code& readEc, std::size_t)
            {
                if (readEc)
                {
                    if (readEc != boost::asio::error::operation_aborted)
                    {
                        lllog(5) << "API: Error reading initial HTTP request: " << readEc << std::endl;
                    }
                    return;
                }

                auto request = parser->release();
                const bool isWebSocket = boost::beast::iequals(request[http::field::upgrade], "websocket");

                if (isWebSocket)
                {
                    auto con = std::make_shared<RemoteClient>(m_io, stream->release_socket(), m_dobConnectionRegistry,
                                                              [this](const RemoteClient* client) { OnConnectionClosed(client); });
                    con->Start(std::move(request), [this, con](bool started)
                    {
                        if (started)
                        {
                            OnConnectionOpen(con);
                            lllog(5) << "API: New WebSocket connection: " << con->ToString().c_str() << std::endl;
                            m_tracer << L"WebSocket connected: " << ts::Utilities::ToWstring(con->ToString()) << std::endl;
                        }
                    });
                }
                else
                {
                    if (m_tracer.IsEnabled())
                    {
                        m_tracer << L"REST " << ts::Utilities::ToWstring(std::string(request.method_string()))
                                 << L" " << ts::Utilities::ToWstring(std::string(request.target())) << std::endl;
                    }
                    StartRestSession(stream->release_socket(), std::move(request),
                                     [this](const std::string& connId) { return m_dobConnectionRegistry->GetConnection(connId); },
                                     [this]() { return m_dobConnectionRegistry->GetAllConnectionNames(); });
                }
            });
        }

        StartAccept();
    });
}

void ApiServer::OnConnectionOpen(const std::shared_ptr<RemoteClient>& con)
{
    boost::asio::post(m_connectionsStrand, [this, con]
    {
        m_connections.insert(con);
        PrintConnections();
    });
}

void ApiServer::OnConnectionClosed(const RemoteClient* con)
{
    boost::asio::post(m_connectionsStrand, [this, con]
    {
        auto it = std::find_if(m_connections.begin(), m_connections.end(),
                               [con](const std::shared_ptr<RemoteClient>& p) {return p.get() == con;});

        if (it != m_connections.end())
        {
            lllog(5)<<"API: WebSocket connection closed: "<<con->ToString().c_str()<<std::endl;
            m_tracer << L"WebSocket disconnected: " << ts::Utilities::ToWstring(con->ToString()) << std::endl;
            m_connections.erase(it);

            if (m_connections.empty())
            {
                lllog(5)<<"API: Last WebSocket connection removed"<<std::endl;
            }
        }
        else
        {
            lllog(5)<<"API: Closed connection was not found."<<std::endl;
        }
    });
}

void ApiServer::OnStopOrder()
{
    m_tracer << L"StopOrder received" << std::endl;
    lllog(5)<<"API: Got StopOrder. All connected clients will be disconnected."<<std::endl;
    Terminate();
}

void ApiServer::PrintConnections() const
{
    if (Safir::Utilities::Internal::Internal::LowLevelLogger::Instance().LogLevel() >= 5)
    {
        lllog(5)<<"API: ----- WebSocket connections -----"<<std::endl;
        for (const auto& c : m_connections)
        {
            lllog(5)<<c->ToString().c_str()<<std::endl;
        }
    }
}

