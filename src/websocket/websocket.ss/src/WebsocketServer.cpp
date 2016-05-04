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
#include <iostream>
#include <signal.h>
#include <boost/make_shared.hpp>
#include <boost/thread.hpp>
#include <Safir/Websocket/Parameters.h>
#include "WebsocketServer.h"

WebsocketServer::WebsocketServer(boost::asio::io_service& ioService)
    :m_server()
    ,m_ioService(ioService)
    ,m_work(new boost::asio::io_service::work(m_ioService))
    ,m_connections()
    //,m_signals(m_ioService, SIGINT, SIGABRT, SIGTERM)
    ,m_dobConnection()
    ,m_dobDispatcher(m_dobConnection, m_ioService)
{
    m_server.clear_access_channels(websocketpp::log::alevel::frame_header | websocketpp::log::alevel::frame_payload);
    m_server.clear_access_channels(websocketpp::log::alevel::all);
}

void WebsocketServer::Run()
{
    std::cout<<"Starting ws server"<<std::endl;

    //m_signals.async_wait([=](const boost::system::error_code&, int signal){});

//    std::cout<<"Wait for DOB to let us open a connection..."<<std::endl;
//    m_dobConnection.Open(L"safir_websocket", L"", 0, this, &m_dobDispatcher);
//    std::cout<<"done!"<<std::endl;

    // Initialize ASIO
    m_server.init_asio(&m_ioService);

    m_server.set_open_handler([=](websocketpp::connection_hdl hdl)
    {
        std::cout<<"Server: new connection added"<<std::endl;
        auto con=boost::make_shared<RemoteClient>(m_server, m_ioService, hdl, [=](const RemoteClient* con){OnConnectionClosed(con);});
        m_connections.insert(con);
        std::cout<<"new connection: "<<con->ToString()<<std::endl;
        PrintConnections();
    });

    //Set up listening port
    m_server.listen(ws::Parameters::Port());

    // Start the server accept loop
    m_server.start_accept();

    std::cout<<"Running ws server on port "<<ws::Parameters::Port()<<std::endl;

}

void WebsocketServer::Terminate()
{

}

void WebsocketServer::OnConnectionClosed(const RemoteClient* con)
{
    auto it=std::find_if(m_connections.begin(), m_connections.end(), [&](const boost::shared_ptr<RemoteClient>& p){return p.get()==con;});

    if (it!=m_connections.end())
    {
        std::cout<<"Connection closed: "<<con->ToString()<<std::endl;
        m_connections.erase(it);
    }
    else
    {
        std::cout<<"Server closed con not found"<<std::endl;
    }

    PrintConnections();
}

void WebsocketServer::OnStopOrder()
{
    m_work.reset();
    m_server.stop_listening();
    for (auto& con : m_connections)
    {
        con->Close();
    }

}

void WebsocketServer::PrintConnections() const
{
    std::cout<<"Connections\n-----------"<<std::endl;
    for (auto con : m_connections)
    {
        std::cout<<con->ToString()<<std::endl;
    }
}

