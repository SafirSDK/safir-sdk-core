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
#include <iostream>
#include <queue>
#include <atomic>
#include <sstream>
#include <thread>
#include <rapidjson/document.h>

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4005)
#pragma warning(disable: 4100)
#pragma warning(disable: 4355)
#pragma warning(disable: 4127)
#pragma warning(disable: 4267)
#pragma warning(disable: 4996)
#pragma warning(disable: 4244)
#endif

#include <boost/thread.hpp>
#include <boost/function.hpp>
#include <boost/asio/connect.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/core/flat_buffer.hpp>
#include <boost/beast/core/buffers_to_string.hpp>
#include <boost/beast/websocket.hpp>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

namespace stress_handler_net = boost::asio;
namespace stress_handler_beast = boost::beast;
namespace stress_handler_websocket = boost::beast::websocket;
namespace stress_handler_http = boost::beast::http;
using StressHandlerTcp = boost::asio::ip::tcp;

class ServiceHandler
{
public:
    ServiceHandler(const boost::function<void()>& isRegistered)
        :m_isRegistered(isRegistered)
        ,m_numHandledRequests(0)
        ,m_io()
        ,m_resolver(m_io)
        ,m_ws(m_io)
        ,m_buffer()
    {
    }

    virtual ~ServiceHandler()
    {
        Stop();

        if (m_thread.joinable())
        {
            m_thread.join();
        }
    }

    void Run()
    {
        m_isStopping = false;
        m_io.post([this]
        {
            m_resolver.async_resolve("localhost", "10000", boost::beast::bind_front_handler(&ServiceHandler::OnResolve, this));
        });

        m_thread = std::thread([this]{m_io.run();});
    }

    void Stop()
    {
        m_isStopping = true;
        m_io.stop();

        if (m_thread.joinable())
        {
            m_thread.join();
        }
    }


private:
    boost::function<void()> m_isRegistered;
    std::int64_t m_numHandledRequests;
    stress_handler_net::io_context m_io;
    StressHandlerTcp::resolver m_resolver;
    stress_handler_websocket::stream<stress_handler_beast::tcp_stream> m_ws;
    stress_handler_beast::flat_buffer m_buffer;
    std::thread m_thread;
    std::atomic<bool> m_isStopping{false};
    bool m_isRegisteredNotified = false;

    void Send(const std::string& payload)
    {
        boost::asio::post(m_io, [this, payload]
        {
            m_ws.write(stress_handler_net::buffer(payload));
        });
    }

    void OnClose()
    {
        std::cout<<"HANDLER"<<" OnClose"<<std::endl;
    }

    void OnError(const std::exception& e)
    {
        if (!m_isStopping)
        {
            std::cout<<"HANDLER"<<" ***OnError*** "<<e.what()<<std::endl;
        }
    }

    void OnMessage(const std::string& data)
    {
        rapidjson::Document doc;
        doc.Parse(data.c_str());

        if (doc.HasParseError() || !doc.IsObject())
        {
            return;
        }

        if (doc.HasMember("id") && doc["id"].IsInt())
        {
            auto id=doc["id"].GetInt();
            if (id==-123)
            {
                auto regSrvHandler = "{\"jsonrpc\":\"2.0\", \"method\":\"registerServiceHandler\", \"params\":{\"typeId\":\"Safir.Control.Command\", \"handlerId\":1}, \"id\":-100}";
                Send(regSrvHandler);
            }
            else if (id==-100)
            {
                m_isRegisteredNotified = true;
                m_isRegistered();
            }
            else if (doc.HasMember("method") && doc["method"].IsString() && doc["method"].GetString()==std::string("onServiceRequest"))
            {
                std::ostringstream os;
                os<<"{\"jsonrpc\":\"2.0\", \"result\":{\"_DouType\":\"Safir.Dob.SuccessResponse\"}, \"id\":"<<id<<"}";
                Send(os.str());
                ++m_numHandledRequests;
            }
            return;
        }
    }

    void OnResolve(boost::beast::error_code ec, StressHandlerTcp::resolver::results_type results)
    {
        if (ec)
        {
            throw boost::system::system_error(ec);
        }

        stress_handler_beast::get_lowest_layer(m_ws).expires_after(std::chrono::seconds(30));
        stress_handler_beast::get_lowest_layer(m_ws).async_connect(
            results,
            boost::beast::bind_front_handler(&ServiceHandler::OnConnect, this));
    }

    void OnConnect(boost::beast::error_code ec, StressHandlerTcp::resolver::results_type::endpoint_type)
    {
        if (ec)
        {
            throw boost::system::system_error(ec);
        }

        stress_handler_beast::get_lowest_layer(m_ws).expires_never();
        m_ws.set_option(stress_handler_websocket::stream_base::timeout::suggested(stress_handler_beast::role_type::client));
        m_ws.set_option(stress_handler_websocket::stream_base::decorator(
            [](stress_handler_websocket::request_type& req)
            {
                req.set(stress_handler_http::field::user_agent,
                        std::string(BOOST_BEAST_VERSION_STRING) + " websocket-client-async");
            }));

        m_ws.async_handshake("localhost:10000", "/", boost::beast::bind_front_handler(&ServiceHandler::OnHandshake, this));
    }

    void OnHandshake(boost::beast::error_code ec)
    {
        if (ec)
        {
            std::cout << "ServiceHandler OnHandshake Error " << ec.message()<< ec.value()<<std::endl;
            throw boost::system::system_error(ec);
        }

        std::cout << "ServiceHandler Start send " << std::endl;

        // Send open message to Dob to open the connection and get a connection id
        auto open = "{\"jsonrpc\":\"2.0\", \"method\":\"open\", \"params\":{\"connectionName\":\"testHandler\"}, \"id\":-123}";
        Send(open);

        // Start reading for responses
        m_ws.async_read(m_buffer, boost::beast::bind_front_handler(&ServiceHandler::OnRead, this));
    }

    void OnRead(boost::beast::error_code ec, std::size_t)
    {
        if (ec)
        {
            if (ec == stress_handler_websocket::error::closed
                || ec == boost::asio::error::operation_aborted
                || ec == boost::asio::error::eof)
            {
                OnClose();
                return;
            }

            throw boost::system::system_error(ec);
        }

        auto payload = stress_handler_beast::buffers_to_string(m_buffer.data());
        m_buffer.consume(m_buffer.size());

        try
        {
            OnMessage(payload);
        }
        catch (const std::exception& e)
        {
            OnError(e);
            return;
        }

        if (!m_isStopping)
        {
            m_ws.async_read(m_buffer, boost::beast::bind_front_handler(&ServiceHandler::OnRead, this));
        }
    }

};
