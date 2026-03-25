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
#include <sstream>
#include <rapidjson/document.h>
#include <thread>

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

namespace beast = boost::beast;         // from <boost/beast.hpp>
namespace http = beast::http;           // from <boost/beast/http.hpp>
namespace websocket = beast::websocket; // from <boost/beast/websocket.hpp>
namespace net = boost::asio;            // from <boost/asio.hpp>
using tcp = boost::asio::ip::tcp;       // from <boost/asio/ip/tcp.hpp>

class ServiceUser : public std::enable_shared_from_this<ServiceUser>
{
public:
    ServiceUser(int id, int numToSend, boost::function<void(int,bool)> done)
        :m_id(id)
        ,m_numToSend(numToSend)
        ,m_done(done)
        ,m_lastSentId(0)
        ,m_io()
        ,m_resolver(m_io)
        ,m_ws(m_io)
        ,m_buffer()
    {
    }

    virtual ~ServiceUser()
    {
    }

    void Stop()
    {
        m_io.stop();
        if (m_thread.joinable())
        {
            m_thread.join();
        }
    }

    void Run()
    {
        auto self=shared_from_this();
        m_io.post([this, self]{ m_resolver.async_resolve("localhost", "10000", beast::bind_front_handler(&ServiceUser::OnResolve, shared_from_this()));});
        m_thread = std::thread([this]{m_io.run();});
    }

    void OnResolve(beast::error_code ec, tcp::resolver::results_type results)
    {
        if(ec)
        {
            std::cout<<"USER_"<<m_id<<" *** OnResolve Error ***"<<std::endl;
            exit(1);
        }

        // Set the timeout for the operation
        beast::get_lowest_layer(m_ws).expires_after(std::chrono::seconds(30));

        // Make the connection on the IP address we get from a lookup
        beast::get_lowest_layer(m_ws).async_connect(
            results,
            beast::bind_front_handler(&ServiceUser::OnConnect, shared_from_this()));
    }

    void OnConnect(beast::error_code ec, tcp::resolver::results_type::endpoint_type)
    {
        if(ec)
        {
            std::cout<<"USER_"<<m_id<<" *** OnConnect Error ***"<<std::endl;
            exit(1);
        }

        // Turn off the timeout on the tcp_stream, because
        // the websocket stream has its own timeout system.
        beast::get_lowest_layer(m_ws).expires_never();

        // Set suggested timeout settings for the websocket
        m_ws.set_option(websocket::stream_base::timeout::suggested(beast::role_type::client));

        // Set a decorator to change the User-Agent of the handshake
        m_ws.set_option(websocket::stream_base::decorator(
            [](websocket::request_type& req)
            {
                req.set(http::field::user_agent,
                    std::string(BOOST_BEAST_VERSION_STRING) +
                        " websocket-client-async");
            }));

        // Update the host_ string. This will provide the value of the
        // Host HTTP header during the WebSocket handshake.
        // See https://tools.ietf.org/html/rfc7230#section-5.4
        std::string host = "localhost:10000";
        
        // Perform the websocket handshake
        m_ws.async_handshake(host, "/",
            beast::bind_front_handler(
                &ServiceUser::OnHandshake,
                shared_from_this()));
    }

    void OnHandshake(beast::error_code ec)
    {
        if (ec)
        {
            std::cout<<"USER_"<<m_id<<" *** OnHandshake Error ***"<<std::endl;
            exit(1);
        }
        
        // Send the message open message to Dob
        std::ostringstream os;
        os<<"{\"jsonrpc\":\"2.0\", \"method\":\"open\", \"params\":{\"connectionName\":\"testUser_"<<m_id<<"\"}, \"id\":-123}";
        m_ws.async_write(
            net::buffer(os.str()),
            beast::bind_front_handler(
                &ServiceUser::OnWrite,
                shared_from_this()));
    }

    void OnWrite(
        beast::error_code ec,
        std::size_t bytes_transferred)
    {
        boost::ignore_unused(bytes_transferred);

        if(ec)
        {
            std::cout<<"USER_"<<m_id<<" *** OnWrite Error ***"<<std::endl;
            std::cout << ec.message() << std::endl;
            exit(1);
        }
        
        // Read a message into our buffer
        m_ws.async_read(
            m_buffer,
            beast::bind_front_handler(
                &ServiceUser::OnRead,
                shared_from_this()));
    }

    void OnRead(
        beast::error_code ec,
        std::size_t bytes_transferred)
    {
        boost::ignore_unused(bytes_transferred);

        if(ec)
        {
            std::cout<<"USER_"<<m_id<<" *** OnRead Error ***"<<std::endl;
            std::cout << ec.message() << std::endl;
            exit(1);
        }

        auto payload = boost::beast::buffers_to_string(m_buffer.data());
        m_buffer.consume(m_buffer.size());

        if (OnMessage(payload))
        {
            m_done(m_id, false);

            // Close the WebSocket connection
            m_ws.async_close(websocket::close_code::normal,
            beast::bind_front_handler(
                &ServiceUser::OnClose,
                shared_from_this()));
        }
    }

    void OnClose(beast::error_code ec)
    {
        if(ec)
        {
            std::cout<<"USER_"<<m_id<<" *** OnClose Error ***"<<std::endl;
            std::cout << ec.message() << std::endl;
            exit(1);
        }
    }
    
private:
    int m_id;
    int m_numToSend;
    boost::function<void(int, bool)> m_done;
    int m_lastSentId;
    boost::asio::io_context m_io;
    tcp::resolver m_resolver;
    websocket::stream<beast::tcp_stream> m_ws;
    beast::flat_buffer m_buffer;
    std::thread m_thread;

    void SendReq()
    {
        ++m_lastSentId;
        std::ostringstream os;
        os<<"{\"jsonrpc\":\"2.0\", \"method\":\"serviceRequest\", \"params\":{\"handlerId\":1,\"request\":{\"_DouType\":\"Safir.Control.Command\",\"NodeId\":"<<m_id<<"}}, \"id\":"<<m_lastSentId<<"}";
        m_ws.async_write(
            net::buffer(os.str()),
            beast::bind_front_handler(
                &ServiceUser::OnWrite,
                shared_from_this()));
    }

    // Returns true if the user is done and connection can be closed, false if more messages are expected
    bool OnMessage(const std::string& data)
    {
        rapidjson::Document doc;
        doc.Parse(data.c_str());

        if (doc.HasParseError())
        {
            std::cout<<"USER_"<<m_id<<" Got invalid JSON payload"<<std::endl;
            return false;
        }

        if (!doc.IsObject())
        {
            return false;
        }

        if (!doc.HasMember("id") || !doc["id"].IsInt())
        {
            return false;
        }

        int id=doc["id"].GetInt();

        // This is the response to the open message. Just send the service request and ignore the id
        if (id==-123)
        {
            SendReq();
            return false;
        }

        if (id!=m_lastSentId)
        {
            std::cout<<"USER_"<<m_id<<" Got wrong response id. Got "<<id<<", expected "<<m_lastSentId<<std::endl;
            exit(1);
        }

        if (m_lastSentId<m_numToSend)
        {
            SendReq();
        }
        else
        {
            return true;
        }

        return false;
    }
};
