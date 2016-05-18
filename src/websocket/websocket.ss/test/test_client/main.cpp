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
#include <queue>
#include <websocketpp/config/asio_no_tls_client.hpp>
#include <websocketpp/client.hpp>

typedef websocketpp::client<websocketpp::config::asio_client> client;
typedef websocketpp::config::asio_client::message_type::ptr message_ptr;

typedef std::pair<std::string, std::string> QueueItem;

int main(int argc, char* argv[]) {

    // Requests
    //----------------------------------------------------------
    //    {\"jsonrpc\":\"2.0\", \"method\":\"getTypeHierarchy\", \"id\":\"joot\"}
    //    {\"jsonrpc\":\"2.0\", \"method\":\"isOpen", \"id\":\"joot\"}
    //    {\"jsonrpc\":\"2.0\", \"method\":\"open\", \"params\":{\"connectionName\":\"test\"}, \"id\":\"joot\"}
    //    {\"jsonrpc\":\"2.0\", \"method\":\"close", \"id\":\"joot\"}
    //    {\"jsonrpc\":\"2.0\", \"method\":\"subscribeMessage", \"params\":{\"typeId":\"Safir.Application.BackdoorCommand\"}, \"id\":\"joot\"}
    //    {\"jsonrpc\":\"2.0\", \"method\":\"sendMessage", \"params\":{\"message\":{\"_DouType\": \"Safir.Application.BackdoorCommand\", \"NodeName\": \"Hello\", \"Command\": \"World\"}}, \"id\":\"joot\"}


    // Responses
    //----------------------------------------------------------
    //    {\"jsonrpc\":\"2.0\",\"result\":true,\"id\":\"joot\"}
    //    {\"jsonrpc\":\"2.0\",\"result\":false,\"id\":\"joot\"}
    //    {\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":\"joot\"}

    // Notifications
    //----------------------------------------------------------
    //  {\"jsonrpc\":\"2.0\",\"method\":\"onMessage\",\"params\":{\"channelId\":1,\"message\":{\"_DouType\": \"Safir.Application.BackdoorCommand\", \"NodeName\": \"Hello\", \"Command\": \"World\"}}}

    std::queue<QueueItem> items;
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"isOpen\", \"id\":\"aaa\"}", "{\"jsonrpc\":\"2.0\",\"result\":false,\"id\":\"aaa\"}"));
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"open\", \"params\":{\"connectionName\":\"test\"}, \"id\":\"bbb\"}", "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":\"bbb\"}"));
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"isOpen\", \"id\":\"ccc\"}", "{\"jsonrpc\":\"2.0\",\"result\":true,\"id\":\"ccc\"}"));
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"ping\", \"id\":\"bbb\"}", "{\"jsonrpc\":\"2.0\",\"result\":\"pong\",\"id\":\"bbb\"}"));
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"close\", \"id\":\"ddd\"}", "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":\"ddd\"}"));
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"isOpen\", \"id\":1}", "{\"jsonrpc\":\"2.0\",\"result\":false,\"id\":1}"));
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"open\", \"params\":{\"connectionName\":\"test\"}, \"id\":2}", "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":2}"));
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"subscribeMessage\", \"params\":{\"channelId\":1,\"typeId\":\"Safir.Application.BackdoorCommand\"}, \"id\":3}", "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":3}"));
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"subscribeMessage\", \"params\":{\"channelId\":2,\"typeId\":\"Safir.Application.BackdoorCommand\"}, \"id\":4}", "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":4}"));
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"sendMessage\", \"params\":{\"channelId\":1,\"message\":{\"_DouType\":\"Safir.Application.BackdoorCommand\",\"NodeName\":\"Hello\",\"Command\":\"World\"}},\"id\":5}", "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":5}"));
    items.push(QueueItem("", "{\"jsonrpc\":\"2.0\",\"method\":\"onMessage\",\"params\":{\"channelId\":1,\"message\":{\"_DouType\":\"Safir.Application.BackdoorCommand\",\"NodeName\":\"Hello\",\"Command\":\"World\"}}}"));
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"sendMessage\", \"params\":{\"channelId\":2,\"message\":{\"_DouType\":\"Safir.Application.BackdoorCommand\",\"NodeName\":\"Hello\",\"Command\":\"World\"}},\"id\":5}", "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":5}"));
    items.push(QueueItem("", "{\"jsonrpc\":\"2.0\",\"method\":\"onMessage\",\"params\":{\"channelId\":2,\"message\":{\"_DouType\":\"Safir.Application.BackdoorCommand\",\"NodeName\":\"Hello\",\"Command\":\"World\"}}}"));
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"unsubscribeMessage\", \"params\":{\"channelId\":1,\"typeId\":\"Safir.Application.BackdoorCommand\"}, \"id\":3}", "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":3}"));
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"sendMessage\", \"params\":{\"channelId\":1,\"message\":{\"_DouType\":\"Safir.Application.BackdoorCommand\",\"NodeName\":\"Hello\",\"Command\":\"World\"}},\"id\":5}", "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":5}"));
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"sendMessage\", \"params\":{\"channelId\":2,\"message\":{\"_DouType\":\"Safir.Application.BackdoorCommand\",\"NodeName\":\"Hello\",\"Command\":\"World\"}},\"id\":5}", "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":5}"));
    items.push(QueueItem("", "{\"jsonrpc\":\"2.0\",\"method\":\"onMessage\",\"params\":{\"channelId\":2,\"message\":{\"_DouType\":\"Safir.Application.BackdoorCommand\",\"NodeName\":\"Hello\",\"Command\":\"World\"}}}"));


    std::cout<<"Starting client..."<<std::endl;
    // Create a client endpoint
    client c;

    std::string uri = "ws://localhost:10000";

    try {
        // Set logging to be pretty verbose (everything except message payloads)
        c.set_access_channels(websocketpp::log::alevel::none);

        // Initialize ASIO
        c.init_asio();

        client::connection_ptr con;

        c.set_open_handler([&](websocketpp::connection_hdl hdl)
        {
            //we are connected, send first message to get started
            auto& data=items.front().first;
            std::cout<<"--> "<<data<<std::endl;
            c.send(hdl, data, websocketpp::frame::opcode::text);
        });

        // Register our message handler
        c.set_message_handler([&](websocketpp::connection_hdl hdl, message_ptr msg)
        {
            std::string data = msg->get_payload();
            std::cout<<"<-- "<<data<<std::endl;
            if (data!=items.front().second)
            {
                std::cout<<"Received unexpected data. I exptected:\n"<<items.front().second<<std::endl;
                exit(1);
            }
            items.pop();
            if (!items.empty())
            {
                auto& data=items.front().first;
                if (!data.empty())
                {
                    std::cout<<"--> "<<data<<std::endl;
                    c.send(hdl, data, websocketpp::frame::opcode::text);
                }
            }
            else
            {
               con->close(websocketpp::close::status::normal, "finished");
            }
        });

        c.set_close_handler([&](websocketpp::connection_hdl hdl)
        {

        });

        websocketpp::lib::error_code ec;
        con = c.get_connection(uri, ec);
        if (ec) {
            std::cout << "could not create connection because: " << ec.message() << std::endl;
            return 0;
        }

        // Note that connect here only requests a connection. No network messages are
        // exchanged until the event loop starts running in the next line.
        c.connect(con);

        // Start the ASIO io_service run loop
        // this will cause a single connection to be made to the server. c.run()
        // will exit when this connection is closed.
        c.run();
    } catch (websocketpp::exception const & e) {
        std::cout << e.what() << std::endl;
    }

    std::cout<<"Test passed!"<<std::endl;
}
