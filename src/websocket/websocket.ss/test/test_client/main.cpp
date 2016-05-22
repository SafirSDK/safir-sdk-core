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

struct QueueItem
{
    QueueItem(const std::string& req) : request(req)
    {
    }

    QueueItem(const std::string& req, const std::string& res) : request(req), response(res)
    {
    }

    QueueItem(const std::string& req, const std::string& res, const std::string& n) : request(req), response(res), notification(n)
    {
    }

    std::string request;
    std::string response;
    std::string notification;
};

int main(int argc, char* argv[]) {

    // Requests
    //----------------------------------------------------------
    //    {\"jsonrpc\":\"2.0\", \"method\":\"getTypeHierarchy\", \"id\":\"joot\"}
    //    {\"jsonrpc\":\"2.0\", \"method\":\"isOpen", \"id\":\"joot\"}
    //    {\"jsonrpc\":\"2.0\", \"method\":\"open\", \"params\":{\"connectionName\":\"test\"}, \"id\":\"joot\"}  -- context
    //    {\"jsonrpc\":\"2.0\", \"method\":\"close", \"id\":\"joot\"}
    //    {\"jsonrpc\":\"2.0\", \"method\":\"subscribeMessage", \"params\":{\"typeId":\"Safir.Application.BackdoorCommand\"}, \"id\":\"joot\"}  --channelId
    //    {\"jsonrpc\":\"2.0\", \"method\":\"sendMessage", \"params\":{\"message\":{\"_DouType\": \"Safir.Application.BackdoorCommand\", \"NodeName\": \"Hello\", \"Command\": \"World\"}}, \"id\":\"joot\"} --channelId
    //    {\"jsonrpc\":\"2.0\", \"method\":\"registerEntity\", \"params\":{\"typeId\":\"Safir.Control.Status\", \"handlerId\":1}, \"id\":\"joot\"}  --handlerId, policy, injection, pending
    //    {\"jsonrpc\":\"2.0\", \"method\":\"subscribeEntity\", \"params\":{\"typeId\":\"Safir.Control.Status\", \"instanceId\":1}, \"id\":\"joot\"}  --instanceId, includeUpdates, includeSubclasses, restartSubscription
    //    {\"jsonrpc\":\"2.0\", \"method\":\"setEntity\", \"params\":{\"instanceId\":1,\"entity\":{\"_DouType\":\"Safir.Dob.ProcessInfo\",\"Name\":\"Dilbert\",\"Pid\":123,\"ConnectionNames\":[\"Wally\",\"Asok\"]}, \"handlerId\":1}, \"id\":\"setEnt\"}  --handler
    //    {\"jsonrpc\":\"2.0\", \"method\":\"setEntityChanges\", \"params\":{\"instanceId\":1,\"entity\":{\"_DouType\":\"Safir.Dob.ProcessInfo\",\"Name\":\"Dogbert\",\"Pid\":123,\"ConnectionNames\":[\"Wally\",\"Asok\"]}, \"handlerId\":1}, \"id\":\"setEntChanges\"}  --handler
    //    {\"jsonrpc\":\"2.0\", \"method\":\"deleteEntity\", \"params\":{\"instanceId\":1,\"typeId\":\"Safir.Dob.ProcessInfo\"}, \"handlerId\":1}, \"id\":\"delEnt\"}  --handler

    // Responses
    //----------------------------------------------------------
    //    {\"jsonrpc\":\"2.0\",\"result\":true,\"id\":\"joot\"}
    //    {\"jsonrpc\":\"2.0\",\"result\":false,\"id\":\"joot\"}
    //    {\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":\"joot\"}

    // Notifications
    //----------------------------------------------------------
    //  {\"jsonrpc\":\"2.0\",\"method\":\"onMessage\",\"params\":{\"channelId\":1,\"message\":{\"_DouType\":\"Safir.Application.BackdoorCommand\", \"NodeName\": \"Hello\", \"Command\": \"World\"}}}
    //  {\"jsonrpc\":\"2.0\",\"method\":\"onNewEntity\",\"params\":{\"instanceId\":1,\"entity\":{\"_DouType\":\"Safir.Dob.ProcessInfo\",\"Name\":\"Dilbert\",\"Pid\":123,\"ConnectionNames\":[\"Wally\",\"Asok\"]}}}
    //  {\"jsonrpc\":\"2.0\",\"method\":\"onUpdatedEntity\",\"params\":{\"instanceId\":1,\"entity\":{\"_DouType\":\"Safir.Dob.ProcessInfo\",\"Name\":\"Dogbert\",\"Pid\":123,\"ConnectionNames\":[\"Wally\",\"Asok\"]}}}



    //*********************************************************************************
    //  Queue<RequestJSON, ResponseJSON, NotificationJSON>
    //----------------------------------
    // This is a queue of request-response-notification values.
    // Take front in queue, send item.request and expect to get item.response and item.notification
    // before continue with next item.
    //*********************************************************************************
    std::queue<QueueItem> items;

    //Open, Close, IsOpen
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"isOpen\", \"id\":\"aaa\"}",
                         "{\"jsonrpc\":\"2.0\",\"result\":false,\"id\":\"aaa\"}",
                         ""));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"open\", \"params\":{\"connectionName\":\"test\"}, \"id\":\"bbb\"}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":\"bbb\"}",
                         ""));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"isOpen\", \"id\":\"ccc\"}",
                         "{\"jsonrpc\":\"2.0\",\"result\":true,\"id\":\"ccc\"}",
                         ""));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"ping\", \"id\":\"bbb\"}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"pong\",\"id\":\"bbb\"}",
                         ""));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"close\", \"id\":\"ddd\"}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":\"ddd\"}",
                         ""));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"isOpen\", \"id\":1}",
                         "{\"jsonrpc\":\"2.0\",\"result\":false,\"id\":1}",
                         ""));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"open\", \"params\":{\"connectionName\":\"test\"}, \"id\":2}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":2}"
                         ""));

    //Messages
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"subscribeMessage\", \"params\":{\"channelId\":1,\"typeId\":\"Safir.Application.BackdoorCommand\"}, \"id\":3}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":3}",
                         ""));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"subscribeMessage\", \"params\":{\"channelId\":2,\"typeId\":\"Safir.Application.BackdoorCommand\"}, \"id\":4}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":4}",
                         ""));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"sendMessage\", \"params\":{\"channelId\":1,\"message\":{\"_DouType\":\"Safir.Application.BackdoorCommand\",\"NodeName\":\"Hello\",\"Command\":\"World\"}},\"id\":5}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":5}",
                         "{\"jsonrpc\":\"2.0\",\"method\":\"onMessage\",\"params\":{\"channelId\":1,\"message\":{\"_DouType\":\"Safir.Application.BackdoorCommand\",\"NodeName\":\"Hello\",\"Command\":\"World\"}}}"));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"sendMessage\", \"params\":{\"channelId\":2,\"message\":{\"_DouType\":\"Safir.Application.BackdoorCommand\",\"NodeName\":\"Hello\",\"Command\":\"World\"}},\"id\":5}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":5}",
                         "{\"jsonrpc\":\"2.0\",\"method\":\"onMessage\",\"params\":{\"channelId\":2,\"message\":{\"_DouType\":\"Safir.Application.BackdoorCommand\",\"NodeName\":\"Hello\",\"Command\":\"World\"}}}"));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"unsubscribeMessage\", \"params\":{\"channelId\":1,\"typeId\":\"Safir.Application.BackdoorCommand\"}, \"id\":3}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":3}",
                         ""));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"sendMessage\", \"params\":{\"channelId\":1,\"message\":{\"_DouType\":\"Safir.Application.BackdoorCommand\",\"NodeName\":\"Hello\",\"Command\":\"World\"}},\"id\":5}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":5}",
                         ""));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"sendMessage\", \"params\":{\"channelId\":2,\"message\":{\"_DouType\":\"Safir.Application.BackdoorCommand\",\"NodeName\":\"Hello\",\"Command\":\"World\"}},\"id\":5}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":5}",
                         "{\"jsonrpc\":\"2.0\",\"method\":\"onMessage\",\"params\":{\"channelId\":2,\"message\":{\"_DouType\":\"Safir.Application.BackdoorCommand\",\"NodeName\":\"Hello\",\"Command\":\"World\"}}}"));

    //Entity - register and subscribe
    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"registerEntityHandler\", \"params\":{\"typeId\":\"Safir.Dob.ProcessInfo\", \"handlerId\":1}, \"id\":\"regEnt\"}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":\"regEnt\"}",
                         ""));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"subscribeEntity\", \"params\":{\"typeId\":\"Safir.Dob.ProcessInfo\", \"instanceId\":1}, \"id\":\"subEnt\"}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":\"subEnt\"}",
                         ""));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"subscribeEntity\", \"params\":{\"typeId\":\"Safir.Control.Status\"}, \"id\":\"subEnt\"}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":\"subEnt\"}",
                         ""));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"subscribeRegistration\", \"params\":{\"typeId\":\"Safir.Control.Status\",\"handlerId\":1}, \"id\":\"subReg\"}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":\"subReg\"}",
                         ""));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"registerEntityHandler\", \"params\":{\"typeId\":\"Safir.Control.Status\", \"handlerId\":1}, \"id\":\"regEnt\"}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":\"regEnt\"}",
                         "{\"jsonrpc\":\"2.0\",\"method\":\"onRegistered\",\"params\":{\"typeId\":\"Safir.Control.Status\",\"handlerId\":1}}"));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"setEntity\", \"params\":{\"instanceId\":1,\"entity\":{\"_DouType\":\"Safir.Dob.ProcessInfo\",\"Name\":\"Dilbert\",\"Pid\":123,\"ConnectionNames\":[\"Wally\",\"Asok\"]},\"handlerId\":1}, \"id\":\"setEnt\"}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":\"setEnt\"}",
                         "{\"jsonrpc\":\"2.0\",\"method\":\"onNewEntity\",\"params\":{\"instanceId\":1,\"entity\":{\"_DouType\":\"Safir.Dob.ProcessInfo\",\"Name\":\"Dilbert\",\"Pid\":123,\"ConnectionNames\":[\"Wally\",\"Asok\"]}}}"));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"setEntityChanges\", \"params\":{\"instanceId\":1,\"entity\":{\"_DouType\":\"Safir.Dob.ProcessInfo\",\"Name\":\"Dogbert\",\"Pid\":123,\"ConnectionNames\":[\"Wally\",\"Asok\"]}, \"handlerId\":1}, \"id\":\"setEntChanges\"}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":\"setEntChanges\"}",
                         "{\"jsonrpc\":\"2.0\",\"method\":\"onUpdatedEntity\",\"params\":{\"instanceId\":1,\"entity\":{\"_DouType\":\"Safir.Dob.ProcessInfo\",\"Name\":\"Dogbert\",\"Pid\":123,\"ConnectionNames\":[\"Wally\",\"Asok\"]}}}"));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"deleteEntity\", \"params\":{\"instanceId\":1,\"typeId\":\"Safir.Dob.ProcessInfo\", \"handlerId\":1}, \"id\":\"delEnt\"}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":\"delEnt\"}",
                         "{\"jsonrpc\":\"2.0\",\"method\":\"onDeletedEntity\",\"params\":{\"instanceId\":1,\"entity\":{\"_DouType\":\"Safir.Dob.ProcessInfo\",\"Name\":\"Dogbert\",\"Pid\":123,\"ConnectionNames\":[\"Wally\",\"Asok\"]}}}"));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"setEntity\", \"params\":{\"instanceId\":1,\"entity\":{\"_DouType\":\"Safir.Control.Status\",\"NodeId\":1},\"handlerId\":1}, \"id\":\"setEnt1\"}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":\"setEnt1\"}",
                         "{\"jsonrpc\":\"2.0\",\"method\":\"onNewEntity\",\"params\":{\"instanceId\":1,\"entity\":{\"_DouType\":\"Safir.Control.Status\",\"NodeId\":1}}}"));

    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"setEntity\", \"params\":{\"instanceId\":2,\"entity\":{\"_DouType\":\"Safir.Control.Status\",\"NodeId\":2},\"handlerId\":1}, \"id\":\"setEnt2\"}",
                         "{\"jsonrpc\":\"2.0\",\"result\":\"OK\",\"id\":\"setEnt2\"}",
                         "{\"jsonrpc\":\"2.0\",\"method\":\"onNewEntity\",\"params\":{\"instanceId\":2,\"entity\":{\"_DouType\":\"Safir.Control.Status\",\"NodeId\":2}}}"));

//    items.push(QueueItem("{\"jsonrpc\":\"2.0\", \"method\":\"getNumberOfInstances\", \"params\":{\"typeId\":\"Safir.Control.Status\"}, \"id\":\"numInst\"}",
//                         "{\"jsonrpc\":\"2.0\",\"result\":2,\"id\":\"numInst\"}",
//                         ""));

    //items.push(QueueItem("", "", ""));

    //Entity - requests

    //SeviceRequests

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
            auto& request=items.front().request;
            std::cout<<"--> "<<request<<std::endl;
            c.send(hdl, request, websocketpp::frame::opcode::text);
        });

        // Register our message handler
        c.set_message_handler([&](websocketpp::connection_hdl hdl, message_ptr msg)
        {
            std::string data = msg->get_payload();
            std::cout<<"<-- "<<data<<std::endl;

            if (items.empty())
            {
                return;
            }

            if (items.front().response==data)
            {
                items.front().response.clear();
            }
            else if (items.front().notification==data)
            {
                items.front().notification.clear();
            }
            else
            {
                std::cout<<"Received unexpected data."<<std::endl;
                std::cout<<"    Next expected response: "<<items.front().response<<std::endl;
                std::cout<<"    Next expected notification: "<<items.front().notification<<std::endl;
                exit(1);
            }

            if (items.front().response.empty() && items.front().notification.empty())
            {
                items.pop();

                if (!items.empty())
                {
                    std::cout<<"--> "<<items.front().request<<std::endl;
                    c.send(hdl, items.front().request, websocketpp::frame::opcode::text);
                }
                else
                {
                    con->close(websocketpp::close::status::normal, "finished");
                }
            }
        });

        c.set_close_handler([&](websocketpp::connection_hdl hdl)
        {
            std::cout<<"OnClose"<<std::endl;
            if (!items.empty())
            {
                std::cout<<"Connection unexpecedly closed by server!"<<std::endl;
                std::cout<<"    Next expected response: "<<items.front().response<<std::endl;
                std::cout<<"    Next expected notification: "<<items.front().notification<<std::endl;
                exit(1);
            }
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
