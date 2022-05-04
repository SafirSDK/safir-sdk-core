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
#include <websocketpp/config/asio_no_tls_client.hpp>
#include <websocketpp/client.hpp>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

typedef websocketpp::client<websocketpp::config::asio_client> client;
typedef websocketpp::config::asio_client::message_type::ptr message_ptr;

class ServiceHandler
{
public:
    ServiceHandler(const boost::function<void()>& isRegistered)
        :m_isRegistered(isRegistered)
    {

    }

    virtual ~ServiceHandler()
    {
        m_runner->join();
    }

    void Run()
    {
        std::string uri = "ws://localhost:10000";
        m_client.set_access_channels(websocketpp::log::alevel::none);
        m_client.init_asio();
        m_client.set_open_handler([=](websocketpp::connection_hdl hdl){OnOpen(hdl);});
        m_client.set_close_handler([=](websocketpp::connection_hdl hdl){OnClose(hdl);});
        m_client.set_fail_handler([=](websocketpp::connection_hdl hdl){OnError(hdl);});
        m_client.set_message_handler([=](websocketpp::connection_hdl hdl, message_ptr msg){OnMessage(hdl, msg);});
        websocketpp::lib::error_code ec;
        m_con = m_client.get_connection(uri, ec);
        if (ec) {
            std::cout << "could not create connection because: " << ec.message() << std::endl;
            exit(1);
        }
        m_client.connect(m_con);

        m_runner=std::make_shared<boost::thread>([=]{m_client.run();});
    }

    void Stop()
    {
        m_con->close(websocketpp::close::status::normal, "");
    }


private:
    boost::function<void()> m_isRegistered;
    client m_client;
    client::connection_ptr m_con;
    boost::int64_t m_numHandledRequests;
    std::shared_ptr<boost::thread> m_runner;

    void OnOpen(websocketpp::connection_hdl /*hdl*/)
    {
        auto open="{\"jsonrpc\":\"2.0\", \"method\":\"open\", \"params\":{\"connectionName\":\"testHandler\"}, \"id\":\"open\"}";
        m_con->send(open);
        auto regSrvHandler="{\"jsonrpc\":\"2.0\", \"method\":\"registerServiceHandler\", \"params\":{\"typeId\":\"Safir.Control.Command\", \"handlerId\":1}, \"id\":-100}";
        m_con->send(regSrvHandler);
    }

    void OnClose(websocketpp::connection_hdl /*hdl*/)
    {
        std::cout<<"HANDLER"<<" OnClose"<<std::endl;
    }

    void OnError(websocketpp::connection_hdl /*hdl*/)
    {
        std::cout<<"HANDLER"<<" ***OnError***"<<std::endl;
    }

    void OnMessage(websocketpp::connection_hdl /*hdl*/, message_ptr msg)
    {
        std::string data = msg->get_payload();
        //std::cout<<"HANDLER RECV: "<<data<<std::endl;

        rapidjson::Document doc;
        doc.Parse(data.c_str());

        if (doc["id"]==-100)
        {
            //Tell Test manager that service is registered and users can start to use it.
            m_isRegistered();
            return;
        }

        //Here goes received requests
        if (doc.HasMember("method") && doc["method"].GetString()==std::string("onServiceRequest"))
        {
            auto id=doc["id"].GetInt();
            std::ostringstream os;
            os<<"{\"jsonrpc\":\"2.0\", \"result\":{\"_DouType\":\"Safir.Dob.SuccessResponse\"}, \"id\":"<<id<<"}";
            //std::cout<<"HANDLER SEND: "<<os.str()<<std::endl;
            m_con->send(os.str());
            ++m_numHandledRequests;
        }
    }

};
