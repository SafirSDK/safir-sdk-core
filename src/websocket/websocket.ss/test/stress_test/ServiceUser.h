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
#endif

#include <boost/make_shared.hpp>
#include <boost/thread.hpp>
#include <boost/function.hpp>
#include <websocketpp/config/asio_no_tls_client.hpp>
#include <websocketpp/client.hpp>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

typedef websocketpp::client<websocketpp::config::asio_client> client;
typedef websocketpp::config::asio_client::message_type::ptr message_ptr;

class ServiceUser
{
public:
    ServiceUser(int id, int numToSend, boost::function<void(int,bool)> done)
        :m_id(id)
        ,m_numToSend(numToSend)
        ,m_done(done)
        ,m_lastSentId(0)
    {

    }

    virtual ~ServiceUser()
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
        m_runner=boost::make_shared<boost::thread>([=]{m_client.run();});
    }

private:
    int m_id;
    int m_numToSend;
    boost::function<void(int, bool)> m_done;
    int m_lastSentId;
    client m_client;
    client::connection_ptr m_con;
    boost::shared_ptr<boost::thread> m_runner;


    void SendReq()
    {
        ++m_lastSentId;
        std::ostringstream os;
        os<<"{\"jsonrpc\":\"2.0\", \"method\":\"serviceRequest\", \"params\":{\"handlerId\":1,\"request\":{\"_DouType\":\"Safir.Control.Command\",\"NodeId\":"<<m_id<<"}}, \"id\":"<<m_lastSentId<<"}";        
        m_con->send(os.str());
    }

    void OnOpen(websocketpp::connection_hdl /*hdl*/)
    {
        //std::cout<<"USER_"<<m_id<<" OnOpen"<<std::endl;
        std::ostringstream os;
        os<<"{\"jsonrpc\":\"2.0\", \"method\":\"open\", \"params\":{\"connectionName\":\"testUser_"<<m_id<<"\"}, \"id\":-123}";
        m_con->send(os.str());
        SendReq();
    }

    void OnClose(websocketpp::connection_hdl /*hdl*/)
    {
    }

    void OnError(websocketpp::connection_hdl hdl)
    {
        if (m_con->get_state()==websocketpp::session::state::closed)
        {
            std::cout<<"USER_"<<m_id<<" Retry to connect..."<<std::endl;
            m_done(m_id, true);
            return;
        }

        client::connection_ptr con = m_client.get_con_from_hdl(hdl);
        std::ostringstream os;
        os<<"USER_"<<m_id<<" *** OnError ***"<<std::endl;
        os << "Fail handler" << std::endl;
        os << con->get_state() << std::endl;
        os << con->get_local_close_code() << std::endl;
        os << con->get_local_close_reason() << std::endl;
        os << con->get_remote_close_code() << std::endl;
        os << con->get_remote_close_reason() << std::endl;
        os << con->get_ec() << " - " << con->get_ec().message() << std::endl;
        os<<"   *** End Error ***";
        std::cout<<os.str()<<std::endl;
    }

    void OnMessage(websocketpp::connection_hdl /*hdl*/, message_ptr msg)
    {
        std::string data = msg->get_payload();
        //std::cout<<"USER_"<<m_id<<" RECV: "<<data<<std::endl;

        rapidjson::Document doc;
        doc.Parse(data.c_str());
        int id=doc["id"].GetInt();
        if (id==-123)
        {
            return; //open response
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
            m_con->close(websocketpp::close::status::normal, "");
            m_done(m_id, false);
            return;
        }
    }
};
