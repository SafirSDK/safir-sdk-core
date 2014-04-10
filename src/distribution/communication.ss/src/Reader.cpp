/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <boost/chrono.hpp>
#include "Reader.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    Reader::Reader(boost::asio::io_service& ioService,
                   const Node& me,
                   const std::function<bool(const char*, size_t)>& onRecv,
                   const std::function<bool(void)>& isReceiverIsReady)
        :m_ioService(ioService)
        ,m_strand(ioService)
        ,m_timer(m_ioService, boost::chrono::milliseconds(10))
        ,m_me(me)
        ,m_onRecv(onRecv)
        ,m_isReceiverReady(isReceiverIsReady)
        ,m_separateMulticastSocket(false)
        ,m_running(false)
    {
        m_socket.reset(new boost::asio::ip::udp::socket(m_ioService));
        m_socket->open(me.Endpoint().protocol());

        if (me.IsMulticastEnabled())
        {
            boost::asio::ip::udp::endpoint mcEndpoint=Node::CreateEndpoint(me.MulticastAddress(), me.Endpoint().port());
            m_separateMulticastSocket=(mcEndpoint.port()!=me.Endpoint().port());
            if (m_separateMulticastSocket)
            {
                m_multicastSocket.reset(new boost::asio::ip::udp::socket(m_ioService));
                m_multicastSocket->open(mcEndpoint.protocol());
                m_multicastSocket->set_option(boost::asio::ip::udp::socket::reuse_address(true));
                m_multicastSocket->set_option(boost::asio::ip::multicast::enable_loopback(true));
                m_multicastSocket->set_option(boost::asio::ip::multicast::join_group(mcEndpoint.address()));
                BindSocket(m_multicastSocket, me.IpVersion(), mcEndpoint.port());
            }
            else
            {
                m_socket->set_option(boost::asio::ip::udp::socket::reuse_address(true));
                m_socket->set_option(boost::asio::ip::multicast::enable_loopback(true));
                m_socket->set_option(boost::asio::ip::multicast::join_group(mcEndpoint.address()));
            }
        }

        BindSocket(m_socket, me.IpVersion(), me.Endpoint().port());
    }

    void Reader::BindSocket(boost::shared_ptr<boost::asio::ip::udp::socket>& socket, int ipv, unsigned short port)
    {
//        try
//        {
            if (ipv==4)
            {
                boost::asio::ip::udp::endpoint listenEp(boost::asio::ip::address_v4::from_string("0.0.0.0"), port);
                socket->bind(listenEp);
            }
            else
            {
                boost::asio::ip::udp::endpoint listenEp(boost::asio::ip::address_v6::from_string("0.0.0.0"), port);
                socket->bind(listenEp);
            }
//        }
//        catch (const boost::system::system_error& error)
//        {
//            std::cout<<"Bind failed, error "<<error.what()<<std::endl;
//            SEND_SYSTEM_LOG(Error, <<"Read failed, error "<<error.what());
//            throw error;
//        }
    }

    void Reader::Start()
    {
        m_strand.dispatch([=]
        {
            m_running=true;
            AsyncReceive(m_bufferUnicast, m_socket.get());
            if (m_separateMulticastSocket)
            {
                AsyncReceive(m_bufferMulticast, m_multicastSocket.get());
            }
        });
    }

    void Reader::Stop()
    {
        m_strand.dispatch([=]
        {
            m_running=false;
            m_timer.cancel();

            if (m_socket->is_open())
            {
                m_socket->close();
            }
            if (m_separateMulticastSocket && m_multicastSocket->is_open())
            {
                m_multicastSocket->close();
            }
        });
    }

    void Reader::AsyncReceive(char* buf, boost::asio::ip::udp::socket* socket)
    {
        socket->async_receive(boost::asio::buffer(buf, Parameters::ReceiveBufferSize),
                              m_strand.wrap([=](const boost::system::error_code& error, size_t bytesRecv)
                            {
                                HandleReceive(error, bytesRecv, buf, socket);
                            }));
    }

    void Reader::HandleReceive(const boost::system::error_code& error, size_t bytesRecv, char* buf, boost::asio::ip::udp::socket* socket)
    {
        if (!m_running)
        {
            return;
        }

        if (!error)
        {
            if (m_onRecv(buf, bytesRecv))
            {
                //receiver is keeping up with our pace, continue to read incoming messages
                AsyncReceive(buf, socket);
            }
            else
            {
                // we must wait for a while before delivering more messages
                lllog(7)<<"COM: Reader has to wait for application to handle delivered messages"<<std::endl;
                SetWakeUpTimer(buf, socket);
            }
        }
        else
        {
            std::cout<<"Read failed, error "<<error.message().c_str()<<std::endl;
            SEND_SYSTEM_LOG(Error, <<"Read failed, error "<<error.message().c_str());
        }
    }

    void Reader::SetWakeUpTimer(char* buf, boost::asio::ip::udp::socket* socket)
    {
        m_timer.expires_from_now(boost::chrono::milliseconds(10));
        m_timer.async_wait(m_strand.wrap([=](const boost::system::error_code& error){WakeUpAfterSleep(error, buf, socket);}));
    }

    void Reader::WakeUpAfterSleep(const boost::system::error_code& /*error*/, char* buf, boost::asio::ip::udp::socket* socket)
    {
        if (!m_running)
        {
            return;
        }

        if (m_isReceiverReady())
        {
            lllog(7)<<"COM: Reader wakes up from sleep and starts receiving again"<<std::endl;
            AsyncReceive(buf, socket);
        }
        else
        {
            lllog(7)<<"COM: Reader wakes up but must go back to sleep until application is ready"<<std::endl;
            SetWakeUpTimer(buf, socket);
        }
    }
}
}
}
}
