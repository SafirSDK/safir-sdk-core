/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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
#pragma once

#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <boost/chrono.hpp>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "Parameters.h"
#include "Message.h"
#include "Node.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    /**
     * The DataReceiver class is responsible for receiving data on both unicast and multicast.
     * All received messages are passed to onRecv-callback. DataReceiver is unaware of sequenceNumbers and fragments.
     * If callback onRecv is returning false it means that there is maximum number of messages waiting to be retrieved
     * by the application and in that case DataReceiver will sleep until callback isReceiverReady is returning true again.
     */
    template <class ReaderType>
    class DataReceiverType : private ReaderType, private boost::noncopyable
    {
    public:

        DataReceiverType(boost::asio::io_service::strand& receiveStrand,
                         const std::string& unicastAddress,
                         const std::string& multicastAddress,
                         const std::function<bool(const char*, size_t)>& onRecv,
                         const std::function<bool(void)>& isReceiverIsReady)
            :m_strand(receiveStrand)
            ,m_timer(m_strand.get_io_service(), boost::chrono::milliseconds(10))
            ,m_onRecv(onRecv)
            ,m_isReceiverReady(isReceiverIsReady)
            ,m_running(false)
        {
            int unicastIpVersion;
            auto unicastEndpoint=Utilities::CreateEndpoint(unicastAddress, unicastIpVersion);

            m_socket.reset(new boost::asio::ip::udp::socket(m_strand.get_io_service()));
            m_socket->open(unicastEndpoint.protocol());
            m_socket->bind(unicastEndpoint);
            m_socket->set_option(boost::asio::socket_base::receive_buffer_size(Parameters::SocketBufferSize));

            if (!multicastAddress.empty())
            {
                //using multicast
                int multicastIpVersion=0;
                auto mcEndpoint=Utilities::CreateEndpoint(multicastAddress, multicastIpVersion);
                if (multicastIpVersion!=unicastIpVersion)
                {
                    throw std::logic_error("Unicast address and multicast address is not in same format (IPv4 and IPv6)");
                }
                m_multicastSocket.reset(new boost::asio::ip::udp::socket(m_strand.get_io_service()));
                m_multicastSocket->open(mcEndpoint.protocol());
                m_multicastSocket->set_option(boost::asio::ip::udp::socket::reuse_address(true));
                m_multicastSocket->set_option(boost::asio::ip::multicast::enable_loopback(true));

                //to join mcGroup with specific interface, the address must be a IPv4. Bug report https://svn.boost.org/trac/boost/ticket/3247
                m_multicastSocket->bind(boost::asio::ip::udp::endpoint(boost::asio::ip::address_v4::any(), mcEndpoint.port())); //bind to all interfaces and the multicast port
                m_multicastSocket->set_option(boost::asio::ip::multicast::join_group(mcEndpoint.address().to_v4(), unicastEndpoint.address().to_v4())); //join group on specific interface
                m_multicastSocket->set_option(boost::asio::socket_base::receive_buffer_size(Parameters::SocketBufferSize));
            }

        }

        void Start()
        {
            m_strand.dispatch([=]
            {
                m_running=true;
                AsyncReceive(m_bufferUnicast, m_socket.get());
                if (m_multicastSocket)
                {
                    AsyncReceive(m_bufferMulticast, m_multicastSocket.get());
                }
            });
        }

        void Stop()
        {
            m_strand.dispatch([=]
            {
                m_running=false;
                m_timer.cancel();

                if (m_socket->is_open())
                {
                    m_socket->close();
                }
                if (m_multicastSocket && m_multicastSocket->is_open())
                {
                    m_multicastSocket->close();
                }
            });
        }

#ifndef SAFIR_TEST
    private:
#endif
        boost::asio::io_service::strand& m_strand;
        boost::asio::steady_timer m_timer;
        std::function<bool(const char*, size_t)> m_onRecv;
        std::function<bool(void)> m_isReceiverReady;
        boost::shared_ptr<boost::asio::ip::udp::socket> m_socket;
        boost::shared_ptr<boost::asio::ip::udp::socket> m_multicastSocket;
        bool m_running;

        char m_bufferUnicast[Parameters::ReceiveBufferSize];
        char m_bufferMulticast[Parameters::ReceiveBufferSize];

        void AsyncReceive(char* buf, boost::asio::ip::udp::socket* socket)
        {
            ReaderType::AsyncReceive(buf,
                                     Parameters::ReceiveBufferSize,
                                     socket,
                                     m_strand.wrap([=](const boost::system::error_code& error, size_t bytesRecv)
                                     {
                                         HandleReceive(error, bytesRecv, buf, socket);
                                     }));
        }

        bool ValidCrc(const char* buf, size_t size)
        {
            boost::crc_32_type crc;
            crc.process_bytes(static_cast<const void*>(buf), size-sizeof(uint32_t));
            uint32_t checksum=*reinterpret_cast<const uint32_t*>(buf+size-sizeof(uint32_t));
            bool valid=(checksum==crc.checksum());
            if (!valid)
            {
                std::cout<<"CRC expected: "<<crc.checksum()<<", got: "<<checksum<<std::endl;
            }

            return valid;
        }

        void HandleReceive(const boost::system::error_code& error, size_t bytesRecv, char* buf, boost::asio::ip::udp::socket* socket)
        {
            //if we have got at stop-order just return and dont start a new read
            if (!m_running)
            {
                return;
            }

            //if an error occured, log the error and stop
            if (error)
            {
                std::cout<<"Read failed, error "<<error.message().c_str()<<std::endl;
                SEND_SYSTEM_LOG(Error, <<"Read failed, error "<<error.message().c_str());
                return;
            }

            bool receiverReady=true;

            if (ValidCrc(buf, bytesRecv))
            {
                //received message with correct checksum
                receiverReady=m_onRecv(buf, bytesRecv-sizeof(uint32_t)); //Remove the crc from size. Will return true if it is ready to handle a new message immediately
            }
            else
            {
                //received message with invalid checksum. Throw away the message and then continue as normal.
                std::ostringstream os;

                os<<"COM: Received message with bad CRC, size="<<bytesRecv<<". Throw away and continue."<<std::endl;

                if (bytesRecv>CommonHeaderSize)
                {
                    const CommonHeader* commonHeader=reinterpret_cast<const CommonHeader*>(buf);                    

                    if (commonHeader->dataType==123) //used by communication_test
                    {
                        const MessageHeader* messageHeader=reinterpret_cast<const MessageHeader*>(buf);
                        os<<"     If trying to parse despite CRC error: "<<messageHeader->ToString()<<std::endl;
                    }
                    else
                    {
                        os<<"     If trying to parse despite CRC error: "<<commonHeader->ToString()<<std::endl;
                        if (bytesRecv>MessageHeaderSize)
                        {
                            os<<Hexdump(buf, MessageHeaderSize, bytesRecv)<<std::endl;
                        }
                    }
                }

                std::cout<<os.str()<<std::endl;
                lllog(7)<<os.str().c_str()<<std::endl;

                receiverReady=m_isReceiverReady(); //explicitly ask if receiver is ready to handle incoming data
            }

            if (receiverReady)
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

        void SetWakeUpTimer(char* buf, boost::asio::ip::udp::socket* socket)
        {
            m_timer.expires_from_now(boost::chrono::milliseconds(10));
            m_timer.async_wait(m_strand.wrap([=](const boost::system::error_code& error){WakeUpAfterSleep(error, buf, socket);}));
        }

        void WakeUpAfterSleep(const boost::system::error_code& /*error*/, char* buf, boost::asio::ip::udp::socket* socket)
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
    };

    struct SocketReader
    {
        void AsyncReceive(char* buf,
                          size_t bufSize,
                          boost::asio::ip::udp::socket* socket,
                          const boost::function< void(const boost::system::error_code&, size_t) >& completionHandler)
        {
            socket->async_receive(boost::asio::buffer(buf, bufSize), completionHandler);
        }
    };

    typedef DataReceiverType<SocketReader> DataReceiver;
}
}
}
}
