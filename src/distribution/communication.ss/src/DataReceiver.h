/******************************************************************************
*
* Copyright Saab AB, 2013-2015 (http://safirsdkcore.com)
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

#include <memory>
#include <functional>
#include <boost/chrono.hpp>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "Parameters.h"
#include "Message.h"
#include "Node.h"
#include "Resolver.h"

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
    class DataReceiverType : private ReaderType
    {
    public:
        DataReceiverType(boost::asio::io_context::strand& receiveStrand,
                         const std::string& unicastAddress,
                         const std::string& multicastAddress,
                         const std::function<bool(const char*, size_t, bool multicast)>& onRecv,
                         const std::function<bool(void)>& isReceiverIsReady)
            :m_strand(receiveStrand)
            ,m_timer(m_strand.context(), boost::chrono::milliseconds(10))
            ,m_checkMcTimer(m_strand.context())
            ,m_onRecv(onRecv)
            ,m_isReceiverReady(isReceiverIsReady)
            ,m_running(false)
            ,m_logPrefix(Parameters::LogPrefix + "DataReceiver - ")
        {
            m_unicastEndpoint = Resolver::StringToEndpoint(unicastAddress);
            if (!multicastAddress.empty())
            {
                m_multicastEndpoint = Resolver::StringToEndpoint(multicastAddress);
                if (m_multicastEndpoint.protocol() != m_unicastEndpoint.protocol())
                {
                    throw std::logic_error("Unicast address and multicast address is not in same format (IPv4 and IPv6)");
                }
                if (!m_multicastEndpoint.address().is_multicast())
                {
                    throw std::logic_error("Given multicast address does not specify a valid multicast address");
                }
            }
        }

        //make noncopyable
        DataReceiverType(const DataReceiverType&) = delete;
        const DataReceiverType& operator=(const DataReceiverType&) = delete;

        void Start()
        {
            boost::asio::post(m_strand, [this]
            {
                m_running=true;

                m_socket.reset(new boost::asio::ip::udp::socket(m_strand.context()));
                m_socket->open(m_unicastEndpoint.protocol());
                m_socket->set_option(boost::asio::ip::udp::socket::reuse_address(true));
                m_socket->bind(m_unicastEndpoint);
                m_socket->set_option(boost::asio::socket_base::receive_buffer_size(Parameters::SocketBufferSize));
                AsyncReceive(m_bufferUnicast, m_socket.get());

                if (!m_multicastEndpoint.address().is_unspecified())
                {
                    m_multicastSocket.reset(new boost::asio::ip::udp::socket(m_strand.context()));
                    m_multicastSocket->open(m_multicastEndpoint.protocol());
                    m_multicastSocket->set_option(boost::asio::ip::udp::socket::reuse_address(true));
                    m_multicastSocket->set_option(boost::asio::ip::multicast::enable_loopback(true));

                    //to join mcGroup with specific interface, the address must be a IPv4. Bug report https://svn.boost.org/trac/boost/ticket/3247
                    m_multicastSocket->bind(boost::asio::ip::udp::endpoint(boost::asio::ip::address_v4::any(), m_multicastEndpoint.port())); //bind to all interfaces and the multicast port

                    boost::system::error_code ec;
                    m_multicastSocket->set_option(boost::asio::socket_base::receive_buffer_size(Parameters::SocketBufferSize), ec);
                    m_multicastSocket->set_option(boost::asio::ip::multicast::join_group(m_multicastEndpoint.address().to_v4(), m_unicastEndpoint.address().to_v4()), ec); //join group on specific interface
                    if (ec)
                    {
                        lllog(7)<<m_logPrefix.c_str()<<L"Failed to join multicast group. Will try again in a while."<<std::endl;
                    }
                    else
                    {
                        AsyncReceive(m_bufferMulticast, m_multicastSocket.get());
                    }

                    m_lastMcRecv = boost::chrono::steady_clock::now();
                    m_checkMcTimer.expires_after(boost::chrono::milliseconds(Parameters::SendPingThreshold * 2));
                    m_checkMcTimer.async_wait(boost::asio::bind_executor(m_strand, [this](const boost::system::error_code&){CheckMulticast();}));
                }
            });
        }

        void Stop()
        {
            boost::asio::post(m_strand, [this]
            {
                m_running=false;
                m_timer.cancel();
                m_checkMcTimer.cancel();
                ++m_runCount;

                if (m_socket && m_socket->is_open())
                {
                    m_socket->cancel();
                    m_socket->close();
                    m_socket.reset();
                }
                if (m_multicastSocket && m_multicastSocket->is_open())
                {
                    m_multicastSocket->cancel();
                    m_multicastSocket->close();
                    m_multicastSocket.reset();
                }
            });
        }

#ifndef SAFIR_TEST
    private:
#endif
        boost::asio::io_context::strand& m_strand;
        boost::asio::steady_timer m_timer;
        boost::asio::steady_timer m_checkMcTimer;
        std::function<bool(const char*, size_t, bool multicast)> m_onRecv;
        std::function<bool(void)> m_isReceiverReady;
        std::unique_ptr<boost::asio::ip::udp::socket> m_socket;
        std::unique_ptr<boost::asio::ip::udp::socket> m_multicastSocket;
        boost::asio::ip::udp::endpoint m_unicastEndpoint;
        boost::asio::ip::udp::endpoint m_multicastEndpoint;
        bool m_running;
        std::string m_logPrefix;

        unsigned int m_runCount = 0;
        boost::chrono::time_point<boost::chrono::steady_clock> m_lastMcRecv;
        char m_bufferUnicast[Parameters::ReceiveBufferSize];
        char m_bufferMulticast[Parameters::ReceiveBufferSize];

        void AsyncReceive(char* buf, boost::asio::ip::udp::socket* socket)
        {
            ReaderType::AsyncReceive(buf,
                                     Parameters::ReceiveBufferSize,
                                     socket,
                                     [this, buf, socket](const boost::system::error_code& error, size_t bytesRecv)
            {
                //if an error occured, log the error and stop
                if (error == boost::asio::error::operation_aborted)
                {
                    // This happens when socket is closed by the Stop-method, and is a normal case.
                    if (m_running)
                    {
                        lllog(7)<<m_logPrefix.c_str()<<L"Socket read operation aborted."<<std::endl;
                    }
                    else
                    {
                        lllog(7)<<m_logPrefix.c_str()<<L"Socket has been closed. Read operation aborted."<<std::endl;
                    }
                    
                    return;
                }
                if (error)
                {
                    std::cout<<m_logPrefix.c_str()<<"Read failed, error "<<error.message().c_str()<<std::endl;
                    SEND_SYSTEM_LOG(Error, <<m_logPrefix.c_str()<<L"Read failed, error "<<error.message().c_str());
                    return;
                }

                auto runCount = m_runCount; // make copy
                boost::asio::post(m_strand, [this, runCount, bytesRecv, buf, socket]
                {
                    if (runCount == m_runCount)
                    {
                        HandleReceive(bytesRecv, buf, socket);
                    }
                    else
                    {
                        lllog(7)<<m_logPrefix.c_str()<<L"Data is old, it was read before restarting DataReader. Throw away! RunCount is now "
                               <<m_runCount<<L", read at " <<runCount<<std::endl;
                    }

                });
            });
        }

        bool ValidCrc(const char* buf, size_t size)
        {
            boost::crc_32_type crc;
            crc.process_bytes(static_cast<const void*>(buf), size-sizeof(uint32_t));
            uint32_t checksum=*reinterpret_cast<const uint32_t*>(buf+size-sizeof(uint32_t));
            return checksum==crc.checksum();
        }

        void HandleReceive(size_t bytesRecv, char* buf, boost::asio::ip::udp::socket* socket)
        {
            //if we have got at stop-order just return and dont start a new read
            if (!m_running)
            {
                return;
            }

            bool receiverReady=true;

            if (ValidCrc(buf, bytesRecv))
            {
                const bool multicast = socket == m_multicastSocket.get();
                if (multicast)
                {
                    lllog(9)<<m_logPrefix.c_str()<<L"received multicast"<<std::endl;
                    m_lastMcRecv = boost::chrono::steady_clock::now();
                }
                else
                {
                    lllog(9)<<m_logPrefix.c_str()<<L"received unicast"<<std::endl;
                }
                    
                //received message with correct checksum
                receiverReady=m_onRecv(buf, bytesRecv-sizeof(uint32_t), multicast); //Remove the crc from size. Will return true if it is ready to handle a new message immediately
            }
            else
            {
                //received message with invalid checksum. Throw away the message and then continue as normal.
                std::ostringstream os;
                os<<m_logPrefix.c_str()<<"Received message with bad CRC, size="<<bytesRecv<<". Throw away and continue."<<std::endl;
                if (bytesRecv>CommonHeaderSize)
                {
                    const CommonHeader* commonHeader=reinterpret_cast<const CommonHeader*>(buf);
                    os<<"     If trying to parse despite CRC error: "<<commonHeader->ToString()<<std::endl;
                }
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
                lllog(7)<<m_logPrefix.c_str()<<L"Reader has to wait for application to handle delivered messages"<<std::endl;
                SetWakeUpTimer(buf, socket);
            }
        }

        void SetWakeUpTimer(char* buf, boost::asio::ip::udp::socket* socket)
        {
            m_timer.expires_after(boost::chrono::milliseconds(10));
            m_timer.async_wait(boost::asio::bind_executor(m_strand, [this,buf,socket](const boost::system::error_code& error){WakeUpAfterSleep(error, buf, socket);}));
        }

        void WakeUpAfterSleep(const boost::system::error_code& /*error*/, char* buf, boost::asio::ip::udp::socket* socket)
        {
            if (!m_running)
            {
                return;
            }

            if (m_isReceiverReady())
            {
                lllog(7)<<m_logPrefix.c_str()<<L"Reader wakes up from sleep and starts receiving again"<<std::endl;
                AsyncReceive(buf, socket);
            }
            else
            {
                lllog(7)<<m_logPrefix.c_str()<<L"Reader wakes up but must go back to sleep until application is ready"<<std::endl;
                SetWakeUpTimer(buf, socket);
            }
        }

        void CheckMulticast()
        {
            if (!m_running)
            {
                return;
            }

            static const boost::chrono::milliseconds McRecvThreshold = boost::chrono::milliseconds(Parameters::SendPingThreshold + 1000);
            auto timeSinceLastMc = boost::chrono::steady_clock::now() - m_lastMcRecv;
            if (timeSinceLastMc > McRecvThreshold)
            {
                if (m_multicastSocket && m_multicastSocket->is_open())
                {
                    // Check if out network interface seems to be available. (Just add 1 as dummy port at the end of the ip-addr)
                    if (!Resolver::ResolveLocalEndpoint(m_unicastEndpoint.address().to_string() + ":1") .empty())
                    {
                        lllog(7)<<m_logPrefix.c_str()<<L"We haven't received any multicast data for a while. Trying to rejoin multicastgroup"<<std::endl;
                        
                        m_multicastSocket->cancel(); // cancel any ongoing async_read

                        // rejoin mc group, by doing a leave followed by a join
                        boost::system::error_code ec;
                        m_multicastSocket->set_option(boost::asio::ip::multicast::leave_group(m_multicastEndpoint.address().to_v4()), ec); //leave group
                        m_multicastSocket->set_option(boost::asio::ip::multicast::join_group(m_multicastEndpoint.address().to_v4(), m_unicastEndpoint.address().to_v4()), ec); //join group on specific interface

                        // start a new async_read
                        boost::asio::post(m_strand, [this]{AsyncReceive(m_bufferMulticast, m_multicastSocket.get());});
                    }
                }
            }

            m_checkMcTimer.expires_after(boost::chrono::milliseconds(Parameters::SendPingThreshold));
            m_checkMcTimer.async_wait(boost::asio::bind_executor(m_strand, [this](const boost::system::error_code&){CheckMulticast();}));
        }
    };

    class SocketReader
    {
    public:
        void AsyncReceive(char* buf,
                          size_t bufSize,
                          boost::asio::ip::udp::socket* socket,
                          const std::function< void(const boost::system::error_code&, size_t) >& completionHandler)
        {

            if (Parameters::NetworkEnabled)
            {
                socket->async_receive(boost::asio::buffer(buf, bufSize), completionHandler);
            }
            else
            {
                SimulateSilence(buf, bufSize, socket, completionHandler);
            }
        }

    private:

        void SimulateSilence(char* buf,
                             size_t bufSize,
                             boost::asio::ip::udp::socket* socket,
                             const std::function< void(const boost::system::error_code&, size_t) >& completionHandler)
        {
            socket->async_receive(boost::asio::buffer(buf, bufSize),
                                  [this, buf, bufSize, socket, completionHandler](const boost::system::error_code& ec, size_t s)
            {
                if (Parameters::NetworkEnabled || ec)
                {
                    completionHandler(ec, s);
                }
                else
                {
                    SimulateSilence(buf, bufSize, socket, completionHandler);
                }
            });

        }
    };

    typedef DataReceiverType<SocketReader> DataReceiver;
}
}
}
}
