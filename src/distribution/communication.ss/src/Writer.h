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

#include <map>
#include <boost/noncopyable.hpp>
#include <boost/function.hpp>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "Message.h"
#include "Utilities.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

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

//#define COM_USE_UNRELIABLE_SEND_POLICY //Only uncomment during test.
#ifndef COM_USE_UNRELIABLE_SEND_POLICY
    //------------------------------------------------------------
    // Normal send policy using udp sockets
    //------------------------------------------------------------
    template <class T>
    struct BasicSendPolicy
    {
        void Send(const boost::shared_ptr<T>& val,
                  boost::asio::ip::udp::socket& socket,
                  const boost::asio::ip::udp::endpoint& to)
        {
            //calculate crc and add it last in sendBuffer
            boost::crc_32_type crc;
            crc.process_bytes(static_cast<const void*>(val.get()), sizeof(T));
            uint32_t crc32=crc.checksum();
            std::vector< boost::asio::const_buffer > bufs;
            bufs.push_back(boost::asio::buffer(static_cast<const void*>(val.get()), sizeof(T)));
            bufs.push_back(boost::asio::buffer(reinterpret_cast<const char*>(&crc32), sizeof(uint32_t)));

            try
            {
                socket.send_to(bufs, to);
            }
            catch (const boost::system::system_error& sysErr)
            {
                std::cout<<"Write<T> failed with systemError: "<<sysErr.what()<<std::endl;
                SEND_SYSTEM_LOG(Error, <<"Write failed with systemError: "<<sysErr.what());
            }
        }
    };

    template <>
    struct BasicSendPolicy<UserData>
    {
        void Send(const UserDataPtr& val,
                  boost::asio::ip::udp::socket& socket,
                  const boost::asio::ip::udp::endpoint& to)
        {
            const char* header=reinterpret_cast<const char*>(&(val->header));
            boost::crc_32_type crc;
            std::vector< boost::asio::const_buffer > bufs;

            bufs.push_back(boost::asio::buffer(header, MessageHeaderSize));
            crc.process_bytes(static_cast<const void*>(header), MessageHeaderSize);

            if (val->header.fragmentContentSize>0)
            {
                bufs.push_back(boost::asio::buffer(val->fragment, val->header.fragmentContentSize));
                crc.process_bytes(static_cast<const void*>(val->fragment), val->header.fragmentContentSize);
            }

            uint32_t crc32=crc.checksum();
            bufs.push_back(boost::asio::buffer(reinterpret_cast<const char*>(&crc32), sizeof(uint32_t)));


            try
            {
                socket.send_to(bufs, to);
            }
            catch (const boost::system::system_error& sysErr)
            {
                std::cout<<"Write<UserData> failed with systemError: "<<sysErr.what()<<std::endl;
                SEND_SYSTEM_LOG(Error, <<"Write failed with systemError: "<<sysErr.what());
            }
        }
    };

#else
    //------------------------------------------------------------
    // Unsafe sendpolicy - simulates a very unreliable network
    //------------------------------------------------------------
    struct UnreliableSender
    {
        static const int LossPercent        = 3;
        static const int DuplicatePercent   = 3;
        static const int ReorderPercent     = 3;

        void Send(std::vector< boost::asio::const_buffer > bufs,
                  boost::asio::ip::udp::socket& socket,
                  const boost::asio::ip::udp::endpoint& to)
        {
            static Utilities::Random random(0, 100);

            if (!m_reorderBuf.empty()) //we have something to reorder
            {
                socket.send_to(bufs, to);
                socket.send_to(boost::asio::buffer(m_reorderBuf), to);
                m_reorderBuf.clear();
                return;
            }

            auto n=random.Get();
            if (Loss(n))
            {
                return;
            }
            else if (Duplicate(n))
            {
                socket.send_to(bufs, to);
                socket.send_to(bufs, to);
                socket.send_to(bufs, to);
            }
            else if (Reorder(n))
            {
                m_reorderBuf.clear();
                m_reorderBuf.resize(boost::asio::buffer_size(bufs));
                boost::asio::buffer_copy(boost::asio::buffer(m_reorderBuf), bufs);
            }
            else //normal send
            {
                socket.send_to(bufs, to);
            }
        }

    private:
        std::vector<char> m_reorderBuf{};

        inline bool Loss(int n) const {return n<LossPercent;} //0 - 5
        inline bool Duplicate(int n) const {return n>=LossPercent && n<(LossPercent+DuplicatePercent);} // 10 - 15
        inline bool Reorder(int n) const {return n>=(LossPercent+DuplicatePercent) && n<(LossPercent+DuplicatePercent+ReorderPercent);} //5 - 10
    };

    template <class T>
    struct BasicSendPolicy : private UnreliableSender
    {
        void Send(const boost::shared_ptr<T>& val,
                  boost::asio::ip::udp::socket& socket,
                  const boost::asio::ip::udp::endpoint& to)
        {
            //calculate crc and add it last in sendBuffer
            boost::crc_32_type crc;
            crc.process_bytes(static_cast<const void*>(val.get()), sizeof(T));
            uint32_t crc32=crc.checksum();
            std::vector< boost::asio::const_buffer > bufs;
            bufs.push_back(boost::asio::buffer(static_cast<const void*>(val.get()), sizeof(T)));
            bufs.push_back(boost::asio::buffer(reinterpret_cast<const char*>(&crc32), sizeof(uint32_t)));
            UnreliableSender::Send(bufs, socket, to);
        }
    };

    template <>
    struct BasicSendPolicy<UserData> : private UnreliableSender
    {
        void Send(const UserDataPtr& val,
                  boost::asio::ip::udp::socket& socket,
                  const boost::asio::ip::udp::endpoint& to)
        {
            const char* header=reinterpret_cast<const char*>(&(val->header));
            boost::crc_32_type crc;
            std::vector< boost::asio::const_buffer > bufs;

            bufs.push_back(boost::asio::buffer(header, MessageHeaderSize));
            crc.process_bytes(static_cast<const void*>(header), MessageHeaderSize);

            if (val->header.fragmentContentSize>0)
            {
                bufs.push_back(boost::asio::buffer(val->fragment, val->header.fragmentContentSize));
                crc.process_bytes(static_cast<const void*>(val->fragment), val->header.fragmentContentSize);
            }

            uint32_t crc32=crc.checksum();
            bufs.push_back(boost::asio::buffer(reinterpret_cast<const char*>(&crc32), sizeof(uint32_t)));
            UnreliableSender::Send(bufs, socket, to);
        }
    };
    //------------------------------------------------------------
#endif

    /**
     * The writer class is responsible for sending data using asynchronous UDP unicast or multicast.
     * It handles the socket setup and add an abstraction level to the send process that makes other parts of
     * Communication easier to test.
     */
    template <class T, class SendPolicy=BasicSendPolicy<T> >
    class Writer : private SendPolicy, private boost::noncopyable
    {
    public:
        typedef boost::shared_ptr<T> Ptr;

        Writer(boost::asio::io_service& ioService, int protocol)
            :m_socket(ioService,  Utilities::Protocol(protocol))
            ,m_multicastEndpoint()
            ,m_multicastEnabled(false)
        {
            m_socket.set_option(boost::asio::ip::udp::socket::reuse_address(true));
            m_socket.set_option(boost::asio::socket_base::send_buffer_size(Parameters::SocketBufferSize));
        }

        Writer(boost::asio::io_service& ioService,
               int protocol,
               const std::string& localIf,
               const std::string& multicastAddress)
            :m_socket(ioService, Utilities::Protocol(protocol))
            ,m_multicastEndpoint(Utilities::CreateEndpoint(multicastAddress))
            ,m_multicastEnabled(!multicastAddress.empty())
        {
            m_socket.set_option(boost::asio::ip::udp::socket::reuse_address(true));
            m_socket.set_option(boost::asio::socket_base::send_buffer_size(Parameters::SocketBufferSize));
            if (m_multicastEnabled)
            {
                m_socket.set_option(boost::asio::ip::multicast::outbound_interface(Utilities::CreateEndpoint(localIf).address().to_v4())); //TODO: Test this code with IP6 and figure out how to specify outbound_if for ip6
                m_socket.set_option(boost::asio::ip::multicast::enable_loopback(true));
                m_socket.set_option(boost::asio::ip::multicast::join_group(m_multicastEndpoint.address()));
            }
        }

        bool IsMulticastEnabled() const {return m_multicastEnabled;}

        void SendTo(const Ptr& val, const boost::asio::ip::udp::endpoint& to)
        {
            SendPolicy::Send(val, m_socket, to);
        }

        void SendMulticast(const Ptr& val)
        {
            SendTo(val, m_multicastEndpoint);
        }

    private:
        boost::asio::ip::udp::socket m_socket;
        boost::asio::ip::udp::endpoint m_multicastEndpoint;
        bool m_multicastEnabled;
    };
}
}
}
}
