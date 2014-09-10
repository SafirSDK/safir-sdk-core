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
#ifndef __SAFIR_DOB_COMMUNICATION_WRITER_H__
#define __SAFIR_DOB_COMMUNICATION_WRITER_H__

#include <map>
#include <boost/noncopyable.hpp>
#include <boost/asio.hpp>
#include <boost/function.hpp>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "Message.h"
#include "Utilities.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
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
            socket.async_send_to(bufs, to,
                                 [val](const boost::system::error_code& error, size_t){if (error) std::cout<<"Send failed, error "<<error.message().c_str()<<std::endl;});
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

            //calculate crc and add it last in sendBuffer
            boost::crc_32_type crc;
            crc.process_bytes(static_cast<const void*>(header), MessageHeaderSize);
            crc.process_bytes(static_cast<const void*>(val->fragment), val->header.fragmentContentSize);
            uint32_t crc32=crc.checksum();
            std::vector< boost::asio::const_buffer > bufs;
            bufs.push_back(boost::asio::buffer(header, MessageHeaderSize));
            bufs.push_back(boost::asio::buffer(val->fragment, val->header.fragmentContentSize));
            bufs.push_back(boost::asio::buffer(reinterpret_cast<const char*>(&crc32), sizeof(uint32_t)));
            socket.async_send_to(bufs, to, [val](const boost::system::error_code& error, size_t){if (error) std::cout<<"Send UserData failed, error "<<error.message().c_str()<<std::endl;});
        }
    };

    /**
     * The writer class is responsible for sending data using asynchronous UDP unicast or multicast.
     * It handles the socket setup and add an abstraction level to the send process that makes other parts of
     * Communication easier to test.
     */
    template <class T, class SendPolicy=BasicSendPolicy<T> >
    class Writer : private SendPolicy
    {
    public:
        typedef boost::shared_ptr<T> Ptr;

        Writer(boost::asio::io_service& ioService, int protocol)
            :m_socket(ioService,  Utilities::Protocol(protocol))
            ,m_multicastEndpoint()
            ,m_multicastEnabled(false)
        {
            m_socket.set_option(boost::asio::ip::udp::socket::reuse_address(true));
        }

        Writer(boost::asio::io_service& ioService, int protocol, const std::string& multicastAddress)
            :m_socket(ioService, Utilities::Protocol(protocol))
            ,m_multicastEndpoint(Utilities::CreateEndpoint(multicastAddress))
            ,m_multicastEnabled(!multicastAddress.empty())
        {
            m_socket.set_option(boost::asio::ip::udp::socket::reuse_address(true));
            if (m_multicastEnabled)
            {
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

#endif
