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
#include <Safir/Utilities/SteadyTimer.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "Node.h"
#include "Message.h"
#include "MessageQueue.h"

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
            socket.async_send_to(boost::asio::buffer(static_cast<const void*>(val.get()), sizeof(T)),
                                 to,
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
            std::vector< boost::asio::const_buffer > bufs;
            const char* header=reinterpret_cast<const char*>(&(val->header));
            bufs.push_back(boost::asio::buffer(header, MessageHeaderSize));
            bufs.push_back(boost::asio::buffer(val->fragment, val->header.fragmentContentSize));
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

        Writer(boost::asio::io_service& ioService, const Node& me)
            :m_me(me)
            ,m_socket(ioService, m_me.Endpoint().protocol())
        {
            m_socket.set_option(boost::asio::ip::udp::socket::reuse_address(true));
            if (m_me.IsMulticastEnabled())
            {
                m_multicastEndpoint=Node::CreateEndpoint(m_me.MulticastAddress(), m_me.Endpoint().port());
                m_socket.set_option(boost::asio::ip::multicast::enable_loopback(true));
                m_socket.set_option(boost::asio::ip::multicast::join_group(m_multicastEndpoint.address()));
            }
        }

        void SendTo(const Ptr& val, const boost::asio::ip::udp::endpoint& to)
        {
            SendPolicy::Send(val, m_socket, to);
        }

        void SendMulticast(const Ptr& val)
        {
            SendTo(val, m_multicastEndpoint);
        }

        void SendToAllSystemNodes(const Ptr& val, const NodeMap& nodes)
        {
            if (m_me.IsMulticastEnabled())
            {
                bool sendMulticast=false;
                for (auto& vt : nodes)
                {
                    if (vt.second.IsSystemNode())
                    {
                        if (vt.second.IsMulticastEnabled())
                        {
                            sendMulticast=true;
                        }
                        else
                        {
                            SendTo(val, vt.second.Endpoint());
                        }
                    }
                }

                if (sendMulticast)
                {
                    SendMulticast(val);
                }
            }
            else
            {
                for (auto& vt : nodes)
                {
                    if (vt.second.IsSystemNode())
                    {
                        SendTo(val, vt.second.Endpoint());
                    }
                }
            }
        }

    private:
        Node m_me;
        boost::asio::ip::udp::endpoint m_multicastEndpoint;
        boost::asio::ip::udp::socket m_socket;
    };
}
}
}
}

#endif
