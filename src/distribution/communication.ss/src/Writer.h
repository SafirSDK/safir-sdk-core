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
#include <boost/function.hpp>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "Message.h"
#include "Utilities.h"
#include "MessageQueue.h"
#include "Parameters.h"

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
    template <class T>
    class BasicSendPolicy
    {
    public:
        void Send(const boost::shared_ptr<T>& val,
                  boost::asio::ip::udp::socket& socket,
                  boost::function<void(const boost::system::error_code& error, size_t)> completionHandler)
        {
            std::vector< boost::asio::const_buffer > bufs;
            CreateBuffer(val, bufs);
            socket.async_send(bufs, completionHandler);
        }

        void SendTo(const boost::shared_ptr<T>& val,
                    boost::asio::ip::udp::socket& socket,
                    const boost::asio::ip::udp::endpoint& endpoint)
        {
            std::vector< boost::asio::const_buffer > bufs;
            CreateBuffer(val, bufs);
            socket.send_to(bufs, endpoint);
        }

    private:
        boost::crc_32_type m_crcComputer;
        uint32_t m_crc32Value;

        inline void CreateBuffer(const boost::shared_ptr<T>& val, std::vector<boost::asio::const_buffer>& bufs)
        {
            m_crcComputer.reset();
            m_crcComputer.process_bytes(static_cast<const void*>(val.get()), sizeof(T));
            m_crc32Value=m_crcComputer.checksum();

            bufs.push_back(boost::asio::buffer(static_cast<const void*>(val.get()), sizeof(T)));
            bufs.push_back(boost::asio::buffer(reinterpret_cast<const char*>(&m_crc32Value), sizeof(uint32_t)));
        }
    };

    template <>
    class BasicSendPolicy<UserData>
    {
    public:
        void Send(const UserDataPtr& val,
                  boost::asio::ip::udp::socket& socket,
                  boost::function<void(const boost::system::error_code& error, size_t)> completionHandler)
        {
            std::vector< boost::asio::const_buffer > bufs;
            CreateBuffer(val, bufs);
            socket.async_send(bufs, completionHandler);
        }

        void SendTo(const UserDataPtr& val,
                    boost::asio::ip::udp::socket& socket,
                    const boost::asio::ip::udp::endpoint& endpoint)
        {
            std::vector< boost::asio::const_buffer > bufs;
            CreateBuffer(val, bufs);
            socket.send_to(bufs, endpoint);
        }

    private:
        boost::crc_32_type m_crcComputer;
        uint32_t m_crc32Value;

        inline void CreateBuffer(const UserDataPtr& val, std::vector<boost::asio::const_buffer>& bufs)
        {
            const char* header=reinterpret_cast<const char*>(&(val->header));

            m_crcComputer.reset();
            m_crcComputer.process_bytes(static_cast<const void*>(header), MessageHeaderSize);
            m_crcComputer.process_bytes(static_cast<const void*>(val->fragment), val->header.fragmentContentSize);
            m_crc32Value=m_crcComputer.checksum();

            bufs.push_back(boost::asio::buffer(header, MessageHeaderSize));
            bufs.push_back(boost::asio::buffer(val->fragment, val->header.fragmentContentSize));
            bufs.push_back(boost::asio::buffer(reinterpret_cast<const char*>(&m_crc32Value), sizeof(uint32_t)));
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

        //--------------------------------------------------
        // Synchronous writer
        //--------------------------------------------------
        Writer(boost::asio::io_service& ioService, int protocol)
            :m_strand(nullptr)
            ,m_socket(ioService, Utilities::Protocol(protocol))
            ,m_sendBuf(0)
        {
            m_socket.set_option(boost::asio::ip::udp::socket::reuse_address(true));
        }

        void SendTo(const Ptr& val, const boost::asio::ip::udp::endpoint& endpoint)
        {
            SendPolicy::SendTo(val, m_socket, endpoint);
        }

        //--------------------------------------------------
        // Asynchronous writer
        //--------------------------------------------------
        Writer(boost::asio::io_service::strand& strand, const std::string& localIf, const std::string& receiver)
            :m_strand(&strand)
            ,m_endpoint(Utilities::CreateEndpoint(receiver))
            ,m_socket(strand.get_io_service(),  m_endpoint.protocol())
            ,m_sendBuf(Parameters::SlidingWindowSize)
        {
            m_socket.set_option(boost::asio::ip::udp::socket::reuse_address(true));

            if (m_endpoint.address().is_multicast())
            {
                auto localEndpoint=Utilities::CreateEndpoint(localIf);
                m_socket.set_option(boost::asio::ip::multicast::outbound_interface(localEndpoint.address().to_v4())); //TODO: Test this code with IP6 and figure out how to specify outbound_if for ip6
                m_socket.set_option(boost::asio::ip::multicast::enable_loopback(true));
                m_socket.set_option(boost::asio::ip::multicast::join_group(m_endpoint.address()));
            }

            m_socket.connect(m_endpoint);
        }

        //Send must be called from the same strand that was passed to constructor
        bool Send(const Ptr& val)
        {
            if (m_sendBuf.full())
            {
                return false; //could not send, to much to do already
            }

            auto empty=m_sendBuf.empty();
            m_sendBuf.enqueue(val);

            if (empty) //if there was no ongoing send before, we have to call send. If there was its an error to call send again.
            {
                SendAsync();
            }
            return true; //message will be sent
        }

    private:
        boost::asio::io_service::strand* m_strand;
        boost::asio::ip::udp::endpoint m_endpoint;
        boost::asio::ip::udp::socket m_socket;
        MessageQueue<Ptr> m_sendBuf;

        void SendAsync()
        {
            if (!m_sendBuf.empty())
            {
                SendPolicy::Send(m_sendBuf.front(), m_socket,
                                 m_strand->wrap([=](const boost::system::error_code& error, size_t size){OnSendCompleted(error, size);}));
            }
        }

        void OnSendCompleted(const boost::system::error_code& error, size_t)
        {
            if (error)
            {
                std::cout<<"Send failed, error "<<error.message().c_str()<<std::endl;
                SEND_SYSTEM_LOG(Error, <<"Send failed, error "<<error.message().c_str());
                throw std::logic_error(error.message());
            }

            m_sendBuf.front().reset();
            m_sendBuf.dequeue();
            if (!m_sendBuf.empty())
            {
                SendAsync();
            }
        }
    };
}
}
}
}

#endif
