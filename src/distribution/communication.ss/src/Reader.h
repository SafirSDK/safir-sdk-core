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
#ifndef __SAFIR_DOB_COMMUNICATION_READER_H__
#define __SAFIR_DOB_COMMUNICATION_READER_H__

#include <boost/noncopyable.hpp>
#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/function.hpp>
#include "Parameters.h"
#include "Node.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    /**
     * The Reader class is responsible for receiving data on both unicast and multicast.
     * All received messages are passed to onRecv-callback. Reader is unaware of sequenceNumbers and fragments.
     * If callback onRecv is returning false it means that there is maximum number of messages waiting to be retrieved
     * by the application and in that case Reader will sleep until callback isReceiverReady is returning true again.
     */
    class Reader : private boost::noncopyable
    {
    public:
        Reader(const boost::shared_ptr<boost::asio::io_service>& ioService,
               const std::string& unicastAddress,
               const std::string& multicastAddress, //empty if not using multicast
               const std::function<bool(const char*, size_t)>& onRecv,
               const std::function<bool(void)>& isReceiverIsReady);

        void Start();
        void Stop();
        boost::asio::io_service::strand& Strand() {return m_strand;}

#ifndef SAFIR_TEST
    private:
#endif
        boost::asio::io_service::strand m_strand;
        boost::asio::steady_timer m_timer;
        std::function<bool(const char*, size_t)> m_onRecv;
        std::function<bool(void)> m_isReceiverReady;
        boost::shared_ptr<boost::asio::ip::udp::socket> m_socket;
        boost::shared_ptr<boost::asio::ip::udp::socket> m_multicastSocket;
        bool m_running;

        char m_bufferUnicast[Parameters::ReceiveBufferSize];
        char m_bufferMulticast[Parameters::ReceiveBufferSize];

        void AsyncReceive(char* buf, boost::asio::ip::udp::socket* socket);
        void HandleReceive(const boost::system::error_code& error, size_t bytesRecv,char* buf, boost::asio::ip::udp::socket* socket);
        void BindSocket(boost::shared_ptr<boost::asio::ip::udp::socket>& socket, int ipv, unsigned short mcPort);
        void SetWakeUpTimer(char* buf, boost::asio::ip::udp::socket* socket);
        void WakeUpAfterSleep(const boost::system::error_code& error, char* buf, boost::asio::ip::udp::socket* socket);
    };
}
}
}
}

#endif
