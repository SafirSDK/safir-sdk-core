/******************************************************************************
*
* Copyright Saab AB, 2022 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@gmail.com
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

#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include "Resolver.h"
#include "Parameters.h"
#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <iterator>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    class DebugCommandServer
    {
    public:
        DebugCommandServer(boost::asio::io_context& io, unsigned int safirInst, const std::string& logPrefix)
            :m_socket(io)
            ,m_safirInst(std::to_string(safirInst))
            ,m_logPrefix(logPrefix)
        {
            auto endpoint = Resolver::StringToEndpoint("239.6.6.6:16666");

            m_socket.open(endpoint.protocol());
            m_socket.set_option(boost::asio::ip::udp::socket::reuse_address(true));
            m_socket.set_option(boost::asio::ip::multicast::enable_loopback(true));

            //to join mcGroup with specific interface, the address must be a IPv4. Bug report https://svn.boost.org/trac/boost/ticket/3247
            m_socket.bind(boost::asio::ip::udp::endpoint(boost::asio::ip::address_v4::any(), endpoint.port())); //bind to all interfaces and the multicast port

            //boost::asio::ip::make_address("0.0.0.0").to_v4()
            m_socket.set_option(boost::asio::ip::multicast::join_group(endpoint.address().to_v4())); //join group on specific interface

            //start receiving
            AsyncReceive();
        }

        void Stop()
        {
            m_socket.close();
        }

    private:
        std::array<char, 1024> m_buf;
        boost::asio::ip::udp::socket m_socket;
        std::string m_safirInst;
        std::string m_logPrefix;

        void AsyncReceive()
        {
            m_socket.async_receive(boost::asio::buffer(m_buf, m_buf.size()),
                                   [this](const boost::system::error_code& ec, size_t size)
            {
                if (ec)
                {
                    return;
                }

                std::string cmd(m_buf.data(), m_buf.data() + size);
                auto network = ParseCommand(cmd);


                if (network == 1)
                {
                    lllog(1)<<m_logPrefix.c_str()<<L"DebugCommand - Network is now enabled"<<std::endl;
                    Parameters::NetworkEnabled = true;
                }
                else if (network == 0)
                {
                    lllog(1)<<m_logPrefix.c_str()<<L"DebugCommand - Network is now disabled"<<std::endl;
                    Parameters::NetworkEnabled = false;
                }

                AsyncReceive();
            });
        }

        int ParseCommand(const std::string& cmd) const
        {
            std::istringstream iss(cmd);
            std::vector<std::string> tokens;
            std::copy(std::istream_iterator<std::string>(iss),
                      std::istream_iterator<std::string>(),
                      std::back_inserter(tokens));

            if (tokens.size() < 2)
            {
                return -1;
            }

            for (const auto& t : tokens)
            {
                if (t == m_safirInst)
                {
                    return tokens[0] == "up" ? 1 : (tokens[0] == "down" ? 0 : -1);
                }
            }

            return -1;
        }
    };
}
}
}
}

#ifdef _MSC_VER
#pragma warning (pop)
#endif
