/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
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

#include <boost/lexical_cast.hpp>
#include <boost/asio.hpp>
#include <boost/asio/ip/tcp.hpp>

namespace IpAddressHelper
{
    //Split addrss into ip and port. Indata on form "addr:port"
    inline bool SplitAddress(const std::string& address, std::string& ip, unsigned short& port)
    {
        size_t startPortSearch=address.find_last_of(']'); //if ip6, start search after address end
        if (startPortSearch==address.npos)
        {
            startPortSearch=0; //not found, then we search from beginning
        }
        size_t index=address.find_first_of(':', startPortSearch);

        if (index==address.npos)
        {
            ip=address;
            return false; //no port found
        }

        ip=address.substr(0, index);
        try
        {
            port=boost::lexical_cast<unsigned short>(address.substr(index+1));
        }
        catch (const boost::bad_lexical_cast&)
        {
            return false;
        }

        return true;
    }

    //Create and endpoint from ip and port.
    inline boost::asio::ip::tcp::endpoint CreateEndpoint(const std::string& ip, unsigned short port)
    {
        if (ip.empty())
        {
            return boost::asio::ip::tcp::endpoint(boost::asio::ip::tcp::v4(), port);
        }

        boost::system::error_code ec;
        auto a4 = boost::asio::ip::make_address_v4(ip, ec);
        if (!ec) //ip v4 address
        {
            return boost::asio::ip::tcp::endpoint(a4, port);
        }

        auto a6 = boost::asio::ip::make_address_v6(ip, ec);
        if (!ec) //ip v6 address
        {
            return boost::asio::ip::tcp::endpoint(a6, port);
        }

        throw std::logic_error("Failed to parse '"+ip+"' as an tcp endpoint.");
    }
}
