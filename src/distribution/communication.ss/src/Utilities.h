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
#ifndef __SAFIR_DOB_COMMUNICATION_UTILIITIES_H__
#define __SAFIR_DOB_COMMUNICATION_UTILIITIES_H__

#include <string>
#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio/ip/udp.hpp>

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
namespace Utilities
{
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

    inline boost::asio::ip::udp::endpoint CreateEndpoint(const std::string& address, int& ipVersion)
    {
        std::string addr;
        unsigned short port=0;
        if (!SplitAddress(address, addr, port))
        {
            throw std::logic_error("Failed to parse '"+address+"' as an udp endpoint with port_number on form <ip>:<port>");
        }

        boost::system::error_code ec;
        boost::asio::ip::address_v4 a4=boost::asio::ip::address_v4::from_string(addr, ec);
        if (!ec) //ip v4 address
        {
            ipVersion=4;
            return boost::asio::ip::udp::endpoint(a4, port);
        }

        boost::asio::ip::address_v6 a6=boost::asio::ip::address_v6::from_string(addr, ec);
        if (!ec) //ip v6 address
        {
            ipVersion=6;
            return boost::asio::ip::udp::endpoint(a6, port);
        }

        throw std::logic_error("Failed to parse '"+address+"' as an udp endpoint.");
    }

    inline boost::asio::ip::udp::endpoint CreateEndpoint(const std::string& address)
    {
        if (address.empty())
        {
            return boost::asio::ip::udp::endpoint();
        }

        int dummy;
        return CreateEndpoint(address, dummy);
    }

    inline boost::asio::ip::udp::endpoint::protocol_type Protocol(int p)
    {
        if (p==4)
        {
            return boost::asio::ip::udp::v4();
        }
        else if (p==6)
        {
            return boost::asio::ip::udp::v6();
        }
        throw std::logic_error("Invalid ip protocol. IPv4 and IPv6 supported.");
    }

    inline int Protocol(const std::string& address)
    {
        std::string addr;
        unsigned short port=0;
        if (!SplitAddress(address, addr, port))
        {
            throw std::logic_error("Failed to parse '"+address+"' as an udp endpoint with port_number on form <ip>:<port>");
        }

        boost::system::error_code ec;
        boost::asio::ip::address_v4::from_string(addr, ec);
        if (!ec) //ip v4 address
        {
            return 4;
        }

        boost::asio::ip::address_v6::from_string(addr, ec);
        if (!ec) //ip v6 address
        {
            return 6;
        }

        throw std::logic_error("Failed to parse '"+address+"' as an udp endpoint.");
    }
}
}
}
}
}

#endif
