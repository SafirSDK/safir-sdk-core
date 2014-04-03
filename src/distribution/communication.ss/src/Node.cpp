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
#include <iostream>
#include <boost/lexical_cast.hpp>
#include "Node.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    Node::Node(const std::string& name_,
               boost::int64_t id_,
               const std::string& unicastAddr,
               const std::string& multicastAddr)
        :m_name(name_)
        ,m_id(id_)
        ,m_unicastAddress(unicastAddr)
        ,m_multicastAddress(multicastAddr)
        ,m_ipVersion(V4)
        ,m_unicastEndpoint(Node::CreateEndpoint(unicastAddr, m_ipVersion))
        ,m_multicastEnabled(!multicastAddr.empty())
        ,m_systemNode(false)
        ,m_lastSentUnicastSeqNo(0)
    {
    }

    std::string Node::ToString() const
    {
        std::ostringstream ss;
        ss<<"["<<m_id<<"]"<<""<<m_name<<" Uc<"<<m_unicastAddress<<">";
        if (m_multicastEnabled)
            ss<<" M ";
        if (m_systemNode)
            ss<<" S";
        return ss.str();
    }

    bool Node::SplitAddress(const std::string& address, std::string& ip, unsigned short& port)
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

    boost::asio::ip::udp::endpoint Node::CreateEndpoint(const std::string& address, IpVersionType& version, unsigned short defaultPort)
    {
        //Note: defaultPort=0 means that no default port exists.
        //Default port will be used if address does not contain a port number. If address contains port number, defaultPort will not be used.

        std::string addr;
        unsigned short port=0;
        if (!SplitAddress(address, addr, port))
        {
            if (defaultPort>0)
            {
                port=defaultPort;
            }
            else
            {
                throw std::logic_error("Failed to parse '"+address+"' as an udp endpoint with port_number on form <ip>:<port>");
            }
        }

        boost::system::error_code ec;
        boost::asio::ip::address_v4 a4=boost::asio::ip::address_v4::from_string(addr, ec);
        if (!ec) //ip v4 address
        {
            version=V4;
            return boost::asio::ip::udp::endpoint(a4, port);
        }

        boost::asio::ip::address_v6 a6=boost::asio::ip::address_v6::from_string(addr, ec);
        if (!ec) //ip v6 address
        {
            version=V6;
            return boost::asio::ip::udp::endpoint(a6, port);
        }

        throw std::logic_error("Failed to parse '"+address+"' as an udp endpoint.");
    }

    boost::asio::ip::udp::endpoint Node::CreateEndpoint(const std::string& address,
                                                         unsigned short defaultPort)
    {
        IpVersionType dummy;
        return CreateEndpoint(address, dummy, defaultPort);
    }

}
}
}
}
