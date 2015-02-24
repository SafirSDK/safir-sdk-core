/******************************************************************************
*
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

#ifdef _MSC_VER
    #pragma warning (push)
    #pragma warning (disable: 4996)
    #pragma warning (disable: 4267)    

    //Windows implementation
    #include <boost/asio.hpp>
    #include <boost/asio/ip/udp.hpp>    
    #include <winsock2.h>
    #include <ws2tcpip.h>

#else
    //Linux implementation
    #include <sys/ioctl.h>
    #include <net/if.h>
    #include <netinet/in.h>
    #include <sys/types.h>
    #include <ifaddrs.h>
    #include <boost/asio.hpp>
    #include <boost/asio/ip/udp.hpp>
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    class Resolver
    {
    public:
        Resolver(boost::asio::io_service& io)
            :m_resolver(io)
        {
        }

        boost::asio::ip::udp::endpoint ResolveLocalEndpoint(const std::string& expr) const
        {
            std::string ipExpr;
            unsigned short port;
            if (!SplitAddress(expr, ipExpr, port))
            {
                throw std::logic_error(std::string("COM: Resolver.ResolveLocalEndpoint could not separate ip and port: "+expr));
            }

            auto ip=GetIPAddressBestMatch(ipExpr);
            if (ip.empty())
            {
                throw std::logic_error(std::string("COM: Resolver.ResolveLocalEndpoint failed to resolve address: "+ipExpr));
            }

            return CreateEndpoint(ip, port);
        }

        boost::asio::ip::udp::endpoint ResolveRemoteEndpoint(const std::string& expr, const std::string& myAddress, int protocol) const
        {
            std::string ipExpr;
            unsigned short port;
            if (!SplitAddress(expr, ipExpr, port))
            {
                throw std::logic_error(std::string("COM: Resolver.ResolveRemoteEndpoint could not separate ip and port: "+expr));
            }

            auto addresses=DnsLookup(ipExpr, protocol);
            auto ip=FindBestMatch(myAddress, addresses);
            if (ip.empty())
            {
                std::ostringstream os;
                os<<"COM: Resolver.ResolveRemoteEndpoint failed to resolve address '"<<expr<<"' to an ip address that matches my ip: "<<myAddress;
                throw std::logic_error(os.str());
            }

            return CreateEndpoint(ip, port);
        }


#ifndef SAFIR_TEST
    private:
#endif

        struct AdapterInfo
        {
            std::string name;
            std::string ipAddress;
            int ipVersion;
        };

        mutable boost::asio::ip::udp::resolver m_resolver;

        size_t DiffIndex(const std::string& original, const std::string& val) const
        {
            size_t index=0;
            while (index<std::min(original.size(), val.size()))
            {
                if (original[index]!=val[index])
                {
                    return index;
                }

                ++index;
            }
           return index;
        }

        bool SplitAddress(const std::string& address, std::string& ip, unsigned short& port) const
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

        std::string FindBestMatch(const std::string& pattern, const std::vector<std::string>& addresses) const
        {
            size_t highScore=0;
            int bestIndex=-1;

            for (size_t i=0; i<addresses.size(); ++i)
            {
                auto tmp=DiffIndex(pattern, addresses[i]);
                if (tmp>=highScore)
                {
                    highScore=tmp;
                    bestIndex=static_cast<int>(i);
                }
            }

            if (bestIndex<0)
            {
                return ""; //no match at all
            }

            return addresses[static_cast<size_t>(bestIndex)];
        }

        std::string GetIPAddressBestMatch(const std::string& expr) const
        {
            //check if a adapter name has been specified
            auto adapters = GetAdapters();
            std::vector<std::string> addresses;

            for (const auto& ai : adapters)
            {
                if (ai.name == expr || ai.ipAddress==expr)
                {
                    return ai.ipAddress;
                }
                addresses.push_back(ai.ipAddress);
            }

            auto bestMatchingIp=FindBestMatch(expr, addresses);
            if (!bestMatchingIp.empty())
            {
                return bestMatchingIp;
            }

            //still have not found an ip we can use, try dns lookup
            auto dnsV4=DnsLookup(expr, 4);
            if (!dnsV4.empty())
            {
                return dnsV4[0];
            }

            auto dnsV6=DnsLookup(expr, 6);
            if (!dnsV6.empty())
            {
                return dnsV6[0];
            }

            return ""; //could not find any ip address to matching the expression
        }

        std::vector<std::string> DnsLookup(const std::string& hostName, int protocol) const
        {
            std::vector<std::string> result;
            boost::asio::ip::udp::resolver::query query(hostName, "");
            auto it=m_resolver.resolve(query);

            while(it!=boost::asio::ip::udp::resolver::iterator())
            {
                auto addr=(it++)->endpoint().address();

                if (protocol==4)
                {
                    if(addr.is_v4())
                    {
                        result.emplace_back(addr.to_string());
                    }
                }
                else if (protocol==6)
                {
                    if(addr.is_v6())
                    {
                        result.emplace_back(addr.to_string());
                    }
                }
            }

            return result;
        }

        boost::asio::ip::udp::endpoint CreateEndpoint(const std::string& ip, unsigned short port) const
        {
            boost::system::error_code ec;
            boost::asio::ip::address_v4 a4=boost::asio::ip::address_v4::from_string(ip, ec);
            if (!ec) //ip v4 address
            {
                return boost::asio::ip::udp::endpoint(a4, port);
            }

            boost::asio::ip::address_v6 a6=boost::asio::ip::address_v6::from_string(ip, ec);
            if (!ec) //ip v6 address
            {
                return boost::asio::ip::udp::endpoint(a6, port);
            }

            throw std::logic_error("Failed to parse '"+ip+"' as an udp endpoint.");
        }

#ifdef _MSC_VER

        //Windows implementation
        std::vector<AdapterInfo> GetAdapters() const
        {
            std::vector<AdapterInfo> result;
            WSADATA WinsockData;
            if (WSAStartup(MAKEWORD(2, 2), &WinsockData) != 0)
            {
                return result;
            }

            SOCKET sd = WSASocketW(AF_INET, SOCK_DGRAM, 0, 0, 0, 0);
            if (sd == SOCKET_ERROR)
            {
                return result;
            }

            INTERFACE_INFO InterfaceList[20];
            unsigned long nBytesReturned;
            if (WSAIoctl(sd, SIO_GET_INTERFACE_LIST, 0, 0, &InterfaceList,
                sizeof(InterfaceList), &nBytesReturned, 0, 0) == SOCKET_ERROR)
            {
                return result;
            }

            int nNumInterfaces = nBytesReturned / sizeof(INTERFACE_INFO);
            for (int i = 0; i < nNumInterfaces; ++i)
            {
                AdapterInfo ai;
                const sockaddr_in *pAddress = reinterpret_cast<const sockaddr_in*>(&(InterfaceList[i].iiAddress));
                ai.ipAddress = inet_ntoa(pAddress->sin_addr);
                ai.name = ai.ipAddress;
                ai.ipVersion = 4;
                result.push_back(ai);
            }

            WSACleanup();
            return result;
        }

#else
        std::vector<AdapterInfo> GetAdapters() const
        {
            std::vector<AdapterInfo> result;

            auto names=GetInterfaceNames();
            for (const auto& name : names)
            {
                auto a4=GetInterfaceIpAddress(name, 4);
                if (!a4.empty())
                {
                    AdapterInfo ai;
                    ai.name=name;
                    ai.ipAddress=a4;
                    ai.ipVersion=4;
                    result.push_back(ai);
                    continue;
                }

                auto a6=GetInterfaceIpAddress(name, 6);
                if (!a6.empty())
                {
                    AdapterInfo ai;
                    ai.name=name;
                    ai.ipAddress=a6;
                    ai.ipVersion=6;
                    result.push_back(ai);
                    continue;
                }
            }

            return result;
        }

        //Linux implementation
        std::vector<std::string> GetInterfaceNames() const
        {
            std::vector<std::string> result;
            struct ifaddrs *addrs,*tmp;
            getifaddrs(&addrs);
            tmp = addrs;
            while (tmp)
            {
                if (tmp->ifa_addr && tmp->ifa_addr->sa_family == AF_PACKET)
                {
                    result.push_back(std::string(tmp->ifa_name));
                }
                tmp = tmp->ifa_next;
            }

            freeifaddrs(addrs);
            return result;
        }

        std::string GetInterfaceIpAddress(const std::string& interfaceName, int protocol) const
        {
            try
            {
                struct ifreq ifr;
                strcpy(ifr.ifr_name, interfaceName.c_str());
                int fd=socket((protocol==6 ? AF_INET6 : AF_INET), SOCK_DGRAM, 0);
                if (ioctl(fd, SIOCGIFADDR, &ifr)==-1)
                {
                    return "";
                }
                close(fd);

                struct sockaddr_in* ipaddr = (struct sockaddr_in*)&ifr.ifr_addr;
                return std::string(inet_ntoa(ipaddr->sin_addr));
            }
            catch(...)
            {
                return "";
            }
        }
#endif
    };
}
}
}
}

#ifdef _MSC_VER
#pragma warning (pop)
#endif
