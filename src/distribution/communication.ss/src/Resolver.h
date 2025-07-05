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

#include <iostream>
#include <boost/lexical_cast.hpp>
#include <boost/asio.hpp>
#include <boost/asio/ip/udp.hpp>


#ifdef _MSC_VER
#  pragma warning(push)
#  pragma warning (disable: 4005)
#endif

#include <boost/regex.hpp>

#ifdef _MSC_VER
#  pragma warning(pop)
#endif


#ifdef _MSC_VER
    #pragma warning (push)
    #pragma warning (disable: 4996)
    #pragma warning (disable: 4267)

    //Windows implementation
    #include <winsock2.h>
    #include <ws2tcpip.h>
#else
    //Linux implementation
    #include <sys/ioctl.h>
    #include <net/if.h>
    #include <netinet/in.h>
    #include <sys/types.h>
    #include <ifaddrs.h>
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    /**
     * The Resolver class is responsible for resolving hostnames and other expressions to endpoints, i.e address and port.
     */
    class Resolver
    {
    public:

        /**
         * Constructor
         *
         * @param io [in] - Reference to io_context, needed to make dns lookups.
         */
        Resolver(boost::asio::io_context& io, bool verbose=false)
            :m_resolver(io)
            ,m_verbose(verbose)
        {
        }

        /**
         * @brief ResolveLocalEndpoint - Resolve an expresson to best matching endpoint. Will look at all network adapters and adapter names.
         *                              Will also make dns lookup if needed. If expr has the form of an ip address the best matching ip address
         *                              will be selected. I.e if expr="192.168.0.0:10000" and the computer has two network adapters with ip
         *                              addresses {127.0.0.1, 192.168.100.100}, the adapter with address "192.168.100.100" will be chosen.
         * @param expr [in] - Expression that can be a hostname, localhost, adapter name (ex: eth0), ip-address. Must end with port number,
         *                    For example ip:port, adapter_name:port, host_name:port.
         * @return Resolved address as a string on form <ip_address>:<port> or empty string if resolution failed.
         */
        static std::string ResolveLocalEndpoint(const std::string& expr, const bool verbose = false)
        {
            std::string ipExpr;
            unsigned short port;
            if (!SplitAddress(expr, ipExpr, port))
            {
                throw std::logic_error(std::string("COM: Resolver.ResolveLocalEndpoint could not separate ip and port: "+expr));
            }

            auto ip=GetIPAddressBestMatch(ipExpr, verbose);
            if (ip.empty())
            {
                // one more try
                ip=GetIPAddressBestMatch(ipExpr, verbose);
            }
            if (ip.empty())
            {
                return "";
            }

            return ip+std::string(":")+boost::lexical_cast<std::string>(port);
        }

        /**
         * @brief ResolveRemoteEndpoint - Resolve an expresson to an endpoint. expr can have the form hostname:port or ip_address:port.
         *                              Will use dns lookup to resolve host names.
         * @param expr [in] - Expression that can be a hostname or ip-address. Must end with port number.
         * @param protocol [in] - Protocol required of the remote endpoint, 4 or 6.
         * @throw Throws logic_error of expr could not be resolved.
         * @return Resolved address as a string on form <ip_address>:<port>
         */
        std::string ResolveRemoteEndpoint(const std::string& expr, int protocol) const
        {
            std::string ipExpr;
            unsigned short port;
            if (!SplitAddress(expr, ipExpr, port))
            {
                throw std::logic_error(std::string("COM: Resolver.ResolveRemoteEndpoint could not separate ip and port: "+expr));
            }

            auto addresses=DnsLookup(ipExpr, protocol);

            if (m_verbose)
            {
                std::wcout<<"Candidates after DNS lookup:"<<std::endl;
                for (auto s = addresses.cbegin(); s != addresses.cend(); ++s)
                {
                    std::wcout<<"  " << s->c_str() << std::endl;
                }
            }

            if (addresses.empty())
            {
                std::ostringstream os;
                os<<"COM: Resolver.ResolveRemoteEndpoint failed to resolve address '"<<expr<<"'";
                throw std::logic_error(os.str());
            }

            return addresses[0]+std::string(":")+boost::lexical_cast<std::string>(port);
        }

        /**
         * @brief StringToEndpoint - Converts a string on the form ip_address:port to an endpoint. No attempts to resolve host names or adapter names.
         * @param address [in] - Address string, ip:port. Assumed to be correct.
         * @throw std::logic_error on error.
         * @return Endpoint.
         */
        static boost::asio::ip::udp::endpoint StringToEndpoint(const std::string& address)
        {
            std::string addr;
            unsigned short port=0;
            if (!SplitAddress(address, addr, port))
            {
                throw std::logic_error("COM: Failed to parse '"+address+"' as an udp endpoint with port_number on form <ip>:<port>");
            }
            return CreateEndpoint(addr, port);
        }

        /**
         * @brief Protocol - Convert number 4 or 6 to corresponding IP protocol type.
         * @param p [in] - Protocol version, valid values are 4 and 6.
         * @throw std::logic_error on error if invalid protocol is specified.
         * @return protocol_type.
         */
        static boost::asio::ip::udp::endpoint::protocol_type Protocol(int p)
        {
            if (p==4)
            {
                return boost::asio::ip::udp::v4();
            }
            else if (p==6)
            {
                return boost::asio::ip::udp::v6();
            }
            throw std::logic_error("COM: Invalid ip protocol. IPv4 and IPv6 supported.");
        }

        /**
         * @brief Protocol - Finds out protocol version of address.
         * @param address [in] - Address on form ip_address:port.
         * @throw std::logic_error on error.
         * @return Ip version 4 or 6.
         */
        static int Protocol(const std::string& address)
        {
            std::string addr;
            unsigned short port=0;
            if (!SplitAddress(address, addr, port))
            {
                throw std::logic_error("COM: Failed to parse '"+address+"' as an udp endpoint with port_number on form <ip>:<port>");
            }

            boost::system::error_code ec;
            boost::asio::ip::make_address_v4(addr, ec);
            if (!ec) //ip v4 address
            {
                return 4;
            }

            boost::asio::ip::make_address_v6(addr, ec);
            if (!ec) //ip v6 address
            {
                return 6;
            }

            throw std::logic_error("COM: Failed to parse '"+address+"' as an udp endpoint.");
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
        const bool m_verbose;


        //Split address into ip and port. Indata on form "addr:port"
        static bool SplitAddress(const std::string& address, std::string& ip, unsigned short& port)
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

        //Match all addresses against pattern and return first match
        static std::string FindBestMatch(const std::string& pattern, const std::vector<std::string>& addresses, const bool verbose)
        {
            if (pattern == "0.0.0.0")
            {
                return pattern;
            }

            //addresses may only have numbers and stars in them
            if(!boost::regex_match(pattern,boost::regex("[0-9\\*]+\\.[0-9\\*]+\\.[0-9\\*]+\\.[0-9\\*]+")))
            {
                if (verbose)
                {
                    std::wcout << "Ip addresses may only consist of numbers and stars..." << std::endl;
                }
                return "";
            }
            const auto dotsReplaced = boost::regex_replace(pattern,boost::regex("\\."),"\\\\.");
            const auto regex = boost::regex_replace(dotsReplaced,boost::regex("\\*"),".*");
            if (verbose)
            {
                std::wcout << "Converted pattern '" << pattern.c_str() << "' to regex '" << regex.c_str() << "'" << std::endl;
            }

            for (auto it = addresses.cbegin(); it != addresses.end(); ++it)
            {
                if (boost::regex_match(*it,boost::regex(regex)))
                {
                    return *it;
                }
            }
            return ""; //no match at all
        }

        //Get the ip address of local machine that best matches expr.
        static std::string GetIPAddressBestMatch(const std::string& expr, const bool verbose)
        {
            //check if a adapter name has been specified
            auto adapters = GetAdapters();

            if (verbose)
            {
                std::wcout<<L"Resolver is trying to resolve expression: "<< expr.c_str()<<std::endl;
                std::wcout<<L"Own interface addresses available:"<<std::endl;
                for (auto a = adapters.cbegin(); a != adapters.cend(); ++a)
                {
                    std::wcout<<"  "<<a->ipAddress.c_str()<<std::endl;
                }
            }

            std::vector<std::string> addresses;

            //if we have an exact match on interface name or ip address we use that
            for (auto ai = adapters.cbegin(); ai != adapters.cend(); ++ai)
            {
                if (ai->name == expr || ai->ipAddress==expr)
                {
                    if (verbose)
                    {
                        std::wcout<<L"Found exact match: "<<ai->ipAddress.c_str()<<std::endl;
                    }
                    return ai->ipAddress;
                }
                addresses.push_back(ai->ipAddress);
            }

            auto bestMatch = FindBestMatch(expr,addresses,verbose);
            if (verbose)
            {
                std::wcout<<L"Best match: "<<bestMatch.c_str()<<std::endl;
            }
            return bestMatch;
        }

        //Make dns lookup and return list of all ip addresses that support specified protocol
        //protocol=46 means both 4 and 6
        std::vector<std::string> DnsLookup(const std::string& hostName, int protocol) const
        {
            std::vector<std::string> result;
            boost::system::error_code ec;
            auto results=m_resolver.resolve(hostName, "", ec);
            if (ec)
            {
                throw std::logic_error(std::string("COM: DnsLookup failed. Host not found ")+hostName);
            }

            for(const auto& res: results)
            {
                auto addr=res.endpoint().address();

                if (protocol==4 || protocol==46)
                {
                    if(addr.is_v4())
                    {
                        result.emplace_back(addr.to_string());
                    }
                }

                if (protocol==6 || protocol==46)
                {
                    if(addr.is_v6())
                    {
                        result.emplace_back(addr.to_string());
                    }
                }
            }

            return result;
        }

        //Create and endpoint from ip and port.
        static boost::asio::ip::udp::endpoint CreateEndpoint(const std::string& ip, unsigned short port)
        {
            boost::system::error_code ec;
            boost::asio::ip::address_v4 a4=boost::asio::ip::make_address_v4(ip, ec);
            if (!ec) //ip v4 address
            {
                return boost::asio::ip::udp::endpoint(a4, port);
            }

            boost::asio::ip::address_v6 a6=boost::asio::ip::make_address_v6(ip, ec);
            if (!ec) //ip v6 address
            {
                return boost::asio::ip::udp::endpoint(a6, port);
            }

            throw std::logic_error("COM: Failed to parse '"+ip+"' as an udp endpoint.");
        }

#ifdef _MSC_VER

        //Windows implementation
        static std::vector<AdapterInfo> GetAdapters()
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
        static std::vector<AdapterInfo> GetAdapters()
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
        static std::vector<std::string> GetInterfaceNames()
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

        static std::string GetInterfaceIpAddress(const std::string& interfaceName, int protocol)
        {
            try
            {
                struct ifreq ifr;
                strncpy(ifr.ifr_name, interfaceName.c_str(), IFNAMSIZ);
                ifr.ifr_name[IFNAMSIZ - 1] = 0;
                int fd=socket((protocol==6 ? AF_INET6 : AF_INET), SOCK_DGRAM, 0);
                if (ioctl(fd, SIOCGIFADDR, &ifr)==-1)
                {
                    close(fd);
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
