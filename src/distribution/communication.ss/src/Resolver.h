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
    #include <winsock2.h>
    #include <ws2tcpip.h>
    #include <boost/asio.hpp>
    #include <boost/asio/ip/udp.hpp>    
    #include <boost/lexical_cast.hpp>

#else
    //Linux implementation
    #include <sys/ioctl.h>
    #include <net/if.h>
    #include <netinet/in.h>
    #include <sys/types.h>
    #include <ifaddrs.h>
    #include <boost/asio.hpp>
    #include <boost/asio/ip/udp.hpp>
    #include <boost/lexical_cast.hpp>
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
         * @param io [in] - Reference to io_service, needed to make dns lookups.
         */
        Resolver(boost::asio::io_service& io, bool verbose=false)
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
         * @throw Throws logic_error of expr could not be resolved.
         * @return Resolved address as a string on form <ip_address>:<port>
         */
        std::string ResolveLocalEndpoint(const std::string& expr) const
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
                std::cout<<"Candidates after DNS lookup:"<<std::endl;
                for (auto s = addresses.cbegin(); s != addresses.cend(); ++s)
                {
                    std::cout<<"  " << *s << std::endl;
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

        //Get first index at which val differs from original
        static size_t DiffIndex(const std::string& original, const std::string& val)
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

        //Splig addrss into ip and port. Indata on form "addr:port"
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

        //Match all addresses against pattern and return best match.
        std::string FindBestMatch(const std::string& pattern, const std::vector<std::string>& addresses) const
        {
            size_t highScore=0;
            int bestIndex=-1;

            for (size_t i=0; i<addresses.size(); ++i)
            {
                auto tmp=DiffIndex(pattern, addresses[i]);
                if (tmp>highScore)
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

        //Get the ip address of local machine that best matches expr.
        std::string GetIPAddressBestMatch(const std::string& expr) const
        {
            //check if a adapter name has been specified
            auto adapters = GetAdapters();

            if (m_verbose)
            {
                std::cout<<"Own interface addresses available:"<<std::endl;
                for (auto a = adapters.cbegin(); a != adapters.cend(); ++a)
                {
                    std::cout<<"  "<<a->ipAddress<<std::endl;
                }
            }

            std::vector<std::string> addresses;

            for (auto ai = adapters.cbegin(); ai != adapters.cend(); ++ai)
            {
                if (ai->name == expr || ai->ipAddress==expr)
                {
                    return ai->ipAddress;
                }
                addresses.push_back(ai->ipAddress);
            }

            auto bestMatchingIp=FindBestMatch(expr, addresses);
            if (!bestMatchingIp.empty())
            {
                return bestMatchingIp;
            }

            std::vector<std::string> allSupportingAddresses = DnsLookup(expr, 46);

            for (auto addr = allSupportingAddresses.cbegin(); addr != allSupportingAddresses.cend(); ++addr )
            {
                if (m_verbose)
                {
                    std::cout<<" DNS lookup: "<< *addr <<std::endl;
                }

                auto found=std::find(addresses.begin(), addresses.end(), *addr);
                if (found!=addresses.end())
                {
                    return *found;
                }
            }

            return ""; //could not find any ip address to matching the expression
        }

        //Make dns lookup and retun list of all ip addresses that support specified protocol
        //protocol=46 means both 4 and 6
        std::vector<std::string> DnsLookup(const std::string& hostName, int protocol) const
        {
            std::vector<std::string> result;
            boost::asio::ip::udp::resolver::query query(hostName, "");
            boost::system::error_code ec;
            auto it=m_resolver.resolve(query, ec);
            if (ec)
            {
                throw std::logic_error(std::string("COM: DnsLookup failed. Host not found ")+hostName);
            }

            while(it!=boost::asio::ip::udp::resolver::iterator())
            {
                auto addr=(it++)->endpoint().address();

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

            throw std::logic_error("COM: Failed to parse '"+ip+"' as an udp endpoint.");
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

            for (int ipVersion=4; ipVersion<=6; ipVersion+=2)
            {
                auto dns=DnsLookup(boost::asio::ip::host_name(), ipVersion);
                if (!dns.empty())
                {
                    AdapterInfo ai;
                    ai.name=boost::asio::ip::host_name();
                    ai.ipAddress=dns[0];
                    ai.ipVersion=ipVersion;
                    result.push_back(ai);
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
