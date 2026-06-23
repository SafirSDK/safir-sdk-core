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
    #include <iphlpapi.h>
#else
    //Linux implementation
    #include <net/if.h>
    #include <netinet/in.h>
    #include <arpa/inet.h>
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

        /**
         * Split a combined address string into its host/address part and numeric
         * port part.
         *
         * The expected input format is <tt>"address:port"</tt>.  The address portion
         * may be:
         *   - an IPv4 literal,  e.g. <tt>"192.168.0.10"</tt>
         *   - an IPv6 literal,  e.g. <tt>"[fe80::1]"</tt>  (with or without brackets)
         *   - a host name or network–interface name (the function does not validate
         *     that the string actually is resolvable here).
         *
         * @param address Input string on the form <tt>"addr:port"</tt>.
         * @param[out] ip   Receives the address/host part (untouched on failure).
         * @param[out] port Receives the numeric port (undefined on failure).
         *
         * @return Whether extraction of both parts succeeded.
         */
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

        //Windows implementation.
        //
        //Enumerate interfaces through GetAdaptersAddresses(), iterating each
        //adapter's FirstUnicastAddress list. Unlike the old
        //WSAIoctl(SIO_GET_INTERFACE_LIST) path this reliably enumerates
        //layer-3-only tunnel adapters (WireGuard/Wintun, OpenVPN TUN, etc.),
        //has no fixed cap on the number of interfaces, and surfaces both IPv4
        //and IPv6 (including secondary) addresses. It also exposes the adapter
        //FriendlyName, enabling name-based resolution as on Linux.
        static std::vector<AdapterInfo> GetAdapters()
        {
            std::vector<AdapterInfo> result;

            //Initialize Winsock defensively before calling inet_ntop() below.
            //In practice Winsock is normally already up (boost::asio does it) and
            //inet_ntop()/GetAdaptersAddresses() tend to work regardless, but this
            //static method may be called standalone (e.g. by the safir_resolver
            //tool), so we don't want to rely on that. WSAStartup is reference
            //counted, so the matching WSACleanup is harmless.
            WSADATA winsockData;
            const bool winsockStarted = (WSAStartup(MAKEWORD(2, 2), &winsockData) == 0);

            //Query the required buffer size first, then fetch into a buffer of
            //that size. Loop a few times to cope with the adapter set changing
            //between the two calls.
            const ULONG flags = GAA_FLAG_SKIP_ANYCAST | GAA_FLAG_SKIP_MULTICAST | GAA_FLAG_SKIP_DNS_SERVER;
            std::vector<unsigned char> buffer;
            ULONG bufLen = 16 * 1024;
            ULONG ret = ERROR_BUFFER_OVERFLOW;
            for (int attempt = 0; attempt < 3 && ret == ERROR_BUFFER_OVERFLOW; ++attempt)
            {
                buffer.resize(bufLen);
                auto* head = reinterpret_cast<IP_ADAPTER_ADDRESSES*>(buffer.data());
                ret = GetAdaptersAddresses(AF_UNSPEC, flags, nullptr, head, &bufLen);
            }

            if (ret != NO_ERROR)
            {
                if (winsockStarted)
                {
                    WSACleanup();
                }
                return result;
            }

            //Collect IPv4 first, then IPv6, so that for a given interface name an
            //IPv4 address is preferred over an IPv6 one (matching the Linux path
            //and the previous behaviour where exact name matches resolved to the
            //IPv4 address).
            const auto* head = reinterpret_cast<const IP_ADAPTER_ADDRESSES*>(buffer.data());
            for (const int family : {AF_INET, AF_INET6})
            {
                for (const IP_ADAPTER_ADDRESSES* aa = head; aa != nullptr; aa = aa->Next)
                {
                    //We deliberately do not filter on OperStatus. Neither the old
                    //SIO_GET_INTERFACE_LIST path nor the Linux getifaddrs() path
                    //(see #606) filter on link/operational state, so any address
                    //the OS reports for an adapter is enumerated here too.

                    //FriendlyName is a wide string; convert it to UTF-8 for
                    //name-based matching (e.g. "Ethernet", "WireGuard tunnel").
                    //FriendlyNames are user-/OS-localized and may contain
                    //non-ASCII characters, so we use a proper conversion rather
                    //than a lossy per-character narrowing.
                    std::string name;
                    if (aa->FriendlyName != nullptr)
                    {
                        const int needed = WideCharToMultiByte(CP_UTF8, 0, aa->FriendlyName, -1,
                                                               nullptr, 0, nullptr, nullptr);
                        if (needed > 0)
                        {
                            //needed includes the terminating null; size the string
                            //to the character count (needed - 1).
                            name.resize(static_cast<size_t>(needed - 1));
                            WideCharToMultiByte(CP_UTF8, 0, aa->FriendlyName, -1,
                                                &name[0], needed, nullptr, nullptr);
                        }
                    }

                    for (const IP_ADAPTER_UNICAST_ADDRESS* ua = aa->FirstUnicastAddress; ua != nullptr; ua = ua->Next)
                    {
                        const sockaddr* sa = ua->Address.lpSockaddr;
                        if (sa == nullptr || sa->sa_family != family)
                        {
                            continue;
                        }

                        char buf[INET6_ADDRSTRLEN] = {0};
                        if (family == AF_INET)
                        {
                            const auto* sin = reinterpret_cast<const sockaddr_in*>(sa);
                            if (inet_ntop(AF_INET, &sin->sin_addr, buf, sizeof(buf)) == nullptr)
                            {
                                continue;
                            }
                        }
                        else
                        {
                            const auto* sin6 = reinterpret_cast<const sockaddr_in6*>(sa);
                            if (inet_ntop(AF_INET6, &sin6->sin6_addr, buf, sizeof(buf)) == nullptr)
                            {
                                continue;
                            }
                        }

                        AdapterInfo ai;
                        ai.name = name;
                        ai.ipAddress = buf;
                        ai.ipVersion = (family == AF_INET) ? 4 : 6;
                        result.push_back(ai);
                    }
                }
            }

            if (winsockStarted)
            {
                WSACleanup();
            }

            return result;
        }

#else
        //Linux implementation.
        //
        //Enumerate the AF_INET/AF_INET6 entries from getifaddrs() directly. Every
        //interface that carries an IP address yields such an entry with both the
        //interface name and the address in a single pass, so we do not depend on a
        //link-layer (AF_PACKET) entry being present. This makes layer-3-only
        //interfaces (WireGuard, OpenVPN TUN, etc.) first-class, and also surfaces
        //secondary addresses that the old SIOCGIFADDR (primary-only) path missed.
        static std::vector<AdapterInfo> GetAdapters()
        {
            std::vector<AdapterInfo> result;

            struct ifaddrs* addrs = nullptr;
            if (getifaddrs(&addrs) == -1)
            {
                return result;
            }

            //Collect IPv4 first, then IPv6, so that for a given interface name an
            //IPv4 address is preferred over an IPv6 one (preserving the previous
            //behaviour where exact name matches resolved to the IPv4 address).
            for (const int family : {AF_INET, AF_INET6})
            {
                for (struct ifaddrs* tmp = addrs; tmp != nullptr; tmp = tmp->ifa_next)
                {
                    if (tmp->ifa_addr == nullptr || tmp->ifa_addr->sa_family != family)
                    {
                        continue;
                    }

                    char buf[INET6_ADDRSTRLEN] = {0};
                    if (family == AF_INET)
                    {
                        const auto* sa = reinterpret_cast<const struct sockaddr_in*>(tmp->ifa_addr);
                        if (inet_ntop(AF_INET, &sa->sin_addr, buf, sizeof(buf)) == nullptr)
                        {
                            continue;
                        }
                    }
                    else
                    {
                        const auto* sa = reinterpret_cast<const struct sockaddr_in6*>(tmp->ifa_addr);
                        if (inet_ntop(AF_INET6, &sa->sin6_addr, buf, sizeof(buf)) == nullptr)
                        {
                            continue;
                        }
                    }

                    AdapterInfo ai;
                    ai.name = tmp->ifa_name;
                    ai.ipAddress = buf;
                    ai.ipVersion = (family == AF_INET) ? 4 : 6;
                    result.push_back(ai);
                }
            }

            freeifaddrs(addrs);
            return result;
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
