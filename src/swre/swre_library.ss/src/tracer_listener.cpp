/******************************************************************************
*
* Copyright Saab AB, 2012,2014,2022 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include <Safir/Application/Internal/TracerData.h>

#include <boost/asio.hpp>
#include <boost/asio/ip/multicast.hpp>
#include <boost/program_options.hpp>

#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <boost/optional.hpp>
#include <string>
#include <unordered_map>
#include <vector>
#include <regex>
#include <algorithm>

#if _MSC_VER
#   include <iphlpapi.h>
#else                           // POSIX – automatic interface enumeration
#   include <ifaddrs.h>
#   include <net/if.h>
#endif
#include <chrono>
#include <iomanip>
#include <sstream>

using boost::asio::ip::udp;

#if defined _MSC_VER
//and disable a warning that happens on instantiation
#  pragma warning (disable : 4505)
#endif


namespace {

    /* ---------- command-line parsing --------------------------------------------------------- */

    struct Args
    {
        std::string          group        = "224.11.11.12";
        unsigned short       port         = 49500;
        std::string          bindAddr     = "0.0.0.0";
        std::vector<std::string> joinAddrs{ "0.0.0.0", "127.0.0.1" };
        boost::optional<int64_t> filterInc;
        boost::optional<int64_t>  filterSender;
        boost::optional<std::regex> progRegex;
        boost::optional<std::regex> nodeRegex;
        bool                  prefixProgramName = false;   // print program name at start of every output line
        bool                  showNodeName      = false;   // prefix node name
        bool                  showSendTime      = false;   // prefix datagram send time
        bool                  showRecvTime      = false;   // prefix receive time
        bool                  csv               = false;   // emit CSV (implies all --show* flags)
        bool                  verbose           = false;   // verbose logging
    };

    Args parseArgs(int argc, char* argv[])
    {
        Args a;
        namespace po = boost::program_options;

        po::options_description desc("Options");
        desc.add_options()
            ("help,h", "Show help")
            ("group,g",      po::value<std::string>(&a.group)->default_value("224.11.11.12"), "Multicast group")
            ("port,p",       po::value<unsigned short>(&a.port)->default_value(49500),        "UDP port")
            ("bind-address,B",  po::value<std::string>(&a.bindAddr)->default_value("0.0.0.0"), "Local address to bind socket")
            ("join-addresses,J", po::value<std::vector<std::string>>()->multitoken(),          "Addresses to send multicast join (space or comma separated)")
            ("incarnation-id,i", po::value<std::int64_t>(), "Only datagrams whose incarnation id matches this number")
            ("sender-id,s",      po::value<std::int64_t>(), "Only datagrams whose sender id matches this number")
            ("program-regex",    po::value<std::string>(),  "Only datagrams whose program name matches this regex")
            ("node-regex",       po::value<std::string>(),  "Only datagrams whose node name matches this regex")
            ("show-program-name,P",
             po::bool_switch(&a.prefixProgramName),
             "Prefix each output line with the program name")
            ("show-node-name",      po::bool_switch(&a.showNodeName),    "Prefix each output line with the node name")
            ("show-send-time",      po::bool_switch(&a.showSendTime),    "Prefix each output line with the datagram send time")
            ("show-recv-time",      po::bool_switch(&a.showRecvTime),    "Prefix each output line with the local receive time")
            ("csv",                 po::bool_switch(&a.csv),             "Output as CSV (implies all --show* flags)")
            ("verbose,v",           po::bool_switch(&a.verbose),         "Verbose logging");

        po::variables_map vm;
        po::store(po::parse_command_line(argc, argv, desc), vm);
        po::notify(vm);

        if (vm.count("help"))
        {
            std::cout << "Usage: tracer_listener [options]\n" << desc << std::endl;
            std::exit(EXIT_SUCCESS);
        }

        if (vm.count("incarnation-id"))
            a.filterInc = vm["incarnation-id"].as<std::int64_t>();
        if (vm.count("sender-id"))
            a.filterSender = vm["sender-id"].as<std::int64_t>();
        if (vm.count("program-regex"))
            a.progRegex = std::regex(vm["program-regex"].as<std::string>());
        if (vm.count("node-regex"))
            a.nodeRegex = std::regex(vm["node-regex"].as<std::string>());

        if (vm.count("join-addresses"))
        {
            a.joinAddrs.clear();
            const auto& vals = vm["join-addresses"].as<std::vector<std::string>>();
            for (const std::string& v : vals)
            {
                std::stringstream ss(v);
                std::string token;
                while (std::getline(ss, token, ','))
                {
                    if (!token.empty())
                        a.joinAddrs.push_back(token);
                }
            }
        }

        if (a.csv)
        {
            a.prefixProgramName = true;
            a.showNodeName      = true;
            a.showSendTime      = true;
            a.showRecvTime      = true;
        }

        return a;
    }

    /* ---------- helper ----------------------------------------------------------------------- */

    template<typename MapT>
    void noteSequence(MapT& lastSeq, std::int64_t senderId, std::uint32_t seqNo,
                      const std::string& programName, const std::string& nodeName,
                      bool csvMode, std::vector<std::string>* csvOut,
                      const std::string& sendTimeStr, const std::string& recvTimeStr)
    {
        auto& last = lastSeq[senderId];
        auto emitDiag = [&](const std::string& msg)
        {
            if (csvMode && csvOut)
            {
                std::ostringstream row;
                row << std::quoted(sendTimeStr) << ','
                    << std::quoted(recvTimeStr) << ','
                    << std::quoted(programName) << ','
                    << std::quoted(nodeName) << ','
                    << std::quoted(msg);
                csvOut->push_back(row.str());
            }
            else
            {
                std::cout << msg << '\n';
            }
        };

        if (last)   // 0 means “unseen”
        {
            if (seqNo == last + 1)
            {
                // in order
            }
            else if (seqNo > last + 1)
            {
                std::ostringstream os;
                os << "### MISSING packet detected from " << programName << "@"
                   << nodeName << ". Expected " << (last + 1)
                   << ", got " << seqNo << " ###";
                emitDiag(os.str());
            }
            else
            {
                std::ostringstream os;
                os << "### OUT-OF-ORDER packet from " << programName << "@"
                   << nodeName << ". Got " << seqNo << " after " << last << " ###";
                emitDiag(os.str());
            }
        }
        if (seqNo > last)
            last = seqNo;
    }

    inline std::string formatIsoTime(std::uint64_t microsecSinceEpoch)
    {
        using namespace std::chrono;
        system_clock::time_point tp = system_clock::time_point{seconds(microsecSinceEpoch / 1000000)} +
                                                                                                          microseconds(microsecSinceEpoch % 1000000);
        std::time_t tt = system_clock::to_time_t(tp);
        std::tm tm{};
#ifdef _WIN32
        gmtime_s(&tm, &tt);
#else
        gmtime_r(&tt, &tm);
#endif
        std::ostringstream os;
        os << std::put_time(&tm, "%F %T")
           << '.' << std::setw(6) << std::setfill('0') << (microsecSinceEpoch % 1000000) << 'Z';
        return os.str();
    }

    /* ---------------------------------------------------------------------
     * Decide which IPv4 interfaces we should join the multicast group on.
     * If the user supplied -J the given addresses override auto-enumeration.
     * -------------------------------------------------------------------*/
    std::vector<std::string> gatherJoinInterfaces(const Args& args)
    {
        // Explicit list overrides defaults (two placeholder addrs == defaults)
        if (!args.joinAddrs.empty() &&
            !(args.joinAddrs.size() == 2 &&
              args.joinAddrs[0] == "0.0.0.0" &&
              args.joinAddrs[1] == "127.0.0.1"))
        {
            return args.joinAddrs;
        }

        std::vector<std::string> result;

#if defined(_WIN32)
        ULONG bufLen = 0;
        if (GetAdaptersAddresses(AF_INET, 0, nullptr, nullptr, &bufLen)
            == ERROR_BUFFER_OVERFLOW)
        {
            std::vector<BYTE> buffer(bufLen);
            auto* head = reinterpret_cast<IP_ADAPTER_ADDRESSES*>(buffer.data());
            if (GetAdaptersAddresses(AF_INET, 0, nullptr, head, &bufLen) == NO_ERROR)
            {
                for (auto* aa = head; aa; aa = aa->Next)
                {
                    if (aa->OperStatus != IfOperStatusUp || aa->NoMulticast)
                        continue;
                    for (auto* ua = aa->FirstUnicastAddress; ua; ua = ua->Next)
                    {
                        if (!ua->Address.lpSockaddr ||
                            ua->Address.lpSockaddr->sa_family != AF_INET)
                            continue;
                        auto* sa = reinterpret_cast<sockaddr_in*>(ua->Address.lpSockaddr);
                        boost::asio::ip::address_v4 addr(ntohl(sa->sin_addr.s_addr));
                        result.push_back(addr.to_string());
                    }
                }
            }
        }
#else   // POSIX
        struct ifaddrs* ifaddr = nullptr;
        if (getifaddrs(&ifaddr) == 0)
        {
            for (auto* ifa = ifaddr; ifa; ifa = ifa->ifa_next)
            {
                if (!ifa->ifa_addr)                                   continue;
                if ((ifa->ifa_flags & IFF_UP) == 0 ||
                    (ifa->ifa_flags & IFF_MULTICAST) == 0)            continue;
                if (ifa->ifa_addr->sa_family != AF_INET)              continue;

                auto* sa = reinterpret_cast<sockaddr_in*>(ifa->ifa_addr);
                boost::asio::ip::address_v4 addr(ntohl(sa->sin_addr.s_addr));
                result.push_back(addr.to_string());
            }
            freeifaddrs(ifaddr);
        }
#endif
        // Always include loopback interface so we can receive packets even if
        // the machine has no other suitable multicast-capable interfaces.
        if (std::find(result.begin(), result.end(), "127.0.0.1") == result.end())
        {
            result.push_back("127.0.0.1");
        }
        return result;
    }

} // anonymous namespace

/* ========================================================================================= */

int main(int argc, char* argv[])
{
    try
    {
        const Args args = parseArgs(argc, argv);

        boost::asio::io_context io;
        udp::endpoint listenEndpoint(boost::asio::ip::address::from_string(args.bindAddr),
                                     args.port);

        udp::socket sock(io);
        sock.open(listenEndpoint.protocol());
        sock.set_option(boost::asio::ip::udp::socket::reuse_address(true));
        sock.bind(listenEndpoint);

        const auto multicastAddr = boost::asio::ip::address_v4::from_string(args.group);

        std::vector<std::string> joinTargets = gatherJoinInterfaces(args);
        std::vector<std::string> joinedIfaces;
        for (const auto& ipStr : joinTargets)
        {
            try
            {
                sock.set_option(
                    boost::asio::ip::multicast::join_group(
                        multicastAddr,
                        boost::asio::ip::address_v4::from_string(ipStr)));
                joinedIfaces.push_back(ipStr);
            }
            catch (...) { /* ignore */ }
        }

        if (joinedIfaces.empty())
        {
            std::cerr << "Warning: failed to join multicast group on any interface\n";
        }

        if (args.verbose)
        {
            std::cerr << "Listening on " << args.group << ":" << args.port
                      << " (bind " << args.bindAddr << ", join ";
            if (joinedIfaces.empty())
            {
                std::cerr << "NONE";
            }
            else
            {
                for (std::size_t i = 0; i < joinedIfaces.size(); ++i)
                {
                    if (i) std::cerr << ", ";
                    std::cerr << joinedIfaces[i];
                }
            }
            std::cerr << ")\n";
        }

        if (args.csv)
            std::cout << "\"sendTime\",\"recvTime\",\"program\",\"node\",\"message\"\n";
        std::unordered_map<std::int64_t, std::uint32_t> lastSeq;
        std::unordered_map<std::int64_t, std::string>  tailPerSender;
        std::vector<char> buffer(65535);
        bool startOfLine = true;   // remember if next character starts a new output line

        while (true)
        {
            udp::endpoint senderEndpoint;
            std::size_t   len = sock.receive_from(boost::asio::buffer(buffer), senderEndpoint);
            if (args.verbose) std::cerr << "Received " << len << " bytes from " << senderEndpoint.address().to_string() << "\n";

            if (len < sizeof(TracerDataHeader))
            {
                if (args.verbose) std::cerr << "Discarding packet: too small for header\n";
                continue;
            }

            const auto* hdr = reinterpret_cast<const TracerDataHeader*>(buffer.data());

            const std::int64_t incId       = hdr->incarnationId;
            const std::int64_t senderId    = hdr->senderId;
            const std::uint32_t seqNo      = hdr->sequenceNumber;
            const std::uint32_t payloadLen = hdr->payloadLength;
            const std::uint64_t timestamp = hdr->timestampUsec;

            // -----------------------------------------------------------------
            // Pre-compute CSV timestamp fields for possible diagnostic messages
            // -----------------------------------------------------------------
            std::string sendTimeCsv;
            std::string recvTimeCsv;
            if (args.csv)
            {
                if (args.showSendTime)
                    sendTimeCsv = formatIsoTime(timestamp);

                if (args.showRecvTime)
                {
                    auto now = std::chrono::system_clock::now();
                    auto us  = std::chrono::duration_cast<std::chrono::microseconds>(now.time_since_epoch()).count();
                    recvTimeCsv = formatIsoTime(static_cast<std::uint64_t>(us));
                }
            }
            const std::uint8_t  progLen    = hdr->programNameLength;
            const std::uint8_t  nodeLen    = hdr->nodeNameLength;

            if (args.filterInc && *args.filterInc != incId)
            {
                if (args.verbose) std::cerr << "Discarding packet: incarnation id filter mismatch\n";
                continue;
            }
            if (args.filterSender && *args.filterSender != senderId)
            {
                if (args.verbose) std::cerr << "Discarding packet: sender id filter mismatch\n";
                continue;
            }

            const std::size_t headerAndNames = sizeof(TracerDataHeader) + progLen + nodeLen;
            if (len < headerAndNames + payloadLen)
            {
                if (args.verbose) std::cerr << "Discarding packet: malformed length\n";
                continue;   // malformed
            }

            const char* progPtr = buffer.data() + sizeof(TracerDataHeader);
            std::string programName(progPtr, progLen);
            const char* nodePtr = progPtr + progLen;
            std::string nodeName(nodePtr, nodeLen);

            if (args.progRegex && !std::regex_search(programName, *args.progRegex))
            {
                if (args.verbose) std::cerr << "Discarding packet: program regex mismatch\n";
                continue;
            }
            if (args.nodeRegex && !std::regex_search(nodeName, *args.nodeRegex))
            {
                if (args.verbose) std::cerr << "Discarding packet: node regex mismatch\n";
                continue;
            }

            const char* payloadPtr = buffer.data() + headerAndNames;
            std::string payload(payloadPtr, payloadLen);

            // -----------------------------------------------------------------
            // Re-assemble split lines: prepend any stored tail fragment
            // -----------------------------------------------------------------
            auto& tail = tailPerSender[senderId];
            if (!tail.empty())
            {
                payload.insert(0, tail);
                tail.clear();
            }

            std::vector<std::string> diagCsv;
            noteSequence(lastSeq, senderId, seqNo, programName, nodeName,
                         args.csv, &diagCsv, sendTimeCsv, recvTimeCsv);

            if (args.csv)
            {
                for (const auto& row : diagCsv)
                    std::cout << row << '\n';
            }

            /* ---------------- CSV output ---------------------------------------------------- */
            if (args.csv)
            {
                std::string sendTimeStr = sendTimeCsv;
                std::string recvTimeStr = recvTimeCsv;

                // Handle potential split line: keep unfinished fragment for next packet
                bool payloadEndsWithNl = !payload.empty() && payload.back() == '\n';
                if (!payloadEndsWithNl)
                {
                    std::size_t pos = payload.find_last_of('\n');
                    if (pos == std::string::npos)
                    {
                        tail = payload;
                        payload.clear();
                    }
                    else
                    {
                        tail = payload.substr(pos + 1);
                        payload.resize(pos + 1);
                    }
                }

                std::istringstream iss(payload);
                std::string line;
                while (std::getline(iss, line))
                {
                    if (!line.empty() && line.back() == '\r')
                        line.pop_back();          // strip CR if present
                    std::cout << std::quoted(sendTimeStr) << ','
                              << std::quoted(recvTimeStr) << ','
                              << std::quoted(programName) << ','
                              << std::quoted(nodeName) << ','
                              << std::quoted(line) << '\n';
                }
                std::cout << std::flush;
                continue;   // next datagram
            }

            /* ---------------- Plain text output -------------------------------------------- */
            // Build optional prefix for each output line
            std::string prefix;
            {
                std::ostringstream ps;
                bool first = true;
                auto add = [&first, &ps](const std::string& s)
                {
                    if (!first) ps << ' ';
                    first = false;
                    ps << s;
                };
                if (args.showSendTime)      add(formatIsoTime(timestamp));
                if (args.showRecvTime)
                {
                    auto now = std::chrono::system_clock::now();
                    auto us  = std::chrono::duration_cast<std::chrono::microseconds>(now.time_since_epoch()).count();
                    add(formatIsoTime(static_cast<std::uint64_t>(us)));
                }
                if (args.prefixProgramName) add(programName);
                if (args.showNodeName)      add(nodeName);
                if (!first)
                    ps << " ";
                prefix = ps.str();
            }

            // Handle potential split line similarly to GUI receiver
            bool payloadEndsWithNl = !payload.empty() && payload.back() == '\n';
            if (!payloadEndsWithNl)
            {
                std::size_t pos = payload.find_last_of('\n');
                if (pos == std::string::npos)
                {
                    tail = payload;
                    payload.clear();
                }
                else
                {
                    tail = payload.substr(pos + 1);
                    payload.resize(pos + 1);
                }
            }

            if (!prefix.empty())
            {
                for (char ch : payload)
                {
                    if (startOfLine)
                    {
                        std::cout << prefix;
                        startOfLine = false;
                    }
                    std::cout << ch;
                    if (ch == '\n')
                    {
                        startOfLine = true;
                    }
                }
            }
            else
            {
                std::cout << payload;
            }
            std::cout << std::flush;
        }
    }
    catch(const std::exception& ex)
    {
        std::cerr << "Error: " << ex.what() << std::endl;
        return EXIT_FAILURE;
    }
    catch(...)
    {
        std::cerr << "Unknown error" << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
