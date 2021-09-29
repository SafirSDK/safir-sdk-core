/******************************************************************************
*
* Copyright Saab AB, 2013-2015 (http://safirsdkcore.com)
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

#include <map>
#include "DataSender.h"
#include "HeartbeatSender.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

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
    class NodeType
    {
    public:
        NodeType(boost::asio::io_service& ioService,
                 int64_t thisNodeId,
                 const std::string& localIf,
                 bool useMulticast,
                 int64_t id,
                 const std::string &name,
                 const std::string &multicastAddr,
                 int ipVersion,
                 int heartbeatInterval,
                 int maxLostHeartbeats,
                 int slidingWindowSize,
                 int ackRequestThreshold,
                 int fragmentSize,
                 const std::vector<int>& retryTimeout)
            :m_id(id)
            ,m_name(name)
            ,m_multicastAddress(multicastAddr)
            ,m_ipVersion(ipVersion)
            ,m_heartbeatInterval(heartbeatInterval)
            ,m_maxLostHeartbeats(maxLostHeartbeats)
            ,m_slidingWindowSize(slidingWindowSize)
            ,m_ackRequestThreshold(ackRequestThreshold)
            ,m_retryTimeout(retryTimeout)
            ,m_useMulticast(useMulticast)
            ,m_heartbeatSender(ioService, id, thisNodeId, m_ipVersion, localIf, McAddr(m_multicastAddress, m_useMulticast), heartbeatInterval)
            ,m_ackedDataSender(ioService, Acked, m_id, thisNodeId, m_ipVersion, localIf, McAddr(m_multicastAddress, m_useMulticast), slidingWindowSize, ackRequestThreshold, retryTimeout, fragmentSize)
            ,m_unackedDataSender(ioService, Unacked, m_id, thisNodeId, m_ipVersion, localIf, McAddr(m_multicastAddress, m_useMulticast), slidingWindowSize, ackRequestThreshold, retryTimeout, fragmentSize)
        {
        }

        int64_t Id() const {return m_id;}
        const std::string& Name() const {return m_name;}
        bool UseMulticast() const {return m_useMulticast;}
        const std::string& MulticastAddress() const {return m_multicastAddress;}
        int IpVersion() const {return m_ipVersion;}
        int HeartbeatInterval() const {return m_heartbeatInterval;}
        int MaxLostHeartbeats() const {return m_maxLostHeartbeats;}
        int SlidingWindowSize() const {return m_slidingWindowSize;}
        int AckRequestThreshold() const {return m_ackRequestThreshold;}
        const std::vector<int>& RetryTimeout() const {return m_retryTimeout;}

        HeartbeatSender& GetHeartbeatSender() {return m_heartbeatSender;}
        const HeartbeatSender& GetHeartbeatSender() const {return m_heartbeatSender;}

        DataSender& GetAckedDataSender() {return m_ackedDataSender;}
        const DataSender& GetAckedDataSender() const {return m_ackedDataSender;}

        DataSender& GetUnackedDataSender() {return m_unackedDataSender;}
        const DataSender& GetUnackedDataSender() const {return m_unackedDataSender;}

    private:
        const int64_t m_id;
        const std::string m_name;               //unique readable name
        const std::string m_multicastAddress;   //multicast address including port number, 'address:port' empty string if not multicast enabled
        const int m_ipVersion;
        const int m_heartbeatInterval;          //time between heartbeats
        const int m_maxLostHeartbeats;
        const int m_slidingWindowSize;
        const int m_ackRequestThreshold;
        const std::vector<int> m_retryTimeout;  //time to wait before retransmitting unacked data
        const bool m_useMulticast;

        HeartbeatSender m_heartbeatSender;
        DataSender m_ackedDataSender;
        DataSender m_unackedDataSender;

        static std::string McAddr(const std::string& addr, bool use){return use ? addr : "";}
    };

    typedef std::shared_ptr<NodeType> NodeTypePtr;
    typedef std::map<int64_t, NodeTypePtr> NodeTypeMap;
}
}
}
}
