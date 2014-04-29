/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
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
#ifndef __SAFIR_DOB_COMMUNICATION_NODETYPE_H__
#define __SAFIR_DOB_COMMUNICATION_NODETYPE_H__

#include <map>
#include <boost/asio.hpp>
#include "AckedDataSender.h"
#include "HeartbeatSender.h"

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
        NodeType(const boost::shared_ptr<boost::asio::io_service>& ioService,
                 boost::int64_t thisNodeId,
                 bool useMulticast,
                 boost::int64_t id,
                 const std::string &name,
                 const std::string &multicastAddr,
                 int ipVersion,
                 int heartbeatInterval,
                 int retryTimeout)
            :m_thisNodeId(thisNodeId)
            ,m_id(id)
            ,m_name(name)
            ,m_multicastAddress(multicastAddr)
            ,m_ipVersion(ipVersion)
            ,m_heartbeatInterval(heartbeatInterval)
            ,m_retryTimeout(retryTimeout)
            ,m_useMulticast(useMulticast)
            ,m_heartbeatSender(ioService, thisNodeId, m_ipVersion, McAddr(m_multicastAddress, m_useMulticast), heartbeatInterval)
            ,m_ackedDataSender(ioService, m_id, thisNodeId, m_ipVersion, McAddr(m_multicastAddress, m_useMulticast), retryTimeout)
        {
        }

        boost::int64_t Id() const {return m_id;}
        const std::string& Name() const {return m_name;}
        bool UseMulticast() const {return m_useMulticast;}
        const std::string& MulticastAddress() const {return m_multicastAddress;}
        int IpVersion() const {return m_ipVersion;}
        int HeartbeatInterval() const {return m_heartbeatInterval;}
        int RetryTimeout() const {return m_retryTimeout;}

        HeartbeatSender& GetHeartbeatSender() {return m_heartbeatSender;}
        const HeartbeatSender& GetHeartbeatSender() const {return m_heartbeatSender;}
        AckedDataSender& GetAckedDataSender() {return m_ackedDataSender;}
        const AckedDataSender& GetAckedDataSender() const {return m_ackedDataSender;}

    private:
        boost::int64_t m_thisNodeId;
        boost::int64_t m_id;
        std::string m_name;               //unique readable name
        std::string m_multicastAddress;   //multicast address including port number, 'address:port' empty string if not multicast enabled
        int m_ipVersion;
        int m_heartbeatInterval;          //time between heartbeats
        int m_retryTimeout;               //time to wait before retransmitting unacked data
        bool m_useMulticast;

        HeartbeatSender m_heartbeatSender;
        AckedDataSender m_ackedDataSender;

        static std::string McAddr(const std::string& addr, bool use){return use ? addr : "";}
    };

    typedef boost::shared_ptr<NodeType> NodeTypePtr;
    typedef std::map<boost::int64_t, NodeTypePtr> NodeTypeMap;
}
}
}
}

#endif
