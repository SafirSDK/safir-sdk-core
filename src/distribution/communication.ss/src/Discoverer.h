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
#ifndef __SAFIR_DOB_COMMUNICATION_DISCOVERER_H__
#define __SAFIR_DOB_COMMUNICATION_DISCOVERER_H__

#include <boost/function.hpp>
#include <boost/random.hpp>
#include <boost/asio/steady_timer.hpp>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include "Parameters.h"
#include "Node.h"
#include "Message.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    class Discoverer
    {
    public:
        Discoverer(const boost::shared_ptr<boost::asio::io_service>& ioService,
                   const Node& me,
                   const boost::function<void(const UserDataPtr&, const boost::asio::ip::udp::endpoint&)>& sendTo,
                   const boost::function<void(const Node&)>& onNewNode);

        void Start();
        void Stop();
        void AddSeeds(const std::vector<std::string>& seeds);
        void HandleReceivedDiscover(const CommunicationMessage_Discover& msg);
        void HandleReceivedNodeInfo(const CommunicationMessage_NodeInfo& msg);

#ifndef SAFIR_TEST
    private:
#endif
        struct NodeInfo
        {
            boost::int64_t nodeId;
            boost::int64_t nodeTypeId;
            std::string name;
            std::string unicastAddress;


            NodeInfo()
                :nodeId(0)
                ,nodeTypeId(0)
                ,name()
                ,unicastAddress()
            {
            }

            NodeInfo(boost::int64_t nodeId_,
                     boost::int64_t nodeTypeId_,
                     const std::string& name_,
                     const std::string& address_)
                :nodeId(nodeId_)
                ,nodeTypeId(nodeTypeId_)
                ,name(name_)
                ,unicastAddress(address_)
            {
            }
        };

        bool m_running {false};

        //Constant defining how many nodes that can be sent in a singel NodeInfo message without risking that fragmentSize is exceeded.
        //The stuff in CommunicationMessage+NodeInfo is less than 30 bytes plus sent_from_node, and each individual Node (also sent_from_node) is less than 100 bytes.
        //Hence this constant is calculated as (MaxFragmentSize-100-30)/100 = MaxNumberOfNodesPerMessage. (Assuming sent_from_node is set in every msg)
        static const int NodeInfoPerNodeSize=100;
        static const int NodeInfoFixedSize=30+NodeInfoPerNodeSize;
        static const int NumberOfNodesPerNodeInfoMsg=(Parameters::FragmentSize-NodeInfoFixedSize)/NodeInfoPerNodeSize;

        std::map<boost::int64_t, NodeInfo> m_seeds; //id generated from ip:port
        std::map<boost::int64_t, NodeInfo> m_nodes; //known nodes
        std::map<boost::int64_t, NodeInfo> m_reportedNodes; //nodes only heard about from others, never talked to
        std::map<boost::int64_t, std::vector<bool> > m_incompletedNodes; //talked to but still haven't received all node info from this node

        boost::asio::io_service::strand m_strand;
        boost::int64_t m_myNodeId;
        std::string m_myNodeName;
        std::string m_myAddress;
        boost::int64_t m_myNodeTypeId;

        boost::function<void(const UserDataPtr&, const boost::asio::ip::udp::endpoint&)> m_sendTo;
        boost::function<void(const Node&)> m_onNewNode;
        boost::asio::steady_timer m_timer;

        mutable boost::random::mt19937 m_randomGenerator;

        void SendDiscover();
        void SendNodeInfo(boost::int64_t toId, boost::int64_t fromId, const boost::asio::ip::udp::endpoint& toEndpoint);
        void SendMessageTo(const CommunicationMessage& cm, const boost::asio::ip::udp::endpoint& toEndpoint);
        inline bool IsNewNode(boost::int64_t id) const {return m_nodes.find(id)==m_nodes.cend();}
        bool UpdateIncompleteNodes(boost::int64_t id, size_t numberOfPackets, size_t packetNumber); //returns true if node is completed
        void AddNewNode(const CommunicationMessage_Node& node); //node we know exists, we have talked to
        void AddReportedNode(const CommunicationMessage_Node& node); //nodes we've heard from others that exist but havent talked to
        void AddSeed(const std::string& seed);

        void OnTimeout(const boost::system::error_code& error);

        inline int Random(int min, int max) const {return (boost::random::uniform_int_distribution<>(min, max))(m_randomGenerator);}
    };
}
}
}
}

#endif
