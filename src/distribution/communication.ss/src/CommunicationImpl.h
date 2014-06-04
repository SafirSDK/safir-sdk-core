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
#ifndef __SAFIR_DOB_COMMUNICATION_IMPL_H__
#define __SAFIR_DOB_COMMUNICATION_IMPL_H__

#include <set>
#include <boost/noncopyable.hpp>
#include <boost/asio.hpp>
#include <boost/function.hpp>
#include "NodeType.h"
#include "Node.h"
#include "Reader.h"
#include "DeliveryHandler.h"
#include "Writer.h"
#include "Discoverer.h"
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
    typedef std::function<void(const std::string& name, int64_t nodeId, int64_t nodeTypeId, const std::string& controlAddress, const std::string& dataAddress)> NewNode;

    class CommunicationImpl : private boost::noncopyable
    {
    public:
        CommunicationImpl(boost::asio::io_service& ioService,
                          const std::string& nodeName,
                          int64_t nodeId, //0 is not a valid id.
                          int64_t nodeTypeId,
                          const std::string& controlAddress,
                          const std::string& dataAddress,
                          bool isControlInstance,
                          const NodeTypeMap& nodeTypes);

        virtual ~CommunicationImpl();

        //set callbacks
        void SetNewNodeCallback(const NewNode& callback);
        void SetGotReceiveFromCallback(const GotReceiveFrom& callback);
        void SetRetransmitToCallback(const RetransmitTo& callback);
        void SetQueueNotFullCallback(const QueueNotFull& callback, int freePartThreshold);
        void SetDataReceiver(const ReceiveData& callback, int64_t dataTypeIdentifier);

        void InjectSeeds(const std::vector<std::string>& seeds);

        void Start();
        void Stop();

        void IncludeNode(int64_t nodeId);
        void ExcludeNode(int64_t nodeId);
        void InjectNode(const std::string& name, int64_t id, int64_t nodeTypeId, const std::string& dataAddress);

        bool SendToNode(int64_t nodeId, int64_t nodeTypeId, const boost::shared_ptr<char[]>& data, size_t size, int64_t dataTypeIdentifier);
        bool SendToNodeType(int64_t nodeTypeId, const boost::shared_ptr<char[]>& data, size_t size, int64_t dataTypeIdentifier);

        size_t NumberOfQueuedMessages(int64_t nodeTypeId) const;

        const std::string& Name() const {return m_me.name;}
        int64_t Id() const {return m_me.nodeId;}

    private:
        ::google::protobuf::LogSilencer m_disableProtobufLogs;
        boost::asio::io_service& m_ioService;
        Node m_me;
        bool m_isControlInstance;
        NodeTypeMap m_nodeTypes;

        //Callbacks
        NewNode m_onNewNode;
        GotReceiveFrom m_gotRecv;

        //main components of communication
        Discoverer m_discoverer;
        DeliveryHandler m_deliveryHandler;
        Reader m_reader;

        void SetSystemNode(int64_t id, bool isSystemNode);
        bool OnRecv(const char* data, size_t size); //returns true if it is ok to call OnRecv again, false if flooded with received messages
        void OnNewNode(const Node& node);

        //Received internal Communication msg that is not directly passed to application, i.e discover, nodeInfo etc.
        void ReceivedControlData(const MessageHeader* header, const char* payload);

        NodeType& GetNodeType(int64_t nodeTypeId)
        {
            auto findIt = m_nodeTypes.find(nodeTypeId);
            if (findIt == m_nodeTypes.end())
            {
                throw std::logic_error("Invalid nodeTypeId");
            }
            return *(findIt->second);
        }

        const NodeType& GetNodeType(int64_t nodeTypeId) const
        {
            return const_cast<CommunicationImpl*>(this)->GetNodeType(nodeTypeId);
        }
    };
}
}
}
}

#endif
