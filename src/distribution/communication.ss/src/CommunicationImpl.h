/******************************************************************************
*
* Copyright Saab AB, 2013-2022 (http://safirsdkcore.com)
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

#include <set>
#include <memory>
#include <functional>
#include "NodeType.h"
#include "Node.h"
#include "DataReceiver.h"
#include "DeliveryHandler.h"
#include "Writer.h"
#include "Discoverer.h"
#include "DataSender.h"
#include "HeartbeatSender.h"
#include "Resolver.h"

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
    typedef std::function<void(const std::string& name, int64_t nodeId, int64_t nodeTypeId, const std::string& controlAddress, const std::string& dataAddress, bool multicast)> NewNode;

    class CommunicationImpl
    {
    public:
        CommunicationImpl(boost::asio::io_context& ioContext,
                          const std::string& nodeName,
                          int64_t nodeId, //0 is not a valid id.
                          int64_t nodeTypeId,
                          const std::string& controlAddress,
                          const std::string& dataAddress,
                          bool isControlInstance,
                          const NodeTypeMap& nodeTypes,
                          int fragmentSize);

        virtual ~CommunicationImpl();

        CommunicationImpl(const CommunicationImpl&) = delete;
        const CommunicationImpl&operator=(const CommunicationImpl&) = delete;
        
        //set callbacks
        void SetNewNodeCallback(const NewNode& callback);
        void SetGotReceiveFromCallback(const GotReceiveFrom& callback);
        void SetRetransmitToCallback(const RetransmitTo& callback);
        void SetQueueNotFullCallback(const QueueNotFull& callback, int64_t nodeTypeId);
        void SetDataReceiver(const ReceiveData& callback, int64_t dataTypeIdentifier, const Allocator& allocator, const DeAllocator& deallocator);

        void InjectSeeds(const std::vector<std::string>& seeds);

        void Start();
        void Stop();
        void Reset();

        void IncludeNode(int64_t nodeId);
        void ExcludeNode(int64_t nodeId);
        void InjectNode(const std::string& name, int64_t id, int64_t nodeTypeId, const std::string& dataAddress);

        bool Send(int64_t nodeId,
                  int64_t nodeTypeId,
                  const Safir::Utilities::Internal::SharedConstCharArray& data,
                  size_t size,
                  int64_t dataTypeIdentifier,
                  bool deliveryGuarantee);

        size_t SendQueueCapacity(int64_t /*nodeTypeId*/) const {return Parameters::SendQueueSize;}
        size_t NumberOfQueuedMessages(int64_t nodeTypeId) const;
        const std::string& Name() const {return m_me.name;}
        int64_t Id() const {return m_me.nodeId;}
        std::string ControlAddress() const {return m_me.controlAddress;}
        std::string DataAddress() const {return m_me.dataAddress;}

    private:
        ::google::protobuf::LogSilencer m_disableProtobufLogs;
        boost::asio::io_context& m_ioContext;
        boost::asio::io_context::strand m_receiveStrand;
        Node m_me;
        int m_protocol;
        bool m_isControlInstance;
        NodeTypeMap m_nodeTypes;

        //Callbacks
        NewNode m_onNewNode;
        GotReceiveFrom m_gotRecvFrom;

        //main components of communication
        Discoverer m_discoverer;
        DeliveryHandler m_deliveryHandler;
        DataReceiver m_reader;

        std::string m_logPrefix;

        //returns true if it is ok to call OnRecv again, false if flooded with received messages
        bool OnRecv(const char* data, size_t size, bool multicast);
        void OnNewNode(const Node& node);

        //Received internal Communication msg that is not directly passed to application, i.e discover, nodeInfo etc.
        void ReceivedControlData(const MessageHeader* header, const char* payload);

        void IncludeNodeInternal(int64_t nodeId);

        NodeType& GetNodeType(int64_t nodeTypeId)
        {
            auto findIt = m_nodeTypes.find(nodeTypeId);
            if (findIt == m_nodeTypes.end())
            {
                throw std::logic_error(std::string("COM: GetNodeType Invalid, nodeTypeId: ")+boost::lexical_cast<std::string>(nodeTypeId));
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
