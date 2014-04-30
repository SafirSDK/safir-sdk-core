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
    typedef boost::function<void(const std::string& name, boost::int64_t nodeId, boost::int64_t nodeTypeId, const std::string& address)> NewNode;

    class CommunicationImpl : private boost::noncopyable
    {
    public:
        CommunicationImpl(const boost::shared_ptr<boost::asio::io_service>& ioService,
                          const std::string& nodeName,
                          boost::int64_t nodeId, //0 is not a valid id.
                          boost::int64_t nodeTypeId,
                          const std::string& address,
                          bool discovering,
                          const NodeTypeMap& nodeTypes);

        virtual ~CommunicationImpl();

        //set callbacks
        void SetNewNodeCallback(const NewNode& callback);
        void SetGotReceiveFromCallback(const GotReceiveFrom& callback);
        void SetRetransmitToCallback(const RetransmitTo& callback);
        void SetQueueNotFullCallback(const QueueNotFull& callback, int freePartThreshold);
        void SetDataReceiver(const ReceiveData& callback, boost::int64_t dataTypeIdentifier);

        void InjectSeeds(const std::vector<std::string>& seeds);

        void Start();
        void Stop();

        void IncludeNode(boost::int64_t nodeId);
        void ExcludeNode(boost::int64_t nodeId);

        bool SendToNode(boost::int64_t nodeTypeId, boost::int64_t nodeId, const boost::shared_ptr<char[]>& data, size_t size, boost::int64_t dataTypeIdentifier);
        bool SendToNodeType(boost::int64_t nodeTypeId, const boost::shared_ptr<char[]>& data, size_t size, boost::int64_t dataTypeIdentifier);

        size_t NumberOfQueuedMessages(boost::int64_t nodeTypeId) const;

        const std::string& Name() const {return m_me.Name();}
        boost::int64_t Id() const {return m_me.Id();}

    private:
        ::google::protobuf::LogSilencer m_disableProtobufLogs;
        boost::shared_ptr<boost::asio::io_service> m_ioService;
        Node m_me;
        bool m_discovering;
        NodeTypeMap m_nodeTypes;

        //Callbacks
        NewNode m_onNewNode;
        GotReceiveFrom m_gotRecv;

        //main components of communication
        Discoverer m_discoverer;
        DeliveryHandler m_deliveryHandler;
        Reader m_reader;

        void SetSystemNode(boost::int64_t id, bool isSystemNode);
        bool OnRecv(const char* data, size_t size); //returns true if it is ok to call OnRecv again, false if flooded with received messages
        void OnNewNode(const Node& node);

        //Received internal Communication msg that is not directly passed to application, i.e discover, nodeInfo etc.
        void ReceivedControlData(const MessageHeader* header, const char* payload);

        NodeType& GetNodeType(boost::int64_t nodeTypeId) {return *(m_nodeTypes.find(nodeTypeId)->second);}
        const NodeType& GetNodeType(boost::int64_t nodeTypeId) const {return *(m_nodeTypes.find(nodeTypeId)->second);}
    };
}
}
}
}

#endif
