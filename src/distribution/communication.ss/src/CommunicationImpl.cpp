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
#include <iostream>
#include <boost/lexical_cast.hpp>
#include <boost/make_shared.hpp>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include "Parameters.h"
#include "CommunicationImpl.h"
#include "Message.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4355)
#endif
    CommunicationImpl::CommunicationImpl(boost::asio::io_service& ioService,
                                         const std::string& nodeName,
                                         int64_t nodeId, //0 is not a valid id.
                                         int64_t nodeTypeId,
                                         const std::string& controlAddress,
                                         const std::string& dataAddress,
                                         bool isControlInstance,
                                         const NodeTypeMap& nodeTypes,
                                         int fragmentSize)
        :m_disableProtobufLogs()
        ,m_ioService(ioService)
        ,m_receiveStrand(ioService)
        ,m_me(nodeName, nodeId, nodeTypeId, controlAddress, dataAddress, isControlInstance)
        ,m_protocol(Resolver::Protocol(m_me.unicastAddress))
        ,m_isControlInstance(isControlInstance)
        ,m_nodeTypes(nodeTypes)
        ,m_onNewNode()
        ,m_gotRecvFrom()
        ,m_discoverer(m_ioService, m_me, fragmentSize, [=](const Node& n){OnNewNode(n);})
        ,m_deliveryHandler(m_receiveStrand, m_me.nodeId, m_protocol, m_nodeTypes[nodeTypeId]->SlidingWindowSize())
        ,m_reader(m_receiveStrand, m_me.unicastAddress, m_nodeTypes[nodeTypeId]->MulticastAddress(),
                  [=](const char* d, size_t s, const bool mc){return OnRecv(d,s,mc);},
                  [=](){return m_deliveryHandler.NumberOfUndeliveredMessages()<Parameters::MaxNumberOfUndelivered;})
    {
        if (nodeId==0)
        {
            throw std::invalid_argument("Safir.Communication ctor: Id=0 is reserved for internal usage and is not valid. You should consider using a random generated id.");
        }
        auto myNodeType=m_nodeTypes[nodeTypeId];
        lllog(1)<<L"COM: -------------------------------------------------"<<std::endl;
        lllog(1)<<L"COM: Communication initiated"<<std::endl;
        lllog(1)<<L"COM:     id:    "<<m_me.nodeId<<std::endl;
        lllog(1)<<L"COM:     name:    "<<m_me.name.c_str()<<std::endl;
        lllog(1)<<L"COM:     data address: "<<m_me.dataAddress.c_str()<<std::endl;
        lllog(1)<<L"COM:     control address: "<<m_me.controlAddress.c_str()<<std::endl;
        lllog(1)<<L"COM:     multicast: "<<myNodeType->MulticastAddress().c_str()<<std::endl;
        lllog(1)<<L"COM:     using multicast: "<<std::boolalpha<<myNodeType->UseMulticast()<<std::dec<<std::endl;
        lllog(1)<<L"COM: -------------------------------------------------"<<std::endl;

#ifdef COM_USE_UNRELIABLE_SEND_POLICY
        lllog(1)<<L"*** COM_USE_UNRELIABLE_SEND_POLICY IS DEFINED ***"<<std::endl;
        std::wcout<<L"*** COM_USE_UNRELIABLE_SEND_POLICY IS DEFINED ***"<<std::endl;
#endif
    }
#ifdef _MSC_VER
#pragma warning (pop)
#endif

    CommunicationImpl::~CommunicationImpl()
    {
    }

    void CommunicationImpl::SetNewNodeCallback(const NewNode& callback)
    {
        m_onNewNode=callback;
    }

    void CommunicationImpl::SetGotReceiveFromCallback(const GotReceiveFrom& callback)
    {
        m_receiveStrand.dispatch([=]
        {
            m_gotRecvFrom=callback;
            m_deliveryHandler.SetGotRecvCallback(callback);
        });
    }

    void CommunicationImpl::SetRetransmitToCallback(const RetransmitTo& callback)
    {
        for (auto vt = m_nodeTypes.cbegin(); vt != m_nodeTypes.cend(); ++vt)
        {
            vt->second->GetAckedDataSender().SetRetransmitCallback(callback);
        }
    }

    void CommunicationImpl::SetDataReceiver(const ReceiveData& callback, int64_t dataTypeIdentifier, const Allocator& allocator, const DeAllocator& deallocator)
    {
        m_receiveStrand.post([=]{m_deliveryHandler.SetReceiver(callback, dataTypeIdentifier, allocator, deallocator);});
    }

    void CommunicationImpl::SetQueueNotFullCallback(const QueueNotFull& callback, int64_t nodeTypeId)
    {
        if (nodeTypeId!=0)
        {
            auto nodeTypeIt=m_nodeTypes.find(nodeTypeId);
            if (nodeTypeIt==m_nodeTypes.end())
            {
                throw std::logic_error("COM: Calling SetQueueNotFullCallback with nodeTypeId that does not exist. Used nodeTypeId: "+boost::lexical_cast<std::string>(nodeTypeId));
            }

            nodeTypeIt->second->GetAckedDataSender().SetNotFullCallback(callback);
        }
        else //set callback for all nodeTypes
        {
            for (auto nt = m_nodeTypes.cbegin(); nt != m_nodeTypes.cend(); ++nt)
            {
                nt->second->GetAckedDataSender().SetNotFullCallback(callback);
            }
        }
    }

    void CommunicationImpl::Start()
    {
        m_deliveryHandler.Start();
        m_reader.Start();
        for (auto vt = m_nodeTypes.cbegin(); vt != m_nodeTypes.cend(); ++vt)
        {
            vt->second->GetHeartbeatSender().Start();
            vt->second->GetAckedDataSender().Start();
        }
        if (m_isControlInstance)
        {
            m_discoverer.Start();
        }
    }

    void CommunicationImpl::Stop()
    {
        m_reader.Stop();
        m_deliveryHandler.Stop();
        for (auto vt = m_nodeTypes.cbegin(); vt != m_nodeTypes.cend(); ++vt)
        {
            vt->second->GetHeartbeatSender().Stop();
            vt->second->GetAckedDataSender().Stop();
        }
        if (m_isControlInstance)
        {
            m_discoverer.Stop();
        }
    }

    void CommunicationImpl::IncludeNode(int64_t id)
    {
        lllog(6)<<L"COM: IncludeNode "<<id<<std::endl;
        if (!m_isControlInstance)
        {
            std::logic_error("COM: InclueNode was called on instance running in DataMode.");
        }

        IncludeNodeInternal(id);
    }

    void CommunicationImpl::IncludeNodeInternal(int64_t id)
    {
        //There is no way for us at this point to know which nodeType the node belongs to without entering the
        //m_receiveStrand, and that is not an option here. We must be sure to post the includeNode right now to
        //ensure it is handled before any forthcoming Sends. Since includeNode in DataSender will ignore any calls
        //when the node does not exist, we call includeNode on all nodeTypes knowing it will only have effect on
        //the one where the node exists.
        for (auto nt = m_nodeTypes.begin(); nt != m_nodeTypes.end(); ++nt)
        {
             //calling IncludeNode will post on the sendStrand of the nodeType
            nt->second->GetAckedDataSender().IncludeNode(id);
            nt->second->GetUnackedDataSender().IncludeNode(id);
        }

        //We do post (not dispatch) here to be sure the AddNode job will be executed before IncludeNode. Otherwize we
        //risk losing a node.
        m_receiveStrand.post([=]{m_deliveryHandler.IncludeNode(id);});
    }

    void CommunicationImpl::ExcludeNode(int64_t id)
    {
        lllog(6)<<L"COM: ExcludeNode "<<id<<std::endl;

        m_receiveStrand.post([=]
        {
            lllog(6)<<L"COM: Execute ExcludeNode id="<<id<<std::endl;
            auto node=m_deliveryHandler.GetNode(id);

            if (node==nullptr)
            {
                lllog(6)<<L"COM: Exclude unknown node, call will be ignored."<<std::endl;
                return;
            }

            auto& nodeType=GetNodeType(node->nodeTypeId);
            nodeType.GetAckedDataSender().RemoveNode(id);
            nodeType.GetUnackedDataSender().RemoveNode(id);
            nodeType.GetHeartbeatSender().RemoveNode(id);
            m_deliveryHandler.RemoveNode(id);
            if (m_isControlInstance)
            {
                m_discoverer.ExcludeNode(id);
            }
        });
    }

    void CommunicationImpl::InjectNode(const std::string& name, int64_t id, int64_t nodeTypeId, const std::string& dataAddress)
    {
        if (m_isControlInstance)
        {
            throw std::logic_error("COM: InjectNode was called on instance running in ControlMode.");
        }

        lllog(6)<<L"COM: Inject node '"<<name.c_str()<<L"' ["<<id<<L"]"<<std::endl;

        Node node(name, id, nodeTypeId, "", Resolver(m_ioService).ResolveRemoteEndpoint(dataAddress, m_protocol), false);
        OnNewNode(node);
        IncludeNodeInternal(id);
    }

    bool CommunicationImpl::Send(int64_t nodeId, int64_t nodeTypeId, const boost::shared_ptr<const char[]>& data, size_t size, int64_t dataTypeIdentifier, bool deliveryGuarantee)
    {
        if (deliveryGuarantee)
        {
            return GetNodeType(nodeTypeId).GetAckedDataSender().AddToSendQueue(nodeId, data, size, dataTypeIdentifier);
        }
        else
        {
            return GetNodeType(nodeTypeId).GetUnackedDataSender().AddToSendQueue(nodeId, data, size, dataTypeIdentifier);
        }
    }

    size_t CommunicationImpl::NumberOfQueuedMessages(int64_t nodeTypeId) const
    {
        return GetNodeType(nodeTypeId).GetAckedDataSender().SendQueueSize();
    }

    void CommunicationImpl::InjectSeeds(const std::vector<std::string>& seeds)
    {
        if (!m_isControlInstance)
            return;

        std::vector<std::string> resolvedSeeds;
        for (auto seed = seeds.cbegin(); seed != seeds.cend(); ++seed)
        {
            try
            {
                auto rs=Resolver(m_ioService).ResolveRemoteEndpoint(*seed, m_protocol);
                resolvedSeeds.push_back(rs);
            }
            catch(const std::logic_error& badSeed)
            {
                lllog(2)<<"COM: InjectSeeds injecting a seed that could not be resolved to a valid ip_address and port. Seed: "
                          <<seed->c_str()<<". "<<badSeed.what()<<std::endl;

            }
        }

        m_discoverer.AddSeeds(resolvedSeeds);
    }

    void CommunicationImpl::OnNewNode(const Node& node)
    {
        lllog(6)<<L"COM: New node '"<<node.name.c_str()<<L"' ["<<node.nodeId<<L"]"<<std::endl;

        auto& nodeType=GetNodeType(node.nodeTypeId);
        nodeType.GetAckedDataSender().AddNode(node.nodeId, node.unicastAddress);
        nodeType.GetUnackedDataSender().AddNode(node.nodeId, node.unicastAddress);
        nodeType.GetHeartbeatSender().AddNode(node.nodeId, node.unicastAddress);
        m_receiveStrand.dispatch([this, node]{m_deliveryHandler.AddNode(node);});

        //callback to host application
        m_onNewNode(node.name,
                    node.nodeId,
                    node.nodeTypeId,
                    node.controlAddress,
                    node.dataAddress,
                    nodeType.UseMulticast());
    }

    //returns true if it is ok to call OnRecv again, false if flooded with received messages
    bool CommunicationImpl::OnRecv(const char* data, size_t size, bool multicast)
    {
        //Always called from readStrand

        if (size<CommonHeaderSize)
        {
            lllog(4)<<L"COM: Received corrupt data"<<std::endl;
            return true; //corrupt message, return true means it is ok to receive another message
        }

        const CommonHeader* commonHeader=reinterpret_cast<const CommonHeader*>(data);
        if (commonHeader->receiverId!=0 && commonHeader->receiverId!=m_me.nodeId)
        {
            return true; //received message that is not for me. Can happen if node has been restarted with same ip-addr but different nodeId
        }

        if (commonHeader->senderId==m_me.nodeId)
        {
            return true; //Message sent from myself
        }

        switch (commonHeader->dataType)
        {
        case HeartbeatType:
        {
            const Node* senderNode=m_deliveryHandler.GetNode(commonHeader->senderId);
            if (senderNode!=nullptr && senderNode->systemNode)
            {
                m_gotRecvFrom(commonHeader->senderId, multicast, false);
                lllog(9)<<"COM: Heartbeat from "<<commonHeader->senderId<<std::endl;
            }
        }
        break;

        case AckType:
        {
            const Node* senderNode=m_deliveryHandler.GetNode(commonHeader->senderId);
            if (senderNode!=nullptr && senderNode->systemNode)
            {
                m_gotRecvFrom(commonHeader->senderId, multicast, false);
                const Ack* ack=reinterpret_cast<const Ack*>(data);
                GetNodeType(senderNode->nodeTypeId).GetAckedDataSender().HandleAck(*ack);
            }
        }
        break;

        case AckRequestType:
        {
            if (size<MessageHeaderSize)
            {
                lllog(4)<<L"COM: Received corrupt AckRequest"<<std::endl;
                return true; //corrupt message, return true means it is ok to receive another message
            }
            const MessageHeader* ackReq=reinterpret_cast<const MessageHeader*>(data);
            m_deliveryHandler.ReceivedAckRequest(ackReq, multicast);
        }
        break;

        case ControlDataType:
        {
            if (size<MessageHeaderSize)
            {
                lllog(4)<<L"COM: Received corrupt ControlData"<<std::endl;
                return true; //corrupt message, return true means it is ok to receive another message
            }
            const MessageHeader* msgHeader=reinterpret_cast<const MessageHeader*>(data);
            const char* payload=data+MessageHeaderSize;
            ReceivedControlData(msgHeader, payload);
        }
        break;

        default: //some user defined type or welcome or ping
        {
            //Application data
            if (size<MessageHeaderSize)
            {
                lllog(4)<<L"COM: Received corrupt ApplicationData"<<std::endl;
                return true; //corrupt message, return true means it is ok to receive another message
            }
            const MessageHeader* msgHeader=reinterpret_cast<const MessageHeader*>(data);
            const char* payload=data+MessageHeaderSize;
            m_deliveryHandler.ReceivedApplicationData(msgHeader, payload, multicast);
        }
        break;
        }

        return m_deliveryHandler.NumberOfUndeliveredMessages()<Parameters::MaxNumberOfUndelivered;
    }

    //Received internal Communication msg that shall not be immediately passed to application, i.e discover, nodeInfo etc.
    void CommunicationImpl::ReceivedControlData(const MessageHeader* header, const char* payload)
    {
        //Always called from readStrand

        //Protobuf messagenodeTypeId
        CommunicationMessage cm;
        bool parsedOk=cm.ParseFromArray(static_cast<const void*>(payload), static_cast<int>(header->fragmentContentSize));
        if (!parsedOk)
        {
            lllog(4)<<L"COM: Received message with valid header but corrupt data."<<std::endl;
            return;
        }

        if (cm.has_discover())
        {
            m_discoverer.HandleReceivedDiscover(cm.discover());
        }
        if (cm.has_node_info())
        {
            m_discoverer.HandleReceivedNodeInfo(cm.node_info());
        }
    }
}
}
}
}
