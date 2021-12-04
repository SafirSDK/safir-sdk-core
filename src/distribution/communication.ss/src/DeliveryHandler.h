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

#include <memory>
#include <atomic>
#include <unordered_map>
#include <functional>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/SharedCharArray.h>
#include "Parameters.h"
#include "Message.h"
#include "MessageQueue.h"
#include "Node.h"
#include "Writer.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>

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
    typedef std::function<char*(size_t)> Allocator;
    typedef std::function<void(const char *)> DeAllocator;
    typedef std::function<void(int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t size)> ReceiveData;
    typedef std::function<void(int64_t fromNodeId, bool isMulticast, bool isDuplicate)> GotReceiveFrom;

    template <class WriterType>
    class DeliveryHandlerBasic : private WriterType
    {
    public:
        DeliveryHandlerBasic(boost::asio::io_service::strand& receiveStrand, int64_t myNodeId, int ipVersion, int slidingWindowSize)
            :WriterType(receiveStrand.context(), ipVersion)
            ,m_running(false)
            ,m_myId(myNodeId)
            ,m_slidingWindowSize(static_cast<size_t>(slidingWindowSize))
            ,m_receiveStrand(receiveStrand)
            ,m_deliverStrand(receiveStrand.context())
            ,m_nodes()
            ,m_receivers()
            ,m_gotRecvFrom()
        {
            m_numberOfUndeliveredMessages=0;

            m_receivers.insert(std::make_pair(WelcomeDataType,
                                              DataReceiver([=](size_t size){return new char[size];},
                                                           [=](const char* data) {delete[] data;},
                                                           [=](int64_t, int64_t, const char* data, size_t) {delete[] data;})
                                              )
                               );

            m_receivers.insert(std::make_pair(PingDataType,
                                              DataReceiver([=](size_t size){return new char[size];},
                                                           [=](const char* data) {delete[] data;},
                                                           [=](int64_t, int64_t, const char* data, size_t) {delete[] data;})
                                              )
                               );
        }

        void Start()
        {
            m_receiveStrand.dispatch([this]
            {
                m_running=true;
            });
        }

        void Stop()
        {
            m_receiveStrand.dispatch([this]
            {
                m_running=false;
            });

        }

        //Received data to be delivered up to the application. Everythin must be called from readStrand.
        void SetGotRecvCallback(const GotReceiveFrom& callback)
        {
            m_gotRecvFrom=callback;
        }

        void SetReceiver(const ReceiveData& callback, int64_t dataTypeIdentifier, const Allocator& allocator, const DeAllocator& deallocator)
        {
            m_receivers.insert(std::make_pair(dataTypeIdentifier, DataReceiver(allocator, deallocator, callback)));
        }

        //Handle received data and deliver to application if possible and sends ack back to sender.
        void ReceivedApplicationData(const MessageHeader* header, const char* payload, bool multicast)
        {
            //Always called from readStrand
            auto senderIt=m_nodes.find(header->commonHeader.senderId);

            if (senderIt==m_nodes.end() || !senderIt->second.node.systemNode)
            {
                lllog(4)<<L"COM: Received data from unknown node or a non system node with id="<<header->commonHeader.senderId<<std::endl;
                return;
            }

            lllog(8)<<L"COM: Received AppData from "<<header->commonHeader.senderId<<" "<<
                      SendMethodToString(header->sendMethod).c_str()<<", seq: "<<header->sequenceNumber<<std::endl;
            //m_gotRecvFrom(header->commonHeader.senderId, multicast); //report that we are receiving intact data from the node

            bool ackNow=false;
            if (header->deliveryGuarantee==Acked)
            {
                ackNow=HandleAckedMessage(header, multicast, payload, senderIt->second);
            }
            else
            {
                HandleUnackedMessage(header, multicast, payload, senderIt->second);
            }

            Deliver(senderIt->second, header); //if something is now fully received, deliver it to application

            if (ackNow)
            {
                SendAck(senderIt->second, header);
            }
        }

        void ReceivedAckRequest(const MessageHeader* header, bool multicast)
        {
            //Always called from readStrand
            lllog(8)<<L"COM: Received AckRequest from "<<header->commonHeader.senderId<<" "<<SendMethodToString(header->sendMethod).c_str()<<std::endl;
            auto senderIt=m_nodes.find(header->commonHeader.senderId);

            if (senderIt==m_nodes.end() || !senderIt->second.node.systemNode)
            {
                lllog(4)<<L"COM: Received ackRequest from unknown node or a non system node with id="<<header->commonHeader.senderId<<std::endl;
                return;
            }

            SendAck(senderIt->second, header);
            m_gotRecvFrom(header->commonHeader.senderId, multicast, false); //report that we are receivinga data
        }

        //Add a node
        void AddNode(const Node& node)
        {
            if (GetNode(node.nodeId)!=nullptr)
            {
                std::ostringstream os;
                os<<"COM: Duplicated call to DeliveryHandler.AddNode with same nodeId! Node: "<<node.name<<" ["<<node.nodeId<<"]";
                throw std::logic_error(os.str());
            }

            //Always called from readStrand
            m_nodes.insert(std::make_pair(node.nodeId, NodeInfo(node, m_slidingWindowSize)));
        }

        //Make node included. If excluded it is also removed.
        void IncludeNode(int64_t id)
        {
            lllog(6)<<L"COM: DeliveryHandler IncludeNode id="<<id<<std::endl;
            const auto it=m_nodes.find(id);
            if (it==m_nodes.end())
            {
                throw std::logic_error(std::string("COM: DeliveryHandler IncludeNode unknown or excluded node. NodeId: ")+boost::lexical_cast<std::string>(id));
            }

            it->second.node.systemNode=true;
        }

        //Make node included or excluded. If excluded it is also removed.
        void RemoveNode(int64_t id)
        {
            auto it=m_nodes.find(id);

            if (it!=m_nodes.end())
            {
                lllog(8) << L"COM: DeliveryHandler got RemoveNode, will now deallocate used memory then remove the node " <<it->second.node.name.c_str()<< std::endl;
                ClearChannel(it->second.ackedMultiReceiverChannel);
                ClearChannel(it->second.ackedSingleReceiverChannel);
                ClearChannel(it->second.unackedMultiReceiverChannel);
                ClearChannel(it->second.unackedSingleReceiverChannel);
            }
            
            m_nodes.erase(id);
        }

        const Node* GetNode(int64_t id) const
        {
            //Always called from readStrand
            auto it=m_nodes.find(id);
            if (it!=m_nodes.end())
            {
                return &it->second.node;
            }
            return nullptr;
        }

        uint32_t NumberOfUndeliveredMessages() const
        {
            return m_numberOfUndeliveredMessages;
        }

#ifndef SAFIR_TEST
    private:
#endif

        struct DataReceiver
        {
            Allocator alloc;
            DeAllocator dealloc;
            ReceiveData onRecv;

            DataReceiver(const Allocator& a, const DeAllocator& d, const ReceiveData& r)
                :alloc(a)
                ,dealloc(d)
                ,onRecv(r)
            {
            }
        };

        typedef std::unordered_map<int64_t, DataReceiver>  ReceiverMap;

        struct RecvData
        {
            bool free;
            int64_t dataType;
            uint64_t sequenceNumber;
            uint16_t fragmentNumber;
            uint16_t numberOfFragments;
            char* data;
            size_t dataSize;

            RecvData()
                :free(true)
                ,dataType(0)
                ,sequenceNumber(0)
                ,fragmentNumber(0)
                ,numberOfFragments(0)
                ,data(nullptr)
                ,dataSize(0)
            {
            }

            void Clear()
            {
                free=true;
                data=nullptr; //we are not responsible for deleting data if the data has been delivered to the subscriber
                dataSize=0;
            }

            std::string ToString() const
            {
                std::ostringstream os;
                os<<"free: "<<std::boolalpha<<free<<std::dec<<"dataType: "<<dataType<<", seq: "<<sequenceNumber<<", frag: "<<fragmentNumber<<", #frags: "<<numberOfFragments<<
                ", size: "<<dataSize<<", data: ";
                if (data!=nullptr)
                    os<<(uint64_t)data;
                else
                    os<<"null";

                return os.str();
            }
        };

        struct Channel
        {
            uint64_t welcome; //first received message that we are supposed to handle
            uint64_t lastInSequence; //last sequence number that was moved out of the queue. seq(queue[0])-1
            uint64_t biggestSequence; //biggest sequence number recevived (within our window)
            CircularArray<RecvData> queue;
            Channel(size_t slidingWindowSize)
                :welcome(0)
                ,lastInSequence(0)
                ,biggestSequence(0)
                ,queue(slidingWindowSize)
            {
            }

            std::string ToString() const
            {
                std::ostringstream os;
                os<<"welcome: "<<welcome<<", lastInSeq: "<<lastInSequence<<", biggestSeq: "<<biggestSequence<<std::endl;
                for (int i=0; i<queue.Size(); ++i)
                {
                    os<<"q["<<i<<"]: "<<queue[i].ToString()<<std::endl;
                }
                return os.str();
            }
        };

        struct NodeInfo
        {
            Node node;
            boost::asio::ip::udp::endpoint endpoint;

            Channel unackedSingleReceiverChannel;
            Channel unackedMultiReceiverChannel;
            Channel ackedSingleReceiverChannel;
            Channel ackedMultiReceiverChannel;

            NodeInfo(const Node& node_, size_t slidingWindowSize)
                :node(node_)
                ,endpoint(Resolver::StringToEndpoint(node.unicastAddress))
                ,unackedSingleReceiverChannel(slidingWindowSize)
                ,unackedMultiReceiverChannel(slidingWindowSize)
                ,ackedSingleReceiverChannel(slidingWindowSize)
                ,ackedMultiReceiverChannel(slidingWindowSize)
            {
                ackedMultiReceiverChannel.welcome=UINT64_MAX;
            }

            Channel& GetChannel(const MessageHeader* header)
            {
                if (header->deliveryGuarantee==Acked)
                {
                    if (header->sendMethod==SingleReceiverSendMethod)
                        return ackedSingleReceiverChannel;
                    else
                        return ackedMultiReceiverChannel;
                }
                else //Unacked
                {
                    if (header->sendMethod==SingleReceiverSendMethod)
                        return unackedSingleReceiverChannel;
                    else
                        return unackedMultiReceiverChannel;
                }
            }
        };
        typedef std::map<int64_t, NodeInfo> NodeInfoMap;

        bool m_running;
        const int64_t m_myId;
        const size_t m_slidingWindowSize;
        boost::asio::io_service::strand& m_receiveStrand; //for sending acks, same strand as all public methods are supposed to be called from
        boost::asio::io_service::strand m_deliverStrand; //for delivering data to application
        std::atomic<unsigned int> m_numberOfUndeliveredMessages;

        NodeInfoMap m_nodes;
        ReceiverMap m_receivers;
        GotReceiveFrom m_gotRecvFrom;

        //loop through all RecvData in a channel and deallocate any allocated memory. Reset each RecvData in the channel.
        void ClearChannel(Channel& ch)
        {
            bool firstFound=false;
            for (size_t i=0; i<ch.queue.Size(); ++i)
            {
                if (ch.queue[i].data!=nullptr && (!firstFound || ch.queue[i].fragmentNumber==0))
                {
                    //the first message in queue with data, or the first fragment of a message
                    firstFound=true;

                    auto recvIt=m_receivers.find(ch.queue[i].dataType);
                    if (recvIt!=m_receivers.end())
                    {
                        recvIt->second.dealloc(ch.queue[i].data);
                    }
                    else
                    {
                        std::ostringstream os;
                        os << "COM: ClearChannel, cant find registered receiver for dataType "<<ch.queue[i].dataType<<" when trying to deallocate data."
                           <<" This should be impossible since we have managed to allocate the data using the same dataType.";
                        SEND_SYSTEM_LOG(Error, <<os.str().c_str());
                        throw std::logic_error(os.str());
                    }
                }

                ch.queue[i].Clear();
            }
        }

        void Insert(const MessageHeader* header, const char* payload, NodeInfo& ni)
        {
            Channel& ch=ni.GetChannel(header);
            size_t currentIndex, firstIndex, lastIndex;
            CalculateIndices(ch.lastInSequence, header, currentIndex, firstIndex, lastIndex);
            RecvData& recvData=ch.queue[currentIndex];
            if (!recvData.free)
            {
                if (recvData.sequenceNumber==header->sequenceNumber)
                {
                    //duplicate, just throw away
                    lllog(8)<<L"COM: Recv duplicated message ahead. Seq: "<<header->sequenceNumber<<std::endl;
                    return;
                }
                else
                {
                    //Programming error, should never happen
                    std::ostringstream os;
                    os<<"COM: Logical error detected. We received msg with seqNo="<<header->sequenceNumber<<" and calculated queue index to "
                     <<currentIndex<<". But that index is already occupied with seqNo="<<recvData.sequenceNumber<<". Last received seqNo in non-broken sequence was "
                    <<ch.lastInSequence<<". Current sequenceNumbers in queue is (start with index=0): [";
                    for (size_t i=0; i<ch.queue.Size(); ++i)
                    {
                        const auto& item=ch.queue[i];
                        if (item.free)
                            os<<"X  ";
                        else
                            os<<ch.queue[i].sequenceNumber<<"  ";
                    }
                    os<<"]";
                    SEND_SYSTEM_LOG(Error, <<os.str().c_str());

                    throw std::logic_error(os.str());
                    return;
                }
            }
            recvData.free=false;
            recvData.numberOfFragments=header->numberOfFragments;
            recvData.fragmentNumber=header->fragmentNumber;
            recvData.sequenceNumber=header->sequenceNumber;
            recvData.dataType=header->commonHeader.dataType;

            ch.biggestSequence=std::max(recvData.sequenceNumber, ch.biggestSequence);

            //Check if buffer is not created for us. In the case the message is fragmented
            //another fragment may already have arrived and the buffer is created.
            if (recvData.data==nullptr)
            {
                auto recvIt=m_receivers.find(recvData.dataType); //m_receivers shall be safe to use inside m_deliverStrand since it is not supposed to be modified after start
                if (recvIt==m_receivers.end())
                {
                    std::ostringstream os;
                    os << "COM: (Insert) Received data from node "
                       << header->commonHeader.senderId
                       << " that has no registered receiver. DataTypeIdentifier: "<<recvData.dataType
                       << ", hdr = " << header->ToString();
                    SEND_SYSTEM_LOG(Error, <<os.str().c_str());
                    throw std::logic_error(os.str());
                }

                recvData.data=recvIt->second.alloc(header->totalContentSize);
                recvData.dataSize=header->totalContentSize;

                //copy buffer to other indices if it shall be shared among many fragments
                uint16_t tmpFragNo=0;
                for (size_t i=firstIndex; i<currentIndex; ++i)
                {
                    ch.queue[i].data=recvData.data;
                    ch.queue[i].dataSize=recvData.dataSize;
                    ch.queue[i].dataType=recvData.dataType;
                    ch.queue[i].numberOfFragments=recvData.numberOfFragments;
                    ch.queue[i].fragmentNumber=tmpFragNo++;
                }
                for (size_t i=currentIndex+1; i<=lastIndex; ++i)
                {
                    ch.queue[i].data=recvData.data;
                    ch.queue[i].dataSize=recvData.dataSize;
                    ch.queue[i].dataType=recvData.dataType;
                    ch.queue[i].numberOfFragments=recvData.numberOfFragments;
                    ch.queue[i].fragmentNumber=++tmpFragNo;
                }
            }

            //copy data to correct offset in buffer depending on fragment number
            char* dest=recvData.data+header->fragmentOffset;
            memcpy(dest, payload, header->fragmentContentSize);
        }

        void HandleUnackedMessage(const MessageHeader* header, bool multicast, const char* payload, NodeInfo& ni)
        {
            lllog(8)<<L"COM: recvUnacked from: "<<ni.node.nodeId<<L", sendMethod: "<<
                      SendMethodToString(header->sendMethod).c_str()<<
                      L", seq: "<<header->sequenceNumber<<std::endl;

            Channel& ch=ni.GetChannel(header);

            if (header->sequenceNumber==ch.lastInSequence+1)
            {
                m_gotRecvFrom(header->commonHeader.senderId, multicast, false);
                Insert(header, payload, ni); //message received in correct order, just insert
            }
            else if (header->sequenceNumber>ch.lastInSequence)
            {
                m_gotRecvFrom(header->commonHeader.senderId, multicast, false);

                //this is a message with bigger seq but we have missed something inbetween
                //reset receive queue, since theres nothing old we want to keep any longer
                //we need to deallocate the data since it has not been delivered to the subscriber

                lllog(8) << L"COM: Recv unacked message with seqNo gap (i.e messages have been lost), received seqNo " << header->sequenceNumber << std::endl;

                //deallocate memory, use first slot to be sure to only do it once
                if (ch.queue[0].data!=nullptr)
                {
                    auto recvIt = m_receivers.find(ch.queue[0].dataType);
                    if (recvIt == m_receivers.end())
                    {
                        std::ostringstream os;
                        os << "COM: (HandleUnackedMessage) Cant find registered receiver for dataType "<<ch.queue[0].dataType<<" when trying to deallocate data."
                           <<" This should be impossible since we have managed to allocate the data using the ame dataType."
                           << " When it happened we got message = " << header->ToString();
                        SEND_SYSTEM_LOG(Error, <<os.str().c_str());
                        throw std::logic_error(os.str());
                    }

                    recvIt->second.dealloc(ch.queue[0].data);
                    //ch.queue[0].Dealloc(recvIt->second.dealloc);
                }

                //clear the queue and be ready for a new message
                for (size_t i=0; i<ch.queue.Size(); ++i)
                {
                    ch.queue[i].Clear();
                }

                if (header->fragmentNumber==0)
                {
                    //this is something that we want to keep
                    ch.lastInSequence=header->sequenceNumber-1;
                    Insert(header, payload, ni);
                }
                else
                {
                    //in the middle of a fragmented message, nothing we want to keep. Set lastInSeq to match with the beginning of next new message
                    ch.lastInSequence=header->sequenceNumber+header->numberOfFragments-header->fragmentNumber-1; //calculate nextStartOfNewMessage-1
                }
            }
            else
            {
                lllog(8)<<L"COM: Recv unacked message too old seqNo, received seqNo "<<header->sequenceNumber<<L", expected "<<ch.lastInSequence+1<<std::endl;
                m_gotRecvFrom(header->commonHeader.senderId, multicast, true); //duplicated message
            }
        }

        void HandleWelcome(const MessageHeader* header, const char* payload, NodeInfo& ni)
        {
            int64_t nodeThatIsWelcome=*reinterpret_cast<const int64_t*>(payload);

            Channel& ch=ni.GetChannel(header);

            if (nodeThatIsWelcome==m_myId) //another node says welcome to us
            {
                if (ch.welcome==UINT64_MAX)
                {
                    lllog(8)<<L"COM: Got welcome from node "<<header->commonHeader.senderId<<
                              L", seq: "<<header->sequenceNumber<<", "<<SendMethodToString(header->sendMethod).c_str()<<std::endl;

                    ch.welcome=header->sequenceNumber;
                    ch.lastInSequence=ch.welcome-1; //will make this message to be exactly what was expected to come
                    ch.biggestSequence=ch.welcome;
                }
                else if (header->sequenceNumber==ch.welcome)
                {
                    //duplicated welcome
                    lllog(8)<<L"COM: Got duplicated welcome from node "<<header->commonHeader.senderId<<
                              L", seq: "<<header->sequenceNumber<<", "<<SendMethodToString(header->sendMethod).c_str()<<std::endl;
                }
                else
                {
                    //should not happen, logical error. Got new welcome from same node.
                    std::ostringstream os;
                    os<<"COM ["<<m_myId<<"]: Logical error, got new welcome from node "<<header->commonHeader.senderId<<
                        ", seq: "<<header->sequenceNumber<<", "<<SendMethodToString(header->sendMethod)<<
                        ". Already receive welcome from that node, old value was: "<<ch.welcome;
                    SEND_SYSTEM_LOG(Error, <<os.str().c_str());
                    lllog(8)<<os.str().c_str()<<std::endl;
                    throw std::logic_error(os.str());
                }
            }
            else
            {
                //welcome message not for this node. we have to check if we
                std::wostringstream os;
                os<<L"COM ["<<m_myId<<L"]: Welcome not for us. From "<<header->commonHeader.senderId<<L" to "<<
                    nodeThatIsWelcome<<L", seq: "<<header->sequenceNumber;

                if (ch.welcome<=header->sequenceNumber)
                {
                    os<<L". I was welcome at seq "<<ch.welcome<<L" and will ack this message.";
                }
                else if (ch.welcome==UINT64_MAX)
                {
                    os<<L". I have still not got any welcome from that node and will NOT ack this message.";
                }
                else
                {
                    os<<L". I was welcome at "<<ch.welcome<<L" and will NOT ack this message.";
                }

                lllog(8)<<os.str()<<std::endl;
            }
        }

        //Return true if ack shall be sent immediately.
        bool HandleAckedMessage(const MessageHeader* header, bool multicast, const char* payload, NodeInfo& ni)
        {
            if (header->commonHeader.dataType==WelcomeDataType)
            {
                HandleWelcome(header, payload, ni);
            }

            lllog(8)<<L"COM: HandleAckedMessage from: "<<header->commonHeader.senderId<<L", sendMethod: "<<
                      SendMethodToString(header->sendMethod).c_str()<<
                      L", seq: "<<header->sequenceNumber<<std::endl;

            Channel& ch=ni.GetChannel(header);

            if (header->sequenceNumber<ch.welcome)
            {
                //this message was sent before we got a welcome message, i.e not for us
                lllog(5)<<"Acked msg seq: "<<header->sequenceNumber<<" from: "<<header->commonHeader.senderId<<", was sent before we got welcome. I will not ack."<<std::endl;
                m_gotRecvFrom(header->commonHeader.senderId, multicast, false);
                return false; //dont send ack
            }
            else if (header->sequenceNumber<=ch.lastInSequence)
            {
                //duplicated message, we must always ack this since it is possible that an ack is lost and the sender has started to resend.
                lllog(8)<<L"COM: Recv duplicated message in order. Seq: "<<header->sequenceNumber<<L" from node "<<ni.node.name.c_str()<<std::endl;
                m_gotRecvFrom(header->commonHeader.senderId, multicast, true);
                return true;
            }
            else if (header->sequenceNumber==ch.lastInSequence+1)
            {
                //The Normal case: Message in correct order. Ack only if sender has requested an ack.
                m_gotRecvFrom(header->commonHeader.senderId, multicast, false);
                Insert(header, payload, ni);
                return header->ackNow==1;
            }
            else if (header->sequenceNumber<=ch.lastInSequence+m_slidingWindowSize)
            {
                //This is something within our receive window but out of order, the gaps will eventually be filled in
                //when sender retransmits non-acked messages.
                m_gotRecvFrom(header->commonHeader.senderId, multicast, false);
                Insert(header, payload, ni);
                return true; //we ack everything we have so far so that the sender becomes aware of the gaps
            }
            else //lost messages, got something too far ahead
            {
                m_gotRecvFrom(header->commonHeader.senderId, multicast, false);

                //This is a logic error in the code. We received something too far ahead. Means that the sender thinks that we have acked a
                //message that we dont think we have got at all.
                //All the code here just produce helpfull logs.
                std::ostringstream os;
                os<<"COM: I must be dead, the others are moving forward without me!!! Node "<<m_myId<<" received message from node "<<header->commonHeader.senderId<<
                    " that is too far ahead which means that we have lost a message. "<<
                    SendMethodToString(header->sendMethod)<<", seq: "<<header->sequenceNumber<<"\n     RecvQueue - lastInSeq: "<<ch.lastInSequence<<
                    ", biggestSeq: "<<ch.biggestSequence<<", welcome: "<<ch.welcome<<
                    ", recvQueue: [";
                size_t firstNonFree=ch.queue.Size()+1;
                for (size_t i=0; i<ch.queue.Size(); ++i)
                {
                    if (ch.queue[i].free)
                    {
                        os<<"X  ";
                    }
                    else
                    {
                        os<<ch.queue[i].sequenceNumber<<"  ";
                        if (i<firstNonFree)
                        {
                            firstNonFree=i;
                        }
                    }
                }
                os<<"]";
                if (firstNonFree<ch.queue.Size() && ch.queue[firstNonFree].free==false)
                {
                    const auto& recvData=ch.queue[firstNonFree];
                    os<<". More info about seq "<<recvData.sequenceNumber<<
                        ", size: "<<recvData.dataSize<<
                        ", numFrags: "<<recvData.numberOfFragments<<
                        ", fragment: "<<recvData.fragmentNumber;

                }
                SEND_SYSTEM_LOG(Error, <<os.str().c_str());
                lllog(8)<<os.str().c_str()<<std::endl;
                return false;
            }
        }

        void CalculateIndices(uint64_t lastSeq, const MessageHeader* header,
                              size_t& currentIndex, size_t& firstIndex, size_t& lastIndex) const
        {
            currentIndex=static_cast<size_t>(header->sequenceNumber-lastSeq-1);
            int first=static_cast<int>(currentIndex)-header->fragmentNumber;
            firstIndex=static_cast<size_t>( std::max(0, first) );
            lastIndex=std::min(m_slidingWindowSize-1, currentIndex+header->numberOfFragments-header->fragmentNumber-1);
        }

        void Deliver(NodeInfo& ni, const MessageHeader* header)
        {
            Channel& ch=ni.GetChannel(header);

            for (size_t i=0; i<ch.queue.Size(); ++i)
            {
                RecvData& rd=ch.queue[0];
                if (!rd.free)
                {
                    assert(rd.sequenceNumber==ch.lastInSequence+1);
                    ch.lastInSequence=rd.sequenceNumber;
                    if (rd.fragmentNumber+1==rd.numberOfFragments)
                    {
                        //last fragment has been received, we can deliver this message to application
                        auto fromId=ni.node.nodeId;
                        auto fromNodeType=ni.node.nodeTypeId;
                        auto dataPtr=rd.data;
                        auto dataSize=rd.dataSize;
                        auto dataType=rd.dataType;

                        m_numberOfUndeliveredMessages++;
                        m_deliverStrand.post([this,dataType,fromId, fromNodeType,dataPtr,dataSize]
                        {
                            auto recvIt=m_receivers.find(dataType); //m_receivers shall be safe to use inside m_deliverStrand since it is not supposed to be modified after start
                            if (recvIt!=m_receivers.end())
                            {
                                recvIt->second.onRecv(fromId, fromNodeType, dataPtr, dataSize);
                            }
                            else
                            {
                                std::ostringstream os;
                                os<<"COM: Trying to deliver data from node "<<fromId<<" that has no registered receiver. DataTypeIdentifier: "<<dataType;
                                SEND_SYSTEM_LOG(Error, <<os.str().c_str());
                                throw std::logic_error(os.str());
                            }
                            m_numberOfUndeliveredMessages--;
                        });
                    }

                    //Check if queue contains fragmented message with more fragments than recvWindowSize, needs special handling
                    const RecvData& lastInQueue=ch.queue[ch.queue.Size()-1];
                    if (lastInQueue.dataSize>0 && lastInQueue.fragmentNumber<lastInQueue.numberOfFragments-1)
                    {
                        //lastInQueue has allocated buffer and is not the last fragment of the message
                        //remember if windowSize=1 then lastInQueue==rd, so we have to copy data before clearing rd
                        auto data=lastInQueue.data;
                        auto dataSize=lastInQueue.dataSize;
                        auto dataType=lastInQueue.dataType;
                        auto numberOfFragments=lastInQueue.numberOfFragments;
                        uint16_t fragmentNumber=lastInQueue.fragmentNumber+1;

                        rd.Clear();
                        ch.queue.Step();

                        RecvData& nextFragment=ch.queue[ch.queue.Size()-1];
                        nextFragment.data=data;
                        nextFragment.dataSize=dataSize;
                        nextFragment.dataType=dataType;
                        nextFragment.numberOfFragments=numberOfFragments;
                        nextFragment.fragmentNumber=fragmentNumber;
                    }
                    else //the normal case. Step up queue and the last item will be free without allocated buffer
                    {
                        rd.Clear(); //mark as free and release reference to data
                        ch.queue.Step(); //step queue so index zero (first in queue) is the next item
                    }

                }
                else
                {
                    //nothing more in order
                    break;
                }
            }
        }

        inline void SendAck(NodeInfo& ni, const MessageHeader* header)
        {
            Channel& ch=ni.GetChannel(header);
            auto ackPtr=std::make_shared<Ack>(m_myId, header->commonHeader.senderId, ch.biggestSequence, header->sendMethod);

            uint64_t seq=ch.biggestSequence;
            for (size_t i=0; i<m_slidingWindowSize; ++i)
            {
                if (seq>ch.lastInSequence)
                {
                    size_t index=static_cast<size_t>(seq-ch.lastInSequence-1);
                    ackPtr->missing[i]=(ch.queue[index].free ? 1 : 0);
                    --seq;
                }
                else
                {
                    ackPtr->missing[i]=0;
                }
            }

            lllog(9)<<L"COM: SendAck "<<ackPtr->ToString().c_str()<<std::endl;
            WriterType::SendTo(ackPtr, ni.endpoint);
        }

        inline Safir::Utilities::Internal::SharedCharArray MakePtr(const char* data, size_t size)
        {
            auto ptr=Safir::Utilities::Internal::MakeSharedArray(size);
            memcpy(ptr.get(), data, size);
            return ptr;
        }
    };

    typedef DeliveryHandlerBasic< Writer<Ack> > DeliveryHandler;
}
}
}
}
