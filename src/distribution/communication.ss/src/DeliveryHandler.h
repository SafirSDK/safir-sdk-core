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
#ifndef __SAFIR_DOB_COMMUNICATION_DELIVERY_HANDLER_H__
#define __SAFIR_DOB_COMMUNICATION_DELIVERY_HANDLER_H__

#include <atomic>
#include <boost/unordered_map.hpp>
#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
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
    typedef std::function<void(int64_t fromNodeId, int64_t fromNodeType, const boost::shared_ptr<char[]>& data, size_t size)> ReceiveData;
    typedef std::function<void(int64_t fromNodeId)> GotReceiveFrom;

    template <class WriterType>
    class DeliveryHandlerBasic : private WriterType
    {
    public:
        DeliveryHandlerBasic(boost::asio::io_service::strand& receiveStrand, int64_t myNodeId, int ipVersion)
            :WriterType(receiveStrand.get_io_service(), ipVersion)
            ,m_running(false)
            ,m_myId(myNodeId)
            ,m_receiveStrand(receiveStrand)
            ,m_deliverStrand(receiveStrand.get_io_service())
            ,m_nodes()
            ,m_receivers()
            ,m_gotRecvFrom()
        {
            //TODO: use initialization instead. This is due to problems with VS2013
            m_numberOfUndeliveredMessages=0;
        }

        void Start()
        {
            m_receiveStrand.dispatch([=]
            {
                m_running=true;
            });
        }

        void Stop()
        {
            m_receiveStrand.dispatch([=]
            {
                m_running=false;
            });

        }

        //Received data to be delivered up to the application. Everythin must be called from readStrand.
        void SetGotRecvCallback(const GotReceiveFrom& callback)
        {
            m_gotRecvFrom=callback;
        }

        void SetReceiver(const ReceiveData& callback, int64_t dataTypeIdentifier)
        {
            m_receivers.insert(std::make_pair(dataTypeIdentifier, callback));
        }

        //Handle received data and deliver to application if possible and sends ack back to sender.
        void ReceivedApplicationData(const MessageHeader* header, const char* payload)
        {
            //Always called from readStrand
            auto senderIt=m_nodes.find(header->commonHeader.senderId);

            if (senderIt==m_nodes.end() || !senderIt->second.node.systemNode)
            {
                lllog(4)<<L"COM: Received data from unknown node or a non system node with id="<<header->commonHeader.senderId<<std::endl;
                return;
            }

            lllog(8)<<L"COM: Received AppData from "<<header->commonHeader.senderId<<std::endl;
            m_gotRecvFrom(header->commonHeader.senderId); //report that we are receivinga intact data from the node

            bool ackNow=HandleMessage(header, payload, senderIt->second);
            Deliver(senderIt->second, header); //if something is now fully received, deliver it to application

            if (ackNow)
            {
                SendAck(senderIt->second, header);
            }
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
            m_nodes.insert(std::make_pair(node.nodeId, NodeInfo(node)));
        }

        //Make node included. If excluded it is also removed.
        void IncludeNode(int64_t id)
        {
            const auto it=m_nodes.find(id);
            assert(it!=m_nodes.end());

            it->second.node.systemNode=true;
        }

        //Make node included or excluded. If excluded it is also removed.
        void RemoveNode(int64_t id)
        {
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

        typedef boost::unordered_map<int64_t, ReceiveData>  ReceiverMap;

        struct RecvData
        {
            bool free;
            uint64_t dataType;
            uint64_t sequenceNumber;
            uint16_t fragmentNumber;
            uint16_t numberOfFragments;
            boost::shared_ptr<char[]> data;
            size_t dataSize;

            RecvData()
                :free(true)
                ,dataType(0)
                ,sequenceNumber(0)
                ,fragmentNumber(0)
                ,numberOfFragments(0)
                ,data()
                ,dataSize(0)
            {
            }

            void Clear()
            {
                free=true;
                data.reset();
                dataSize=0;
            }
        };

        struct Channel
        {
            uint64_t lastInSequence; //last sequence number that was moved out of the queue. seq(queue[0])-1
            uint64_t biggestSequence; //biggest sequence number recevived (within our window)
            CircularArray<RecvData> queue;
            Channel()
                :lastInSequence(0)
                ,biggestSequence(0)
                ,queue(Parameters::SlidingWindowSize)
            {
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

            NodeInfo(const Node& node_)
                :node(node_)
                ,endpoint(Utilities::CreateEndpoint(node.unicastAddress))
            {
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
        int64_t m_myId;
        boost::asio::strand& m_receiveStrand; //for sending acks, same strand as all public methods are supposed to be called from
        boost::asio::strand m_deliverStrand; //for delivering data to application
        std::atomic_uint m_numberOfUndeliveredMessages;

        NodeInfoMap m_nodes;
        ReceiverMap m_receivers;
        GotReceiveFrom m_gotRecvFrom;

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
            if (!recvData.data)
            {
                recvData.data.reset(new char[header->totalContentSize]);
                recvData.dataSize=header->totalContentSize;

                //copy buffer to other indices if it shall be shared among many fragments
                uint16_t tmpFragNo=0;
                for (size_t i=firstIndex; i<currentIndex; ++i)
                {
                    ch.queue[i].data=recvData.data;
                    ch.queue[i].dataSize=recvData.dataSize;
                    ch.queue[i].numberOfFragments=recvData.numberOfFragments;
                    ch.queue[i].fragmentNumber=tmpFragNo++;
                }
                for (size_t i=currentIndex+1; i<=lastIndex; ++i)
                {
                    ch.queue[i].data=recvData.data;
                    ch.queue[i].dataSize=recvData.dataSize;
                    ch.queue[i].numberOfFragments=recvData.numberOfFragments;
                    ch.queue[i].fragmentNumber=++tmpFragNo;
                }
            }

            //copy data to correct offset in buffer depending on fragment number
            char* dest=recvData.data.get()+header->fragmentOffset;
            memcpy(dest, payload, header->fragmentContentSize);
        }

        void ForceInsert(const MessageHeader* header, const char* payload, NodeInfo& ni)
        {
            Channel& ch=ni.GetChannel(header);
            ch.lastInSequence=header->sequenceNumber-1; //we pretend this was exactly what we expected to get
            //Clear the queue, should not be necessary as long as we dont let excluded nodes come back.
            for (size_t i=0; i<Parameters::SlidingWindowSize; ++i)
            {
                ch.queue[i].Clear();
            }

            //Call the normal insert
            Insert(header, payload, ni);
        }

        //Return true if ack shall be sent immediately.
        bool HandleMessage(const MessageHeader* header, const char* payload, NodeInfo& ni)
        {
            lllog(8)<<L"COM: recv from: "<<ni.node.nodeId<<L", sendMethod: "<<
                      (header->sendMethod==SingleReceiverSendMethod ? L"Unicast" : L"Multicast")<<
                      L", seq: "<<header->sequenceNumber<<std::endl;

            Channel& ch=ni.GetChannel(header);

            if (header->deliveryGuarantee==Acked)
            {
                if (ch.lastInSequence==0)
                {
                    //first time we receive anything, we accept any seqNo and start counting from there
                    //however we cannot start in the middle of a fragmented message.
                    if (header->fragmentNumber==0) //we cannot start in the middle of a fragmented message
                    {
                        //this is the first fragment of the message, keep it and start sequnceNumber from here.
                        ForceInsert(header, payload, ni);
                        return header->ackNow==1;
                    }
                    else
                    {
                        //not the first fragment of the message, we have to wait for beginning of a new message before we start
                        return false; //no ack to send
                    }
                }
                else if (header->sequenceNumber<=ch.lastInSequence)
                {
                    //duplicated message, we must always ack this since it is possible that an ack is lost and the sender has started to resend.
                    lllog(8)<<L"COM: Recv duplicated message in order. Seq: "<<header->sequenceNumber<<L" from node "<<ni.node.name.c_str()<<std::endl;
                    return true;
                }
                else if (header->sequenceNumber==ch.lastInSequence+1)
                {
                    //The Normal case: Message in correct order. Ack only if sender has requested an ack.
                    Insert(header, payload, ni);
                    return header->ackNow==1;
                }
                else if (header->sequenceNumber<=ch.lastInSequence+Parameters::SlidingWindowSize)
                {
                    //This is something within our receive window but out of order, the gaps will eventually be filled in
                    //when sender retransmits non-acked messages.
                    Insert(header, payload, ni);
                    return true; //we ack evertying we have so far so that the sender becomes aware of the gaps
                }
                else //lost messages, got something too far ahead
                {
                    //This can occur if the senders send window is bigger than the receivers receive window, and it should never happen.
                    //If everything is working as expected we can just ignore this message, but still it is a logic error in the code!
                    //Sooner or later the sender must retransmit all non-acked messages, and in time this message will come into our receive window.
                    lllog(8)<<L"COM: Received Seq: "<<header->sequenceNumber<<" wich means that we have lost a message. LastInSequence="<<ch.lastInSequence<<std::endl;
                    std::wcout<<L"COM: Received Seq: "<<header->sequenceNumber<<" wich means that we have lost a message. LastInSequence="<<ch.lastInSequence<<std::endl;
                    return false; //we dont ack messages we are not supposed to get
                }                                
            }
            else //unacked
            {
                if (header->sequenceNumber==ch.lastInSequence+1)
                {
                    Insert(header, payload, ni); //message received in correct order, just insert
                }
                else if (header->sequenceNumber>ch.lastInSequence)
                {
                    //this is a message with bigger seq but we have missed something inbetween
                    //reset receive queue, since theres nothing old we want to keep any longer
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

                    lllog(8)<<L"COM: Recv unacked message with seqNo gap (i.e messages have been lost), received seqNo "<<header->sequenceNumber<<std::endl;
                }
                else
                {
                    lllog(8)<<L"COM: Recv unacked message too old seqNo, received seqNo "<<header->sequenceNumber<<L", expected "<<ch.lastInSequence+1<<std::endl;
                }
                return false;
            }
        }

        void CalculateIndices(uint64_t lastSeq, const MessageHeader* header,
                              size_t& currentIndex, size_t& firstIndex, size_t& lastIndex) const
        {
            currentIndex=static_cast<size_t>(header->sequenceNumber-lastSeq-1);
            int first=static_cast<int>(currentIndex)-header->fragmentNumber;
            firstIndex=static_cast<size_t>( std::max(0, first) );
            lastIndex=std::min(Parameters::SlidingWindowSize-1, currentIndex+header->numberOfFragments-header->fragmentNumber-1);
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
                        //auto seqNo=rd.sequenceNumber;


                        m_numberOfUndeliveredMessages++;

                        m_deliverStrand.post([=]
                        {
                            auto recvIt=m_receivers.find(dataType); //m_receivers shall be safe to use inside m_deliverStrand since it is not supposed to be modified after start
                            if (recvIt!=m_receivers.end())
                            {
                                recvIt->second(fromId, fromNodeType, dataPtr, dataSize);
                            }
                            else
                            {
                                std::ostringstream os;
                                os<<"COM: Received data from node "<<fromId<<" that has no registered receiver. DataTypeIdentifier: "<<dataType<<std::endl;
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
                        boost::shared_ptr<char[]> data=lastInQueue.data;
                        size_t dataSize=lastInQueue.dataSize;
                        uint16_t numberOfFragments=lastInQueue.numberOfFragments;
                        uint16_t fragmentNumber=lastInQueue.fragmentNumber+1;

                        rd.Clear();
                        ch.queue.Step();

                        RecvData& nextFragment=ch.queue[ch.queue.Size()-1];
                        nextFragment.data=data;
                        nextFragment.dataSize=dataSize;
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
            auto ackPtr=boost::make_shared<Ack>(m_myId, header->commonHeader.senderId, ch.biggestSequence, header->sendMethod);

            uint64_t seq=ch.biggestSequence;
            for (size_t i=0; i<Parameters::SlidingWindowSize; ++i)
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

            WriterType::SendTo(ackPtr, ni.endpoint);
        }

        inline boost::shared_ptr<char[]> MakePtr(const char* data, size_t size)
        {
            boost::shared_ptr<char[]> ptr=boost::make_shared<char[]>(size);
            memcpy(ptr.get(), data, size);
            return ptr;
        }
    };

    typedef DeliveryHandlerBasic< Writer<Ack> > DeliveryHandler;
}
}
}
}

#endif
