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
#include <memory>
#include <atomic>
#include <functional>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/SharedCharArray.h>
#include "Node.h"
#include "MessageQueue.h"
#include "Parameters.h"
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


#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4127)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    /**
     * @brief DataSender keeps a send queue of messages. It will keep track of received Ack's and free the
     * messageQueue item when all ack's have been received. The class is also responsible for retransmission of
     * messages that have not been acked.
     */

    typedef std::function<void(int64_t toNodeId, size_t transmitCount)> RetransmitTo;
    typedef std::function<void(int64_t nodeTypeId)> QueueNotFull;

    template <class WriterType>
    class DataSenderBasic : private WriterType
    {
    public:
        DataSenderBasic(boost::asio::io_service& ioService,
                        uint8_t deliveryGuarantee,
                        int64_t nodeTypeId,
                        int64_t nodeId,
                        int ipVersion,
                        const std::string& localIf,
                        const std::string& multicastAddress,
                        int slidingWindowSize,
                        int ackRequestThreshold,
                        const std::vector<int>& retryTimeout,
                        int fragmentSize)
            :WriterType(ioService, ipVersion, localIf, multicastAddress)
            ,m_strand(ioService)
            ,m_deliveryGuarantee(deliveryGuarantee)
            ,m_nodeTypeId(nodeTypeId)
            ,m_nodeId(nodeId)
            ,m_slidingWindowSize(static_cast<size_t>(slidingWindowSize))
            ,m_ackRequestThreshold(static_cast<size_t>(ackRequestThreshold))
            ,m_sendQueue(Parameters::SendQueueSize)
            ,m_running(false)
            ,m_retryTimeout(retryTimeout)
            ,m_fragmentDataSize(static_cast<size_t>(fragmentSize)-MessageHeaderSize)
            ,m_nodes()
            ,m_lastSentMultiReceiverSeqNo(0)
            ,m_lastAckRequestMultiReceiver(0)
            ,m_resendTimer(ioService)
            ,m_pingTimer(ioService)
            ,m_retransmitNotification()
            ,m_queueNotFullNotification()
            ,m_queueNotFullNotificationLimit(Parameters::SendQueueSize/2)
            ,m_sendAckRequestForMsgIndex()
            ,m_logPrefix(GenerateLogPrefix(deliveryGuarantee,nodeTypeId))
        {
            m_sendQueueSize=0;
            m_notifyQueueNotFull=false;
        }

        //Set notifier called every time something is retransmitted
        void SetRetransmitCallback(const RetransmitTo& callback)
        {
            m_strand.dispatch([this,callback]{m_retransmitNotification=callback;});
        }

        void SetNotFullCallback(const QueueNotFull& callback)
        {
            m_strand.dispatch([this,callback]{m_queueNotFullNotification.push_back(callback);});
        }

        //Start writer component
        void Start()
        {
            m_strand.dispatch([this]
            {
                m_running=true;
                //start retransmit timer
                if (m_deliveryGuarantee==Acked)
                {
                    RetransmitUnackedMessages();
                    if (IsMulticastEnabled())
                    {
                        //if multicast nodeType we have to assure some acked traffic to prevent nodes from missunderstanding received heartbeats
                        //from nodes that have excluded us. By sending low traffic acked pings, we detect nodes that does not respond to us.
                        m_lastSendTime=boost::chrono::steady_clock::now();
                        Ping();
                    }
                }
            });
        }

        //Stop
        void Stop()
        {
            m_strand.dispatch([this]
            {
                m_running=false;
                if (m_deliveryGuarantee==Acked)
                {
                    m_resendTimer.cancel();
                    if (IsMulticastEnabled())
                    {
                        m_pingTimer.cancel();
                    }
                }
            });
        }

        //Add message to send queue. Message will be retranmitted unitl all receivers have acked. Returns false if queue is full.
        bool AddToSendQueue(int64_t toId, const Safir::Utilities::Internal::SharedConstCharArray& msg, size_t size, int64_t dataTypeIdentifier)
        {
            if (size==0 || !msg)
            {
                return true; //we dont sent empty messages
            }

            //calculate number of fragments
            size_t numberOfFullFragments=size/m_fragmentDataSize;
            size_t restSize=size%m_fragmentDataSize;
            size_t totalNumberOfFragments=numberOfFullFragments+(restSize>0 ? 1 : 0);

            if (++m_sendQueueSize<=Parameters::SendQueueSize)
            {
                //there is room for at least one fragment within the queue limit.
                //then we step up the total amount, even if it will exceed the queue limit. Send queue will handle this case.
                m_sendQueueSize+=static_cast<unsigned int>(totalNumberOfFragments-1); //note that one already been added
            }
            else //not room for one more fragment
            {
                lllog(5)<<m_logPrefix.c_str()<<"SendQueue full"<<std::endl;
                --m_sendQueueSize;
                m_notifyQueueNotFull=true;
                return m_deliveryGuarantee!=Acked; //return Acked->false, Unacked->true
            }

            //The actual work where the data is inserted in the queue must be done inside the strand.
            m_strand.post([this,toId,totalNumberOfFragments,numberOfFullFragments,msg,size,restSize,dataTypeIdentifier]
            {
                uint8_t sendMethod;
                uint64_t* sequenceSerie=GetSequenceSerieIfExist(toId, sendMethod);
                if (sequenceSerie==nullptr)
                {
                    m_sendQueueSize-=static_cast<unsigned int>(totalNumberOfFragments);
                    //receiver does not exist so we just throw it away
                    lllog(9)<<m_logPrefix.c_str()<<"Receiver does not exist. Message will not be sent."<<std::endl;
                    return;
                }

                for (size_t frag=0; frag<numberOfFullFragments; ++frag)
                {
                    ++(*sequenceSerie);
                    const char* fragment=msg.get()+frag*m_fragmentDataSize;
                    UserDataPtr userData(new UserData(m_nodeId, toId, dataTypeIdentifier, msg, size, fragment, m_fragmentDataSize));
                    userData->header.commonHeader.receiverId=toId;
                    userData->header.sendMethod=sendMethod;
                    userData->header.sequenceNumber=*sequenceSerie;
                    userData->header.deliveryGuarantee=m_deliveryGuarantee;
                    userData->header.numberOfFragments=static_cast<uint16_t>(totalNumberOfFragments);
                    userData->header.fragmentNumber=static_cast<uint16_t>(frag);
                    SetRequestAck(userData->header);
                    m_sendQueue.enqueue(userData);
                }

                if (restSize>0)
                {
                    ++(*sequenceSerie);
                    const char* fragment=msg.get()+numberOfFullFragments*m_fragmentDataSize;
                    UserDataPtr userData(new UserData(m_nodeId, toId, dataTypeIdentifier, msg, size, fragment, restSize));
                    userData->header.commonHeader.receiverId=toId;
                    userData->header.sendMethod=sendMethod;
                    userData->header.sequenceNumber=*sequenceSerie;
                    userData->header.deliveryGuarantee=m_deliveryGuarantee;
                    userData->header.numberOfFragments=static_cast<uint16_t>(totalNumberOfFragments);
                    userData->header.fragmentNumber=static_cast<uint16_t>(totalNumberOfFragments-1);
                    SetRequestAck(userData->header);
                    m_sendQueue.enqueue(userData);
                }

                HandleSendQueue();
            });

            return true;
        }

        //Handle received Acks. Messages that have been acked from all receivers will be removed from sendQueue.
        void HandleAck(const Ack& ack)
        {
            //Called from readStrand

            //Will only check ack against sent messages. If an ack is received for a message that is still unsent, that ack will be ignored.
            m_strand.dispatch([this,ack]
            {
                if (Safir::Utilities::Internal::Internal::LowLevelLogger::Instance().LogLevel()==9)
                {
                    lllog(9)<<m_logPrefix.c_str()<<"HandleAck "<<ack.ToString().c_str()<<std::endl;
                }

                //Update queue
                for (size_t i=0; i<m_sendQueue.first_unhandled_index(); ++i)
                {
                    UserDataPtr& ud=m_sendQueue[i];

                    auto receiverListIt=ud->receivers.find(ack.commonHeader.senderId);
                    if (receiverListIt==ud->receivers.end())
                    {
                        //the ack-sender is not a receiver of this message, continue
                        continue;
                    }

                    if (ud->header.sendMethod==ack.sendMethod && ud->header.sequenceNumber<=ack.sequenceNumber)
                    {
                        //calculate index in missing-array
                        size_t index=static_cast<size_t>(ack.sequenceNumber-ud->header.sequenceNumber);

                        if (index>m_slidingWindowSize-1)
                        {
                            std::ostringstream os;
                            os<<m_logPrefix<<"Programming Error! Index out of range. Calculated missing index: "<<index<<". AckContent: "<<ack.ToString()<<"\n"<<SendQueueToString();
                            lllog(1)<<os.str().c_str()<<std::endl;
                            SEND_SYSTEM_LOG(Error, <<os.str().c_str()<<std::endl);
                            throw std::logic_error(os.str());
                        }

                        if (ack.missing[index]!=1)
                        {
                            ud->receivers.erase(receiverListIt);
                        }
                        else
                        {
                            //AckSender is missing a message.
                            //if the message has already been transmitted 2 times (i.e retransmitted 1 time), we fall back on the retransmit timeouts only.
                            //This is because every ack will have the missign message marked in the missing list and the retransmit count will explode if
                            //we retransmit for every ack received.
                            if (ud->transmitCount<2)
                            {
                                //resend immediately if we have not retransmitted this message before
                                RetransmitMessage(ud);
                            }
                        }
                    }
                }

                //Remove from beginning as long as all is acked
                RemoveCompletedMessages();
            });
        }

        //Add a node.
        void AddNode(int64_t id, const std::string& address)
        {
            m_strand.dispatch([this,id,address]
            {
                if (m_nodes.find(id)!=m_nodes.end())
                {
                    std::ostringstream os;
                    os<<m_logPrefix.c_str()<<"Duplicated call to DataSender.AddNode with same nodeId! NodeId: "<<id<<", address: "<<address;
                    throw std::logic_error(os.str());
                }

                NodeInfo ni;
                ni.endpoint=Resolver::StringToEndpoint(address);
                ni.lastSentSeqNo=0;
                ni.welcome=UINT64_MAX;
                ni.systemNode=false;
                m_nodes.insert(std::make_pair(id, ni));
            });
        }

        //Make node included or excluded. If excluded it is also removed.
        void IncludeNode(int64_t id)
        {
            m_strand.post([this,id]
            {
                const auto it=m_nodes.find(id);
                if (it!=m_nodes.end())
                {
                    if (it->second.systemNode)
                    {
                        lllog(5)<<m_logPrefix.c_str()<<" IncludeNode called for an already included node, nodeId: "<<boost::lexical_cast<std::wstring>(id)<<std::endl;
                    }
                    else
                    {
                        it->second.systemNode=true;
                        if (m_deliveryGuarantee==Acked)
                        {
                            PostWelcome(id);
                        }
                        else //unacked data does not have to wait for welcome
                        {
                            it->second.welcome=0;
                        }
                    }
                }
            });
        }

        //Make node included or excluded. If excluded it is also removed.
        void RemoveNode(int64_t id)
        {
            m_strand.post([this,id]
            {
                m_nodes.erase(id);
                RemoveExcludedReceivers();
                RemoveCompletedMessages();
            });
        }

        size_t SendQueueSize() const
        {
            return m_sendQueueSize;
        }

#ifndef SAFIR_TEST
    private:
#endif
        //VS2010 does not allow call of WriterType::IsMulticastEnabled inside lambdas (sigh), so
        //we make it explicitly available in this class, so we can call it without qualification.
        using WriterType::IsMulticastEnabled;

        struct NodeInfo
        {
            bool systemNode;
            boost::asio::ip::udp::endpoint endpoint;
            uint64_t lastSentSeqNo; //last used, added to sendQueue, not necessarily sent.
            uint64_t welcome;
        };

        boost::asio::io_service::strand m_strand;
        const uint8_t m_deliveryGuarantee;
        const int64_t m_nodeTypeId;
        const int64_t m_nodeId;
        const size_t m_slidingWindowSize;
        const size_t m_ackRequestThreshold;
        MessageQueue<UserDataPtr> m_sendQueue;
        std::atomic<unsigned int> m_sendQueueSize;
        bool m_running;
        const std::vector<int> m_retryTimeout;
        const size_t m_fragmentDataSize; //size of a fragments data part, excluding header size.
        std::map<int64_t, NodeInfo> m_nodes;
        uint64_t m_lastSentMultiReceiverSeqNo; // used both for multicast and unicast as long as the message is a multireceiver message. Actually it is lastUsedSeq since message may not have been sent yet.
        uint64_t m_lastAckRequestMultiReceiver; //the last seq we have requested ack
        boost::asio::steady_timer m_resendTimer;
        boost::asio::steady_timer m_pingTimer;
        boost::chrono::steady_clock::time_point m_lastSendTime; //timestamp of last time something was sent
        RetransmitTo m_retransmitNotification;
        std::vector<QueueNotFull> m_queueNotFullNotification;
        size_t m_queueNotFullNotificationLimit; //below number of used slots. NOT percent.
        std::atomic<bool> m_notifyQueueNotFull;
        std::vector<size_t> m_sendAckRequestForMsgIndex;
        const std::string m_logPrefix;

        static std::string GenerateLogPrefix(uint8_t deliveryGuarantee, int64_t nodeTypeId)
        {
           std::ostringstream os;
           os<<"COM: ("<<DeliveryGuaranteeToString(deliveryGuarantee)<<"DataSender nodeType "<<nodeTypeId<<") - ";
           return os.str();
        }

        void PostWelcome(int64_t nodeId)
        {
            auto& ni=m_nodes[nodeId];
            ++m_lastSentMultiReceiverSeqNo;
            ni.welcome=m_lastSentMultiReceiverSeqNo;

            ++m_sendQueueSize;
            auto welcome=Safir::Utilities::Internal::MakeSharedArray(sizeof(int64_t));
            memcpy(static_cast<void*>(welcome.get()), static_cast<const void*>(&nodeId), sizeof(int64_t));
            UserDataPtr userData=std::make_shared<UserData>(m_nodeId, 0, WelcomeDataType, welcome, sizeof(int64_t), welcome.get(), sizeof(int64_t));
            userData->header.sendMethod=MultiReceiverSendMethod;
            userData->header.sequenceNumber=ni.welcome;
            userData->header.deliveryGuarantee=true;
            userData->header.numberOfFragments=1;
            userData->header.fragmentNumber=0;
            userData->header.ackNow=1;
            userData->receivers.insert(nodeId);
            m_sendQueue.enqueue(userData);

            lllog(8)<<m_logPrefix.c_str()<<"Welcome posted from "<<m_nodeId<<L" to "<<nodeId<<", seq: "<<ni.welcome<<std::endl;
            HandleSendQueue();
        }

        void SetRequestAck(MessageHeader& header) const
        {
            auto requestAck = (header.sendMethod==SingleReceiverSendMethod) ||
                    (header.sequenceNumber % m_ackRequestThreshold==0) ||
                    header.commonHeader.dataType==PingDataType;
            header.ackNow = requestAck ? 1 : 0;
        }

        //Send new messages in sendQueue. No retransmits sent here.
        void HandleSendQueue()
        {
            //must be called from writeStrand

            //Send all unhandled messges that are within our sender window
            while (m_sendQueue.has_unhandled() && m_sendQueue.first_unhandled_index()<m_slidingWindowSize)
            {
                UserDataPtr& ud=m_sendQueue[m_sendQueue.first_unhandled_index()];
                ++ud->transmitCount;

                if (ud->header.sendMethod==MultiReceiverSendMethod) //this is message that shall be sent to every system node
                {
                    lllog(9)<<m_logPrefix.c_str()<<"Send to all seq: "<<ud->header.sequenceNumber<<", ackNow: "<<static_cast<int>(ud->header.ackNow)<<std::endl;

                    if (WriterType::IsMulticastEnabled()) //this node and all the receivers are capable of sending and receiving multicast
                    {
                        for (auto val = m_nodes.cbegin(); val != m_nodes.cend(); ++val)
                        {
                            if (val->second.systemNode && val->second.welcome<=ud->header.sequenceNumber)
                            {
                                //The node will get the message throuch the multicast message, we just add it to receiver list
                                //to be able to track the ack
                                ud->receivers.insert(val->first);
                            }
                        }

                        if (ud->receivers.size()>0)
                        {
                            WriterType::SendMulticast(ud);
                        }
                    }
                    else //this node is not multicast enabled, only send unicast. However it's still a multiReceiver message and the multiReceiverSeqNo shall be used
                    {
                        for (auto val = m_nodes.cbegin(); val != m_nodes.cend(); ++val)
                        {
                            if (val->second.systemNode && val->second.welcome<=ud->header.sequenceNumber)
                            {
                                ud->receivers.insert(val->first);
                                WriterType::SendTo(ud, val->second.endpoint);
                            }
                        }
                    }
                }
                else //messages has a specific receiver, send using unicast
                {
                    auto nodeIt=m_nodes.find(ud->header.commonHeader.receiverId);
                    if (nodeIt!=m_nodes.end() && nodeIt->second.systemNode)
                    {
                        lllog(9)<<m_logPrefix.c_str()<<"Send to: "<<ud->header.commonHeader.receiverId
                                <<",  seq: "<<ud->header.sequenceNumber<<", ackNow: "<<static_cast<int>(ud->header.ackNow)
                                << ", dataType:" << ud->header.commonHeader.dataType <<std::endl;
                        ud->receivers.insert(ud->header.commonHeader.receiverId);
                        NodeInfo& n=nodeIt->second;
                        WriterType::SendTo(ud, n.endpoint);
                    }
                    else
                    {
                        if (nodeIt==m_nodes.end())
                        {
                            //The node does not exist, this can happen if an application sends a message after the node has been excluded
                            //and is a  valid case that should be handled.
                            lllog(8)<<m_logPrefix.c_str()<<"receiver does not exist, id:  "<<ud->header.commonHeader.receiverId<<std::endl;
                        }
                        else //systemNode==false
                        {
                            //This case should not be possible for SingleReceiverSendMethod.
                            std::ostringstream os;
                            os<<m_logPrefix<<"Programming Error! There is an unsent message for a non-systemNode in the queue using SingleReceiverSendMethod. \n"
                                 <<ud->header.ToString()<<"\n"<<SendQueueToString();
                            lllog(1)<<os.str().c_str()<<std::endl;
                            SEND_SYSTEM_LOG(Error, <<os.str().c_str()<<std::endl);
                            throw std::logic_error(os.str());
                        }
                    }
                }

                if (m_deliveryGuarantee==Acked)
                {
                    ud->sendTime=boost::chrono::steady_clock::now();
                    m_lastSendTime=ud->sendTime;
                    m_sendQueue.step_unhandled();
                }
                else
                {
                    //if unacked, then immediately remove message from queue. Don't wait for ack.
                    m_sendQueue.dequeue();
                    --m_sendQueueSize;
                }
            }
        }

        boost::chrono::milliseconds GetRetryTimeout(size_t transmitCount) const
        {
            size_t index = transmitCount>0 ? transmitCount-1 : 0;
            return m_retryTimeout.size()>index ?
                        boost::chrono::milliseconds(m_retryTimeout[index]) :
                        boost::chrono::milliseconds(m_retryTimeout.back());
        }

        void RetransmitUnackedMessages()
        {
            if (!m_running)
            {
                return;
            }

            //Always called from writeStrand
            static const boost::chrono::milliseconds timerInterval(*std::min_element(m_retryTimeout.begin(), m_retryTimeout.end()) / 2);

            //Check if there is any unacked messages that are old enough to be retransmitted
            for (size_t i=0; i<m_sendQueue.first_unhandled_index(); ++i)
            {
                UserDataPtr& ud=m_sendQueue[i];
                auto durationSinceSend=boost::chrono::steady_clock::now()-ud->sendTime;
                auto retransmitLimit = GetRetryTimeout(ud->transmitCount);
                if (durationSinceSend>retransmitLimit)
                {
                    RetransmitMessage(ud);
                }
                else if (durationSinceSend>retransmitLimit/2 && ud->transmitCount<2 && m_ackRequestThreshold>1)
                {
                    //this message has never been retransmitted, we have not asked receiver to ack,
                    //and half the restransmitInterval has expired. It is time to send an explicit ack-request
                    //to avoid unecessary retransmits. If m_ackRequestThreshold==1 we have alredy requested ack at send-time
                    m_sendAckRequestForMsgIndex.push_back(i);
                }
            }

            SendAckRequests();

            RemoveCompletedMessages();

            //Restart timer
            m_resendTimer.expires_from_now(timerInterval);
            m_resendTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& /*error*/){RetransmitUnackedMessages();}));
        }

        void SendAckRequests()
        {
            if (m_sendAckRequestForMsgIndex.empty())
            {
                return;
            }

            std::set<int64_t> singleReceiverSendMethod;
            std::set<int64_t> multiReceiverSendMethod;

            for (auto index = m_sendAckRequestForMsgIndex.cbegin(); index != m_sendAckRequestForMsgIndex.cend(); ++index)
            {
                const UserDataPtr& ud=m_sendQueue[*index];
                if (ud->header.sendMethod==SingleReceiverSendMethod)
                {
                    singleReceiverSendMethod.insert(ud->receivers.begin(), ud->receivers.end());
                }
                else
                {
                    multiReceiverSendMethod.insert(ud->receivers.begin(), ud->receivers.end());
                }
            }

            Safir::Utilities::Internal::SharedCharArray noData;
            UserDataPtr ud=std::make_shared<UserData>(m_nodeId, 0, AckRequestType, noData, 0);

            //Send ackRequests for SingleReceiver channel
            ud->header.sendMethod=SingleReceiverSendMethod;

            for (auto recvId = singleReceiverSendMethod.cbegin(); recvId != singleReceiverSendMethod.cend(); ++recvId)
            {
                auto nodeIt=m_nodes.find(*recvId);
                if (nodeIt!=m_nodes.end() && nodeIt->second.systemNode)
                {
                    lllog(9)<<m_logPrefix.c_str()<<"Send AckRequest for SingleReceiverSendMethod to "<<*recvId<<std::endl;
                    ud->header.commonHeader.receiverId=*recvId;
                    WriterType::SendTo(ud, nodeIt->second.endpoint);
                }
            }

            //Send ackRequests for MultiReceiver channel
            ud->header.sendMethod=MultiReceiverSendMethod;

            for (auto recvId = multiReceiverSendMethod.cbegin(); recvId != multiReceiverSendMethod.cend(); ++recvId)
            {
                auto nodeIt=m_nodes.find(*recvId);
                if (nodeIt!=m_nodes.end() && nodeIt->second.systemNode)
                {
                    lllog(9)<<m_logPrefix.c_str()<<"Send AckRequest for MultiReceiverSendMethod to "<<*recvId<<std::endl;
                    ud->header.commonHeader.receiverId=*recvId;
                    WriterType::SendTo(ud, nodeIt->second.endpoint);
                }
            }

            m_sendAckRequestForMsgIndex.clear();
        }

        void RetransmitMessage(UserDataPtr& ud)
        {
            ++ud->transmitCount;
            //Always called from writeStrand
            auto recvIt=ud->receivers.begin();
            while (recvIt!=ud->receivers.end())
            {
                const auto nodeIt=m_nodes.find(*recvIt);
                if (nodeIt!=m_nodes.end() && nodeIt->second.systemNode)
                {
                    ud->header.ackNow=1; //request ack immediately for retransmitted messages

                    WriterType::SendTo(ud, nodeIt->second.endpoint);
                    m_retransmitNotification(nodeIt->first, ud->transmitCount);
                    lllog(9)<<m_logPrefix.c_str()<<"Retransmit  "<<SendMethodToString(ud->header.sendMethod).c_str()<<
                              ", seq: "<<ud->header.sequenceNumber<<" to "<<*recvIt<<std::endl;
                    ++recvIt;
                }
                else
                {
                    //node does not exist anymore or is not part of the system, dont wait for this node anymore
                    lllog(6)<<m_logPrefix.c_str()<<"Ignore retransmit to receiver that is not longer a system node  "<<
                              (ud->header.sendMethod==SingleReceiverSendMethod ? "SingleReceiverMessage" : "MultiReceiverMessage")<<
                              ", seq: "<<ud->header.sequenceNumber<<" to "<<*recvIt<<std::endl;
                    ud->receivers.erase(recvIt++);
                }
            }

            ud->sendTime=boost::chrono::steady_clock::now(); //update sendTime so that we will wait for an new WaitForAckTime period before retransmit again
            m_lastSendTime=ud->sendTime;
        }

        void Ping()
        {
            if (!m_running)
            {
                return;
            }

            //Always called from writeStrand
            if (m_sendQueue.empty() && m_nodes.size()>0)
            {
                static const boost::chrono::milliseconds PingSendThreshold = boost::chrono::milliseconds(Parameters::SendPingThreshold);
                auto durationSinceSend=boost::chrono::steady_clock::now()-m_lastSendTime;
                if (durationSinceSend>PingSendThreshold)
                {
                    //add ping to the normal sendQueue
                    lllog(8)<<m_logPrefix.c_str()<<"Add Ping to sendQueue"<<std::endl;
                    auto ping = Safir::Utilities::Internal::MakeSharedArray(1); //AddToSendQueue will throw away empty messages, add dummy content
                    AddToSendQueue(0, ping, 1, PingDataType);
                }
            }

            //Restart timer
            m_pingTimer.expires_from_now(boost::chrono::milliseconds(Parameters::SendPingThreshold));
            m_pingTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& /*error*/){Ping();}));
        }

        void RemoveExcludedReceivers()
        {
            std::for_each(m_sendQueue.begin(), m_sendQueue.end(), [&](UserDataPtr& ud)
            {
                auto recvIt=ud->receivers.begin();
                while (recvIt!=ud->receivers.end())
                {
                    auto nodeIt=m_nodes.find(*recvIt);
                    if (nodeIt==m_nodes.end())
                    {
                        //receiver does not exist anymore, remove it from receiver list
                        ud->receivers.erase(recvIt++);  //post increment after erase
                    }
                    else
                    {
                        ++recvIt;
                    }
                }
            });
        }

        void RemoveCompletedMessages()
        {
            //Always called from writeStrand

            //grab the front message as long as it is a handled (=sent) message. We are done when entire queue is empty or we
            //reach an unhandled message.

            if (!m_nodes.empty())
            {
                while (!m_sendQueue.empty() && m_sendQueue.first_unhandled_index()>0)
                {
                    //we only handle sent messages here
                    const auto& ud=m_sendQueue.front();
                    if (ud->receivers.empty())
                    {
                        //no more receivers in receiver list, then message is completed and shall be removed
                        lllog(8)<<m_logPrefix.c_str()<<"Remove message from sendQueue, seq: "<<ud->header.sequenceNumber<<std::endl;
                        m_sendQueue.dequeue();
                        --m_sendQueueSize;
                    }
                    else
                    {
                        //message has not been acked by everyone
                        for (auto r = ud->receivers.cbegin(); r != ud->receivers.cend(); ++r)
                        {
                            lllog(8)<<m_logPrefix.c_str()<<"Cant remove seq: "<<ud->header.sequenceNumber<<", left: "<<*r<<std::endl;
                        }
                        break;
                    }
                }
            }
            else
            {
                //It is possible that m_sendQueueSize has been increased by 1 in AddToSendQueue but still the item has not been added to the queue.
                //to avoid race conditions we can't just set m_sendQueueSize=0 here but instead decrease it with the actual number of items removed.
                auto numberOfRemoved=m_sendQueue.clear_queue();
                m_sendQueueSize -= static_cast<unsigned int>(numberOfRemoved);
                lllog(8)<<m_logPrefix.c_str()<<"No receivers left, clear sendQueue, numberOfRemoved: "<<numberOfRemoved<<std::endl;
            }

            //if queue has been full and is now below threshold, notify user
            if (m_notifyQueueNotFull)
            {
                NotifyQueueNotFull();
            }

            //Check if messages have been moved from extension part, in that case we must HandleSendQueue
            if (m_sendQueue.has_unhandled())
            {
                //this can only happen if messages have been moved from the extension part of sendQueue
                m_strand.post([this]{HandleSendQueue();});
            }
        }

        //returns the correct sequenceNumberSerie, returns NULL if no receiver exists
        uint64_t* GetSequenceSerieIfExist(int64_t toId, uint8_t& sendMethod)
        {
            if (toId==0) //send to all, check if there are any nodes
            {
                sendMethod=MultiReceiverSendMethod;
                if (m_nodes.empty())
                {
                    return nullptr;
                }

                return &m_lastSentMultiReceiverSeqNo;
            }
            else //check if a specific node exists
            {
                sendMethod=SingleReceiverSendMethod;
                auto it=m_nodes.find(toId);
                if (it==m_nodes.end())
                {
                    return nullptr;
                }
                return &(it->second.lastSentSeqNo);
            }
        }

        void NotifyQueueNotFull()
        {
            if (m_sendQueueSize<=m_queueNotFullNotificationLimit)
            {
                m_notifyQueueNotFull=false; //very important that this flag is set before the notification call since a new overflow may occur after notification.
                if (!m_queueNotFullNotification.empty())
                {
                     m_strand.context().post([this]
                     {
                         for (auto callback = m_queueNotFullNotification.cbegin(); callback != m_queueNotFullNotification.cend(); ++callback)
                         {
                             (*callback)(m_nodeTypeId);
                         }
                     });
                }
            }
        }

    public:
        //debug
        std::string SendQueueToString() const
        {
            std::ostringstream os;
            os<<"===== SendQueue ====="<<std::endl;
            m_sendQueue.DumpInfo(os);
            //Dump send queue
            for (size_t i=0; i<m_sendQueue.size(); ++i)
            {
                if (i<m_sendQueue.first_unhandled_index())
                    os<<"H ";
                else
                    os<<"U ";

                const auto& ud=m_sendQueue[i];
                os<<"q["<<i<<"]"<<" ("<<SendMethodToString(ud->header.sendMethod)
                    <<") sequenceNumber="<<ud->header.sequenceNumber
                    <<" transmitCount="<<ud->transmitCount<<std::endl;
                if (ud->receivers.empty())
                {
                    os<<"    No_Receivers"<<std::endl;
                }
                else
                {
                    for (auto id = ud->receivers.cbegin(); id != ud->receivers.cend(); ++id)
                    {
                        os<<"    recvId="<<*id<<std::endl;
                    }
                }
            }
            os<<"======== End ========"<<std::endl;

            return os.str();
        }
    };

    typedef DataSenderBasic< Writer<UserData> > DataSender;
    //typedef DataSenderBasic< Writer<UserData, LoserPolicy> > DataSender;
}
}
}
}

#ifdef _MSC_VER
#pragma warning (pop)
#endif
