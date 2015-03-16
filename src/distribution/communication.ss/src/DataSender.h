/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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
#include <atomic>
#include <boost/noncopyable.hpp>
#include <boost/function.hpp>
#include <boost/make_shared.hpp>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
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

    typedef std::function<void(int64_t toNodeId)> RetransmitTo;
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
                        int waitForAckTimeout,
                        size_t fragmentSize)
            :WriterType(ioService, ipVersion, localIf, multicastAddress)
            ,m_strand(ioService)
            ,m_deliveryGuarantee(deliveryGuarantee)
            ,m_nodeTypeId(nodeTypeId)
            ,m_nodeId(nodeId)
            ,m_sendQueue(Parameters::SendQueueSize)
            ,m_running(false)
            ,m_waitForAckTimeout(waitForAckTimeout)
            ,m_fragmentDataSize(fragmentSize-MessageHeaderSize)
            ,m_nodes()
            ,m_lastSentMultiReceiverSeqNo(0)
            ,m_lastAckRequestMultiReceiver(0)
            ,m_resendTimer(ioService)
            ,m_retransmitNotification()
            ,m_queueNotFullNotification()
            ,m_queueNotFullNotificationLimit(Parameters::SendQueueSize/2)
            ,m_sendAckRequestForMsgIndex()
            ,m_logPrefix([&]{std::ostringstream os;
                             os<<"COM: ("<<DeliveryGuaranteeToString(deliveryGuarantee)<<"DataSender nodeType "<<nodeTypeId<<") - ";
                             return os.str();}())
        {

            m_sendQueueSize=0;
            m_notifyQueueNotFull=false;
        }

        //Set notifier called every time something is retransmitted
        void SetRetransmitCallback(const RetransmitTo& callback)
        {
            m_strand.dispatch([=]{m_retransmitNotification=callback;});
        }

        void SetNotFullCallback(const QueueNotFull& callback)
        {
            m_strand.dispatch([=]{m_queueNotFullNotification.push_back(callback);});
        }

        //Start writer component
        void Start()
        {
            m_strand.dispatch([=]
            {
                m_running=true;
                //start retransmit timer
                if (m_deliveryGuarantee==Acked)
                {
                    RetransmitUnackedMessages();
                }
            });
        }

        //Stop
        void Stop()
        {
            m_strand.dispatch([=]
            {
                m_running=false;
                if (m_deliveryGuarantee==Acked)
                {
                    m_resendTimer.cancel();
                }
            });
        }

        //Add message to send queue. Message will be retranmitted unitl all receivers have acked. Returns false if queue is full.
        bool AddToSendQueue(int64_t toId, const boost::shared_ptr<const char[]>& msg, size_t size, int64_t dataTypeIdentifier)
        {
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
            m_strand.post([=]
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
            m_strand.dispatch([=]
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

                        if (index>Parameters::SlidingWindowSize-1)
                        {
                            std::ostringstream os;
                            os<<m_logPrefix<<"Programming Error! Index out of range. Calculated missing index: "<<index<<". AckContent: "<<ack.ToString()<<"\n"<<SendQueueToString();
                            lllog(1)<<os.str().c_str()<<std::endl;
                            SEND_SYSTEM_LOG(Error, <<os.str().c_str()<<std::endl);
                            throw std::logic_error(os.str());
                        }

                        if (ack.missing[index]==0)
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
            m_strand.dispatch([=]
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
            m_strand.post([=]
            {
                const auto it=m_nodes.find(id);
                if (it!=m_nodes.end())
                {
                    if (it->second.systemNode)
                    {
                        lllog(5)<<m_logPrefix.c_str()<<" IncluedNode called for an already included node, nodeId: "<<boost::lexical_cast<std::wstring>(id)<<std::endl;
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
            m_strand.post([=]
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
        struct NodeInfo
        {
            bool systemNode;
            boost::asio::ip::udp::endpoint endpoint;
            uint64_t lastSentSeqNo;
            uint64_t welcome;
        };

        boost::asio::io_service::strand m_strand;
        uint8_t m_deliveryGuarantee;
        int64_t m_nodeTypeId;
        int64_t m_nodeId;
        MessageQueue<UserDataPtr> m_sendQueue;
        std::atomic_uint m_sendQueueSize;
        bool m_running;
        int m_waitForAckTimeout;
        size_t m_fragmentDataSize; //size of a fragments data part, excluding header size.
        std::map<int64_t, NodeInfo> m_nodes;
        uint64_t m_lastSentMultiReceiverSeqNo; // used both for multicast and unicast as long as the message is a multireceiver message
        uint64_t m_lastAckRequestMultiReceiver; //the last seq we have requested ack
        boost::asio::steady_timer m_resendTimer;
        RetransmitTo m_retransmitNotification;
        std::vector<QueueNotFull> m_queueNotFullNotification;
        size_t m_queueNotFullNotificationLimit; //below number of used slots. NOT percent.
        std::atomic_bool m_notifyQueueNotFull;
        std::vector<size_t> m_sendAckRequestForMsgIndex;
        const std::string m_logPrefix;

        void PostWelcome(int64_t nodeId)
        {
            auto& ni=m_nodes[nodeId];
            ++m_lastSentMultiReceiverSeqNo;
            ni.welcome=m_lastSentMultiReceiverSeqNo;

            ++m_sendQueueSize;
            boost::shared_ptr<char[]> welcome=boost::make_shared<char[]>(sizeof(int64_t));
            memcpy(static_cast<void*>(welcome.get()), static_cast<const void*>(&nodeId), sizeof(int64_t));
            UserDataPtr userData=boost::make_shared<UserData>(m_nodeId, 0, WelcomeDataType, welcome, sizeof(int64_t), welcome.get(), sizeof(int64_t));
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

        static bool RequestAck(MessageHeader& header)
        {
            static const size_t AckThreshold=Parameters::SlidingWindowSize/2;
            return (header.sequenceNumber % AckThreshold==0);
        }

        static void SetRequestAck(MessageHeader& header)
        {
            if (RequestAck(header))
            {
                header.ackNow=1;
            }

            //let ackNow be as it was before, PostWelcome will explicitly set the ackNow=1
            //so we dont have to check it here for every message
        }

        //Send new messages in sendQueue. No retransmits sent here.
        void HandleSendQueue()
        {
            //must be called from writeStrand

            //Send all unhandled messges that are within our sender window
            while (m_sendQueue.has_unhandled() && m_sendQueue.first_unhandled_index()<Parameters::SlidingWindowSize)
            {
                UserDataPtr& ud=m_sendQueue[m_sendQueue.first_unhandled_index()];
                ++ud->transmitCount;
                ud->header.ackNow=0;

                if (ud->header.sendMethod==MultiReceiverSendMethod) //this is message that shall be sent to every system node
                {
                    SetRequestAck(ud->header);

                    lllog(9)<<m_logPrefix.c_str()<<"Send to all seq: "<<ud->header.sequenceNumber<<", ackNow: "<<static_cast<int>(ud->header.ackNow)<<std::endl;

                    if (WriterType::IsMulticastEnabled()) //this node and all the receivers are capable of sending and receiving multicast
                    {
                        for (const auto& val : m_nodes)
                        {
                            if (val.second.systemNode && val.second.welcome<=ud->header.sequenceNumber)
                            {
                                //The node will get the message throuch the multicast message, we just add it to receiver list
                                //to be able to track the ack
                                ud->receivers.insert(val.first);
                            }
                        }

                        if (ud->receivers.size()>0)
                        {
                            WriterType::SendMulticast(ud);
                        }
                    }
                    else //this node is not multicast enabled, only send unicast. However it's still a multiReceiver message and the multiReceiverSeqNo shall be used
                    {
                        for (const auto& val : m_nodes)
                        {
                            if (val.second.systemNode && val.second.welcome<=ud->header.sequenceNumber)
                            {
                                ud->receivers.insert(val.first);
                                WriterType::SendTo(ud, val.second.endpoint);
                            }
                        }
                    }
                }
                else //messages has a specific receiver, send using unicast
                {
                    ud->header.ackNow=1; //for simplicity we request ack immediately for node specific messages
                    auto nodeIt=m_nodes.find(ud->header.commonHeader.receiverId);
                    if (nodeIt!=m_nodes.end())
                    {
                        lllog(9)<<m_logPrefix.c_str()<<"Send to: "<<ud->header.commonHeader.receiverId<<",  seq: "<<ud->header.sequenceNumber<<", ackNow: "<<static_cast<int>(ud->header.ackNow)<<std::endl;
                        ud->receivers.insert(ud->header.commonHeader.receiverId);
                        NodeInfo& n=nodeIt->second;
                        WriterType::SendTo(ud, n.endpoint);
                    }
                    else
                    {
                        //Not a system node, remove from sendList
                        lllog(8)<<m_logPrefix.c_str()<<"receiver does not exist, id:  "<<ud->header.commonHeader.receiverId<<std::endl;
                    }
                }

                if (m_deliveryGuarantee==Acked)
                {
                    ud->sendTime=boost::chrono::steady_clock::now();
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

        void RetransmitUnackedMessages()
        {
            if (!m_running)
            {
                return;
            }

            //Always called from writeStrand
            static const boost::chrono::milliseconds timerInterval(m_waitForAckTimeout/2-5);
            static const boost::chrono::milliseconds requestAckLimit(m_waitForAckTimeout/2);
            static const boost::chrono::milliseconds retransmitLimit(m_waitForAckTimeout);
            static const boost::chrono::milliseconds mustBeSeriousError(10000);

            //Check if there is any unacked messages that are old enough to be retransmitted
            for (size_t i=0; i<m_sendQueue.first_unhandled_index(); ++i)
            {
                UserDataPtr& ud=m_sendQueue[i];
                auto durationSinceSend=boost::chrono::steady_clock::now()-ud->sendTime;
                if (durationSinceSend>retransmitLimit)
                {
                    RetransmitMessage(ud);
                }
                else if (durationSinceSend>requestAckLimit && ud->transmitCount<2)
                {
                    //this message has never been retransmitted, we have not asked receiver to ack,
                    //and half the restransmitInterval has expired. It is time to send an explicit ack-request
                    //to avoid unecessary retransmits.
                    m_sendAckRequestForMsgIndex.push_back(i);
                }

                if (durationSinceSend>mustBeSeriousError)
                {
                    lllog(9)<<m_logPrefix.c_str()<<"Seems like we are retransmitting forever. Seq: "<<ud->header.sequenceNumber<<", "<<
                              SendMethodToString(ud->header.sendMethod).c_str()<<L"\n"<<SendQueueToString().c_str()<<std::endl;
                }
            }

            SendAckRequests();

            RemoveCompletedMessages();

            //Restart timer
            m_resendTimer.expires_from_now(timerInterval);
            m_resendTimer.async_wait(m_strand.wrap([=](const boost::system::error_code& /*error*/){RetransmitUnackedMessages();}));
        }

        void SendAckRequests()
        {
            if (m_sendAckRequestForMsgIndex.empty())
            {
                return;
            }

            std::set<int64_t> singleReceiverSendMethod;
            std::set<int64_t> multiReceiverSendMethod;

            for (auto index : m_sendAckRequestForMsgIndex)
            {
                const UserDataPtr& ud=m_sendQueue[index];
                if (ud->header.sendMethod==SingleReceiverSendMethod)
                {
                    singleReceiverSendMethod.insert(ud->receivers.begin(), ud->receivers.end());
                }
                else
                {
                    multiReceiverSendMethod.insert(ud->receivers.begin(), ud->receivers.end());
                }
            }

            boost::shared_ptr<char[]> noData;
            UserDataPtr ud=boost::make_shared<UserData>(m_nodeId, 0, AckRequestType, noData, 0);

            //Send ackRequests for SingleReceiver channel
            ud->header.sendMethod=SingleReceiverSendMethod;
            for (auto recvId : singleReceiverSendMethod)
            {
                auto nodeIt=m_nodes.find(recvId);
                if (nodeIt!=m_nodes.end() && nodeIt->second.systemNode)
                {
                    lllog(9)<<m_logPrefix.c_str()<<"Send AckRequest for SingleReceiverSendMethod to "<<recvId<<std::endl;
                    ud->header.commonHeader.receiverId=recvId;
                    WriterType::SendTo(ud, nodeIt->second.endpoint);
                }
            }

            //Send ackRequests for MultiReceiver channel
            ud->header.sendMethod=MultiReceiverSendMethod;
            for (auto recvId : multiReceiverSendMethod)
            {
                auto nodeIt=m_nodes.find(recvId);
                if (nodeIt!=m_nodes.end() && nodeIt->second.systemNode)
                {
                    lllog(9)<<m_logPrefix.c_str()<<"Send AckRequest for MultiReceiverSendMethod to "<<recvId<<std::endl;
                    ud->header.commonHeader.receiverId=recvId;
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
                    m_retransmitNotification(nodeIt->first);
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
                        for (auto r : ud->receivers)
                        {
                            lllog(8)<<m_logPrefix.c_str()<<"Cant remove seq: "<<ud->header.sequenceNumber<<", left: "<<r<<std::endl;
                        }
                        break;
                    }
                }
            }
            else
            {
                m_sendQueue.clear_queue();
                m_sendQueueSize=0;
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
                m_strand.post([=]{HandleSendQueue();});
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
                     m_strand.get_io_service().post([this]
                     {
                         for (auto& callback : m_queueNotFullNotification)
                         {
                             callback(m_nodeTypeId);
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
                os<<"q["<<i<<"]"<<" ("<<SendMethodToString(ud->header.sendMethod)<<") sequenceNumber="<<ud->header.sequenceNumber<<std::endl;
                if (ud->receivers.empty())
                {
                    os<<"    No_Receivers"<<std::endl;
                }
                else
                {
                    for (auto id : ud->receivers)
                    {
                        os<<"    recvId="<<id<<std::endl;
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
#pragma warning (default: 4127)
#endif
