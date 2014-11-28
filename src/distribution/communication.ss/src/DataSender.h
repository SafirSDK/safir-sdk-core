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
#ifndef __SAFIR_DOB_COMMUNICATION_ACKED_DATA_SENDER_H__
#define __SAFIR_DOB_COMMUNICATION_ACKED_DATA_SENDER_H__

#include <map>
#include <atomic>
#include <boost/noncopyable.hpp>
#include <boost/function.hpp>
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
            ,m_queueNotFullNotificationLimit(0)
        {
            m_sendQueueSize=0;
            m_notifyQueueNotFull=false;
        }

        //Set notifier called every time something is retransmitted
        void SetRetransmitCallback(const RetransmitTo& callback)
        {
            m_strand.dispatch([=]{m_retransmitNotification=callback;});
        }

        void SetNotFullCallback(const QueueNotFull& callback, int threshold)
        {
            m_strand.dispatch([=]
            {
                m_queueNotFullNotification=callback;
                double ratio=static_cast<double>(100-threshold)/100.0;
                m_queueNotFullNotificationLimit=static_cast<size_t>(Parameters::SendQueueSize*ratio); //calculate threshold in number of used slots, callback made when size<m_queueNotFullNotificationLimit
            });
        }

        //Start writer component
        void Start()
        {
            m_strand.dispatch([=]
            {
                m_running=true;
                //start retransmit timer
                RetransmitUnackedMessages();
            });
        }

        //Stop
        void Stop()
        {
            m_strand.dispatch([=]
            {
                m_running=false;
                m_resendTimer.cancel();
            });
        }

        //Add message to send queue. Message will be retranmitted unitl all receivers have acked. Returns false if queue is full.
        bool AddToSendQueue(int64_t toId, const boost::shared_ptr<char[]>& msg, size_t size, int64_t dataTypeIdentifier)
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
                lllog(5)<<"COM: SendQueue full"<<std::endl;
                --m_sendQueueSize;
                m_notifyQueueNotFull=true;
                return false;
            }

            //std::cout<<"Send size="<<size<<", fullFrag="<<numberOfFullFragments<<", rest="<<restSize<<", totalFrag="<<totalNumberOfFragments<<std::endl;

            //The actual work where the data is inserted in the queue must be done inside the strand.
            m_strand.post([=]
            {
                if (!ReceiverExists(toId))
                {
                    m_sendQueueSize-=static_cast<unsigned int>(totalNumberOfFragments);
                    //receiver does not exist so we just throw it away
                    lllog(9)<<"COM: Receiver does not exist. Message will not be sent."<<std::endl;
                    return;
                }

                const uint8_t sendMethod=(toId==0 ? MultiReceiverSendMethod : SingleReceiverSendMethod);

                for (size_t frag=0; frag<numberOfFullFragments; ++frag)
                {
                    const char* fragment=msg.get()+frag*m_fragmentDataSize;
                    UserDataPtr userData(new UserData(m_nodeId, toId, dataTypeIdentifier, msg, size, fragment, m_fragmentDataSize));
                    userData->header.commonHeader.receiverId=toId;
                    userData->header.sendMethod=sendMethod;
                    userData->header.deliveryGuarantee=m_deliveryGuarantee;
                    userData->header.numberOfFragments=static_cast<uint16_t>(totalNumberOfFragments);
                    userData->header.fragmentNumber=static_cast<uint16_t>(frag);
                    m_sendQueue.enqueue(userData);
                }

                if (restSize>0)
                {
                    const char* fragment=msg.get()+numberOfFullFragments*m_fragmentDataSize;
                    UserDataPtr userData(new UserData(m_nodeId, toId, dataTypeIdentifier, msg, size, fragment, restSize));
                    userData->header.commonHeader.receiverId=toId;
                    userData->header.sendMethod=sendMethod;
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
            if (Safir::Utilities::Internal::Internal::LowLevelLogger::Instance().LogLevel()==9)
            {
                lllog(9)<<"COM: HandleAck "<<AckToString(ack).c_str()<<std::endl;
            }

            //Will only check ack against sent messages. If an ack is received for a message that is still unsent, that ack will be ignored.
            m_strand.dispatch([=]
            {

                //Update queue
                for (size_t i=0; i<m_sendQueue.first_unhandled_index(); ++i)
                {
                    UserDataPtr& ud=m_sendQueue[i];
                    if (ud->header.sendMethod==ack.sendMethod && ud->header.sequenceNumber<=ack.sequenceNumber)
                    {
                        //calculate index in missing-array
                        size_t index=static_cast<size_t>(ack.sequenceNumber-ud->header.sequenceNumber);
                        if (ack.missing[index]==0)
                        {
                            //This message is now acked, remove from list of still unacked
                            if (ud->receivers.erase(ack.commonHeader.senderId)==0)
                            {
                                lllog(5)<<L"COM: Got ack from node that was not supposed to ack this message (might also be a duplicated ack). "<<AckToString(ack).c_str()<<std::endl;
                            }
                        }
                        else
                        {
                            //the ack-sender is missing a message, a gap. Resend immediately
                            if (ud->receivers.find(ack.commonHeader.senderId)!=ud->receivers.end())
                            {
                                RetransmitMessage(ud);
                            }
                            else
                            {
                                //receiver is missing a message that we dont expect it to be missing. Log all info we have
                                auto nodeIt=m_nodes.find(ack.commonHeader.senderId);
                                if (nodeIt==m_nodes.end())
                                {
                                    lllog(1)<<L"COM: got Ack from node we dont have. Maybe it has been excluded just before we received the ack-message. Ack sender id: "<<ack.commonHeader.senderId<<std::endl;
                                }
                                else if (!nodeIt->second.systemNode)
                                {
                                    lllog(1)<<L"COM: got Ack from node that is not a systemNode. Maybe it was excluded just before we received the ack-message. Ack sender id: "<<
                                              ack.commonHeader.senderId<<std::endl;
                                }
                                else
                                {
                                    //this should never happen, a programming error. Receiver missing message that we think has already been acked.
                                    std::wostringstream os;
                                    os<<L"COM: Node["<<ack.commonHeader.senderId<<L"] is missing a message that has already been acked or we dont expect the node to get at all. "<<
                                              SendMethodToString(ack.sendMethod).c_str()<<L" sequenceNumber: "<<ack.sequenceNumber;
                                    lllog(1)<<os.str()<<std::endl;
                                    DumpSendQueue(1);
                                    SEND_SYSTEM_LOG(Error, <<os.str()<<std::endl);
                                }
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
                    os<<"COM: Duplicated call to DataSender.AddNode with same nodeId! NodeId: "<<id<<", address: "<<address;
                    throw std::logic_error(os.str());
                }

                NodeInfo ni;
                ni.endpoint=Utilities::CreateEndpoint(address);
                ni.lastSentSeqNo=0;
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
                    it->second.systemNode=true;
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
        QueueNotFull m_queueNotFullNotification;
        size_t m_queueNotFullNotificationLimit; //below number of used slots. NOT percent.
        std::atomic_bool m_notifyQueueNotFull;

        bool RequestAck() const
        {

            static const size_t AckThreshold=Parameters::SlidingWindowSize/2;

            if (m_lastSentMultiReceiverSeqNo % AckThreshold==0)
            {
                return true;
            }

            if (m_sendQueue.first_unhandled_index()==m_sendQueue.size()-1)
            {
                return true;
            }

            return false;
        }

        //Send new messages in sendQueue. No retransmits sent here.
        void HandleSendQueue()
        {
            //must be called from writeStrand

            //Send all unhandled messges that are within our sender window
            while (m_sendQueue.has_unhandled() && m_sendQueue.first_unhandled_index()<Parameters::SlidingWindowSize)
            {
                UserDataPtr& ud=m_sendQueue[m_sendQueue.first_unhandled_index()];
                ud->header.ackNow=0;

                if (ud->header.sendMethod==MultiReceiverSendMethod) //this is message that shall be sent to every system node
                {
                    ++m_lastSentMultiReceiverSeqNo;
                    ud->header.sequenceNumber=m_lastSentMultiReceiverSeqNo;
                    if (RequestAck())
                    {
                        ud->header.ackNow=1;
                    }

                    if (WriterType::IsMulticastEnabled()) //this node and all the receivers are capable of sending and receiving multicast
                    {
                        for (const auto& val : m_nodes)
                        {
                            if (val.second.systemNode)
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
                            ud->receivers.insert(val.first);
                            WriterType::SendTo(ud, val.second.endpoint);
                        }
                    }
                }
                else //messages has a specific receiver, send using unicast
                {
                    ud->header.ackNow=1; //for simplicity we request ack immediately for node specific messages
                    auto nodeIt=m_nodes.find(ud->header.commonHeader.receiverId);
                    if (nodeIt!=m_nodes.end())
                    {
                        ud->receivers.insert(ud->header.commonHeader.receiverId);
                        NodeInfo& n=nodeIt->second;
                        ++n.lastSentSeqNo;
                        ud->header.sequenceNumber=n.lastSentSeqNo;
                        WriterType::SendTo(ud, n.endpoint);
                    }
                    else
                    {
                        //Not a system node, remove from sendList
                        lllog(8)<<L"COM: receiver does not exist, id:  "<<ud->header.commonHeader.receiverId<<std::endl;
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
            static const boost::chrono::milliseconds timerInterval(m_waitForAckTimeout-10);
            static const boost::chrono::milliseconds waitLimit(m_waitForAckTimeout);
            static const boost::chrono::milliseconds mustBeSeriousError(10000);

            //Check if there is any unacked messages that are old enough to be retransmitted
            for (size_t i=0; i<m_sendQueue.first_unhandled_index(); ++i)
            {
                UserDataPtr& ud=m_sendQueue[i];
                auto durationSinceSend=boost::chrono::steady_clock::now()-ud->sendTime;
                if (durationSinceSend>waitLimit)
                {
                    RetransmitMessage(ud);
                }

                if (durationSinceSend>mustBeSeriousError)
                {
                    lllog(9)<<"COM: Seems like we are retransmitting forever. Seq: "<<ud->header.sequenceNumber<<", "<<SendMethodToString(ud->header.sendMethod).c_str()<<std::endl;
                    DumpSendQueue();
                }
            }

            RemoveCompletedMessages();

            //Restart timer
            m_resendTimer.expires_from_now(timerInterval);
            m_resendTimer.async_wait(m_strand.wrap([=](const boost::system::error_code& /*error*/){RetransmitUnackedMessages();}));
        }

        void RetransmitMessage(UserDataPtr& ud)
        {
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
                    lllog(6)<<L"COM: retransmitted  "<<(ud->header.sendMethod==SingleReceiverSendMethod ? "SingleReceiverMessage" : "MultiReceiverMessage")<<
                              ", seq: "<<ud->header.sequenceNumber<<" to "<<*recvIt<<std::endl;
                    ++recvIt;
                }
                else
                {
                    //node does not exist anymore or is not part of the system, dont wait for this node anymore
                    lllog(6)<<L"COM: Ignore retransmit to receiver that is not longer a system node  "<<
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
                        lllog(8)<<L"COM: remove message from sendQueue"<<std::endl;
                        m_sendQueue.dequeue();
                        --m_sendQueueSize;
                    }
                    else
                    {
                        //message has not been acked by everyone
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

        bool ReceiverExists(int64_t toId) const
        {
            if (toId==0) //send to all, check if there are any nodes
            {
                return !m_nodes.empty();
            }
            else //check if a specific node exists
            {
                return m_nodes.find(toId)!=m_nodes.end();
            }
        }

        void NotifyQueueNotFull()
        {
            if (m_sendQueueSize<=m_queueNotFullNotificationLimit)
            {
                m_notifyQueueNotFull=false; //very important that this flag is set before the notification call since a new overflow may occur after notification.
                if (m_queueNotFullNotification)
                {
                    m_strand.get_io_service().post([this]{m_queueNotFullNotification(m_nodeTypeId);});
                }
            }
        }

    public:
        //debug
        void DumpSendQueue(int loglevel=9) const
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

            lllog(loglevel)<<L"COM: "<<os.str().c_str()<<std::endl;

            std::cout<<os.str()<<std::endl;
        }
    };

    typedef DataSenderBasic< Writer<UserData> > DataSender;
}
}
}
}

#ifdef _MSC_VER
#pragma warning (default: 4127)
#endif

#endif
