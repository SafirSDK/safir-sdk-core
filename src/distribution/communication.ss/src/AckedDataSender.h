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
#include <boost/noncopyable.hpp>
#include <boost/asio.hpp>
#include <boost/function.hpp>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "Node.h"
#include "Message.h"
#include "MessageQueue.h"
#include "Parameters.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    /**
     * @brief AckedDataSender keeps a send queue of messages. It will keep track of received Ack's and free the
     * messageQueue item when all ack's have been received. The class is also responsible for retransmission of
     * messages that have not been acked.
     */

    typedef boost::function<void(boost::int64_t toId)> RetransmitTo;
    typedef boost::function<void()> QueueNotFull;

    template <class WriterType>
    class AckedDataSenderBasic : private WriterType
    {
    public:
        AckedDataSenderBasic(boost::asio::io_service& ioService, const Node& me)
            :WriterType(ioService, me)
            ,m_strand(ioService)
            ,m_me(me)
            ,m_sendQueue(Parameters::SendQueueSize)
            ,m_running(false)
            ,m_nodes()
            ,m_lastSentMulticastSeqNo(0)
            ,m_resendTimer(ioService)
            ,m_retransmitNotification()
            ,m_queueNotFullNotification()
            ,m_queueNotFullNotificationLimit(0)

        {
            //TODO: use initialization instead. This is due to problems with VS2013
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
        bool AddToSendQueue(boost::int64_t toId, const boost::shared_ptr<char[]>& msg, size_t size, boost::int64_t dataTypeIdentifier)
        {
            //calculate number of fragments
            size_t numberOfFullFragments=size/FragmentDataSize;
            size_t restSize=size%FragmentDataSize;
            size_t totalNumberOfFragments=numberOfFullFragments+(restSize>0 ? 1 : 0);

            if (++m_sendQueueSize<=Parameters::SendQueueSize)
            {
                //there are room for at least one fragment within the queue limit.
                //then we step up the total amount, even if it will exceed the queue limit. Send queue will handle this case.
                m_sendQueueSize+=(totalNumberOfFragments-1); //note that one already been added

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
            m_strand.dispatch([=]
            {
                if (!ReceiverExists(toId))
                {
                    m_sendQueueSize-=totalNumberOfFragments;
                    //receiver does not exist so we just throw it away
                    lllog(7)<<"COM: Receiver does not exist. Message will not be sent."<<std::endl;
                    return;
                }

                boost::uint32_t crc=CalculateCrc32(msg.get(), size);

                for (size_t frag=0; frag<numberOfFullFragments; ++frag)
                {
                    const char* fragment=msg.get()+frag*FragmentDataSize;
                    UserDataPtr userData(new UserData(m_me.Id(), dataTypeIdentifier, msg, size, fragment, FragmentDataSize));
                    userData->header.crc=crc;
                    userData->header.numberOfFragments=static_cast<unsigned short>(totalNumberOfFragments);
                    userData->header.fragmentNumber=static_cast<unsigned short>(frag);
                    if (toId!=0)
                    {
                        userData->sendToAllSystemNodes=false;
                        userData->receivers.insert(std::make_pair(toId, Receiver()));
                    }
                    this->m_sendQueue.enqueue(userData);
                }

                if (restSize>0)
                {
                    const char* fragment=msg.get()+numberOfFullFragments*FragmentDataSize;
                    UserDataPtr userData(new UserData(m_me.Id(), dataTypeIdentifier, msg, size, fragment, restSize));
                    userData->header.crc=crc;
                    userData->header.numberOfFragments=static_cast<unsigned short>(totalNumberOfFragments);
                    userData->header.fragmentNumber=static_cast<unsigned short>(totalNumberOfFragments-1);
                    if (toId!=0)
                    {
                        userData->sendToAllSystemNodes=false;
                        userData->receivers.insert(std::make_pair(toId, Receiver()));
                    }
                    this->m_sendQueue.enqueue(userData);
                }

                this->HandleSendQueue();
            });

            return true;
        }

        //Handle received Acks. Messages that have been acked from all receivers will be removed from sendQueue.
        void HandleAck(const Ack& ack)
        {
            //Called from readStrand
            lllog(8)<<L"COM: got Ack from "<<ack.commonHeader.senderId<<std::endl;

            //Will only check ack against sent messages. If an ack is received for a message that is still unsent, that ack will be ignored.
            m_strand.dispatch([this, ack]
            {
                //Update queue
                for (size_t i=0; i<m_sendQueue.first_unhandled_index(); ++i)
                {
                    UserDataPtr& ud=m_sendQueue[i];
                    auto recvIt=ud->receivers.find(ack.commonHeader.senderId);
                    if (recvIt!=ud->receivers.end())
                    {
                        //here we have something unacked from the node
                        if (recvIt->second.sendMethod==ack.sendMethod && recvIt->second.sequenceNumber<=ack.sequenceNumber)
                        {
                            ud->receivers.erase(recvIt); //This message is now acked, remove from list of still unacked
                        }
                    }
                }

                //Remove from beginning as long as all is acked
                RemoveCompletedMessages();
            });
        }

        //Add a node.
        void AddNode(const Node& node)
        {
            m_strand.dispatch([=]
            {
                m_nodes.insert(std::make_pair(node.Id(), node));
            });
        }

        //Make node included or excluded. If excluded it is also removed.
        void SetSystemNode(boost::int64_t id, bool isSystemNode)
        {
            m_strand.dispatch([=]
            {
                if (isSystemNode)
                {
                    const auto it=m_nodes.find(id);
                    if (it!=m_nodes.end())
                    {
                        it->second.SetSystemNode(isSystemNode);
                    }
                }
                else
                {
                    m_nodes.erase(id);
                    RemoveExcludedReceivers();
                    RemoveCompletedMessages();
                }
            });
        }

        size_t SendQueueSize() const
        {
            return m_sendQueueSize;
        }

    private:
        boost::asio::io_service::strand m_strand;
        Node m_me;
        MessageQueue<UserDataPtr> m_sendQueue;
        std::atomic_uint m_sendQueueSize;
        bool m_running;

        NodeMap m_nodes;
        boost::uint64_t m_lastSentMulticastSeqNo;
        boost::asio::steady_timer m_resendTimer;
        RetransmitTo m_retransmitNotification;
        QueueNotFull m_queueNotFullNotification;
        size_t m_queueNotFullNotificationLimit; //below number of used slots. NOT percent.
        std::atomic_bool m_notifyQueueNotFull;

        static const size_t FragmentDataSize=Parameters::FragmentSize-MessageHeaderSize; //size of a fragments data part, excluding header size.

        //Send new messages in sendQueue. No retransmits sent here.
        void HandleSendQueue()
        {
            //must be called from writeStrand

            //Send all unhandled messges that are within our sender window
            while (m_sendQueue.has_unhandled() && m_sendQueue.first_unhandled_index()<Parameters::SenderWindowSize)
            {
                UserDataPtr& ud=m_sendQueue[m_sendQueue.first_unhandled_index()];

                if (ud->sendToAllSystemNodes) //this is message that shall be sent to every system node
                {
                    if (m_me.IsMulticastEnabled()) //the sending node is capable of sending multicast
                    {
                        bool multicastReceiversExist=false;
                        ud->header.sendMethod=UnicastSendMethod;
                        for (auto it=m_nodes.begin(); it!=m_nodes.end(); ++it)
                        {
                            Node& n=it->second;
                            if (n.IsSystemNode())
                            {
                                if (!n.IsMulticastEnabled())
                                {
                                    //if receiver is not using multicast we have to send by unicast
                                    ++n.LastSentUnicastSeqNo();
                                    ud->header.sequenceNumber=n.LastSentUnicastSeqNo();
                                    WriterType::SendTo(ud, n.Endpoint());
                                    ud->receivers.insert(std::make_pair(n.Id(), Receiver(n.Id(), UnicastSendMethod, n.LastSentUnicastSeqNo())));
                                }
                                else
                                {
                                    //The node will get the message throuch the multicast message, we just add it to receiver list
                                    //to be able to track the ack
                                    ud->receivers.insert(std::make_pair(n.Id(), Receiver(n.Id(), MulticastSendMethod, m_lastSentMulticastSeqNo+1)));
                                    multicastReceiversExist=true;
                                }
                            }
                        }

                        if (multicastReceiversExist)
                        {
                            ++m_lastSentMulticastSeqNo;
                            ud->header.sequenceNumber=m_lastSentMulticastSeqNo;
                            ud->header.sendMethod=MulticastSendMethod;
                            WriterType::SendMulticast(ud);
                        }
                    }
                    else //this node is not multicast enabled, only send unicast
                    {
                        ud->header.sendMethod=UnicastSendMethod;
                        for (auto it=m_nodes.begin(); it!=m_nodes.end(); ++it)
                        {
                            Node& n=it->second;
                            ++n.LastSentUnicastSeqNo();
                            ud->header.sequenceNumber=n.LastSentUnicastSeqNo();
                            WriterType::SendTo(ud, n.Endpoint());
                            ud->receivers.insert(std::make_pair(n.Id(), Receiver(n.Id(), UnicastSendMethod, n.LastSentUnicastSeqNo())));
                        }
                    }
                }
                else //messges has a receiver list, send to them using unicast
                {
                    ud->header.sendMethod=UnicastSendMethod;
                    auto recvIt=ud->receivers.begin();
                    while (recvIt!=ud->receivers.end())
                    {
                        auto nodeIt=m_nodes.find(recvIt->first);
                        if (nodeIt!=m_nodes.end())
                        {
                            Receiver& r=recvIt->second;
                            Node& n=nodeIt->second;
                            ++n.LastSentUnicastSeqNo();
                            ud->header.sequenceNumber=n.LastSentUnicastSeqNo();
                            r.id=recvIt->first;
                            r.sendMethod=UnicastSendMethod;
                            r.sequenceNumber=n.LastSentUnicastSeqNo();
                            WriterType::SendTo(ud, n.Endpoint());
                            ++recvIt;
                        }
                        else
                        {
                            //Not a system node, remove from sendList
                            lllog(8)<<L"COM: receiver does not exist, id:  "<<recvIt->first<<std::endl;
                            ud->receivers.erase(recvIt++);  //post increment iterator
                        }
                    }
                }

                ud->sendTime=boost::chrono::steady_clock::now();
                m_sendQueue.step_unhandled();
            }
        }

        void RetransmitUnackedMessages()
        {
            if (!m_running)
            {
                return;
            }

            //Always called from writeStrand
            static const boost::chrono::milliseconds timerInterval(Parameters::RetransmitCheckInterval);
            static const boost::chrono::milliseconds waitLimit(Parameters::WaitForAckTime);

            //Check if there is any unacked messages that are old enough to be retransmitted
            for (size_t i=0; i<m_sendQueue.first_unhandled_index(); ++i)
            {
                UserDataPtr& ud=m_sendQueue[i];
                auto durationSinceSend=boost::chrono::steady_clock::now()-ud->sendTime;
                if (durationSinceSend>waitLimit)
                {
                    RetransmitMessage(ud);
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
                const auto nodeIt=m_nodes.find(recvIt->first);
                if (nodeIt!=m_nodes.end() && nodeIt->second.IsSystemNode())
                {
                    const auto& r=recvIt->second;
                    ud->header.sendMethod=r.sendMethod; //even if this is Multicast we send retransmit using unicast. This is to specify correct sequence number serie.
                    ud->header.sequenceNumber=r.sequenceNumber;

                    WriterType::SendTo(ud, nodeIt->second.Endpoint());
                    m_retransmitNotification(nodeIt->first);
                    lllog(6)<<L"COM: retransmited  "<<(r.sendMethod==UnicastSendMethod ? "Unicast" : "Multicast")<<", seq: "<<r.sequenceNumber<<" to "<<r.id<<std::endl;
                    ++recvIt;
                }
                else
                {
                    //node does not exist anymore or is not part of the system, dont wait for this node anymore
                    lllog(6)<<L"COM: Ignore retransmit to receiver that is not longer a system node  "<<
                              (recvIt->second.sendMethod==UnicastSendMethod ? "Unicast" : "Multicast")<<", seq: "<<recvIt->second.sequenceNumber<<" to "<<recvIt->first<<std::endl;
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
                    auto nodeIt=m_nodes.find(recvIt->first);
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

        bool ReceiverExists(boost::int64_t toId) const
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
                    m_strand.get_io_service().post([this]{m_queueNotFullNotification();});
                }
            }
        }

    public:
        //debug
        void DumpSendQueue() const
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
                os<<"q["<<i<<"] "<<std::endl;
                if (ud->receivers.empty())
                {
                    os<<" No_Receivers"<<std::endl;
                }
                else
                {
                    for (auto it=ud->receivers.cbegin(); it!=ud->receivers.cend(); ++it)
                    {
                        const auto& r=it->second;
                        os<<"    recvId="<<r.id<<(r.sendMethod==UnicastSendMethod ? " uni seq=" : " mul seq=")<<r.sequenceNumber<<std::endl;
                    }
                }
            }
            os<<"======== End ========"<<std::endl;

            lllog(6)<<L"COM: "<<os.str().c_str()<<std::endl;

            std::cout<<os.str()<<std::endl;
        }
    };

    typedef AckedDataSenderBasic< Writer<UserData> > AckedDataSender;
}
}
}
}

#endif
