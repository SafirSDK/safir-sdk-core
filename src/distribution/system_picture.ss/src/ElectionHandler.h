/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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

#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Dob/Internal/SystemPictureDefs.h>
#include <Safir/Dob/Internal/RawStatistics.h>
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <functional>
#include <limits>
#include <boost/atomic.hpp>
#include <map>
#include <set>

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4244)
#  pragma warning (disable: 4127)
#  pragma warning (disable: 4267)
#endif

#include "ElectionMessage.pb.h"
#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>

#ifdef _MSC_VER
#  pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{

namespace SP
{
    template <class CommunicationT>
    class ElectionHandlerBasic
        : private boost::noncopyable
    {
    public:
        ElectionHandlerBasic(boost::asio::io_service& ioService,
                             CommunicationT& communication,
                             const int64_t id,
                             const std::map<int64_t, NodeType>& nodeTypes,
                             const char* const receiverId,
                             const boost::function<void(const int64_t nodeId,
                                                      const int64_t electionId)>& electionCompleteCallback,
                             const boost::function<void(const int64_t incarnationId)>& setIncarnationIdCallback)
            : m_strand (ioService)
            , m_stopped(false)
            , m_communication(communication)
            , m_receiverId(LlufId_Generate64(receiverId))
            , m_id(id)
            , m_nodeTypes(nodeTypes)
            , m_nonLightNodeTypes(GetLightNodeTypes(nodeTypes))
            , m_aloneTimeout(CalculateAloneTimeout(nodeTypes))
            , m_electionTimeout(CalculateElectionTimeout(nodeTypes))
            , m_electedStorage(new AlignedStorage())
            , m_elected(reinterpret_cast<boost::atomic<int64_t>&>(*m_electedStorage))
            , m_electionTimer(ioService)
            , m_electionInProgress(false)
            , m_currentElectionId(0)
            , m_generateIncarnationIdTimer(ioService)
            , m_sendMessageTimer(ioService)
            , m_electionCompleteCallback(electionCompleteCallback)
            , m_setIncarnationIdCallback(setIncarnationIdCallback)
        {
            new (m_electedStorage.get()) boost::atomic<uint64_t>(std::numeric_limits<int64_t>::min());

            communication.SetDataReceiver([this](const int64_t from,
                                                 const int64_t nodeTypeId,
                                                 const char* const data,
                                                 const size_t size)
                                          {
                                              GotData(from, nodeTypeId, boost::shared_ptr<const char[]>(data), size);
                                          },
                                          m_receiverId,
                                          [](size_t size){return new char[size];},
                                          [](const char* data){delete[] data;});

            lllog(3) << "SP: AloneTimeout will be " << boost::chrono::duration_cast<boost::chrono::milliseconds>(m_aloneTimeout) << std::endl;
            lllog(3) << "SP: ElectionTimeout will be " << boost::chrono::duration_cast<boost::chrono::milliseconds>(m_electionTimeout) << std::endl;
            m_electionInProgress = true;
            m_electionTimer.expires_from_now(m_aloneTimeout);
            m_electionTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& error)
                                                     {
                                                         if (!error && !m_stopped)
                                                         {
                                                             StartElection();
                                                         }
                                                     }));
        }


        bool IsElected() const {return m_id == m_elected;}
        bool IsElected(const int64_t nodeId) const {return nodeId == m_elected;}

        void Stop()
        {
            m_strand.dispatch([this]
                              {
                                  m_pendingAlives.clear();
                                  m_pendingVictories.clear();
                                  m_pendingInquiries.clear();

                                  m_stopped = true;
                                  m_electionTimer.cancel();
                                  m_sendMessageTimer.cancel();
                                  m_generateIncarnationIdTimer.cancel();
                              });
        }

        void NodesChanged(const RawStatistics& statistics, boost::shared_ptr<void> completionSignaller)
        {
            m_strand.dispatch([this, statistics, completionSignaller]
                              {
                                  lllog(5) << "SP: ElectionHandler got new RawStatistics" << std::endl;
                                  m_lastStatistics = std::move(statistics);
                                  StartElection();
                              });
        }

        /** This can be used to force an election outside of node changes.*/
        void ForceElection()
        {
            m_strand.dispatch([this]
                              {
                                  lllog(5) << "SP: ElectionHandler is forcing an election" << std::endl;
                                  StartElection();
                              });
        }
    private:

        /** Finds Returns the subset of nodeTypes with the isLight flag  */
        static std::set<int64_t> GetLightNodeTypes(const std::map<int64_t, NodeType>& nodeTypes)
        {
            std::set<int64_t> res;

            for (auto it = nodeTypes.cbegin(); it != nodeTypes.cend(); ++it)
            {
                if (!it->second.isLight)
                {
                    res.insert(it->first);
                }
            }
            return res;
        }


        /** Calculate the time to wait for other nodes to come up before assuming that
         * we're alone and proclaiming victory. */
        static boost::chrono::steady_clock::duration CalculateAloneTimeout(const std::map<int64_t, NodeType>& nodeTypes)
        {
            //use max of non-light node types heartbeatInterval * maxLostHeartbeats * 2
            boost::chrono::steady_clock::duration max = boost::chrono::milliseconds(100);

            for (auto nt = nodeTypes.cbegin(); nt != nodeTypes.cend(); ++nt)
            {
                if (!nt->second.isLight)
                {
                    max = std::max(max,nt->second.heartbeatInterval * nt->second.maxLostHeartbeats * 2);
                }
            }
            return max;
        }

        /** Calculate the time to wait for other nodes to respond to our INQUIRY. */
        static boost::chrono::steady_clock::duration CalculateElectionTimeout(const std::map<int64_t, NodeType>& nodeTypes)
        {
            //use max of non-light node types retryTimeout * maxLostHeartbeats
            boost::chrono::steady_clock::duration max = boost::chrono::milliseconds(100);

            for (auto nt = nodeTypes.cbegin(); nt != nodeTypes.cend(); ++nt)
            {
                if (!nt->second.isLight)
                {
                    max = std::max(max,nt->second.retryTimeout * nt->second.maxLostHeartbeats);
                }
            }
            return max;
        }


        static std::wstring ToString(const ElectionAction action)
        {
            switch (action)
            {
            case INQUIRY: return L"INQUIRY";
            case ALIVE: return L"ALIVE";
            case VICTORY: return L"VICTORY";
            default:
                throw std::logic_error("Unknown ElectionAction");
            }
        }


        //must be called in strand
        void StartElection()
        {
            //cancel any other pending elections
            m_electionTimer.cancel();

            //This timeout is just to make sure that if several nodes come up at the same time
            //we will not start too many elections
            //The number 4 is dependent on how Communication handles sending discovery information.
            //It sends discovery information every 3*electionTimeout, so we want to wait a little bit
            //longer than that.
            m_electionTimer.expires_from_now(4*m_electionTimeout);
            m_electionInProgress = true;
            m_electionTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& error)
            {
                if (!!error || m_stopped || !m_electionInProgress)
                {
                    return;
                }
                lllog(4) << "SP: Checking if I should start election" << std::endl;

                if (!m_lastStatistics.Valid())
                {
                    lllog(4) << "SP: Haven't heard from any other nodes, electing myself!" << std::endl;
                    m_elected = m_id;
                    m_currentElectionId = LlufId_GenerateRandom64();
                    m_electionCompleteCallback(m_elected, m_currentElectionId);

                    //note that we may not end up with this incarnationId, in case a RAW
                    //is received just after this call, but before it is executed
                    m_setIncarnationIdCallback(LlufId_GenerateRandom64());
                    return;
                }
                else
                {
                    for (int i = 0; i < m_lastStatistics.Size(); ++i)
                    {
                        lllog(7) << "SP:   know of node " << m_lastStatistics.Id(i)
                                 << (m_lastStatistics.IsDead(i) ? " which is dead" : "") << std::endl;

                        if (m_lastStatistics.Id(i) == m_elected && !m_lastStatistics.IsDead(i))
                        {
                            // Note: the last two conditions below changes the behaviour
                            // of SystemPicture in a rather surprising way. For example
                            // it causes the unit tests to take a lot longer.
                            // This is rather surprising, since the extra check seems reasonable
                            // to me...  All it adds is that we need the remote node to
                            // have seen the same election as we did, otherwise we may be
                            // in a situation where a reelection is needed.
                            if (m_elected > m_id &&
                                m_lastStatistics.HasRemoteStatistics(i) &&
                                m_lastStatistics.RemoteStatistics(i).ElectionId() == m_lastStatistics.ElectionId())
                            {
                                lllog(4) << "SP: Found elected node with higher id than me, "
                                         << "not starting election!" << std::endl;
                                return;
                            }
                        }
                    }
                }


                lllog(4) << "SP: Starting election" << std::endl;
                m_currentElectionId = LlufId_GenerateRandom64();

                m_pendingInquiries = m_nonLightNodeTypes;
                SendPendingElectionMessages();

                m_electionInProgress = true;
                m_electionTimer.expires_from_now(m_electionTimeout);

                m_electionTimer.async_wait(m_strand.wrap(boost::bind(&ElectionHandlerBasic<CommunicationT>::ElectionTimeout, this, _1)));
            }));
        }


        //must be called in strand
        void ElectionTimeout(const boost::system::error_code& error)
        {
            if (!error && !m_stopped && m_electionInProgress)
            {
                m_electionInProgress = false;

                lllog(4) << "SP: There can be only one! Will send VICTORY ("
                    << m_currentElectionId << ") to everyone!" << std::endl;

                m_elected = m_id;

                m_pendingVictories = m_nonLightNodeTypes;
                SendPendingElectionMessages();

                m_electionCompleteCallback(m_elected, m_currentElectionId);

                if (m_lastStatistics.IncarnationId() == 0)
                {
                    m_generateIncarnationIdTimer.expires_from_now(m_aloneTimeout);

                    m_generateIncarnationIdTimer.async_wait
                        (m_strand.wrap([this](const boost::system::error_code& error)
                    {
                        if (!!error || m_stopped)
                        {
                            return;
                        }

                        //note that we may not end up with this incarnationId, in case a RAW
                        //is received just after this call, but before it is executed
                        m_setIncarnationIdCallback(LlufId_GenerateRandom64());
                    }));
                }
            }
        }


        //not in strand
        void GotData(const int64_t from,
                     const int64_t nodeTypeId,
                     const boost::shared_ptr<const char[]>& data,
                     size_t size)
        {
            ElectionMessage message;
            message.ParseFromArray(data.get(), static_cast<int>(size));
            lllog(5) << "SP: Got ElectionMessage ("
                     << ToString(message.action())
                     << ", "
                     << message.election_id()
                     << ") from " << from << std::endl;

            m_strand.dispatch([this,message, from, nodeTypeId]
            {
                bool found = false;
                for (int i = 0; i < m_lastStatistics.Size(); ++i)
                {
                    if (m_lastStatistics.Id(i) == from)
                    {
                        found = true;

                        if (m_lastStatistics.IsDead(i))
                        {
                            lllog(5) << "SP: Got ElectionMessage from a node that is dead! Discarding!" << std::endl;
                            return;
                        }

                        break;
                    }
                }

                if (!found)
                {
                    //this would indicate a threading error.
                    SEND_SYSTEM_LOG(Alert,
                                    << "Got ElectionMessage ("
                                    << ToString(message.action())
                                    << ", "
                                    << message.election_id()
                                    << ") from an unknown node with id " << from);

                    throw std::logic_error("Got ElectionMessage from a node that I dont know about!");
                }

                switch (message.action())
                {
                case INQUIRY:
                    {
                        //if we got an inquiry from someone smaller than us we send an alive
                        //and start a new election
                        if (from < m_id)
                        {
                            lllog(5) << "SP: Got an inquiry from someone smaller than us, sending alive and starting election" << std::endl;
                            m_pendingAlives[from] = std::make_pair(nodeTypeId, message.election_id());
                            SendPendingElectionMessages();

                            StartElection();
                        }
                    }
                    break;

                case ALIVE:
                    {
                        //if we got an alive from someone bigger than us we abandon the election
                        if (from > m_id &&
                            message.election_id() == m_currentElectionId)
                        {
                            lllog(5) << "SP: Got alive from someone bigger than me ("
                                     << from << "), abandoning election." << std::endl;
                            m_electionInProgress = false;
                            m_electionTimer.cancel();
                            m_pendingInquiries.clear();
                            m_pendingVictories.clear();
                        }
                    }
                    break;

                case VICTORY:
                    {
                        if (from > m_id)
                        {
                            lllog(4) << "SP: New controller elected: " << from << std::endl;
                            //graciously accept their victory
                            m_elected = from;

                            m_electionCompleteCallback(m_elected, message.election_id());

                            //cancel any ongoing elections
                            m_electionInProgress = false;
                            m_electionTimer.cancel();
                            m_pendingInquiries.clear();
                            m_pendingVictories.clear();
                        }
                        else //No! We're going to usurp him! restart election
                        {
                            lllog(5) << "SP: Got victory from someone smaller than me ("
                                     << from << "), starting new election." << std::endl;

                            StartElection();
                        }
                    }
                    break;
                default:
                    throw std::logic_error("Unknown ElectionAction");
                }
            });
        }


        //must be called in strand
        void SendPendingElectionMessages()
        {
            m_sendMessageTimer.cancel();

            { //ALIVE
                const auto pending = m_pendingAlives;
                m_pendingAlives.clear();

                for (auto it = pending.cbegin(); it != pending.cend(); ++it)
                {
                    ElectionMessage aliveMsg;
                    aliveMsg.set_action(ALIVE);
                    aliveMsg.set_election_id(it->second.second);

                    const auto size = aliveMsg.ByteSize();
                    boost::shared_ptr<char[]> data = boost::make_shared<char[]>(size);
                    aliveMsg.SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));

                    const bool sent = m_communication.Send(it->first,
                                                           it->second.first,
                                                           std::move(data),
                                                           size,
                                                           m_receiverId,
                                                           true);

                    if (!sent)
                    {
                        lllog(9) << "SP: ElectionHandler: Overflow when sending ALIVE to node "
                                 << it->first << std::endl;
                        m_pendingAlives.insert(*it);
                    }
                }
            }

            { //VICTORY
                const auto pending = m_pendingVictories;
                m_pendingVictories.clear();

                for (auto it = pending.cbegin(); it != pending.cend(); ++it)
                {
                    ElectionMessage victoryMsg;
                    victoryMsg.set_action(VICTORY);
                    victoryMsg.set_election_id(m_currentElectionId);

                    const auto size = victoryMsg.ByteSize();
                    boost::shared_ptr<char[]> data = boost::make_shared<char[]>(size);
                    victoryMsg.SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));

                    const bool sent = m_communication.Send(0, *it, std::move(data), size, m_receiverId, true);

                    if (!sent)
                    {
                        lllog(9) << "SP: ElectionHandler: Overflow when sending VICTORY to node type "
                                 << m_nodeTypes.find(*it)->second.name.c_str() << std::endl;
                        m_pendingVictories.insert(*it);
                    }
                }
            }

            { //INQUIRY
                const auto pending = m_pendingInquiries;
                m_pendingInquiries.clear();

                for (auto it = pending.cbegin(); it != pending.cend(); ++it)
                {
                    ElectionMessage message;
                    message.set_action(INQUIRY);
                    message.set_election_id(m_currentElectionId);

                    const auto size = message.ByteSize();
                    boost::shared_ptr<char[]> data = boost::make_shared<char[]>(size);
                    message.SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));

                    const bool sent = m_communication.Send(0, *it, std::move(data), size, m_receiverId, true);
                    if (!sent)
                    {
                        lllog(7) << "SP: ElectionHandler: Overflow when sending INQUIRY to node type "
                                 << m_nodeTypes.find(*it)->second.name.c_str() << std::endl;
                        m_pendingInquiries.insert(*it);
                    }
                    else
                    {
                        lllog(9) << "SP: ElectionHandler: sent INQUIRY to node type "
                                 << m_nodeTypes.find(*it)->second.name.c_str() << std::endl;
                    }
                }
            }

            //Handle retry
            if (!m_pendingAlives.empty() || !m_pendingVictories.empty() || !m_pendingInquiries.empty())
            {
                m_sendMessageTimer.expires_from_now(boost::chrono::milliseconds(10));
                m_sendMessageTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& error)
                                                            {
                                                                if (!error && !m_stopped)
                                                                {
                                                                    SendPendingElectionMessages();
                                                                }
                                                            }));
            }
        }


        mutable boost::asio::strand m_strand;
        bool m_stopped;
        CommunicationT& m_communication;
        const uint64_t m_receiverId;

        const int64_t m_id;
        const std::map<int64_t, NodeType> m_nodeTypes;
        const std::set<int64_t> m_nonLightNodeTypes;

        const boost::chrono::steady_clock::duration m_aloneTimeout;
        const boost::chrono::steady_clock::duration m_electionTimeout;

        RawStatistics m_lastStatistics;

        //64 bit atomic needs to be aligned on 64 bit boundary even on 32 bit systems,
        //so we need to use alignment magic.
        typedef boost::aligned_storage<sizeof(boost::atomic<int64_t>),sizeof(boost::atomic<int64_t>)>::type AlignedStorage;
        std::unique_ptr<AlignedStorage> m_electedStorage;
        boost::atomic<int64_t>& m_elected;
        boost::asio::steady_timer m_electionTimer;
        bool m_electionInProgress;
        boost::atomic<int64_t> m_currentElectionId;

        boost::asio::steady_timer m_generateIncarnationIdTimer;

        boost::asio::steady_timer m_sendMessageTimer;

        //a set of node type ids to which we want to send INQUIRY to, using m_currentElectionId
        std::set<int64_t> m_pendingInquiries;

        //this is a list of the nodes that we need to send ALIVE messages to (key), along with the
        //election id that came in the INQUIRY (value.second) and the nodeTypeId of the recipient
        //(value.first)
        std::map<int64_t, std::pair<int64_t, int64_t>> m_pendingAlives;

        //a set of node type ids to which we want to send VICTORY to, using m_currentElectionId
        std::set<int64_t> m_pendingVictories;

        //callback to call on completed election
        const boost::function<void(const int64_t nodeId,
                                 const int64_t electionId)> m_electionCompleteCallback;

        //callback to call when generating a new incarnation id
        const boost::function<void(const int64_t incarnationId)> m_setIncarnationIdCallback;


    };

}
    namespace Com
    {
        //forward declaration.
        class Communication;
    }

    namespace SP
    {
        typedef ElectionHandlerBasic<Com::Communication> ElectionHandler;
    }
}
}
}
