/******************************************************************************
*
* Copyright Saab AB, 2013, 2022 (http://safirsdkcore.com)
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
#include <Safir/Utilities/Internal/SharedCharArray.h>
#include <Safir/Dob/Internal/SystemPictureDefs.h>
#include <Safir/Dob/Internal/RawStatistics.h>
#include <boost/noncopyable.hpp>
#include <functional>
#include <limits>
#include <map>
#include <memory>
#include <set>
#include <chrono>
#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4127)
#  pragma warning (disable: 4100)
#  pragma warning (disable: 4244)
#endif

#include "ElectionMessage.pb.h"

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
        ElectionHandlerBasic(const std::wstring& logPrefix,
                             boost::asio::io_service::strand& strand,
                             CommunicationT& communication,
                             const int64_t id,
                             const int64_t nodeTypeId,
                             const std::map<int64_t, NodeType>& nodeTypes,
                             const std::chrono::steady_clock::duration& aloneTimeout,
                             const char* const receiverId,
                             const std::function<void(const int64_t nodeId,
                                                      const int64_t electionId)>& electionCompleteCallback,
                             const std::function<void(const int64_t incarnationId)>& formSystemCallback)
            : m_logPrefix(logPrefix)
            , m_strand(strand)
            , m_stopped(false)
            , m_communication(communication)
            , m_receiverId(LlufId_Generate64(receiverId))
            , m_id(id)
            , m_isLightNode(nodeTypes.at(nodeTypeId).isLightNode)
            , m_nodeTypes(nodeTypes)
            , m_allNodeTypeIds(GetAllNodeTypeIds(nodeTypes))
            , m_nonLightNodeTypeIds(GetNonLightNodeTypeIds(nodeTypes))
            , m_aloneTimeout(CalculateAloneTimeout(nodeTypes, aloneTimeout))
            , m_electionTimeout(CalculateElectionTimeout(nodeTypes))
            , m_elected(std::numeric_limits<int64_t>::min())
            , m_electionTimer(strand.context())
            , m_electionInProgress(false)
            , m_currentElectionId(0)
            , m_generateIncarnationIdTimer(strand.context())
            , m_sendMessageTimer(strand.context())
            , m_electionCompleteCallback(electionCompleteCallback)
            , m_formSystemCallback(formSystemCallback)
        {
            communication.SetDataReceiver([this](const int64_t from,
                                                 const int64_t nodeTypeId,
                                                 const char* const data,
                                                 const size_t size)
                                          {
                                              GotData(from, nodeTypeId, Safir::Utilities::Internal::SharedConstCharArray(data), size);
                                          },
                                          m_receiverId,
                                          [](size_t size){return new char[size];},
                                          [](const char* data){delete[] data;});
            lllog(3) << m_logPrefix << "AloneTimeout will be " << std::chrono::duration_cast<std::chrono::milliseconds>(m_aloneTimeout).count() << " ms" << std::endl;
            lllog(3) << m_logPrefix << "ElectionTimeout will be " << std::chrono::duration_cast<std::chrono::milliseconds>(m_electionTimeout).count() << " ms" << std::endl;
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


        bool IsElected() const {CheckStrand(); return m_id == m_elected;}
        bool IsElected(const int64_t nodeId) const {CheckStrand(); return nodeId == m_elected;}

        /**
         * Find out if the election is detached or not. This is not actually always the same as whether the
         * state is detached or not. Ask the Coordinator about that. At state transitions this value may
         * be different from the Coordinator value.
        */
        bool IsElectionDetached() const {CheckStrand(); return m_isLightNode && m_elected == m_id;}

        bool IsLightNode() const {return m_isLightNode;}

        void Stop()
        {
            lllog(4) << m_logPrefix << "ElectionHandler::Stop called." << std::endl;
            m_strand.dispatch([this]
                              {
                                  lllog(4) << m_logPrefix << "ElectionHandler executing Stop" << std::endl;
                                  m_pendingAlives.clear();
                                  m_pendingVictories.clear();
                                  m_pendingInquiries.clear();

                                  m_stopped = true;
                                  m_electionTimer.cancel();
                                  m_sendMessageTimer.cancel();
                                  m_generateIncarnationIdTimer.cancel();
                              });
        }

        void NodesChanged(const RawStatistics& statistics, std::shared_ptr<void> completionSignaller)
        {
            if (statistics.Id() != m_id)
            {
                throw std::logic_error("Got RawStatistics from some other node!");
            }

            //check that we can't see any other light nodes (this is a sanity check)
            if (m_isLightNode && statistics.Valid())
            {
                for (int i = 0; i < statistics.Size(); ++i)
                {
                    if (m_id != statistics.Id(i) && m_nodeTypes.at(statistics.NodeTypeId(i)).isLightNode)
                    {
                        throw std::logic_error("It looks like I can see another light node!");
                    }
                }
            }

            m_strand.dispatch([this, statistics, completionSignaller]
                              {
                                  lllog(5) << m_logPrefix << "ElectionHandler got new RawStatistics" << std::endl;
                                  m_lastStatistics = statistics;
                                  StartElection();
                              });
        }

        /** This can be used to force an election outside of node changes.*/
        void ForceElection()
        {
            m_strand.dispatch([this]
                              {
                                  lllog(5) << m_logPrefix << "ElectionHandler is forcing an election" << std::endl;
                                  StartElection();
                              });
        }
    private:

        /** Returns the subset of nodeTypes that doesn't have the LightNode flag  */
        static std::set<int64_t> GetNonLightNodeTypeIds(const std::map<int64_t, NodeType>& nodeTypes)
        {
            std::set<int64_t> res;

            for (auto it = nodeTypes.cbegin(); it != nodeTypes.cend(); ++it)
            {
                if (!it->second.isLightNode)
                {
                    res.insert(it->first);
                }
            }
            return res;
        }

        /** Returns the node type ids for all node types */
        static std::set<int64_t> GetAllNodeTypeIds(const std::map<int64_t, NodeType>& nodeTypes)
        {
            std::set<int64_t> res;

            for (auto it = nodeTypes.cbegin(); it != nodeTypes.cend(); ++it)
            {
                    res.insert(it->first);
            }
            return res;
        }


        /** Calculate the time to wait for other nodes to come up before assuming that
         * we're alone and proclaiming victory. */
        static std::chrono::steady_clock::duration
        CalculateAloneTimeout(const std::map<int64_t, NodeType>& nodeTypes,
                              const std::chrono::steady_clock::duration& aloneTimeout)
        {
            if (aloneTimeout > std::chrono::seconds(0))
            {
                return aloneTimeout;
            }

            //use max of non-light node types heartbeatInterval * maxLostHeartbeats * 2
            std::chrono::steady_clock::duration max = std::chrono::milliseconds(100);

            for (auto nt = nodeTypes.cbegin(); nt != nodeTypes.cend(); ++nt)
            {
                if (!nt->second.isLightNode)
                {
                    max = std::max(max,nt->second.heartbeatInterval * nt->second.maxLostHeartbeats * 2);
                }
            }
            return max;
        }

        /** Calculate the time to wait for other nodes to respond to our INQUIRY. */
        static std::chrono::steady_clock::duration
        CalculateElectionTimeout(const std::map<int64_t, NodeType>& nodeTypes)
        {
            //use max of non-light node types retryTimeout[0] * maxLostHeartbeats
            std::chrono::steady_clock::duration max = std::chrono::milliseconds(100);

            for (auto nt = nodeTypes.cbegin(); nt != nodeTypes.cend(); ++nt)
            {
                if (!nt->second.isLightNode)
                {
                    max = std::max(max,nt->second.retryTimeout.at(0) * nt->second.maxLostHeartbeats);
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
            CheckStrand();

            if (m_stopped)
            {
                lllog(4) << m_logPrefix << "I am stopping, so will not start new election" << std::endl;
                return;
            }

            if (m_isLightNode &&
                m_lastStatistics.Valid() &&
                m_lastStatistics.IsEveryoneElseDead())
            {
                lllog(4) << m_logPrefix << "I am a light node, and everyone else is gone, so I shall get myself elected anyway." << std::endl;
                m_lastStatistics = RawStatistics();
            }

            lllog(4) << m_logPrefix << "Checking if I should start election" << std::endl;

            if (!m_lastStatistics.Valid())
            {
                lllog(4) << m_logPrefix << "Haven't heard from any other nodes, electing myself!" << std::endl;
                m_elected = m_id;
                m_currentElectionId = LlufId_GenerateRandom64();
                m_electionCompleteCallback(m_elected, m_currentElectionId);

                //Note that we may not end up with this incarnationId.
                //1. We are of a node type that is not allowed to form a system
                //2. A RAW happens to be received before our incarnation id is set.
                m_formSystemCallback(LlufId_GenerateRandom64());
                return;
            }
            else if (m_isLightNode)
            {
                lllog(4) << m_logPrefix << "I am a light node, not starting any elections" << std::endl;

                if (m_currentElectionId != m_lastStatistics.ElectionId())
                {
                    lllog(4) << m_logPrefix << "FYI, I have a different election id than what others have. Not doing anything about that, though." << std::endl;
                }
                return;
            }
            else
            {
                for (int i = 0; i < m_lastStatistics.Size(); ++i)
                {
                    lllog(7) << m_logPrefix << "  know of node " << m_lastStatistics.Id(i)
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
                            lllog(4) << m_logPrefix << "Found elected node with higher id than me, "
                                     << "not starting election!" << std::endl;
                            return;
                        }
                    }
                }
            }

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
                if (error || m_stopped || !m_electionInProgress)
                {
                    return;
                }

                lllog(4) << m_logPrefix << "Starting election" << std::endl;

                m_currentElectionId = LlufId_GenerateRandom64();

                m_pendingInquiries = m_nonLightNodeTypeIds;
                SendPendingElectionMessages();

                m_electionInProgress = true;
                m_electionTimer.expires_from_now(m_electionTimeout);

                m_electionTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& error){ElectionTimeout(error);}));
            }));
        }


        //must be called in strand
        void ElectionTimeout(const boost::system::error_code& error)
        {
            CheckStrand();

            if (!error && !m_stopped && m_electionInProgress)
            {
                m_electionInProgress = false;

                lllog(4) << m_logPrefix << "There can be only one! Will send VICTORY ("
                    << m_currentElectionId << ") to everyone!" << std::endl;

                m_elected = m_id;

                m_pendingVictories = m_allNodeTypeIds;
                SendPendingElectionMessages();

                m_electionCompleteCallback(m_elected, m_currentElectionId);

                if (m_lastStatistics.IncarnationId() == 0)
                {
                    m_generateIncarnationIdTimer.expires_from_now(m_aloneTimeout);

                    m_generateIncarnationIdTimer.async_wait
                        (m_strand.wrap([this](const boost::system::error_code& error)
                    {
                        if (error || m_stopped)
                        {
                            return;
                        }

                        //Note that we may not end up with this incarnationId.
                        //1. We are of a node type that is not allowed to form a system
                        //2. A RAW happens to be received before our incarnation id is set.
                        m_formSystemCallback(LlufId_GenerateRandom64());
                    }));
                }
            }
        }


        //not in strand
        void GotData(const int64_t from,
                     const int64_t nodeTypeId,
                     const Safir::Utilities::Internal::SharedConstCharArray& data,
                     size_t size)
        {
            ElectionMessage message;
            message.ParseFromArray(data.get(), static_cast<int>(size));
            lllog(5) << m_logPrefix << "Got ElectionMessage ("
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

                        if (m_lastStatistics.IsDead(i) && !m_isLightNode)
                        {
#if 1 
                            if (message.action() == VICTORY)
                            {
                                lllog(5) << m_logPrefix << "Got VICTORY from a node that is dead! Starting an election!" << std::endl;
                                StartElection();
                            }
                            else
#endif
                            {
                                lllog(5) << m_logPrefix << "Got ElectionMessage from a node that is dead! Discarding!" << std::endl;
                            }
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
                        if (from < m_id && !m_isLightNode)
                        {
                            lllog(5) << m_logPrefix << "Got an inquiry from someone (" << from << ") smaller than us ("
                                     << m_id << "), sending alive and starting election."  << std::endl;
                            m_pendingAlives[from] = std::make_pair(nodeTypeId, message.election_id());
                            SendPendingElectionMessages();

                            StartElection();
                        }
                    }
                    break;

                case ALIVE:
                    {
                        //if we got an alive from someone bigger than us we abandon the election
                        if (from > m_id && !m_isLightNode &&
                            message.election_id() == m_currentElectionId)
                        {
                            lllog(5) << m_logPrefix << "Got alive from someone bigger than me ("
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
                        if (from > m_id || m_isLightNode)
                        {
                            lllog(4) << m_logPrefix << "New controller elected: " << from << std::endl;
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
                            lllog(5) << m_logPrefix << "Got victory from someone smaller than me ("
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
            CheckStrand();

            m_sendMessageTimer.cancel();

            { //ALIVE
                const auto pending = m_pendingAlives;
                m_pendingAlives.clear();

                for (auto it = pending.cbegin(); it != pending.cend(); ++it)
                {
                    ElectionMessage aliveMsg;
                    aliveMsg.set_action(ALIVE);
                    aliveMsg.set_election_id(it->second.second);

                    const auto size = aliveMsg.ByteSizeLong();
                    Safir::Utilities::Internal::SharedCharArray data = Safir::Utilities::Internal::MakeSharedArray(size);
                    aliveMsg.SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));

                    const bool sent = m_communication.Send(it->first,
                                                           it->second.first,
                                                           std::move(data),
                                                           size,
                                                           m_receiverId,
                                                           true);

                    if (!sent)
                    {
                        lllog(9) << m_logPrefix << "ElectionHandler: Overflow when sending ALIVE to node "
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

                    const auto size = victoryMsg.ByteSizeLong();
                    Safir::Utilities::Internal::SharedCharArray data = Safir::Utilities::Internal::MakeSharedArray(size);
                    victoryMsg.SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));

                    const bool sent = m_communication.Send(0, *it, std::move(data), size, m_receiverId, true);

                    if (!sent)
                    {
                        lllog(9) << m_logPrefix << "ElectionHandler: Overflow when sending VICTORY to node type "
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

                    const auto size = message.ByteSizeLong();
                    Safir::Utilities::Internal::SharedCharArray data = Safir::Utilities::Internal::MakeSharedArray(size);
                    message.SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));

                    const bool sent = m_communication.Send(0, *it, std::move(data), size, m_receiverId, true);
                    if (!sent)
                    {
                        lllog(7) << m_logPrefix << "ElectionHandler: Overflow when sending INQUIRY to node type "
                                 << m_nodeTypes.find(*it)->second.name.c_str() << std::endl;
                        m_pendingInquiries.insert(*it);
                    }
                    else
                    {
                        lllog(9) << m_logPrefix << "ElectionHandler: sent INQUIRY to node type "
                                 << m_nodeTypes.find(*it)->second.name.c_str() << std::endl;
                    }
                }
            }

            //Handle retry
            if (!m_pendingAlives.empty() || !m_pendingVictories.empty() || !m_pendingInquiries.empty())
            {
                m_sendMessageTimer.expires_from_now(std::chrono::milliseconds(10));
                m_sendMessageTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& error)
                                                            {
                                                                if (!error && !m_stopped)
                                                                {
                                                                    SendPendingElectionMessages();
                                                                }
                                                            }));
            }
        }

        void CheckStrand() const
        {
#if (!defined NDEBUG && !defined SAFIR_DISABLE_CHECK_STRAND)
            if (!m_strand.running_in_this_thread())
            {
                throw std::logic_error("Function must be must be called from the strand!");
            }
#endif
        }

        const std::wstring m_logPrefix;
        boost::asio::io_service::strand& m_strand;
        bool m_stopped;
        CommunicationT& m_communication;
        const uint64_t m_receiverId;

        const int64_t m_id;
        const bool m_isLightNode;
        const std::map<int64_t, NodeType> m_nodeTypes;
        const std::set<int64_t> m_allNodeTypeIds;
        const std::set<int64_t> m_nonLightNodeTypeIds;

        const std::chrono::steady_clock::duration m_aloneTimeout;
        const std::chrono::steady_clock::duration m_electionTimeout;

        RawStatistics m_lastStatistics;

        int64_t m_elected;
        boost::asio::steady_timer m_electionTimer;
        bool m_electionInProgress;
        int64_t m_currentElectionId;

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
        const std::function<void(const int64_t nodeId,
                                 const int64_t electionId)> m_electionCompleteCallback;

        //callback to call when generating a new incarnation id
        const std::function<void(const int64_t incarnationId)> m_formSystemCallback;


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
