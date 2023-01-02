/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safirsdkcore.com)
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

#include <Safir/Dob/Internal/RawStatistics.h>
#include <Safir/Dob/Internal/SystemPictureDefs.h>
#include <boost/noncopyable.hpp>
#include <boost/range/algorithm/set_algorithm.hpp>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <limits>
#include <map>
#include <set>
#include <memory>
#include <functional>
#include "ElectionHandler.h"
#include "MessagePrinting.h"
#include "RawChanges.h"
#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4127)
#endif

#include "SystemStateMessage.pb.h"

#ifdef _MSC_VER
#  pragma warning (pop)
#endif

namespace
{
  template< typename ContainerT, typename PredicateT >
  void erase_if( ContainerT& items, const PredicateT& predicate )
  {
    for( auto it = items.begin(); it != items.end(); )
    {
      if( predicate(*it) )
      {
          it = items.erase(it);
      }
      else
      {
          ++it;
      }
    }
  }

    const int RESURRECT_COUNT = 3;
}

namespace Safir
{
namespace Dob
{
namespace Internal
{
    namespace Com
    {
        //forward declaration.
        class Communication;
    }

namespace SP
{

    template <class CommunicationT, class RawHandlerT, class ElectionHandlerT>
    class CoordinatorBasic
        : private boost::noncopyable
    {
    public:
        CoordinatorBasic(boost::asio::io_service::strand& strand,
                         CommunicationT& communication,
                         const std::string& name,
                         const int64_t id,
                         const int64_t nodeTypeId,
                         const std::string& controlAddress,
                         const std::string& dataAddress,
                         const std::map<int64_t, NodeType>& nodeTypes,
                         const boost::chrono::steady_clock::duration& aloneTimeout,
                         const char* const receiverId,
                         RawHandlerT& rawHandler)
            : m_strand (strand)
            , m_communication(communication)
            , m_lastStatisticsDirty(true)
            , m_name(name)
            , m_id(id)
            , m_nodeTypeId(nodeTypeId)
            , m_nodeTypes(nodeTypes)
            , m_controlAddress(controlAddress)
            , m_dataAddress(dataAddress)
            , m_ownElectionId(0)
            , m_rawHandler(rawHandler)
            , m_failedStateUpdates(0)
        {
            m_electionHandler.reset(new ElectionHandlerT(m_strand,
                                                         communication,
                                                         id,
                                                         nodeTypeId,
                                                         nodeTypes,
                                                         aloneTimeout,
                                                         receiverId,
                                                         [this](const int64_t nodeId, const int64_t electionId)
                                                         {
                                                             if (nodeId == m_id)
                                                             {
                                                                 m_ownElectionId = electionId;
                                                             }
                                                             else
                                                             {
                                                                 m_ownElectionId = 0;
                                                             }

                                                             m_rawHandler.SetElectionId(nodeId, electionId);
                                                         },
                                                         [this](const int64_t incarnationId)
                                                         {
                                                             m_rawHandler.FormSystem(incarnationId);
                                                         }));

            m_stateMessage.set_elected_id(0); //our state message is not valid until we have a real id set.
            m_stateMessage.set_is_detached(false);

            rawHandler.AddRawChangedCallback(m_strand.wrap([this](const RawStatistics& statistics,
                                                                  const RawChanges flags,
                                                                  std::shared_ptr<void> completionSignaller)
            {
                lllog(8) << "SP: Coordinator got new raw data (" << flags << ")" << std::endl;

                m_lastStatistics = statistics;
                m_lastStatisticsDirty = true;

                if (m_electionHandler->IsElected())
                {
                    UpdateMyState();
                }

                if (flags.NodesChanged())
                {
                    m_electionHandler->NodesChanged(statistics, completionSignaller);
                }

                if (flags.BadElectionIdDetected())
                {
                    m_electionHandler->ForceElection();
                }

            }));
        }

        /**
         * Find out if the current state is detached or not.
         *
         * Must be called from within the strand!
         */
        bool IsDetached() const
        {
            CheckStrand();

            return m_stateMessage.is_detached();
        }

        void SetStateChangedCallback(const std::function<void(const SystemStateMessage& data)>& callback)
        {
            m_strand.dispatch([this,callback]
                              {
                                  m_stateChangedCallback = callback;
                              });
        }

        //used to send state message
        //if onlyOwnState is true the callback will only be called if we're elected
        //and have a valid own system state that is ok to send.
        void PerformOnStateMessage(const std::function<void(std::unique_ptr<char []> data,
                                   const size_t size)> & fn,
                                   const bool onlyOwnState)
        {
            m_strand.dispatch([this,fn,onlyOwnState]
            {
                lllog(8) << "SP: Coordinator::PerformOnStateMessage:" << std::endl;
                if (onlyOwnState)
                {
                    const bool okToSend = UpdateMyState();

                    if (!okToSend)
                    {
                        lllog(8) << "SP:  - Not sending: System State not ok to send!" << std::endl;
                        return;
                    }
                }

                if (m_stateMessage.elected_id() == 0)
                {
                    lllog(8) << "SP:  - Not sending: System State contains no elected node!" << std::endl;
                    return;
                }

                if (m_stateMessage.election_id() == 0)
                {
                    lllog(8) << "SP: - Not sending: System State contains no election id." << std::endl;
                    return;
                }

                if (m_stateMessage.node_info_size() == 0)
                {
                    lllog(8) << "SP:  - Not sending: System State contains no nodes!" << std::endl;
                    return;
                }

                lllog(8) << "SP:  - Ok to send!\n" << m_stateMessage << std::endl;
                const size_t size = m_stateMessage.ByteSizeLong();
                auto data = std::unique_ptr<char[]>(new char[size]);
                m_stateMessage.SerializeWithCachedSizesToArray
                    (reinterpret_cast<google::protobuf::uint8*>(data.get()));
                fn(std::move(data), size);
            });
        }

        //new incoming system state from elected coordinator
        void NewRemoteStatistics(const int64_t from,
                                 const Safir::Utilities::Internal::SharedConstCharArray& data,
                                 const size_t size)
        {
            m_strand.dispatch([this,from,data,size]
            {
                if (!m_electionHandler->IsElected(from))
                {
                    SEND_SYSTEM_LOG(Informational,
                                    << "SystemPicture (in node " << m_id
                                    << ") got a new system state (from node "
                                    << from << ") from a node that is not elected. Discarding.");

                    if (IsDetached())
                    {
                        lllog(4) << "Am lightnode, forcing election to see if that helps" << std::endl;
                        m_electionHandler->ForceElection();
                    }

                    return;
                }

                //if we did not have a valid election_id before this is the very first
                //SS we have received. But this is really only valid for non-lightnodes. So only use this
                //value on non-lightnodes
                const bool firstMessage = m_stateMessage.election_id() == 0;

                lllog (7) << "SP: Got new SystemState from node " << from << std::endl;
                m_stateMessage.ParseFromArray(data.get(),static_cast<int>(size));

                //do some sanity checks
                if (m_stateMessage.election_id() == 0 ||
                    m_ownElectionId != 0)
                {
                    SEND_SYSTEM_LOG(Alert,
                                    << "Got a State message with election id "
                                    << m_stateMessage.election_id() <<
                                    " while m_ownElectionId was " << m_ownElectionId);
                    throw std::logic_error("Incorrect ElectionIds!");
                }

                if (m_stateMessage.is_detached())
                {
                    SEND_SYSTEM_LOG(Alert,
                                    << "Got a State message from " << from << " which claims to be a detached node");
                    throw std::logic_error("Got detached StateMessage!");
                }
                //Getting a system state that we're marked as dead in signifies that
                //there is a bug in Communication, where an ExcludeNode has not worked
                //or a bug in UpdateMyState where a node has been marked as dead but
                //not excluded.
                bool foundSelf = false;
                for (int i = 0; i < m_stateMessage.node_info_size(); ++i)
                {
                    if (m_stateMessage.node_info(i).id() == m_id)
                    {
                        foundSelf = true;
                        const bool isDead = m_stateMessage.node_info(i).is_dead();
                        if (isDead && m_electionHandler->IsLightNode())
                        {
                            lllog (4) << "SP: This state says that we are dead, hopefully "
                                      << "that is from before we were resurrected, so ignoring it"<< std::endl;
                            //This state may contain information about us before we were resurrected, so we ignore it.
                            //This is done by setting the state to an invalid mode.
                            m_stateMessage.set_election_id(0);
                            return;
                        }
                        else if (isDead && !m_electionHandler->IsLightNode())
                        {
                            SEND_SYSTEM_LOG(Alert,
                                            << "Got a SystemState in which I am dead!");
                            throw std::logic_error("Got a SystemState in which I am dead!");
                        }
                    }
                }

                if (!foundSelf)
                {
                    if (!firstMessage  && !m_electionHandler->IsLightNode())
                    {
                        SEND_SYSTEM_LOG(Alert,
                                        << "I've disappeared from SystemState! Very bad!");
                        throw std::logic_error("Got a SystemState which I am not in!");
                    }

                    //This state does not contain ourselves, so we ignore it.
                    //This is done by setting the state to an invalid mode.
                    m_stateMessage.set_election_id(0);
                    return;
                }

                const auto deadNodes = GetDeadNodes(m_stateMessage);

                //Note: never do exclude on a node that is not one that we have
                //received NewNode for, i.e. is in m_lastStatistics top level.
                for (int i = 0; i < m_lastStatistics.Size(); ++i)
                {
                    const bool deadInState = deadNodes.find(m_lastStatistics.Id(i)) != deadNodes.end();
                    //if we haven't marked the node as dead and electee doesnt think the node
                    //is part of the system we want to exclude the node
                    if (!m_lastStatistics.IsDead(i) && deadInState)
                    {
                        lllog (4) << "SP: Elected coordinator thinks that node "
                                  << m_lastStatistics.Name(i).c_str()
                                  << " with id " << m_lastStatistics.Id(i)
                                  << " is dead, so I'll mark him as dead." << std::endl;

                        m_rawHandler.ExcludeNode(m_lastStatistics.Id(i));
                    }

                    //we resurrect nodes that the coordinator thinks are alive
                    if (!deadInState && m_lastStatistics.IsDead(i) && m_lastStatistics.IsResurrecting(i))
                    {
                        lllog (4) << "SP: Elected coordinator thinks that node "
                                  << m_lastStatistics.Name(i).c_str()
                                  << " with id " << m_lastStatistics.Id(i)
                                  << " is alive, and I think it is resurrecting, so I'll resurrect it."
                                  << std::endl;
                        m_rawHandler.ResurrectNode(m_lastStatistics.Id(i));
                    }
                }


                if (m_stateChangedCallback != nullptr)
                {
                    m_stateChangedCallback(m_stateMessage);
                }
            });
        }


        void Stop()
        {
            m_electionHandler->Stop();
        }

    private:
        static std::set<int64_t> GetDeadNodes(const SystemStateMessage& state)
        {
            lllog(9) << "GetDeadNodes1:" << std::endl;
            std::set<int64_t> dead;
            for (int i = 0; i < state.node_info_size(); ++i)
            {
                if (state.node_info(i).is_dead())
                {
                    lllog(9) << " SystemStateMessage thinks " << state.node_info(i).id()
                             << " (" << state.node_info(i).name().c_str() <<") is dead" << std::endl;
                    dead.insert(state.node_info(i).id());
                }
            }
            return dead;
        }

        /** Get all node ids that any normal node thinks is dead, and noone thinks is resurrecting */
        static std::map<int64_t, std::pair<RawStatistics,int>> GetDeadNodes(const RawStatistics& statistics,
                                                                            const std::map<int64_t, NodeType>& nodeTypes,
                                                                            const int64_t ownId)
        {
            lllog(9) << "GetDeadNodes2:" << std::endl;
            std::map<int64_t, std::pair<RawStatistics,int>> deadNodes;
            std::set<int64_t> resurrecting;
            for (int i = 0; i < statistics.Size(); ++i)
            {
                if (statistics.IsDead(i))
                {
                    lllog(9) << " I think node " << statistics.Id(i)
                             << " (" << statistics.Name(i).c_str() <<") is dead" << std::endl;
                    deadNodes.insert(std::make_pair(statistics.Id(i), std::make_pair(statistics,i)));
                }
                if (statistics.IsResurrecting(i))
                {
                    lllog(9) << " I think node " << statistics.Id(i)
                             << " (" << statistics.Name(i).c_str() <<") is resurrecting" << std::endl;
                    resurrecting.insert(statistics.Id(i));
                }

                const bool isLightNode = nodeTypes.at(statistics.NodeTypeId(i)).isLightNode;

                //We don't trust lightnodes to know about deadness
                if (statistics.HasRemoteStatistics(i) && !isLightNode)
                {
                    const auto remote = statistics.RemoteStatistics(i);
                    for (int j = 0; j < remote.Size(); ++j)
                    {
                        //ignore what other nodes think of me
                        if (remote.Id(j) == ownId)
                        {
                            continue;
                        }

                        if (remote.IsDead(j))
                        {
                            lllog(9) << " Node " << remote.Id() << " (" << remote.Name().c_str() <<") thinks node "
                                     << remote.Id(j) << " (" << remote.Name(j).c_str() <<") is dead" << std::endl;
                            deadNodes.insert(std::make_pair(remote.Id(j), std::make_pair(remote,j)));
                        }
                        if (remote.IsResurrecting(j))
                        {
                            lllog(9) << " Node " << remote.Id() << " (" << remote.Name().c_str() <<") thinks node "
                                     << remote.Id(j) << " (" << remote.Name(j).c_str() <<") is resurrecting" << std::endl;
                            resurrecting.insert(remote.Id(j));
                        }

                    }
                }
            }
            for (const auto undead: resurrecting)
            {
                deadNodes.erase(undead);
            }
            return deadNodes;
        }



        //must be called in strand
        bool SystemStable() const
        {
            CheckStrand();

            //get all node ids that we've heard about so far
            std::set<int64_t> knownNodes;
            knownNodes.insert(m_id); //include ourselves...

            for (int i = 0; i < m_lastStatistics.Size(); ++i)
            {
                knownNodes.insert(m_lastStatistics.Id(i));
            }

            //we want to loop over all nodes that we know of and
            //check what nodes they know of.
            for (int i = 0; i < m_lastStatistics.Size(); ++i)
            {
                if (!m_lastStatistics.HasRemoteStatistics(i))
                {
                    continue;
                }

                const auto& remote = m_lastStatistics.RemoteStatistics(i);
                for (int j = 0; j < remote.Size(); ++j)
                {
                    //ignore nodes that the remote node believe are dead
                    if (remote.IsDead(j))
                    {
                        continue;
                    }

                    //ignore lightnodes if we're a lightnode
                    if (m_electionHandler->IsLightNode() &&
                        m_nodeTypes.at(remote.NodeTypeId(j)).isLightNode)
                    {
                        continue;
                    }

                    if (knownNodes.find(remote.Id(j)) == knownNodes.end())
                    {
                        //found a live node that we don't know about!
                        lllog(7) << "SP: Found node " << remote.Id(j) << "(" << remote.Name(j).c_str()
                                 << ") that we don't know about but which node " << remote.Id()
                                 << "(" << remote.Name().c_str() << ") believes is alive. " << std::endl;

                        return false;
                    }
                }
            }

            //everything seems kosher.
            return true;
        }

        bool CheckPrerequisites() const
        {
            CheckStrand();

            if (!m_lastStatisticsDirty)
            {
                lllog(9) << "SP: Last statistics is not dirty, no need to update." << std::endl;
                return true;
            }

            if (!m_lastStatistics.Valid())
            {
                lllog(9) << "SP: No valid raw data yet, not updating my state" << std::endl;
                return false;
            }

            if (m_lastStatistics.IncarnationId() == 0)
            {
                lllog(9) << "SP: We don't have an incarnation id yet, not updating my state" << std::endl;
                return false;
            }

            //Check that the raw data from all other nodes is from this election
            for (int i = 0; i < m_lastStatistics.Size(); ++i)
            {
                if (m_lastStatistics.IsDead(i))
                {
                    //that node is dead, we dont need any information from it.
                    continue;
                }

                if (!m_lastStatistics.HasRemoteStatistics(i))
                {
                    lllog(9) << "SP: No remote RAW data received from node "
                             << m_lastStatistics.Id(i) << ", not updating my state" << std::endl;
                    return false;
                }

                const auto& remote = m_lastStatistics.RemoteStatistics(i);
                if (remote.ElectionId() != m_ownElectionId)
                {
                    lllog(9) << "SP: Remote RAW data from node "
                             << m_lastStatistics.Id(i) << " has wrong election id ("
                             << remote.ElectionId() << "), not updating my state." << std::endl;
                    return false;
                }
            }

            //check that all nodes that are known by other nodes are also known by us.
            if (!SystemStable())
            {
                lllog(9) << "SP: System is not stable, not updating my state." << std::endl;
                return false;
            }

            return true;
        }

        bool ExcludeNodes(const std::map<int64_t, std::pair<RawStatistics,int>>& deadNodes) const
        {
            CheckStrand();

            //Exclude nodes that we think are alive but some other normal node thinks is
            //dead (the ExcludeNode will cause RawHandler to post another RawChangedEvent)
            //Note: resurrecting nodes are still marked as dead, so this will not exclude those.
            bool changes = false;
            for (int i = 0; i < m_lastStatistics.Size(); ++i)
            {
                if (!m_lastStatistics.IsDead(i) &&
                    deadNodes.find(m_lastStatistics.Id(i)) != deadNodes.end())
                {
                    lllog (4) << "SP: Someone thinks that node " << m_lastStatistics.Name(i).c_str()
                              << " with id " << m_lastStatistics.Id(i)
                              << " is dead, so I'll exclude him."
                              << std::endl;
                    if (m_resurrectingNodes.find(m_lastStatistics.Id(i)) != m_resurrectingNodes.end())
                    {
                        lllog(4) << "SP: No, actually, I won't. I am trying to resurrect that node."<< std::endl;
                        continue;
                    }
                    m_rawHandler.ExcludeNode(m_lastStatistics.Id(i));
                    changes = true;
                }
            }

            return changes;
        }

        bool ResurrectNodes()
        {
            CheckStrand();

            //Perform the first part of resurrection. The call to RawHandler will
            //make it so that we cannot pass CheckPrerequisites until we have received
            //a raw data from the new node. So nodes have to be marked as dead until
            //we pass CheckPrereqs, and then they can be marked as alive again. And until
            //then we store them in m_resurrectingNodes
            bool changes = false;
            for (int i = 0; i < m_lastStatistics.Size(); ++i)
            {
                if (m_lastStatistics.IsResurrecting(i))
                {
                    lllog (4) << "SP: Want to resurrect node " << m_lastStatistics.Name(i).c_str()
                              << " with id " << m_lastStatistics.Id(i)
                              << std::endl;
                    m_rawHandler.ResurrectNode(m_lastStatistics.Id(i));
                    m_resurrectingNodes.insert(std::make_pair(m_lastStatistics.Id(i), RESURRECT_COUNT));

                    changes = true;
                }
            }

            return changes;
        }

        //returns true if the state is okay to publish
        //must be called in strand
        bool UpdateMyState()
        {
            CheckStrand();

            //This function attempts to not flush the logger until the end of the function
            lllog(9) << "SP: Entering UpdateMyState\n";

            if (!m_electionHandler->IsElected())
            {
                lllog(9) << "SP: We're not elected, not updating my state." << std::endl;
                m_failedStateUpdates = 0;
                return false;
            }

            //Check a bunch of prerequisites that must be passed before we are allowed to
            //produce a system state.
            if (!CheckPrerequisites())
            {
                lllog(9) << std::flush;
                ++m_failedStateUpdates;

                if (m_failedStateUpdates > 60)
                {
                    m_failedStateUpdates = 0;
                    lllog(1) << "SP: Have failed at 60 state updates, forcing reelection." << std::endl;
                    m_electionHandler->ForceElection();
                }
                return false;
            }

            lllog(9) << "SP: Passed all checks, looking for dead nodes that "
                     << "I need to exclude before updating my state\n";

            //get all nodes that any normal node thinks is dead in a table
            //(we will be removing nodes from this table as we handle them below)
            auto deadNodes = GetDeadNodes(m_lastStatistics, m_nodeTypes, m_id);

            //Exclude nodes that we think are alive but someone else thinks are dead
            //(code in ExcludeNodes will cause RawHandler to post another RawChangedEvent,
            // so this function will be called again very soon, so we return out of
            // this function after any changes.)
            const bool nodesWereExcluded = ExcludeNodes(deadNodes);

            if (nodesWereExcluded)
            {
                lllog(9) << "SP: At least one node was marked as dead, returning\n"
                         << "SP: UpdateMyState will be called again very soon, "
                         << "which will generate a new state" << std::endl;
                return false;
            }

            const bool nodesWereResurrected = ResurrectNodes();

            if (nodesWereResurrected)
            {
                lllog(9) << "SP: At least one node was resurrected, returning\n"
                         << "SP: UpdateMyState will be called again, and once we have received a new "
                         << "remote statistics from the resurrected node we will be able to generate "
                         << "a new state" << std::endl;
                return false;
            }


            lllog(9) << "SP: Updating my state.\n";
            const bool logState = Safir::Utilities::Internal::Internal::LowLevelLogger::Instance().LogLevel() >= 9;
            if (logState)
            {
                lllog(9) << "SP: Last state:\n" << m_stateMessage << "\n";
            }

            //Note: This code will ignore the case where we for some reason have a RAW from another node
            //that says that we are dead. If that is the case it will stop sending data to us and
            //we will mark him as dead eventually.
            //He should also start a new election, and the thing should resolve itself.

            m_stateMessage.set_elected_id(m_id);
            m_stateMessage.set_election_id(m_ownElectionId);

            // if we're becoming detached we need to clear out all the nodes from state
            if (!IsDetached() && m_electionHandler->IsElectionDetached())
            {
                lllog(6) << "SP: Becoming detached, clearing node info from state" <<std::endl;
                m_stateMessage.clear_node_info();
                m_rawHandler.SetNodeIsDetached();
                m_stateMessage.set_is_detached(true);
                return false;
            }
            else if (IsDetached() && !m_electionHandler->IsElectionDetached())
            {
                lllog(6) << "SP: Becoming attached" <<std::endl;
                m_stateMessage.set_is_detached(false);
            }

            //Finish resurrecting any nodes in m_resurrectingNodes.
            //For us to get here all the resurrecting nodes must have sent a rawstatistics
            //(this is something Checkprereqs guarantees).
            for (auto& resurrectInfo: m_resurrectingNodes)
            {
                for (auto& node: *m_stateMessage.mutable_node_info())
                {
                    if (node.id() == resurrectInfo.first)
                    {
                        lllog(4) << "SP: Finished resurrecting node " << node.id() << std::endl;
                        if (resurrectInfo.second == RESURRECT_COUNT)
                        {
                            node.set_is_dead(false);
                        }
                        --resurrectInfo.second;
                    }
                }
            }

            //remove elements that have been counted down to zero
            erase_if(m_resurrectingNodes, [](const auto& elem){return elem.second <= 0;});

            lllog(9) << "SP: Looking at last state\n";

            //get lists of dead and alive nodes in the last state, for future use.
            //(remember that the last state might not have been produced by us)
            std::map<int64_t, SystemStateMessage_NodeInfo*> lastLiveNodes;
            std::set<int64_t> lastDeadNodes;
            for (int i = 0; i < m_stateMessage.node_info_size(); ++i)
            {
                if (m_stateMessage.node_info(i).is_dead())
                {
                    lllog(9) << "SP:   Dead node " << m_stateMessage.node_info(i).name().c_str()
                             << " (" << m_stateMessage.node_info(i).id() << ")\n";
                    const bool res = lastDeadNodes.insert(m_stateMessage.node_info(i).id()).second;
                    if (!res)
                    {
                        lllog(9) << std::flush;
                        throw std::logic_error("Duplicate dead node in last state! Not good at all!");
                    }

                    //check that it's not in live nodes! (This is just a sanity check, really)
                    if (lastLiveNodes.find(m_stateMessage.node_info(i).id()) != lastLiveNodes.end())
                    {
                        lllog(9) << std::flush;
                        throw std::logic_error("Dead node was already defined as alive in last state!");
                    }
                }
                else
                {
                    lllog(9) << "SP:   Live node " << m_stateMessage.node_info(i).name().c_str()
                             << " (" << m_stateMessage.node_info(i).id() << ")\n";
                    const bool res = lastLiveNodes.insert(std::make_pair(m_stateMessage.node_info(i).id(),
                                                                         m_stateMessage.mutable_node_info(i))).second;
                    if (!res)
                    {
                        lllog(9) << std::flush;
                        throw std::logic_error("Duplicate live node in last state! Not good at all!");
                    }

                    //check that it's not in dead nodes! (This is just a sanity check, really)
                    if (lastDeadNodes.find(m_stateMessage.node_info(i).id()) != lastDeadNodes.end() &&
                        m_resurrectingNodes.find(m_stateMessage.node_info(i).id()) != m_resurrectingNodes.end())
                    {
                        lllog(9) << std::flush;
                        throw std::logic_error("Live node was already defined as dead in last state!");
                    }
                }
            }

            if (lastDeadNodes.find(m_id) != lastDeadNodes.end())
            {
                lllog(9) << std::flush;
                throw std::logic_error("We're dead in the last state! Not good at all!");
            }

            //check that we're in the last state (and alive), and insert us if we're not
            if (lastLiveNodes.find(m_id) == lastLiveNodes.end())
            {
                lllog(9) << "SP: Adding own node to system state\n";

                //add myself
                auto node = m_stateMessage.add_node_info();
                node->set_name(m_name);
                node->set_id(m_id);
                node->set_node_type_id(m_nodeTypeId);
                node->set_control_address(m_controlAddress);
                node->set_data_address(m_dataAddress);
                node->set_is_dead(false);
            }


            {
                std::set<int64_t> died;
                //handle nodes that have died since last state

                for (auto lln = lastLiveNodes.cbegin(); lln != lastLiveNodes.cend(); ++lln)
                {
                    const auto findIt = deadNodes.find(lln->first);
                    if (findIt != deadNodes.end())
                    {
                        lln->second->set_is_dead(true);
                        deadNodes.erase(findIt);
                        died.insert(lln->first);
                        lllog(9) << "SP: Node " << lln->first << " has died since last state\n";
                    }
                }

                //remove the dead ones from lastLiveNodes
                for (auto d = died.cbegin(); d != died.cend(); ++d)
                {
                    lastLiveNodes.erase(*d);
                }
            }

            //handle nodes that were dead already
            for (auto ldn = lastDeadNodes.cbegin(); ldn != lastDeadNodes.cend(); ++ldn)
            {
                deadNodes.erase(*ldn);
            }

            //Note that nodes that died since last are now in neither lastLiveNodes or
            //lastDeadNodes, but that doesnt really matter, since we'll be using
            //deadNodes to add dead nodes to systemState at the end of this fcn.

            lllog(9) << "SP: Looking for new nodes that can be added to the system state\n";

            //we need a list of new nodes that are fully connected, that we can add to
            //the system state
            std::set<int64_t> newNodes;

            //first get all new live nodes that can see all the currently alive nodes
            for (int i = 0; i < m_lastStatistics.Size(); ++i)
            {
                //skip nodes that are dead or that are already part of the system state
                if (m_lastStatistics.IsDead(i) ||
                    lastLiveNodes.find(m_lastStatistics.Id(i)) != lastLiveNodes.end())
                {
                    continue;
                }
                const auto remote = m_lastStatistics.RemoteStatistics(i);

                auto lln = lastLiveNodes;
                for (int j = 0; j < remote.Size(); ++j)
                {
                    lln.erase(remote.Id(j));
                }

                //For light nodes we don't expect them to have seen other light nodes.
                if (m_nodeTypes.at(m_lastStatistics.NodeTypeId(i)).isLightNode)
                {
                    auto llnIt = lln.begin();
                    while(llnIt != lln.end())
                    {
                        if (m_nodeTypes.at(llnIt->second->node_type_id()).isLightNode)
                        {
                            llnIt = lln.erase(llnIt);
                        }
                        else
                        {
                            ++llnIt;
                        }
                    }
                }

                //if remote has seen all live normal nodes lln should be empty
                //(doesnt matter if they're dead, since that has already been handled)
                if (lln.empty())
                {
                    lllog(9) << "SP:   Found a candidate " << m_lastStatistics.Name(i).c_str()
                             << " (" << m_lastStatistics.Id(i) << ")\n";

                    newNodes.insert(m_lastStatistics.Id(i));
                }
            }

            //then remove any nodes that the currently alive nodes cant see
            for (int i = 0; i < m_lastStatistics.Size(); ++i)
            {
                //skip nodes that are dead or that are not part of the system state
                if (m_lastStatistics.IsDead(i) ||
                    lastLiveNodes.find(m_lastStatistics.Id(i)) == lastLiveNodes.end())
                {
                    continue;
                }

                //We don't care about what light nodes think of the world
                if (m_nodeTypes.at(m_lastStatistics.NodeTypeId(i)).isLightNode)
                {
                    continue;
                }

                const auto remote = m_lastStatistics.RemoteStatistics(i);

                auto seen = newNodes;

                for (int j = 0; j < remote.Size(); ++j)
                {
                    seen.erase(remote.Id(j));
                }

                //any nodes left over in "seen" are nodes that remote cannot see.
                //remove them from newNodes
                for (auto notSeen = seen.cbegin(); notSeen != seen.cend(); ++notSeen)
                {
                    lllog(9) << "SP:   Node " << m_lastStatistics.Id(i) << " cannot see node "
                             << *notSeen << ", so we cannot add it to system state yet\n";
                    newNodes.erase(*notSeen);
                }
            }

            //and check that the new nodes can see each other!
            for (int i = 0; i < m_lastStatistics.Size(); ++i)
            {
                //skip nodes that are dead or that are not new
                if (m_lastStatistics.IsDead(i) ||
                    newNodes.find(m_lastStatistics.Id(i)) == newNodes.end())
                {
                    continue;
                }

                //We don't care about what light nodes think of the world
                if (m_nodeTypes.at(m_lastStatistics.NodeTypeId(i)).isLightNode)
                {
                    continue;
                }

                const auto remote = m_lastStatistics.RemoteStatistics(i);

                auto seen = newNodes;

                seen.erase(m_lastStatistics.Id(i)); //remote can see itself...

                for (int j = 0; j < remote.Size(); ++j)
                {
                    seen.erase(remote.Id(j));
                }

                //any nodes left over in "seen" are nodes that remote cannot see.
                //remove them from newNodes
                for (auto notSeen = seen.cbegin(); notSeen != seen.cend(); ++notSeen)
                {
                    lllog(9) << "SP:   Node " << m_lastStatistics.Id(i) << " cannot see node "
                             << *notSeen << ", so we cannot add it to system state yet\n";
                    newNodes.erase(*notSeen);
                }
            }

            //then we can go through and add them
            for (int i = 0; i < m_lastStatistics.Size(); ++i)
            {
                //skip nodes that are dead or that are not new
                if (m_lastStatistics.IsDead(i) ||
                    newNodes.find(m_lastStatistics.Id(i)) == newNodes.end())
                {
                    continue;
                }

                lllog(9) << "SP: Adding new node " << m_lastStatistics.Name(i).c_str()
                         << " (" << m_lastStatistics.Id(i) << ")\n";

                auto node = m_stateMessage.add_node_info();
                node->set_name(m_lastStatistics.Name(i));
                node->set_id(m_lastStatistics.Id(i));
                node->set_node_type_id(m_lastStatistics.NodeTypeId(i));
                node->set_control_address(m_lastStatistics.ControlAddress(i));
                node->set_data_address(m_lastStatistics.DataAddress(i));
                node->set_is_dead(false);
            }

            if (!m_stateMessage.is_detached())
            {
                //and we add all nodes that remain in deadNodes
                for (auto dn = deadNodes.cbegin(); dn != deadNodes.cend(); ++dn)
                {
                    const auto& remote = dn->second.first;
                    const int index = dn->second.second;

                    lllog(9) << "SP: Adding dead node " << remote.Name(index).c_str()
                             << " (" << remote.Id(index) << ")\n";

                    auto node = m_stateMessage.add_node_info();
                    node->set_name(remote.Name(index));
                    node->set_id(remote.Id(index));
                    node->set_node_type_id(remote.NodeTypeId(index));
                    node->set_control_address(remote.ControlAddress(index));
                    node->set_data_address(remote.DataAddress(index));
                    node->set_is_dead(true);
                }

                //Last of all we have to add the nodes that are in more_dead_nodes that we
                //haven't already added
                std::set<int64_t> addedDeadNodes;
                for (int i = 0; i < m_stateMessage.node_info_size(); ++i)
                {
                    if (m_stateMessage.node_info(i).is_dead())
                    {
                        addedDeadNodes.insert(m_stateMessage.node_info(i).id());
                    }
                }

                for (int i = 0; i < m_lastStatistics.MoreDeadNodesSize(); ++i)
                {
                    if (addedDeadNodes.find(m_lastStatistics.MoreDeadNodes(i)) == addedDeadNodes.end())
                    {
                        lllog(9) << "SP: Adding dead node " << m_lastStatistics.MoreDeadNodes(i)
                                 << " from more_dead_nodes\n";
                        auto node = m_stateMessage.add_node_info();
                        node->set_name("<unknown>");
                        node->set_id(m_lastStatistics.MoreDeadNodes(i));
                        node->set_node_type_id(0);
                        node->set_control_address("");
                        node->set_data_address("");
                        node->set_is_dead(true);
                    }
                }
            }
            if (m_stateMessage.is_detached() && m_stateMessage.node_info_size() != 1)
            {
                SEND_SYSTEM_LOG(Alert, << "Internal error: A detached SystemState message should only have one node in it.");
                lllog(1) << "SP: A detached SystemState message should only have one node in it:\n"
                         << m_stateMessage << std::endl;
                throw std::logic_error("A detached SystemState message should only have one node in it");
            }

            lllog(9) << "SP: A new SystemState has been produced\n";
            if (logState)
            {
                lllog(9) << "SP: New state:\n" << m_stateMessage << "\n";
            }

            if (m_stateChangedCallback != nullptr)
            {
                m_stateChangedCallback(m_stateMessage);
            }

            m_lastStatisticsDirty = false;

            lllog(9) << std::flush;
            return true;
        }

        void CheckStrand() const
        {
#if (!defined NDEBUG && !defined SAFIR_DISABLE_CHECK_STRAND)
            if (!m_strand.running_in_this_thread())
            {
                throw std::logic_error("Function must be called from the strand!");
            }
#endif
        }


        boost::asio::io_service::strand& m_strand;
        CommunicationT& m_communication;

        std::unique_ptr<ElectionHandlerT> m_electionHandler;

        RawStatistics m_lastStatistics;
        bool m_lastStatisticsDirty;

        std::function<void(const SystemStateMessage& data)> m_stateChangedCallback;

        SystemStateMessage m_stateMessage;
        std::atomic<bool> m_isDetached;
        std::map<int64_t,int> m_resurrectingNodes; //second is how many more raws need to be received before resurrect is complete
        const std::string m_name;
        const int64_t m_id;
        const int64_t m_nodeTypeId;
        const std::map<int64_t, NodeType> m_nodeTypes;
        const std::string m_controlAddress;
        const std::string m_dataAddress;
        int64_t m_ownElectionId;
        RawHandlerT& m_rawHandler;

        int m_failedStateUpdates;
    };

    //forward declarations
    template <class T>
    class RawHandlerBasic;
    typedef RawHandlerBasic<Com::Communication> RawHandler;

    class RawStatistics;
    class SystemState;


    typedef CoordinatorBasic<Com::Communication, RawHandler, ElectionHandler> Coordinator;
}
}
}
}
