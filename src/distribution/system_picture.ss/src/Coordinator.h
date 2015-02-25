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

#include <Safir/Dob/Internal/RawStatistics.h>
#include <Safir/Dob/Internal/SystemPictureDefs.h>
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <functional>
#include <limits>
#include <atomic>
#include <map>
#include <set>
#include "ElectionHandler.h"
#include "MessagePrinting.h"
#include "RawChanges.h"

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4244)
#  pragma warning (disable: 4127)
#  pragma warning (disable: 4267)
#endif

#include "SystemStateMessage.pb.h"
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
        CoordinatorBasic(boost::asio::io_service& ioService,
                         CommunicationT& communication,
                         const std::string& name,
                         const int64_t id,
                         const int64_t nodeTypeId,
                         const std::string& controlAddress,
                         const std::string& dataAddress,
                         const std::map<int64_t, NodeType>& nodeTypes,
                         const char* const receiverId,
                         RawHandlerT& rawHandler)
            : m_strand (ioService)
            , m_communication(communication)
            , m_electionHandler(ioService,
                                communication,
                                id,
                                nodeTypes,
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
                                })
            , m_name(name)
            , m_id(id)
            , m_nodeTypeId(nodeTypeId)
            , m_controlAddress(controlAddress)
            , m_dataAddress(dataAddress)
            , m_ownElectionId(0)
            , m_rawHandler(rawHandler)
        {
            m_stateMessage.set_elected_id(0); //our state message is not valid until we have a real id set.

            rawHandler.AddRawChangedCallback(m_strand.wrap([this](const RawStatistics& statistics,
                                                                  const RawChanges flags,
                                                                  boost::shared_ptr<void> completionSignaller)
            {
                lllog(8) << "SP: Coordinator got new raw data (" << flags << ")" << std::endl;

                m_lastStatistics = statistics;
                m_lastStatisticsDirty = true;

                if (m_electionHandler.IsElected())
                {
                    UpdateMyState();
                }

                if (flags.NodesChanged())
                {
                    m_electionHandler.NodesChanged(std::move(statistics), completionSignaller);
                }
            }));
        }

        void SetStateChangedCallback(const std::function<void(const SystemStateMessage& data)>& callback)
        {
            m_strand.dispatch([this,callback]
                              {
                                  m_stateChangedCallback = callback;
                              });
        }

        //used to send state message
        //extraSpace adds bytes at the end of the buffer, e.g. for adding a crc
        //if onlyOwnState is true the callback will only be called if we're elected
        //and have a valid own system state that is ok to send.
        void PerformOnStateMessage(const std::function<void(std::unique_ptr<char []> data,
                                                            const size_t size)> & fn,
                                   const size_t extraSpace,
                                   const bool onlyOwnState)
        {
            m_strand.dispatch([this,fn,extraSpace,onlyOwnState]
            {
                if (onlyOwnState)
                {
                    const bool okToSend = UpdateMyState();

                    if (!okToSend)
                    {
                        lllog(8) << " - Not sending: System State not ok to send!" << std::endl;
                        return;
                    }
                }

                if (m_stateMessage.elected_id() == 0)
                {
                    lllog(8) << " - Not sending: System State contains no elected node!" << std::endl;
                    return;
                }

                lllog(8) << " - Sending!" << std::endl;
                const size_t size = m_stateMessage.ByteSize() + extraSpace;
                auto data = std::unique_ptr<char[]>(new char[size]);
                m_stateMessage.SerializeWithCachedSizesToArray
                    (reinterpret_cast<google::protobuf::uint8*>(data.get()));
                fn(std::move(data), size);
            });
        }

        //new incoming system state from elected coordinator
        void NewRemoteStatistics(const int64_t from,
                                 const boost::shared_ptr<char[]>& data,
                                 const size_t size)
        {
            m_strand.dispatch([this,from,data,size]
            {
                if (!m_electionHandler.IsElected(from))
                {
                    SEND_SYSTEM_LOG(Informational,
                                    << "SystemPicture (in node " << m_id
                                    << ") got a new system state (from node "
                                    << from << ") from a node that is not elected. Discarding.");
                    return;
                }

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

                const auto deadNodes = GetDeadNodes(m_stateMessage);

                //Note: never do exclude on a node that is not one that we have
                //received NewNode for, i.e. is in m_lastStatistics top level.
                for (int i = 0; i < m_lastStatistics.Size(); ++i)
                {
                    //if we haven't marked the node as dead and electee doesnt think the node
                    //is part of the system we want to exclude the node
                    if (!m_lastStatistics.IsDead(i) &&
                        deadNodes.find(m_lastStatistics.Id(i)) != deadNodes.end())
                    {
                        lllog (4) << "SP: Elected coordinator thinks that node "
                                  << m_lastStatistics.Name(i).c_str()
                                  << " with id " << m_lastStatistics.Id(i)
                                  << " is dead, so I'll mark him as dead." << std::endl;

                        m_communication.ExcludeNode(m_lastStatistics.Id(i));
                        m_rawHandler.SetDeadNode(m_lastStatistics.Id(i));
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
            m_electionHandler.Stop();
        }

    private:
        static std::set<int64_t> GetDeadNodes(const SystemStateMessage& state)
        {
            std::set<int64_t> nodes;
            for (int i = 0; i < state.node_info_size(); ++i)
            {
                if (state.node_info(i).is_dead())
                {
                    nodes.insert(state.node_info(i).id());
                }
            }
            return nodes;
        }

        /** Get all node ids that any node thinks is dead */
        static std::map<int64_t, std::pair<RawStatistics,int>> GetDeadNodes(const RawStatistics& statistics,
                                                                            const int64_t ownId)
        {
            std::map<int64_t, std::pair<RawStatistics,int>> deadNodes;
            for (int i = 0; i < statistics.Size(); ++i)
            {
                if (statistics.IsDead(i))
                {
                    deadNodes.insert(std::make_pair(statistics.Id(i), std::make_pair(statistics,i)));
                }

                if (statistics.HasRemoteStatistics(i))
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
                            deadNodes.insert(std::make_pair(remote.Id(j), std::make_pair(remote,j)));
                        }
                    }
                }
            }
            return deadNodes;
        }



        //must be called in strand
        bool SystemStable() const
        {
            //get all node ids that we've heard about so far
            std::set<int64_t> knownNodes({m_id}); //include ourselves...
            for (int i = 0; i < m_lastStatistics.Size(); ++i)
            {
                knownNodes.insert(m_lastStatistics.Id(i));
            }

            //we want to loop over all nodes that we know of and
            //check what nodes they know of.
            for (int i = 0; i < m_lastStatistics.Size(); ++i)
            {
                const auto& remote = m_lastStatistics.RemoteStatistics(i);
                for (int j = 0; j < remote.Size(); ++j)
                {
                    //ignore nodes that the remote node believe are dead
                    if (remote.IsDead(j))
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
            if (!m_electionHandler.IsElected())
            {
                return false;
            }

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
            //Exclude nodes that we think are alive but someone else thinks are dead
            //(the SetDeadNode will cause RawHandler to post another RawChangedEvent)
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
                    m_communication.ExcludeNode(m_lastStatistics.Id(i));
                    m_rawHandler.SetDeadNode(m_lastStatistics.Id(i));
                    changes = true;
                }
            }

            return changes;
        }

        //returns true if the state is okay to publish
        //must be called in strand
        bool UpdateMyState()
        {
            //This function attempts to not flush the logger until the end of the function
            lllog(9) << "SP: Entering UpdateMyState\n";

            //Check a bunch of prerequisites that must be passed before we are allowed to
            //produce a system state.
            if (!CheckPrerequisites())
            {
                lllog(9) << std::flush;
                return false;
            }

            lllog(9) << "SP: Passed all checks, looking for dead nodes that "
                     << "I need to exclude before updating my state\n";

            //get all nodes that anyone thinks are dead in a table
            //(we will be removing nodes from this table as we handle them below)
            auto deadNodes = GetDeadNodes(m_lastStatistics, m_id);

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
                    if (lastDeadNodes.find(m_stateMessage.node_info(i).id()) != lastDeadNodes.end())
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
                for (const auto& lln : lastLiveNodes)
                {
                    const auto findIt = deadNodes.find(lln.first);
                    if (findIt != deadNodes.end())
                    {
                        lln.second->set_is_dead(true);
                        deadNodes.erase(findIt);
                        died.insert(lln.first);
                        lllog(9) << "SP: Node " << lln.first << " has died since last state\n";
                    }
                }

                //remove the dead ones from lastLiveNodes
                for (const auto d: died)
                {
                    lastLiveNodes.erase(d);
                }
            }

            //handle nodes that were dead already
            for (const auto ldn : lastDeadNodes)
            {
                deadNodes.erase(ldn);
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

                //if remote has seen all live nodes lln should be empty
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

                const auto remote = m_lastStatistics.RemoteStatistics(i);

                auto seen = newNodes;

                for (int j = 0; j < remote.Size(); ++j)
                {
                    seen.erase(remote.Id(j));
                }

                //any nodes left over in "seen" are nodes that remote cannot see.
                //remove them from newNodes
                for (const auto notSeen : seen)
                {
                    lllog(9) << "SP:   Node " << m_lastStatistics.Id(i) << " cannot see node "
                             << notSeen << ", so we cannot add it to system state yet\n";

                    newNodes.erase(notSeen);
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

                const auto remote = m_lastStatistics.RemoteStatistics(i);

                auto seen = newNodes;

                seen.erase(m_lastStatistics.Id(i)); //remote can see itself...

                for (int j = 0; j < remote.Size(); ++j)
                {
                    seen.erase(remote.Id(j));
                }

                //any nodes left over in "seen" are nodes that remote cannot see.
                //remove them from newNodes
                for (const auto notSeen : seen)
                {
                    lllog(9) << "SP:   Node " << m_lastStatistics.Id(i) << " cannot see node "
                             << notSeen << ", so we cannot add it to system state yet\n";

                    newNodes.erase(notSeen);
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


            //and we add all nodes that remain in deadNodes
            for (const auto& dn : deadNodes)
            {
                const auto& remote = dn.second.first;
                const int index = dn.second.second;

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

            lllog(9) << "SP: A new SystemState has been produced\n";
            if (logState)
            {
                lllog(9) << "SP: New state:\n" << m_stateMessage;
            }

            if (m_stateChangedCallback != nullptr)
            {
                m_stateChangedCallback(m_stateMessage);
            }

            m_lastStatisticsDirty = false;

            lllog(9) << std::flush;
            return true;
        }


        mutable boost::asio::strand m_strand;
        CommunicationT& m_communication;

        ElectionHandlerT m_electionHandler;

        RawStatistics m_lastStatistics;
        bool m_lastStatisticsDirty = true;

        std::function<void(const SystemStateMessage& data)> m_stateChangedCallback;

        SystemStateMessage m_stateMessage;

        const std::string m_name;
        const int64_t m_id;
        const int64_t m_nodeTypeId;
        const std::string m_controlAddress;
        const std::string m_dataAddress;
        int64_t m_ownElectionId;
        RawHandlerT& m_rawHandler;
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
