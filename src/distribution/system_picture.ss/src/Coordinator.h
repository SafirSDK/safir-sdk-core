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
                lllog(9) << "SP: Coordinator got new raw data (" << flags << ")" << std::endl;

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
                        return;
                    }
                }

                if (m_stateMessage.elected_id() == 0)
                {
                    return;
                }

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


        //returns true if the state is okay to publish
        //must be called in strand
        bool UpdateMyState()
        {
            if (!m_electionHandler.IsElected())
            {
                return false;
            }

            if (!m_lastStatisticsDirty)
            {
                lllog(7) << "SP: Last statistics is not dirty, no need to update." << std::endl;
                return true;
            }

            if (!m_lastStatistics.Valid())
            {
                lllog(7) << "SP: No valid raw data yet, not updating my state" << std::endl;
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
                    lllog(7) << "SP: No remote RAW data received from node "
                             << m_lastStatistics.Id(i) << ", not updating my state" << std::endl;
                    return false;
                }

                const auto& remote = m_lastStatistics.RemoteStatistics(i);
                if (remote.ElectionId() != m_ownElectionId)
                {
                    lllog(7) << "SP: Remote RAW data from node "
                             << m_lastStatistics.Id(i) << " has wrong election id ("
                             << remote.ElectionId() << "), not updating my state." << std::endl;
                    return false;
                }
            }

            //check that all nodes that are known by other nodes are also known by us.
            if (!SystemStable())
            {
                lllog(7) << "SP: System is not stable, not updating my state." << std::endl;
                return false;
            }

            lllog(7) << "SP: Passed all checks, updating my state." << std::endl;

            m_stateMessage.set_elected_id(m_id);
            m_stateMessage.set_election_id(m_ownElectionId);

            m_stateMessage.clear_node_info(); //don't care about efficiency...

            //add myself
            auto node = m_stateMessage.add_node_info();
            node->set_name(m_name);
            node->set_id(m_id);
            node->set_node_type_id(m_nodeTypeId);
            node->set_control_address(m_controlAddress);
            node->set_data_address(m_dataAddress);
            node->set_is_dead(false);

            //This code will ignore the case where we for some reason have a RAW from another node
            //that says that we are dead. If that is the case it will stop sending data to us and
            //we will mark him as dead eventually.
            //He should also start a new election, and the thing should resolve itself.


            //get all nodes that anyone thinks are dead in a table
            const auto deadNodes = GetDeadNodes(m_lastStatistics, m_id);

            //this set will be filled with all live nodes that should be part of the system state
            std::set<int64_t> ssNodes;

            //Put all nodes that we know about in ssNodes, and
            //exclude nodes that other nodes consider dead
            for (int i = 0; i < m_lastStatistics.Size(); ++i)
            {
                bool justKilled = false;
                //if we dont think them dead, but someone else does
                if (!m_lastStatistics.IsDead(i) &&
                    deadNodes.find(m_lastStatistics.Id(i)) != deadNodes.end())
                {
                    lllog (4) << "SP: Someone thinks that node " << m_lastStatistics.Name(i).c_str()
                              << " with id " << m_lastStatistics.Id(i)
                              << " is dead, so I'll mark him as dead and spread the word in the SystemState."
                              << std::endl;
                    m_communication.ExcludeNode(m_lastStatistics.Id(i));
                    m_rawHandler.SetDeadNode(m_lastStatistics.Id(i));
                    justKilled = true;
                }

                //insert nodes that are alive
                if(!m_lastStatistics.IsDead(i) && !justKilled)
                {
                    ssNodes.insert(m_lastStatistics.Id(i));
                }
            }

            //now we need to remove nodes that are not fully connected to all other nodes from ssNodes
            for (int i = 0; i < m_lastStatistics.Size(); ++i)
            {
                if (m_lastStatistics.IsDead(i))
                {
                    continue;
                }

                if (m_lastStatistics.HasRemoteStatistics(i))
                {
                    const auto remote = m_lastStatistics.RemoteStatistics(i);

                    //get a set of all nodes that we know of, but that the remote does not know about
                    auto unknown = ssNodes;
                    for (int j = 0; j < remote.Size(); ++j)
                    {
                        unknown.erase(remote.Id(j));
                    }

                    //and we know about ourselves, obviously
                    unknown.erase(remote.Id());

                    //remove them from ssNodes
                    for (const auto id : unknown)
                    {
                        ssNodes.erase(id);
                    }
                }
            }

            //Ok, now ssNodes should contain all nodes that we can see and that everyone else can see, and nothing else.
            //Add all those nodes to the system state
            for (int i = 0; i < m_lastStatistics.Size(); ++i)
            {
                if (ssNodes.find(m_lastStatistics.Id(i)) == ssNodes.end())
                {
                    continue;
                }

                auto node = m_stateMessage.add_node_info();
                node->set_name(m_lastStatistics.Name(i));
                node->set_id(m_lastStatistics.Id(i));
                node->set_node_type_id(m_lastStatistics.NodeTypeId(i));
                node->set_control_address(m_lastStatistics.ControlAddress(i));
                node->set_data_address(m_lastStatistics.DataAddress(i));
                node->set_is_dead(false);
            }

            //ok, time to add all dead nodes
            for (const auto& dn : deadNodes)
            {
                const auto& remote = dn.second.first;
                const int index = dn.second.second;

                lllog(8) << "SP: Adding dead node " << remote.Name(index).c_str()
                         << " (" << remote.Id(index) << ")" << std::endl;

                auto node = m_stateMessage.add_node_info();
                node->set_name(remote.Name(index));
                node->set_id(remote.Id(index));
                node->set_node_type_id(remote.NodeTypeId(index));
                node->set_control_address(remote.ControlAddress(index));
                node->set_data_address(remote.DataAddress(index));
                node->set_is_dead(true);
            }

            if (m_stateChangedCallback != nullptr)
            {
                m_stateChangedCallback(m_stateMessage);
            }

            m_lastStatisticsDirty = false;
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
