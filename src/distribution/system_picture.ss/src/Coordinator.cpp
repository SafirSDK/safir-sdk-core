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
#include "Coordinator.h"
#include "MessageWrapperCreators.h"
#include "RawHandler.h"
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Dob/Internal/RawStatistics.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/Id.h>
#include <boost/make_shared.hpp>
#include <set>

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4244)
#  pragma warning (disable: 4127)
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
    namespace 
    {
        std::set<int64_t> GetDeadNodeIds(const SystemStateMessage& state)
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


        std::set<int64_t> GetDeadNodes(const RawStatistics& statistics)
        {
            std::set<int64_t> deadNodes;
            for (int i = 0; i < statistics.Size(); ++i)
            {
                if (statistics.HasRemoteStatistics(i))
                {
                    const auto remote = statistics.RemoteStatistics(i);
                    for (int j = 0; j < remote.Size(); ++j)
                    {
                        if (remote.IsDead(j))
                        {
                            deadNodes.insert(remote.Id(j));
                        }
                    }
                }
            }
            return deadNodes;
        }
    }

    Coordinator::Coordinator(boost::asio::io_service& ioService,
                             Com::Communication& communication,
                             const std::string& name,
                             const int64_t id,
                             const int64_t nodeTypeId,
                             const std::string& controlAddress,
                             const std::string& dataAddress,
                             const std::map<int64_t, NodeType>& nodeTypes,
                             const char* const dataIdentifier,
                             RawHandler& rawHandler)
        : m_strand (ioService)
        , m_communication(communication)
        , m_electionHandler(ioService,
                            communication,
                            id,
                            nodeTypes,
                            dataIdentifier,
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

        rawHandler.AddNodesChangedCallback(m_strand.wrap([this](const RawStatistics& statistics)
        {
            lllog(9) << "SP: Coordinator got new nodes change callback" << std::endl;
            m_lastStatistics = statistics;
            if (m_electionHandler.IsElected())
            {
                UpdateMyState();
            }
            m_electionHandler.NodesChanged(std::move(statistics));
        }));

        rawHandler.AddRawChangedCallback(m_strand.wrap([this](const RawStatistics& statistics)
        {
            lllog(9) << "SP: Coordinator got new raw data" << std::endl;
            m_lastStatistics = statistics;
            if (m_electionHandler.IsElected())
            {
                UpdateMyState();
            }
        }));
        
    }
    

    void Coordinator::Stop()
    {
        m_electionHandler.Stop();
    }

    //must be called in strand
    bool Coordinator::UpdateMyState()
    {
        if (!m_electionHandler.IsElected())
        {
            return false;
        }
        
        if (!m_lastStatistics.Valid())
        {
            lllog(7) << "SP: No valid raw data yet, not updating my state" << std::endl;
            return false;
        }

        //Check that all other nodes raw data is from this election
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

            const auto remote = m_lastStatistics.RemoteStatistics(i);
            if (remote.ElectionId() != m_ownElectionId)
            {
                lllog(7) << "SP: Remote RAW data from node "
                         << m_lastStatistics.Id(i) << " has wrong election id (" << remote.ElectionId() << "), not updating my state." << std::endl;
                return false;
            }
        }

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

        //get all nodes that someone considers to be dead
        const std::set<int64_t> deadNodes = GetDeadNodes(m_lastStatistics);

        //tell rawhandler about nodes that other nodes consider dead
        //while we also build our new system state message
        //Note: never do exclude on a node that is not one that we have 
        //received NewNode for, i.e. is in m_lastStatistics top level.
        for (int i = 0; i < m_lastStatistics.Size(); ++i)
        {
            bool justKilled = false;
            //if we dont think them dead, but someone else does
            if (!m_lastStatistics.IsDead(i) &&
                deadNodes.find(m_lastStatistics.Id(i)) != deadNodes.end())
            {
                lllog (4) << "Someone thinks that node " << m_lastStatistics.Name(i).c_str()
                          << " with id " << m_lastStatistics.Id(i) 
                          << " is dead, so I'll mark him as dead and spread the word in the SystemState." << std::endl;
                m_communication.ExcludeNode(m_lastStatistics.Id(i));
                m_rawHandler.SetDeadNode(m_lastStatistics.Id(i));
                justKilled = true;
            }

            //add nodes that are not dead or were just killed or 
            //that have been dead a short while
            if (!m_lastStatistics.IsDead(i) ||
                justKilled ||
                (m_lastStatistics.IsDead(i) && !m_lastStatistics.IsLongGone(i)))
            {
                auto node = m_stateMessage.add_node_info();
                node->set_name(m_lastStatistics.Name(i));
                node->set_id(m_lastStatistics.Id(i));
                node->set_node_type_id(m_lastStatistics.NodeTypeId(i));
                node->set_control_address(m_lastStatistics.ControlAddress(i));
                node->set_data_address(m_lastStatistics.DataAddress(i));

                node->set_is_dead(justKilled || m_lastStatistics.IsDead(i));
            }
        }

        return true;
    }
    
    void Coordinator::PerformOnStateMessage(const std::function<void(std::unique_ptr<char []> data, 
                                                                     const size_t size)> & fn,
                                            const size_t extraSpace,
                                            const bool onlyOwnState)
    {
        m_strand.dispatch([this,fn,extraSpace,onlyOwnState]
                          {
                              if (onlyOwnState)
                              {
                                  if (!m_electionHandler.IsElected())
                                  {
                                      return;
                                  }
                                  
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

    //must be called in strand
    void Coordinator::NewRemoteData(const int64_t from, 
                                    const boost::shared_ptr<char[]>& data, 
                                    const size_t size)
    {
        m_strand.dispatch([this,from,data,size]
        {
            if (m_electionHandler.IsElected(from))
            {
                SEND_SYSTEM_LOG(Informational, << "SystemPicture (in node " << m_id << ") got a new system state (from node "
                                << from << ") from a node that is not elected. Discarding.");
            }
            else
            {
                m_stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                
                //TODO: should we check election_id?
                
                const auto deadNodes = GetDeadNodeIds(m_stateMessage);
                
                //Note: never do exclude on a node that is not one that we have 
                //received NewNode for, i.e. is in m_lastStatistics top level.
                for (int i = 0; i < m_lastStatistics.Size(); ++i)
                {
                    //if we haven't marked the node as dead and electee doesnt think the node
                    //is part of the system we want to exclude the node
                    if (!m_lastStatistics.IsDead(i) && deadNodes.find(m_lastStatistics.Id(i)) != deadNodes.end())
                    {
                        lllog (4) << "Elected coordinator thinks that node " << m_lastStatistics.Name(i).c_str()
                                  << " with id " << m_lastStatistics.Id(i) 
                                  << " is dead, so I'll mark him as dead." << std::endl;

                        m_communication.ExcludeNode(m_lastStatistics.Id(i));
                        m_rawHandler.SetDeadNode(m_lastStatistics.Id(i));
                    }
                }
            }
        });
    }





}
}
}
}

