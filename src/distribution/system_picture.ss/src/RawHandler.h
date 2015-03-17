/******************************************************************************
*
* Copyright Saab AB, 2012 (http://safir.sourceforge.net)
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

#include <Safir/Utilities/Internal/Atomic.h>
#include <Safir/Utilities/Internal/AsioPeriodicTimer.h>
#include <Safir/Dob/Internal/SystemPictureDefs.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include "MessageWrapperCreators.h"
#include "RawChanges.h"
#include "AsioLatencyMonitor.h"
#include <boost/make_shared.hpp>
#include <boost/chrono.hpp>
#include <boost/noncopyable.hpp>
#include <string>
#include <unordered_map>
#include <functional>
#include <set>


#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4244)
#  pragma warning (disable: 4127)
#  pragma warning (disable: 4267)
#endif

#include "RawStatisticsMessage.pb.h"
#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
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
    //forward declaration
    class RawStatistics;

    typedef std::function<void(const RawStatistics& statistics,
                               const RawChanges& flags,
                               boost::shared_ptr<void> completionSignaller)> StatisticsCallback;

    template<class CommunicationT>
    class RawHandlerBasic
        : private boost::noncopyable
    {
    public:
        RawHandlerBasic(boost::asio::io_service& ioService,
                        CommunicationT& communication,
                        const std::string& name,
                        const int64_t id,
                        const int64_t nodeTypeId,
                        const std::string& controlAddress,
                        const std::string& dataAddress,
                        const std::map<int64_t, NodeType>& nodeTypes,
                        const bool master,
                        const std::function<bool (const int64_t incarnationId)>& validateIncarnationIdCallback)
            : m_ioService(ioService)
            , m_communication(communication)
            , m_id(id)
            , m_nodeTypes(nodeTypes)
            , m_strand(ioService)
            , m_latencyMonitor("SpRawHandler",m_strand)
            , m_checkDeadNodesTimer(ioService,
                                    CalculateDeadCheckPeriod(nodeTypes),
                                    m_strand.wrap([this](const boost::system::error_code& error)
                                    {
                                        if (m_stopped)
                                        {
                                            return;
                                        }

                                        if (error)
                                        {
                                            SEND_SYSTEM_LOG(Alert,
                                                            << "Unexpected error in CheckDeadNodes: " << error);
                                            throw std::logic_error("Unexpected error in CheckDeadNodes");
                                        }

                                        CheckDeadNodes();
                                    }))
            , m_master(master)
            , m_validateIncarnationIdCallback(validateIncarnationIdCallback)
            , m_stopped(false)
        {
            //set up some info about ourselves in our message
            m_allStatisticsMessage.set_name(name);
            m_allStatisticsMessage.set_id(id);
            m_allStatisticsMessage.set_node_type_id(nodeTypeId);
            m_allStatisticsMessage.set_control_address(controlAddress);
            m_allStatisticsMessage.set_data_address(dataAddress);

            communication.SetNewNodeCallback(m_strand.wrap([this](const std::string& name,
                                                                  const int64_t id,
                                                                  const int64_t nodeTypeId,
                                                                  const std::string& controlAddress,
                                                                  const std::string& dataAddress)
                                                           {
                                                               NewNode(name,
                                                                       id,
                                                                       nodeTypeId,
                                                                       controlAddress,
                                                                       dataAddress);
                                                           }));

            communication.SetGotReceiveFromCallback(m_strand.wrap([this](int64_t id)
                                                                  {
                                                                      GotReceive(id);
                                                                  }));

            communication.SetRetransmitToCallback(m_strand.wrap([this](int64_t id)
                                                                {
                                                                    Retransmit(id);
                                                                }));
            m_checkDeadNodesTimer.Start();
        }


        void Stop()
        {
            const bool was_stopped = m_stopped.exchange(true);
            if (!was_stopped)
            {
                m_latencyMonitor.Stop();

                m_strand.dispatch([this]
                                  {
                                      m_checkDeadNodesTimer.Stop();
                                  });
            }
        }


        void PerformOnMyStatisticsMessage(const std::function<void(std::unique_ptr<char[]> data,
                                                                   const size_t size)> & fn) const
        {
            m_strand.dispatch([this,fn]
            {
                //With newer protobuf (>= 2.5.0) we can be clever
                //we just get the remote statistics out of the way before serializing a
                //a "my statistics" message, and afterwards we put them back
                std::vector<RawStatisticsMessage*> remotes;

                for (int i = 0; i < m_allStatisticsMessage.node_info_size(); ++i)
                {
                    //release_remote_statistics appears to allocate an object if it is null,
                    //which we don't want to do. So we manually push a nullptr in those cases
                    if (m_allStatisticsMessage.node_info(i).has_remote_statistics())
                    {
                        remotes.push_back(m_allStatisticsMessage.mutable_node_info(i)->
                                          release_remote_statistics());
                    }
                    else
                    {
                        remotes.push_back(nullptr);
                    }
                }

                const size_t size = m_allStatisticsMessage.ByteSize();

                auto data = std::unique_ptr<char[]>(new char[size]);

                m_allStatisticsMessage.SerializeWithCachedSizesToArray
                    (reinterpret_cast<google::protobuf::uint8*>(data.get()));

                for (size_t i = 0; i < remotes.size(); ++i)
                {
                    m_allStatisticsMessage.mutable_node_info(static_cast<int>(i))->
                        set_allocated_remote_statistics(remotes[i]);
                }

                //call the function object with the produced data.
                fn (std::move(data), size);
            });

        }


        void PerformOnAllStatisticsMessage(const std::function<void(std::unique_ptr<char []> data,
                                                                    const size_t size)> & fn) const
        {
            m_strand.dispatch([this,fn]
                              {
                                  const size_t size = m_allStatisticsMessage.ByteSize();
                                  auto data = std::unique_ptr<char[]>(new char[size]);
                                  m_allStatisticsMessage.SerializeWithCachedSizesToArray
                                      (reinterpret_cast<google::protobuf::uint8*>(data.get()));
                                  fn(std::move(data), size);
                              });
        }

        void NewRemoteStatistics(const int64_t from, const boost::shared_ptr<const char[]>& data, const size_t size)
        {
            m_strand.dispatch([this,from,data,size]
            {
                lllog(9) << "SP: NewRemoteStatistics for node " << from << std::endl;
                auto findIt = m_nodeTable.find(from);

                if (findIt == m_nodeTable.end())
                {
                    throw std::logic_error("NewRemoteStatistics from unknown node");
                }
                NodeInfo& node = findIt->second; //alias the iterator

                if (node.nodeInfo->is_dead())
                {
                    lllog(8) << "SP: NewRemoteStatistics from dead node, ignoring." << std::endl;
                    return;
                }

                const bool parseResult = node.nodeInfo->mutable_remote_statistics()->
                    ParseFromArray(data.get(),static_cast<int>(size));

                if (!parseResult)
                {
                    SEND_SYSTEM_LOG(Error,
                                    << "Failed to parse remote data from "
                                    << node.nodeInfo->name().c_str()
                                    << " (id = " << from << ", size = " << size << ")");
                    throw std::logic_error("Failed to parse remote data!");
                }

                lllog(9) << "SP: Processing remote node information" << std::endl;

                int changes = 0;

                if (!m_allStatisticsMessage.has_incarnation_id())
                {
                    lllog(6) << "SP: This node does not have an incarnation id" << std::endl;
                    if (node.nodeInfo->remote_statistics().has_incarnation_id())
                    {
                        const bool join = m_validateIncarnationIdCallback != nullptr ||
                            m_validateIncarnationIdCallback(node.nodeInfo->remote_statistics().incarnation_id());

                        if (join)
                        {
                            lllog(1) << "SP: Remote RAW contains incarnation id "
                                     << node.nodeInfo->remote_statistics().incarnation_id()
                                     << " and validation passed, let's use it!" << std::endl;
                            m_allStatisticsMessage.set_incarnation_id(node.nodeInfo->remote_statistics().incarnation_id());

                            changes |= RawChanges::METADATA_CHANGED;
                        }
                        else
                        {
                            lllog(1) << "SP: Remote RAW contains incarnation id "
                                     << node.nodeInfo->remote_statistics().incarnation_id()
                                     << " but validation did not pass. Marking remote dead and excluding!" << std::endl;

                            node.nodeInfo->set_is_dead(true);
                            node.nodeInfo->clear_remote_statistics();
                            m_communication.ExcludeNode(from);

                            changes |= RawChanges::NODES_CHANGED;
                        }
                    }
                    else
                    {
                        lllog(6) << "SP: Remote node does not have incarnation either, discarding remote data" << std::endl;
                        node.nodeInfo->clear_remote_statistics();
                    }
                }
                else if (node.nodeInfo->remote_statistics().has_incarnation_id() &&
                         node.nodeInfo->remote_statistics().incarnation_id() != m_allStatisticsMessage.incarnation_id())
                {
                    lllog(6) << "SP: Remote node has different incarnation from us, excluding node." << std::endl;
                    node.nodeInfo->set_is_dead(true);
                    node.nodeInfo->clear_remote_statistics();
                    m_communication.ExcludeNode(from);

                    changes |= RawChanges::NODES_CHANGED;
                }
                else if (!node.nodeInfo->remote_statistics().has_incarnation_id())
                {
                    lllog(6) << "SP: Remote node has no incarnation discarding remote data." << std::endl;
                    node.nodeInfo->clear_remote_statistics();
                }

                if (node.nodeInfo->has_remote_statistics()) //node might have been excluded or cleared above
                {
                    changes |= RawChanges::NEW_REMOTE_STATISTICS;

                    for (const auto& n: node.nodeInfo->remote_statistics().node_info())
                    {
                        if (n.is_dead())
                        {
                            if (AddToMoreDeadNodes(n.id()))
                            {
                                lllog(8) << "SP: Added " << n.id() << " to more_dead_nodes since "
                                         << from  << " thinks that it is dead" << std::endl;

                                changes |= RawChanges::NODES_CHANGED;
                            }
                        }
                    }

                    lllog(9) << "SP: Processing remote more_dead_nodes" << std::endl;
                    for (const auto id: node.nodeInfo->remote_statistics().more_dead_nodes())
                    {
                        if (AddToMoreDeadNodes(id))
                        {
                            lllog(8) << "SP: Added " << id << " to more_dead_nodes since "
                                     << from  << " has it in more_dead_nodes" << std::endl;

                            changes |= RawChanges::NODES_CHANGED;
                        }
                    }
                }

                if (changes != 0)
                {
                    PostRawChangedCallback(RawChanges(changes));
                }
            });
        }

        void NewDataChannelStatistics(const RawStatistics& data)
        {
            if (!m_master)
            {
                throw std::logic_error("Only Master should be able to receive DataChannelStatistics");
            }

            m_strand.dispatch([this,data]
            {
                lllog(9) << "SP: NewDataChannelStatistics" << std::endl;

                int changes = 0;
                for (int i = 0; i < data.Size(); ++i)
                {
                    //Raw data is not meant to contain info about this node, if we get
                    //this data there is some serious system configuration error.
                    //What is happening is that the exe that is transmitting raw data over
                    //ipc to us believes that it is running in another node than we are!
                    if (data.Id(i) == m_id)
                    {
                        throw std::logic_error("DataChannelStatistics contained own node!");
                    }

                    auto findIt = m_nodeTable.find(data.Id(i));

                    //dose_main might have injected nodes that no longer exist, or that
                    //have yet to be discovered in control, so we ignore any mismatches.
                    if (findIt == m_nodeTable.end())
                    {
                        continue;
                    }
                    NodeInfo& node = findIt->second; //alias the iterator

                    //Check if data channel is dead and control channel is not.
                    //In that case we want to kill our own channel.
                    if (data.IsDead(i) && !node.nodeInfo->is_dead())
                    {
                        node.nodeInfo->set_is_dead(true);
                        m_communication.ExcludeNode(data.Id(i));
                        changes |= RawChanges::NODES_CHANGED;
                    }

                    node.nodeInfo->set_data_receive_count(data.DataReceiveCount(i));
                    node.nodeInfo->set_data_retransmit_count(data.DataRetransmitCount(i));

                    changes |= RawChanges::NEW_DATA_CHANNEL_STATISTICS;
                }

                if (changes != 0)
                {
                    PostRawChangedCallback(changes);
                }
            });
        }

        /**
         * Add a callback that will be called whenever the raw data is changed
         * as a result of receiving new raw data from other node.
         *
         * Will always be posted! data will be a copy
         */
        void AddRawChangedCallback(const StatisticsCallback& callback)
        {
            m_strand.dispatch([this, callback]
                              {
                                  m_rawChangedCallbacks.push_back(callback);
                              });
        }


        /**
         * Mark a node as dead in the raw data.
         *
         * Use this to mark a node that has just been excluded as dead.
         **/
        void SetDeadNode(const int64_t id)
        {
            m_strand.dispatch([this, id]
                              {
                                  auto findIt = m_nodeTable.find(id);

                                  if (findIt == m_nodeTable.end())
                                  {
                                      throw std::logic_error("SetDeadNode on unknown node");
                                  }

                                  if (!findIt->second.nodeInfo->is_dead())
                                  {
                                      findIt->second.nodeInfo->set_is_dead(true);
                                      PostRawChangedCallback(RawChanges::NODES_CHANGED);
                                  }
                              });
        }

        /**
         * Tell RawHandler about nodes that have recently been declared as dead by the
         * master (i.e. the Control exe).
         *
         * This function is only expected to be called on the slave (i.e. the dose_main
         * exe).
         */
        void RecentlyDeadNodes(std::vector<int64_t> nodeIds)
        {
            m_strand.dispatch([this, nodeIds]
                              {
                                  bool changed = false;
                                  for (auto id : nodeIds)
                                  {
                                      auto findIt = m_nodeTable.find(id);
                                      if (findIt != m_nodeTable.end() && !findIt->second.nodeInfo->is_dead())
                                      {
                                          findIt->second.nodeInfo->set_is_dead(true);
                                          m_communication.ExcludeNode(id);
                                          changed = true;
                                      }
                                  }

                                  if (changed)
                                  {
                                      PostRawChangedCallback(RawChanges::NODES_CHANGED);
                                  }
                              });
        }

        void SetElectionId(const int64_t /*nodeId*/, const int64_t electionId)
        {
            m_strand.dispatch([this, electionId]
                              {
                                  lllog(7) << "SP: Election Id " << electionId
                                           << " set in RawHandler." << std::endl;

                                  m_allStatisticsMessage.set_election_id(electionId);

                                  PostRawChangedCallback(RawChanges(RawChanges::METADATA_CHANGED));
                              });
        }

        void SetIncarnationId(const int64_t incarnationId)
        {
            m_strand.dispatch([this, incarnationId]
                              {
                                  if (m_allStatisticsMessage.has_incarnation_id())
                                  {
                                      return;
                                  }

                                  lllog(1) << "SP: Incarnation Id " << incarnationId
                                           << " set in RawHandler." << std::endl;

                                  if (m_validateIncarnationIdCallback != nullptr)
                                  {
                                      if (!m_validateIncarnationIdCallback(incarnationId))
                                      {
                                          throw std::logic_error("Nooooo! You can't say no to this incarnation id!");
                                      }
                                  }
                                  m_allStatisticsMessage.set_incarnation_id(incarnationId);

                                  PostRawChangedCallback(RawChanges(RawChanges::METADATA_CHANGED));
                              });
        }

    private:
        static boost::chrono::steady_clock::duration CalculateDeadCheckPeriod(const std::map<int64_t,
                                                                              NodeType>& nodeTypes)
        {
            boost::chrono::steady_clock::duration result = boost::chrono::seconds(1);
            for (const auto& node: nodeTypes)
            {
                if (!node.second.isLight)
                {
                    result = std::min(result,node.second.heartbeatInterval);
                }
            }

            return result + result / 10;
        }

        //must be called in strand
        bool AddToMoreDeadNodes(const int64_t id)
        {
            if (id == 0)
            {
                throw std::logic_error("Unexpected id in AddToMoreDeadNodes");
            }

            if (id == m_id)
            {
                lllog(4) << "SP: Someone else thinks that I am dead! I guess that is goodbye, then!" << std::endl;
                return false;
            }

            //if the node is one of our top-level nodes we don't want it in more_dead_nodes
            if (m_nodeTable.find(id) != m_nodeTable.end())
            {
                return false;
            }

            const bool inserted = m_moreDeadNodes.insert(id).second;
            if(inserted)
            {
                m_allStatisticsMessage.mutable_more_dead_nodes()->Add(id);
            }

            return inserted;
        }

        //Must be called in strand!
        void NewNode(const std::string& name,
                     const int64_t id,
                     const int64_t nodeTypeId,
                     const std::string& controlAddress,
                     const std::string& dataAddress)
        {
            lllog(4) << "SP: New node '" << name.c_str() << "' with id " << id << " was added" << std::endl;

            if (id == m_id)
            {
                throw std::logic_error("Got a new node that has the same id as me!");
            }

            const auto newNode = this->m_allStatisticsMessage.add_node_info();
            const auto insertResult = this->m_nodeTable.insert(std::make_pair(id,NodeInfo(newNode)));

            if (!insertResult.second)
            {
                throw std::logic_error("Got a new node that I already had");
            }

            if (m_nodeTypes.find(nodeTypeId) == m_nodeTypes.end())
            {
                throw std::logic_error("Got a new node with a node type id that I dont know about!");
            }

            //lastReceiveTime is set by NodeInfo constructor

            newNode->set_name(name);
            newNode->set_id(id);
            newNode->set_node_type_id(nodeTypeId);
            newNode->set_control_address(controlAddress);
            newNode->set_data_address(dataAddress);

            newNode->set_is_dead(false);
            newNode->set_control_receive_count(0);
            newNode->set_control_retransmit_count(0);
            newNode->set_data_receive_count(0);
            newNode->set_data_retransmit_count(0);

            if (m_moreDeadNodes.find(id) != m_moreDeadNodes.end())
            {
                m_communication.ExcludeNode(id);
                newNode->set_is_dead(true);
            }

            //notify our users of the new node, and when they've returned we can
            //let it in.
            PostRawChangedCallback(RawChanges(RawChanges::NODES_CHANGED),
                                   m_strand.wrap([this,name,id,newNode]
                  {
                      if (!newNode->is_dead())
                      {
                          lllog(4) << "SP: Calling IncludeNode for "
                                   << name.c_str() << "(" << id << ")" << std::endl;
                          m_communication.IncludeNode(id);
                      }
                      else
                      {
                          lllog(4) << "SP: Not calling IncludeNode for "
                                   << name.c_str() << "(" << id << ") since it has been marked as dead." << std::endl;
                      }
                  }));
        }

        //Must be called in strand!
        void GotReceive(int64_t id)
        {
            const auto now = boost::chrono::steady_clock::now();
            lllog(9) << "SP: GotReceive from node with id " << id <<", time = " << now << std::endl;

            const auto findIt = m_nodeTable.find(id);

            if (findIt == m_nodeTable.end())
            {
                SEND_SYSTEM_LOG(Alert,
                                << "GotReceive from unknown node " << id);
                throw std::logic_error("GotReceive from unknown node");
            }
            NodeInfo& node = findIt->second; //alias the iterator

            if (node.nodeInfo->is_dead())
            {
                lllog(8) << "SP: GotReceive from dead node, ignoring." << std::endl;
                return;
            }

            if (m_master)
            {
                node.nodeInfo->set_control_receive_count(node.nodeInfo->control_receive_count() + 1);
            }
            else
            {
                node.nodeInfo->set_data_receive_count(node.nodeInfo->data_receive_count() + 1);
            }

            node.lastReceiveTime = now;
        }

        //Must be called in strand!
        void Retransmit(int64_t id)
        {
            lllog(9) << "SP: Retransmit to node with id " << id <<  std::endl;

            const auto findIt = m_nodeTable.find(id);

            if (findIt == m_nodeTable.end())
            {
                throw std::logic_error("Retransmit to unknown node");
            }
            NodeInfo& node = findIt->second; //alias the iterator

            if (node.nodeInfo->is_dead())
            {
                lllog(8) << "SP: Retransmit to dead node, ignoring." << std::endl;
                return;
            }

            if (m_master)
            {
                node.nodeInfo->set_control_retransmit_count(node.nodeInfo->control_retransmit_count() + 1);
            }
            else
            {
                node.nodeInfo->set_data_retransmit_count(node.nodeInfo->data_retransmit_count() + 1);
            }
        }

        //Must be called in strand!
        void CheckDeadNodes()
        {
            const auto now = boost::chrono::steady_clock::now();

            const auto clearThreshold = now - boost::chrono::minutes(5);

            bool somethingChanged = false;

            for (auto& pair : m_nodeTable)
            {
                auto tolerance = 1;
                //We're more tolerant when nodes have just started, i.e. when we've received
                //less than 10 packets from them
                if ((m_master && pair.second.nodeInfo->control_receive_count() < 10) ||
                    (!m_master && pair.second.nodeInfo->data_receive_count() < 10))
                {
                    lllog(4) << "SP: Extra tolerant towards new node " << pair.second.nodeInfo->name().c_str()
                             << " with id " << pair.first << std::endl;
                    tolerance = 2;
                }

                const auto threshold =
                    now - m_nodeTypes.at(pair.second.nodeInfo->node_type_id()).deadTimeout * tolerance;

                if (!pair.second.nodeInfo->is_dead() && pair.second.lastReceiveTime < threshold)
                {
                    lllog(4) << "SP: Node " << pair.second.nodeInfo->name().c_str()
                             << " with id " << pair.first
                             << " was marked as dead" << std::endl;

                    pair.second.nodeInfo->set_is_dead(true);

                    m_communication.ExcludeNode(pair.first);

                    somethingChanged = true;
                }
                else if (pair.second.nodeInfo->is_dead() &&
                         pair.second.lastReceiveTime < clearThreshold)
                {
                    lllog(4) << "SP: Node " << pair.second.nodeInfo->name().c_str()
                             << " with id " << pair.first
                             << " has been dead for five minutes, clearing data." << std::endl;

                    const auto lastIndex = m_allStatisticsMessage.node_info_size() - 1;
                    //get id of last element in protobuf
                    const auto lastId = m_allStatisticsMessage.node_info(lastIndex).id();

                    //if we're not the last element in node_info we need to swap ourselves there
                    if (lastId != pair.first)
                    {
                        auto lastEntry = m_nodeTable.find(lastId);
                        if (lastEntry == m_nodeTable.end())
                        {
                            throw std::logic_error("Failed to find table entry for last index!");
                        }

                        //find index to swap to last (that is the index that the current pair
                        //points to
                        int swapIndex = -1;
                        for (int i = 0; m_allStatisticsMessage.node_info_size(); ++i)
                        {
                            if (m_allStatisticsMessage.node_info(i).id() == pair.first)
                            {
                                swapIndex = i;
                                break;
                            }
                        }
                        if (swapIndex == -1)
                        {
                            throw std::logic_error("Failed to find index for for current pair!");
                        }

                        m_allStatisticsMessage.mutable_node_info()->SwapElements(lastIndex,swapIndex);
                        lastEntry->second.nodeInfo = m_allStatisticsMessage.mutable_node_info(swapIndex);
                    }

                    const auto id = pair.first;

                    //remove node from table
                    m_nodeTable.erase(id);

                    //now we can remove the last element
                    m_allStatisticsMessage.mutable_node_info()->RemoveLast();

                    if (AddToMoreDeadNodes(id))
                    {
                        lllog(8) << "SP: Added " << id << " to more_dead_nodes since "
                                  "it has been dead for a long time." << std::endl;
                    }
                    //node was already dead, so no need to set somethingChanged

                    //we've just modified the table that we're looping through, so
                    //we can't continue the loop. Post another call to this function
                    //and break out of the loop.
                    m_strand.post([this]{CheckDeadNodes();});

                    break;
                }
            }

            if (somethingChanged)
            {
                PostRawChangedCallback(RawChanges(RawChanges::NODES_CHANGED));
            }
        }




        /**
         * Post a copy of the data on the ioservice
         *
         * must be called in strand
         */
        void PostRawChangedCallback(const RawChanges& flags, const std::function<void()>& completionHandler = nullptr)
        {
            lllog(7) << "SP: PostRawChangedCallback " << flags << std::endl;
            const auto copy = RawStatisticsCreator::Create
                (Safir::make_unique<RawStatisticsMessage>(m_allStatisticsMessage));

            //this will create an object that will cause one and only one call to the completion handler
            //when the last callback is complete.
            boost::shared_ptr<void> completionCaller(static_cast<void*>(nullptr),
                                                     [completionHandler](void*)
                                                     {
                                                         if (completionHandler != nullptr)
                                                         {
                                                             completionHandler();
                                                         }
                                                     });

            for (const auto& cb : m_rawChangedCallbacks)
            {
                m_strand.post([cb,copy,flags,completionCaller]{cb(copy,flags,completionCaller);});
            }
        }


        struct NodeInfo
        {
            explicit NodeInfo(RawStatisticsMessage_NodeInfo* const nodeInfo_)
                : lastReceiveTime(boost::chrono::steady_clock::now()),nodeInfo(nodeInfo_) {}

            boost::chrono::steady_clock::time_point lastReceiveTime;
            RawStatisticsMessage_NodeInfo* nodeInfo;
        };
        typedef std::unordered_map<int64_t, NodeInfo> NodeTable;

        boost::asio::io_service& m_ioService;
        CommunicationT& m_communication;

        const int64_t m_id;
        const std::map<int64_t, NodeType> m_nodeTypes;
        mutable boost::asio::strand m_strand;
        AsioLatencyMonitor m_latencyMonitor;

        Safir::Utilities::Internal::AsioPeriodicTimer m_checkDeadNodesTimer;

        NodeTable m_nodeTable;
        mutable RawStatisticsMessage m_allStatisticsMessage;

        std::set<int64_t> m_moreDeadNodes;

        std::vector<StatisticsCallback> m_rawChangedCallbacks;

        const bool m_master; //true if running in SystemPicture master instance
        const std::function<bool (const int64_t incarnationId)> m_validateIncarnationIdCallback;

        std::atomic<bool> m_stopped;
    };

    typedef RawHandlerBasic<Com::Communication> RawHandler;
}
}
}
}
