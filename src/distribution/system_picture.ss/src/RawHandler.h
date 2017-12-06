/******************************************************************************
*
* Copyright Saab AB, 2012 (http://safirsdkcore.com)
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
#include "Function.h"
#include <boost/make_shared.hpp>
#include <boost/chrono.hpp>
#include <boost/noncopyable.hpp>
#include <boost/bind.hpp>
#include <string>
#include <unordered_map>
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

    typedef boost::function<void(const RawStatistics& statistics,
                                 const RawChanges& flags,
                                 boost::shared_ptr<void> completionSignaller)> StatisticsCallback;

    template<class CommunicationT>
    class RawHandlerBasic
        : private boost::noncopyable
    {
    private:
        struct NodeInfo
        {
            explicit NodeInfo(RawStatisticsMessage_NodeInfo* const nodeInfo_, const bool mc)
                : multicast(mc)
                , lastUcReceiveTime(boost::chrono::steady_clock::now())
                , lastMcReceiveTime(boost::chrono::steady_clock::now())
                , nodeInfo(nodeInfo_)
                , numBadElectionIds(0)
                , lastBadElectionId(0)
            {}

            const bool multicast;
            boost::chrono::steady_clock::time_point lastUcReceiveTime;
            boost::chrono::steady_clock::time_point lastMcReceiveTime;
            RawStatisticsMessage_NodeInfo* nodeInfo;
            int numBadElectionIds;
            int64_t lastBadElectionId;
        };
        typedef std::unordered_map<int64_t, NodeInfo> NodeTable;

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
                        const boost::function<bool (const int64_t incarnationId)>& validateJoinSystemCallback,
                        const boost::function<bool (const int64_t incarnationId)>& validateFormSystemCallback)
        SAFIR_GCC_VISIBILITY_BUG_WORKAROUND
            : m_ioService(ioService)
            , m_communication(communication)
            , m_id(id)
            , m_nodeTypes(nodeTypes)
            , m_strand(ioService)
            , m_latencyMonitor("SpRawHandler",CalculateLatencyWarningThreshold(nodeTypes),m_strand)
            , m_checkDeadNodesTimer()
            , m_master(master)
            , m_validateJoinSystemCallback(validateJoinSystemCallback)
            , m_validateFormSystemCallback(validateFormSystemCallback)
            , m_stopped(false)
        {

            m_checkDeadNodesTimer.reset(new Safir::Utilities::Internal::AsioPeriodicTimer
                                        (ioService,
                                         CalculateDeadCheckPeriod(nodeTypes),
                                         m_strand.wrap([this](const boost::system::error_code& error)
                                                       {
                                                           if (m_stopped)
                                                           {
                                                               return;
                                                           }

                                                           if (!!error) //fix for vs2012 warning
                                                           {
                                                               SEND_SYSTEM_LOG(Alert,
                                                                               << "Unexpected error in CheckDeadNodes: " << error);
                                                               throw std::logic_error("Unexpected error in CheckDeadNodes");
                                                           }

                                                           CheckDeadNodes();
                                                       })));


            //set up some info about ourselves in our message
            m_allStatisticsMessage.set_name(name);
            m_allStatisticsMessage.set_id(id);
            m_allStatisticsMessage.set_node_type_id(nodeTypeId);
            m_allStatisticsMessage.set_control_address(controlAddress);
            m_allStatisticsMessage.set_data_address(dataAddress);

            //For some wierdass reason using strand::wrap here doesn't work.
            //Maybe too many arguments? So we do it manually instead.
            communication.SetNewNodeCallback([this](const std::string& name,
                                                    const int64_t id,
                                                    const int64_t nodeTypeId,
                                                    const std::string& controlAddress,
                                                    const std::string& dataAddress,
                                                    const bool multicast)
            {
                m_strand.dispatch([=]
                {
                    NewNode(name,id,nodeTypeId,controlAddress,dataAddress,multicast);
                });
            });

            communication.SetGotReceiveFromCallback(m_strand.wrap([this](int64_t id, bool multicast, bool duplicate)
            {GotReceive(id,multicast,duplicate);}));

            communication.SetRetransmitToCallback(m_strand.wrap([this](int64_t id, size_t tc)
            {Retransmit(id, tc);}));

            m_checkDeadNodesTimer->Start();
        }


        void Stop()
        {
            const bool was_stopped = m_stopped.exchange(true);
            if (!was_stopped)
            {
                m_latencyMonitor.Stop();

                m_strand.dispatch([this]
                                  {
                                      m_checkDeadNodesTimer->Stop();
                                  });
            }
        }


        void PerformOnMyStatisticsMessage(const workaround::function<void(std::unique_ptr<char[]> data,
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


        void PerformOnAllStatisticsMessage(const workaround::function<void(std::unique_ptr<char []> data,
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
                        const bool join = m_validateJoinSystemCallback == nullptr ||
                            m_validateJoinSystemCallback(node.nodeInfo->remote_statistics().incarnation_id());

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

                    for (auto n = node.nodeInfo->remote_statistics().node_info().begin(); n != node.nodeInfo->remote_statistics().node_info().end(); ++n)
                    {
                        if (n->is_dead())
                        {
                            if (AddToMoreDeadNodes(n->id()))
                            {
                                lllog(8) << "SP: Added " << n->id() << " to more_dead_nodes since "
                                         << from  << " thinks that it is dead" << std::endl;

                                changes |= RawChanges::NODES_CHANGED;
                            }
                        }
                    }

                    lllog(9) << "SP: Processing remote more_dead_nodes" << std::endl;

                    for (auto id = node.nodeInfo->remote_statistics().more_dead_nodes().begin(); id != node.nodeInfo->remote_statistics().more_dead_nodes().end(); ++id)
                    {
                        if (AddToMoreDeadNodes(*id))
                        {
                            lllog(8) << "SP: Added " << *id << " to more_dead_nodes since "
                                     << from  << " has it in more_dead_nodes" << std::endl;

                            changes |= RawChanges::NODES_CHANGED;
                        }
                    }
                }

                if (DetectBadElectionIds(node))
                {
                    changes |= RawChanges::BAD_ELECTION_ID_DETECTED;
                }

                if (changes != 0)
                {
                    PostRawChangedCallback(RawChanges(changes), NULL);
                }
            });
        }

        //must be called from within strand
        bool DetectBadElectionIds(NodeInfo& node)
        {
            const int64_t electionId = node.nodeInfo->remote_statistics().election_id();
            if (electionId == 0 || electionId == m_allStatisticsMessage.election_id())
            {
                //good election id
                //reset counters.
                node.numBadElectionIds = 0;
                node.lastBadElectionId = 0;
            }
            else
            {
                //bad id.
                lllog(1) << "Bad election id ("
                         << node.nodeInfo->remote_statistics().election_id()
                         << ") from node " << node.nodeInfo->name().c_str() << "("
                         << node.nodeInfo->id() << ")." << std::endl;

                //check if it was the same bad election id as last time
                if (node.lastBadElectionId != electionId)
                {
                    node.lastBadElectionId = electionId;
                    node.numBadElectionIds = 1;
                }
                else
                {
                    ++node.numBadElectionIds;
                    if (node.numBadElectionIds >= 3)
                    {
                        lllog(1) << "Have detected 3 bad election ids ("
                                 << node.nodeInfo->remote_statistics().election_id()
                                 << ") from node " << node.nodeInfo->name().c_str() << "("
                                 << node.nodeInfo->id() << ")! Forcing election." << std::endl;
                        node.numBadElectionIds = 0;
                        node.lastBadElectionId = 0;
                        return true;
                    }
                }
            }

            return false;
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
                    node.nodeInfo->set_data_duplicate_count(data.DataDuplicateCount(i));
                    node.nodeInfo->set_data_retransmit_count(data.DataRetransmitCount(i));

                    changes |= RawChanges::NEW_DATA_CHANNEL_STATISTICS;
                }

                if (changes != 0)
                {
                    PostRawChangedCallback(changes, NULL);
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
         * Exclude a node and mark it as dead in the raw data.
         **/
        void ExcludeNode(const int64_t id)
        {
            m_communication.ExcludeNode(id);
            m_strand.dispatch([this, id]
                              {
                                  auto findIt = m_nodeTable.find(id);

                                  if (findIt == m_nodeTable.end())
                                  {
                                      throw std::logic_error("ExcludeNode on unknown node");
                                  }

                                  if (!findIt->second.nodeInfo->is_dead())
                                  {
                                      findIt->second.nodeInfo->set_is_dead(true);
                                      PostRawChangedCallback(RawChanges::NODES_CHANGED, NULL);
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

                                  for (auto id = nodeIds.cbegin(); id != nodeIds.cend(); ++id)
                                  {
                                      auto findIt = m_nodeTable.find(*id);
                                      if (findIt != m_nodeTable.end() && !findIt->second.nodeInfo->is_dead())
                                      {
                                          findIt->second.nodeInfo->set_is_dead(true);
                                          m_communication.ExcludeNode(*id);
                                          changed = true;
                                      }
                                  }

                                  if (changed)
                                  {
                                      PostRawChangedCallback(RawChanges::NODES_CHANGED, NULL);
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

                                  PostRawChangedCallback(RawChanges(RawChanges::METADATA_CHANGED), NULL);
                              });
        }

        void FormSystem(const int64_t incarnationId)
        {
            m_strand.dispatch([this, incarnationId]
                              {
                                  if (m_allStatisticsMessage.has_incarnation_id())
                                  {
                                      return;
                                  }

                                  const auto formSystem = m_validateFormSystemCallback == nullptr ||
                                                    m_validateFormSystemCallback(incarnationId);

                                  if (formSystem)
                                  {
                                      m_allStatisticsMessage.set_incarnation_id(incarnationId);

                                      PostRawChangedCallback(RawChanges(RawChanges::METADATA_CHANGED), NULL);

                                      lllog(1) << "SP: Incarnation Id " << incarnationId
                                               << " set in RawHandler." << std::endl;
                                  }
                                  else
                                  {
                                      lllog(8) << "SP: Not allowed to form new system" << std::endl;
                                  }
                              });
        }

    private:
        static boost::chrono::steady_clock::duration
        CalculateDeadCheckPeriod(const std::map<int64_t,NodeType>& nodeTypes)
        {
            boost::chrono::steady_clock::duration result = boost::chrono::seconds(1);

            for (auto node = nodeTypes.cbegin(); node != nodeTypes.cend(); ++node)
            {
                if (!node->second.isLight)
                {
                    result = std::min(result,node->second.heartbeatInterval);
                }
            }

            return result + result / 10;
        }

        static boost::chrono::steady_clock::duration
        CalculateLatencyWarningThreshold(const std::map<int64_t,NodeType>& nodeTypes)
        {
            boost::chrono::steady_clock::duration result = boost::chrono::seconds(120);

            for (auto node = nodeTypes.cbegin(); node != nodeTypes.cend(); ++node)
            {
                if (!node->second.isLight)
                {
                    result = std::min(result,node->second.deadTimeout);
                }
            }

            return result / 2;
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
                     const std::string& dataAddress,
                     const bool multicast)
        {
            lllog(4) << "SP: New node '" << name.c_str() << "' with id "
                     << id << " was added. MC = " << std::boolalpha << multicast << std::endl;

            if (id == m_id)
            {
                throw std::logic_error("Got a new node that has the same id as me!");
            }

            const auto newNode = this->m_allStatisticsMessage.add_node_info();
            const auto insertResult = this->m_nodeTable.insert(std::make_pair(id,NodeInfo(newNode,multicast)));

            if (!insertResult.second)
            {
                throw std::logic_error("Got a new node that I already had");
            }

            if (m_nodeTypes.find(nodeTypeId) == m_nodeTypes.end())
            {
                throw std::logic_error("Got a new node with a node type id that I dont know about!");
            }

            //last receive times are set by NodeInfo constructor

            newNode->set_name(name);
            newNode->set_id(id);
            newNode->set_node_type_id(nodeTypeId);
            newNode->set_control_address(controlAddress);
            newNode->set_data_address(dataAddress);

            newNode->set_is_dead(false);
            newNode->set_control_receive_count(0);
            newNode->set_control_duplicate_count(0);
            newNode->set_control_retransmit_count(0);
            newNode->set_data_receive_count(0);
            newNode->set_data_duplicate_count(0);
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
        void GotReceive(const int64_t id, const bool multicast, const bool duplicate)
        {
            const auto now = boost::chrono::steady_clock::now();
            lllog(9) << "SP: GotReceive (MC=" <<std::boolalpha << multicast
                     << ") from node with id " << id <<", time = " << now << std::endl;

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

            if (duplicate)
            {
                if (m_master)
                {
                    node.nodeInfo->set_control_duplicate_count(node.nodeInfo->control_duplicate_count() + 1);
                }
                else
                {
                    node.nodeInfo->set_data_duplicate_count(node.nodeInfo->data_duplicate_count() + 1);
                }
            }
            else
            {
                if (m_master)
                {
                    node.nodeInfo->set_control_receive_count(node.nodeInfo->control_receive_count() + 1);
                }
                else
                {
                    node.nodeInfo->set_data_receive_count(node.nodeInfo->data_receive_count() + 1);
                }
            }

            if (multicast)
            {
                assert(node.multicast);
                node.lastMcReceiveTime = now;
            }
            else
            {
                node.lastUcReceiveTime = now;
            }
        }

        //Must be called in strand!
        void Retransmit(int64_t id, size_t transmitCount)
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

                if (transmitCount > 0) //we only really need to do this if it is a retransmit
                {
                    //if we have a great number of retransmits it means that either we have one-sided communication
                    //or that the other node has excluded us, but is still sending heartbeats to us (can happen in
                    //multicast scenarios). So we exclude the node.
                    //However, during startup we can get a lot of retransmits while nodes are getting connected.
                    if (node.nodeInfo->control_receive_count() > 0)
                    {
                        if (transmitCount >= 20)
                        {
                            lllog(1) << "Excessive retransmits (" << transmitCount << ") to node "
                                     << node.nodeInfo->name().c_str() << "(" <<  id << "), excluding it!" << std::endl;

                            m_communication.ExcludeNode(id);
                        }
                    }
                    else
                    {
                        //so here (when we have not received anything from the other node) we want to
                        //wait for two minutes before excluding the node.
                        auto nodeType = m_nodeTypes.find(node.nodeInfo->node_type_id());

                        if (nodeType == m_nodeTypes.end())
                        {
                            throw std::logic_error("Failed to find node type in Retransmit");
                        }

                        if (transmitCount >= nodeType->second.twoMinutesOfRetries)
                        {
                            lllog(1) << "Two minutes of retransmits (" << transmitCount << ") to node "
                                     << node.nodeInfo->name().c_str() << "(" <<  id << "), excluding it!" << std::endl;

                            m_communication.ExcludeNode(id);
                        }
                    }
                }
            }
            else
            {
                node.nodeInfo->set_data_retransmit_count(node.nodeInfo->data_retransmit_count() + 1);
            }

        }

        boost::chrono::steady_clock::duration
        CalculateTimeout(const int64_t id, const NodeInfo& nodeInfo) const
        {
            auto tolerance = 1;

            const auto receiveCount = m_master ?
                nodeInfo.nodeInfo->control_receive_count() : nodeInfo.nodeInfo->data_receive_count();

            if (!m_master && receiveCount == 0)
            {
                //if a slave has not received any packets it may mean that the
                //node has just been injected to this node, but the other node
                //has not yet heard about us, so we really need to give the other
                //node plenty of time here.
                lllog(4) << "SP: Slave is extra tolerant towards new node " << nodeInfo.nodeInfo->name().c_str()
                         << " with id " << id << " that has not started sending yet" << std::endl;
                tolerance = 10;
            }
            else if (receiveCount < 10)
            {
                //We're more tolerant when nodes have just started, i.e. when we've received
                //less than 10 packets from them

                lllog(4) << "SP: Extra tolerant towards new node " << nodeInfo.nodeInfo->name().c_str()
                         << " with id " << id << std::endl;

                tolerance = 2;
            }

            return m_nodeTypes.at(nodeInfo.nodeInfo->node_type_id()).deadTimeout * tolerance;
        }

        //Must be called in strand!
        void CheckDeadNodes()
        {
            const auto now = boost::chrono::steady_clock::now();

            const auto clearThreshold = now - boost::chrono::minutes(5);

            bool somethingChanged = false;

            for (auto pair = m_nodeTable.cbegin(); pair != m_nodeTable.cend(); ++pair)
            {
                //some easier names
                const auto id = pair->first;
                const auto& node = pair->second;

                //Multicast timeout is longer than the normal timeout, so that we can be tolerant to
                //starved heartbeats.
                auto timeout = CalculateTimeout(id,node);
                const auto anyThreshold = now - timeout;
                const auto mcThreshold = now - 4 * timeout;

                /*
                 * if not multicast:
                 *    node is dead if anyThreshold is exeeded
                 * if multicast:
                 *    node is dead if anyThreshold or mcThreshold is exeeded, in that
                 * order (remember the timeouts differ)
                 */
                const auto lastAnyReceiveTime = std::max(node.lastUcReceiveTime, node.lastMcReceiveTime);

                if (!node.nodeInfo->is_dead() &&
                    (lastAnyReceiveTime < anyThreshold || (node.multicast && node.lastMcReceiveTime < mcThreshold)))
                {
                    lllog(4) << "SP: Node " << node.nodeInfo->name().c_str()
                             << " with id " << id
                             << " was marked as dead (" << std::boolalpha
                             << node.multicast << ", "
                             << (node.lastUcReceiveTime < anyThreshold) << ", "
                             << (node.lastMcReceiveTime < anyThreshold) << ", "
                             << (node.lastMcReceiveTime < mcThreshold) << ")"
                             << std::endl;

                    node.nodeInfo->set_is_dead(true);

                    m_communication.ExcludeNode(id);

                    somethingChanged = true;
                }
                else if (node.nodeInfo->is_dead() &&
                         lastAnyReceiveTime < clearThreshold)
                {
                    lllog(4) << "SP: Node " << node.nodeInfo->name().c_str()
                             << " with id " << id
                             << " has been dead for five minutes, clearing data." << std::endl;

                    const auto lastIndex = m_allStatisticsMessage.node_info_size() - 1;
                    //get id of last element in protobuf
                    const auto lastId = m_allStatisticsMessage.node_info(lastIndex).id();

                    //if we're not the last element in node_info we need to swap ourselves there
                    if (lastId != id)
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
                            if (m_allStatisticsMessage.node_info(i).id() == id)
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
                PostRawChangedCallback(RawChanges(RawChanges::NODES_CHANGED), NULL);
            }
        }




        /**
         * Post a copy of the data on the ioservice
         *
         * must be called in strand, completionHandler can be 0
         */
        void PostRawChangedCallback(const RawChanges& flags, const boost::function<void()>& completionHandler)
        {
            lllog(7) << "SP: PostRawChangedCallback " << flags << std::endl;
            const auto copy = RawStatisticsCreator::Create
                (Safir::make_unique<RawStatisticsMessage>(m_allStatisticsMessage));

            //this will create an object that will cause one and only one call to the completion handler
            //when the last callback is complete.
            boost::shared_ptr<void> completionCaller(static_cast<void*>(nullptr),
                                                     [completionHandler](void*)
                                                     {
                                                         if (!completionHandler.empty())
                                                         {
                                                             completionHandler();
                                                         }
                                                     });

            for (auto it = m_rawChangedCallbacks.cbegin(); it != m_rawChangedCallbacks.cend(); ++it)
            {
                auto cb = *it;
                m_strand.post([cb,copy,flags,completionCaller]{cb(copy,flags,completionCaller);});
            }
        }


        boost::asio::io_service& m_ioService;
        CommunicationT& m_communication;

        const int64_t m_id;
        const std::map<int64_t, NodeType> m_nodeTypes;
        mutable boost::asio::strand m_strand;
        AsioLatencyMonitor m_latencyMonitor;

        std::unique_ptr<Safir::Utilities::Internal::AsioPeriodicTimer> m_checkDeadNodesTimer;

        NodeTable m_nodeTable;
        mutable RawStatisticsMessage m_allStatisticsMessage;

        std::set<int64_t> m_moreDeadNodes;

        std::vector<StatisticsCallback> m_rawChangedCallbacks;

        const bool m_master; //true if running in SystemPicture master instance
        const boost::function<bool (const int64_t incarnationId)> m_validateJoinSystemCallback;
        const boost::function<bool (const int64_t incarnationId)> m_validateFormSystemCallback;

        boost::atomic<bool> m_stopped;
    };

    typedef RawHandlerBasic<Com::Communication> RawHandler;
}
}
}
}
