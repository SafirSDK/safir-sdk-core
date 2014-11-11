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
#include <boost/make_shared.hpp>
#include <boost/chrono.hpp>
#include <boost/noncopyable.hpp>
#include <string>
#include <unordered_map>
#include <functional>


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
                               const RawChanges& flags)> StatisticsCallback;

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
                        const bool master)
            : m_ioService(ioService)
            , m_communication(communication)
            , m_id(id)
            , m_nodeTypes(nodeTypes)
            , m_strand(ioService)
            , m_checkDeadNodesTimer(ioService,
                                    CalculateDeadCheckPeriod(nodeTypes),
                                    m_strand.wrap([this](const boost::system::error_code& error)
                                                  {
                                                      if (m_stopped)
                                                      {
                                                          return;
                                                      }

                                                      CheckDeadNodes(error);
                                                  }))
            , m_master(master)
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
                m_strand.dispatch([this]
                                  {
                                      m_checkDeadNodesTimer.Stop();
                                  });
            }
        }


        //extraSpace adds bytes at the end of the buffer, e.g. for adding a crc
        void PerformOnMyStatisticsMessage(const std::function<void(std::unique_ptr<char[]> data,
                                                                   const size_t size)> & fn,
                                          const size_t extraSpace) const
        {
            m_strand.dispatch([this,fn,extraSpace]
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

                const size_t size = m_allStatisticsMessage.ByteSize() + extraSpace;

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
                                                                    const size_t size)> & fn,
                                           const size_t extraSpace) const
        {
            m_strand.dispatch([this,fn, extraSpace]
                              {
                                  const size_t size = m_allStatisticsMessage.ByteSize() + extraSpace;
                                  auto data = std::unique_ptr<char[]>(new char[size]);
                                  m_allStatisticsMessage.SerializeWithCachedSizesToArray
                                      (reinterpret_cast<google::protobuf::uint8*>(data.get()));
                                  fn(std::move(data), size);
                              });
        }

        void NewRemoteStatistics(const int64_t from, const boost::shared_ptr<char[]>& data, const size_t size)
        {
            lllog(9) << "SP: NewRemoteStatistics for node " << from << std::endl;
            m_strand.dispatch([this,from,data,size]
            {
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

                PostRawChangedCallback(RawChanges(RawChanges::NEW_REMOTE_STATISTICS));
            });
        }

        void NewDataChannelStatistics(const RawStatistics& data)
        {
            if (!m_master)
            {
                throw std::logic_error("Only Master should be able to receive DataChannelStatistics");
            }

            lllog(9) << "SP: NewDataChannelStatistics" << std::endl;
            m_strand.dispatch([this,data]
            {
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
                    //In that case we want to kill our own channel (TODO: is this right?)
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
            //TODO: should we detect that we're not receiving these and then
            //deduce that control has died?
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

                                  PostRawChangedCallback(RawChanges(RawChanges::ELECTION_ID_CHANGED));
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
            newNode->set_is_long_gone(false);
            newNode->set_control_receive_count(0);
            newNode->set_control_retransmit_count(0);
            newNode->set_data_receive_count(0);
            newNode->set_data_retransmit_count(0);

            m_communication.IncludeNode(id);

            PostRawChangedCallback(RawChanges(RawChanges::NODES_CHANGED));
        }


        //Must be called in strand!
        void GotReceive(int64_t id)
        {
            const auto now = boost::chrono::steady_clock::now();
            lllog(9) << "SP: GotReceive from node with id " << id <<", time = " << now << std::endl;

            const auto findIt = m_nodeTable.find(id);

            if (findIt == m_nodeTable.end())
            {
                lllog(0) << "SP: Got Receive from unknown node " << id << std::endl;
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
        void CheckDeadNodes(const boost::system::error_code& error)
        {
            if (error)
            {
                SEND_SYSTEM_LOG(Alert,
                                << "Unexpected error in CheckDeadNodes: " << error);
                throw std::logic_error("Unexpected error in CheckDeadNodes");
            }

            const auto now = boost::chrono::steady_clock::now();

            const auto clearThreshold = now - boost::chrono::minutes(5);

            bool somethingChanged = false;

            for (auto& pair : m_nodeTable)
            {
                const auto threshold = now - m_nodeTypes.at(pair.second.nodeInfo->node_type_id()).deadTimeout;

                if (!pair.second.nodeInfo->is_dead() && pair.second.lastReceiveTime < threshold)
                {
                    lllog(4) << "SP: Node " << pair.second.nodeInfo->name().c_str()
                             << " with id " << pair.second.nodeInfo->id()
                             << " was marked as dead" << std::endl;

                    pair.second.nodeInfo->set_is_dead(true);

                    m_communication.ExcludeNode(pair.second.nodeInfo->id());

                    somethingChanged = true;
                }
                else if (pair.second.nodeInfo->is_dead() &&
                         pair.second.lastReceiveTime < clearThreshold &&
                         !pair.second.nodeInfo->is_long_gone())
                {
                    lllog(4) << "SP: Node " << pair.second.nodeInfo->name().c_str()
                             << " with id " << pair.second.nodeInfo->id()
                             << " has been dead for five minutes, clearing data." << std::endl;
                    pair.second.nodeInfo->set_is_long_gone(true);
                    pair.second.nodeInfo->clear_remote_statistics();
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
        void PostRawChangedCallback(const RawChanges& flags)
        {
            lllog(7) << "SP: PostRawChangedCallback " << flags << std::endl;
            const auto copy = RawStatisticsCreator::Create
                (Safir::make_unique<RawStatisticsMessage>(m_allStatisticsMessage));
            for (const auto& cb : m_rawChangedCallbacks)
            {
                m_ioService.post([cb,copy,flags]{cb(copy,flags);});
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

        Safir::Utilities::Internal::AsioPeriodicTimer m_checkDeadNodesTimer;

        NodeTable m_nodeTable;
        mutable RawStatisticsMessage m_allStatisticsMessage;

        std::vector<StatisticsCallback> m_nodesChangedCallbacks;
        std::vector<StatisticsCallback> m_electionIdChangedCallbacks;
        std::vector<StatisticsCallback> m_rawChangedCallbacks;

        const bool m_master; //true if running in SystemPicture master instance
        std::atomic<bool> m_stopped;
    };

    typedef RawHandlerBasic<Com::Communication> RawHandler;
}
}
}
}
