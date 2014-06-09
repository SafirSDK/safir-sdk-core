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
#include "RawHandler.h"
#include "MessageWrapperCreators.h"
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <boost/chrono.hpp>
#include <boost/make_shared.hpp>
#include <boost/static_assert.hpp>


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
        typedef boost::chrono::steady_clock steady_clock;


        using Safir::Utilities::Internal::AsioPeriodicTimer;
    }

    RawHandler::RawHandler(boost::asio::io_service& ioService,
                           Com::Communication& communication,
                           const std::string& name,
                           const int64_t id,
                           const int64_t nodeTypeId,
                           const std::string& controlAddress,
                           const std::string& dataAddress,
                           const std::map<int64_t, NodeType>& nodeTypes)
        : m_ioService(ioService)
        , m_communication(communication)
        , m_id(id)
        , m_nodeTypes(nodeTypes)
        , m_epoch(steady_clock::now() - boost::chrono::hours(1))
        , m_strand(ioService)
        , m_checkDeadNodesTimer(ioService, 
                                boost::chrono::milliseconds(1100),
                                m_strand.wrap([this](const boost::system::error_code& error)
                                              {
                                                  if (m_stopped)
                                                  {
                                                      return;
                                                  }
                                                  
                                                  CheckDeadNodes(error);
                                              }))
        , m_postStatisticsChangedTimer(ioService,
                                       boost::chrono::milliseconds(1000),
                                       m_strand.wrap([this](const boost::system::error_code& error)
                                                     {
                                                         if (m_stopped)
                                                         {
                                                             return;
                                                         }
                                                         
                                                         if (error)
                                                         {
                                                             SEND_SYSTEM_LOG(Alert,
                                                                             << "Unexpected error in postStatisticsChangedTimer: " << error);
                                                             throw std::logic_error("Unexpected error in postStatisticsChangedTimer");
                                                         }
                                                         PostStatisticsChangedCallback();
                                                     }))
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
                                                            NewNode(name,id,nodeTypeId,controlAddress,dataAddress);
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
        //m_postStatisticsChangedTimer->Start();
    }

    void RawHandler::Stop()
    {
        const bool was_stopped = m_stopped.exchange(true);
        if (!was_stopped)
        {
            m_strand.dispatch([this]
                              {
                                  m_checkDeadNodesTimer.Stop();
                                  m_postStatisticsChangedTimer.Stop();
                              });
        }
    }

    uint32_t RawHandler::GetTime() const
    {
        return static_cast<uint32_t>((steady_clock::now() - m_epoch).count() / 100000000);
    }

    //Must be called in strand!
    void RawHandler::NewNode(const std::string& name,
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
        const auto insertResult = this->m_nodeTable.insert(std::make_pair(id,RawHandler::NodeInfo(newNode)));
        
        if (!insertResult.second)
        {
            throw std::logic_error("Got a new node that I already had");
        }
        
        if (m_nodeTypes.find(nodeTypeId) == m_nodeTypes.end())
        {
            throw std::logic_error("Got a new node with a node type id that I dont know about!");
        }

        newNode->set_name(name);
        newNode->set_id(id);
        newNode->set_node_type_id(nodeTypeId);
        newNode->set_control_address(controlAddress);
        newNode->set_data_address(dataAddress);
        
        newNode->set_is_dead(false);
        newNode->set_receive_count(0);
        newNode->set_retransmit_count(0);
        
        insertResult.first->second.lastReceiveTime = GetTime();
        
        m_communication.IncludeNode(id);

        PostStatisticsChangedCallback();
    }

    //Must be called in strand!
    void RawHandler::GotReceive(const int64_t id)
    {
        const uint32_t now = GetTime();
        lllog(9) << "SP: GotReceive from node with id " << id <<", time = " << now << std::endl;

        const auto findIt = m_nodeTable.find(id);
        
        if (findIt == m_nodeTable.end())
        {
            throw std::logic_error("GotReceive from unknown node");
        }
        NodeInfo& node = findIt->second; //alias the iterator
        
        if (node.nodeInfo->is_dead())
        {
            lllog(8) << "SP: GotReceive from dead node, ignoring." << std::endl;
            return;
        }
        
        node.nodeInfo->set_receive_count(node.nodeInfo->receive_count() + 1);
        node.lastReceiveTime = now;
    }
    
    //Must be called in strand!
    void RawHandler::Retransmit(const int64_t id)
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
        
        node.nodeInfo->set_retransmit_count(node.nodeInfo->retransmit_count() + 1);
    }

    //Must be called in strand!
    void RawHandler::CheckDeadNodes(const boost::system::error_code& error)
    {
        if (error)
        {
            SEND_SYSTEM_LOG(Alert,
                            << "Unexpected error in CheckDeadNodes: " << error);
            throw std::logic_error("Unexpected error in CheckDeadNodes");
        }

        //TODO: Use node type information for this!
        const auto threshold = GetTime() - 90; //9 seconds back in time 
        const auto clearThreshold = GetTime() - 600*5; //5 minutes back in time.
        //        std::wcout << "  Threshold time is " << threshold << std::endl;

        bool somethingChanged = false;

        for (auto pair : m_nodeTable)            
        {
            //lllog(5) << "SP:   lastReceiveTime is " << pair.second->lastReceiveTime.value() << std::endl;
            //                lllog(5) << "SP:   isDead = " << pair.second->isDead.value() << std::endl;
            
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
                     pair.second.nodeInfo->has_remote_statistics())
            {
                lllog(4) << "SP: Node " << pair.second.nodeInfo->name().c_str() 
                         << " with id " << pair.second.nodeInfo->id() 
                         << " has been dead for five minutes, clearing data." << std::endl;
                pair.second.nodeInfo->clear_remote_statistics();
            }
        }

        if (somethingChanged)
        {
            PostStatisticsChangedCallback();
        }
    }


    void RawHandler::PerformOnMyStatisticsMessage(const std::function<void(const boost::shared_ptr<char[]>& data, 
                                                                             const size_t size)> & fn,
                                                  const size_t extraSpace) const
    {
        m_strand.dispatch([this,fn,extraSpace]
        {
#if GOOGLE_PROTOBUF_VERSION >= 2005000
            //With newer protobuf (>= 2.5.0) we can be clever
            //we just get the remote statistics out of the way before serializing a 
            //a "my statistics" message, and afterwards we put them back
            std::vector<NodeStatisticsMessage*> remotes;

            for (int i = 0; i < m_allStatisticsMessage.node_info_size(); ++i)
            {
                //release_remote_statistics appears to allocate an object if it is null,
                //which we don't want to do. So we manually push a nullptr in those cases
                if (m_allStatisticsMessage.node_info(i).has_remote_statistics())
                {
                    remotes.push_back(m_allStatisticsMessage.mutable_node_info(i)->release_remote_statistics());
                }
                else
                {
                    remotes.push_back(nullptr);
                }
            }

            const size_t size = m_allStatisticsMessage.ByteSize() + extraSpace;
#else
            //On older protobuf we have to make a complete copy of the message
            //and then remove the parts we dont want...

            m_myStatisticsMessage = m_allStatisticsMessage;

            for (int i = 0; i < m_myStatisticsMessage.node_info_size(); ++i)
            {
                m_myStatisticsMessage.mutable_node_info(i)->clear_remote_statistics();
            }

            const size_t size = m_myStatisticsMessage.ByteSize() + extraSpace;
#endif

            boost::shared_ptr<char[]> data = boost::make_shared<char[]>(size);

#if GOOGLE_PROTOBUF_VERSION >= 2005000
            m_allStatisticsMessage.SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
        
            for (size_t i = 0; i < remotes.size(); ++i)
            {
                m_allStatisticsMessage.mutable_node_info(static_cast<int>(i))->
                    set_allocated_remote_statistics(remotes[i]);
            }
#else
            m_myStatisticsMessage.SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
#endif

            //call the function object with the produced data.
            fn (data, size);
        });

    }
    
    void RawHandler::PerformOnAllStatisticsMessage(const std::function<void(std::unique_ptr<char[]> data,
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
    
    void RawHandler::UpdateRemoteStatistics(const int64_t from, 
                                            const boost::shared_ptr<char[]>& data,
                                            const size_t size)
    {
        lllog(9) << "SP: UpdateRemoteStatistics for node " << from << std::endl;
        m_strand.dispatch([this,from,data,size]
        {
            auto findIt = m_nodeTable.find(from);
            
            if (findIt == m_nodeTable.end())
            {
                throw std::logic_error("UpdateRemoteStatistics from unknown node");
            }
            NodeInfo& node = findIt->second; //alias the iterator
            
            if (node.nodeInfo->is_dead())
            {
                lllog(8) << "SP: UpdateRemoteStatistics from dead node, ignoring." << std::endl;
                return;
            }
            
            const bool parseResult = node.nodeInfo->mutable_remote_statistics()->ParseFromArray(data.get(),static_cast<int>(size));
            if (!parseResult)
            {
                SEND_SYSTEM_LOG(Error,
                                << "Failed to parse remote data from " 
                                << node.nodeInfo->name().c_str() 
                                << " (id = " << from << ", size = " << size << ")");
                node.nodeInfo->clear_remote_statistics();
            }
        });
    }

    void RawHandler::AddStatisticsChangedCallback(const StatisticsChangedCallback& callback)
    {
        m_strand.dispatch([this, callback]
                          {
                              m_statisticsChangedCallbacks.push_back(callback);
                          });
    }
    
    //must be called in strand
    void RawHandler::PostStatisticsChangedCallback()
    {
        const auto copy = RawStatisticsCreator::Create(Safir::make_unique<NodeStatisticsMessage>(m_allStatisticsMessage));
        for (auto cb : m_statisticsChangedCallbacks)
        {
            m_ioService.post([cb,copy]{cb(copy);});
        }
    }

    void RawHandler::SetDeadNode(const int64_t id)
    {
        m_strand.dispatch([this, id]
                          {
                              auto findIt = m_nodeTable.find(id);
            
                              if (findIt == m_nodeTable.end())
                              {
                                  throw std::logic_error("SetDeadNode on unknown node");
                              }
                              findIt->second.nodeInfo->set_is_dead(true);
                          });
    }

}
}
}
}
