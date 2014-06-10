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
    std::wstring ToString(const ElectionAction action)
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

    std::set<int64_t> GetNodeIds(const SystemStateMessage& state)
    {
        std::set<int64_t> nodes;
        for (int i = 0; i < state.node_info_size(); ++i)
        {
            nodes.insert(state.node_info(i).id());
        }
        return nodes;
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
        , m_dataIdentifier(LlufId_Generate64(dataIdentifier))
        , m_name(name)
        , m_id(id)
        , m_nodeTypeId(nodeTypeId)
        , m_controlAddress(controlAddress)
        , m_dataAddress(dataAddress)
        , m_nodeTypes(nodeTypes)
        , m_nonLightNodeTypes([&nodeTypes]
                              {
                                  std::set<int64_t> res;
                                  for (const auto& it: nodeTypes)
                                  {
                                      if (!it.second.isLight)
                                      {
                                          res.insert(it.first);
                                      }
                                  }
                             return res;
                         }())
        , m_elected(std::numeric_limits<int64_t>::min())
        , m_electionTimer(ioService)
        , m_sendMessageTimer(ioService)
        , m_rawHandler(rawHandler)
    {
        rawHandler.AddStatisticsChangedCallback(m_strand.wrap([this](const RawStatistics& statistics)
                                                               {
                                                                   StatisticsChanged(statistics);
                                                               }));

        communication.SetDataReceiver([this](const int64_t from, 
                                             const int64_t nodeTypeId, 
                                             const boost::shared_ptr<char[]>& data, 
                                             const size_t size)
                                       {
                                           GotData(from, nodeTypeId, data, size);
                                       },
                                       m_dataIdentifier);


        //TODO use average of non-light node types heartbeatInterval * maxLostHeartbeats / 2.0
        m_electionTimer.expires_from_now(boost::chrono::seconds(6)); 
        m_electionTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& error)
                                                      {
                                                          if (!error)
                                                          {
                                                              StartElection();
                                                          }
                                                      }));                                      
    }
    

    void Coordinator::Stop()
    {
        m_electionTimer.cancel();
        m_sendMessageTimer.cancel();
    }

    //must be called in strand
    void Coordinator::StatisticsChanged(const RawStatistics& statistics)
    {
        m_lastStatistics = statistics;
        if (IsElected())
        {
            UpdateMyState();
        }
        StartElection();
    }

    //must be called in strand
    bool Coordinator::UpdateMyState()
    {
        if (!IsElected())
        {
            return false;
        }
        
        if (!m_lastStatistics.Valid())
        {
            lllog(8) << "SP: No valid raw data yet, not updating my state" << std::endl;
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
                lllog(6) << "SP: No remote RAW data received from node " 
                          << m_lastStatistics.Id(i) << ", not updating my state" << std::endl;
                return false;
            }

            const auto remote = m_lastStatistics.RemoteStatistics(i);
            if (remote.ElectionId() != m_currentElectionId)
            {
                lllog(6) << "SP: Remote RAW data from node "
                          << m_lastStatistics.Id(i) << " has wrong election id, not updating my state." << std::endl;
                return false;
            }
        }

        m_stateMessage.set_elected_id(m_id);
        m_stateMessage.set_election_id(m_currentElectionId);

        m_stateMessage.clear_node_info(); //don't care about efficiency...

        //add myself
        auto node = m_stateMessage.add_node_info();
        node->set_name(m_name);
        node->set_id(m_id);
        node->set_node_type_id(m_nodeTypeId);
        node->set_control_address(m_controlAddress);
        node->set_data_address(m_dataAddress);

        //This code will ignore the case where we for some reason have a RAW from another node
        //that says that we are dead. If that is the case it will stop sending data to us and 
        //we will mark him as dead eventually.
        //He should also start a new election, and the thing should resolve itself.

        if (m_lastStatistics.Valid())
        {
            std::set<int64_t> deadNodes;
            for (int i = 0; i < m_lastStatistics.Size(); ++i)
            {
                if (m_lastStatistics.HasRemoteStatistics(i))
                {
                    const auto remote = m_lastStatistics.RemoteStatistics(i);
                    for (int j = 0; j < remote.Size(); ++j)
                    {
                        if (remote.IsDead(j))
                        {
                            deadNodes.insert(remote.Id(j));
                        }
                    }
                }
            }
            
            
            for (int i = 0; i < m_lastStatistics.Size(); ++i)
            {
                if (!m_lastStatistics.IsDead(i))
                {
                    //does another node think that that node is dead?
                    if (deadNodes.find(m_lastStatistics.Id(i)) != deadNodes.end())
                    {
                        m_communication.ExcludeNode(m_lastStatistics.Id(i));
                        m_rawHandler.SetDeadNode(m_lastStatistics.Id(i));
                    }
                    else
                    {
                        auto node = m_stateMessage.add_node_info();
                        node->set_name(m_lastStatistics.Name(i));
                        node->set_id(m_lastStatistics.Id(i));
                        node->set_node_type_id(m_lastStatistics.NodeTypeId(i));
                        node->set_control_address(m_lastStatistics.ControlAddress(i));
                        node->set_data_address(m_lastStatistics.DataAddress(i));
                    }
                }
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
                                  if (!IsElected())
                                  {
                                      return;
                                  }
                                  
                                  const bool okToSend = UpdateMyState();

                                  if (!okToSend)
                                  {
                                      return;
                                  }
                              }

                              const size_t size = m_stateMessage.ByteSize() + extraSpace;
                              auto data = std::unique_ptr<char[]>(new char[size]);
                              m_stateMessage.SerializeWithCachedSizesToArray
                                  (reinterpret_cast<google::protobuf::uint8*>(data.get()));
                              fn(std::move(data), size);
                              
                          });
    }

    //must be called in strand
    void Coordinator::NewSystemState(const int64_t from, 
                                     const boost::shared_ptr<char[]>& data, 
                                     const size_t size)
    {
        m_strand.dispatch([this,from,data,size]
        {
            if (from != m_elected)
            {
                SEND_SYSTEM_LOG(Informational, << "SystemPicture (in node " << m_id << ") got a new system state (from node "
                                << from << ") from a node that is not elected (elected node is " << m_elected << "). Discarding.");
            }
            else
            {
                m_stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                
                const auto nodes = GetNodeIds(m_stateMessage);
                
                for (int i = 0; i < m_lastStatistics.Size(); ++i)
                {
                    //if we haven't marked the node as dead and electee doesnt think the node
                    //is part of the system we want to exclude the node
                    if (!m_lastStatistics.IsDead(i) && nodes.find(m_lastStatistics.Id(i)) == nodes.end())
                    {
                        m_communication.ExcludeNode(m_lastStatistics.Id(i));
                        m_rawHandler.SetDeadNode(m_lastStatistics.Id(i));
                    }
                }
            }
        });
    }


    //must be called in strand
    void Coordinator::StartElection()
    {
        //cancel any other pending elections
        m_electionTimer.cancel(); //
        
        m_electionTimer.expires_from_now(boost::chrono::milliseconds(100)); 
        m_electionTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& error)
        {
            if (!!error)
            {
                return;
            }
            lllog(4) << "SP: Checking if I should start election" << std::endl;
            
            if (!m_lastStatistics.Valid())
            {
                lllog(4) << "SP: Haven't heard from any other nodes, electing myself!" << std::endl;
                m_elected = m_id;
                return;
            }
            else
            {
                for (int i = 0; i < m_lastStatistics.Size(); ++i)
                {
                    lllog(7) << "SP:   know of node " << m_lastStatistics.Id(i) << std::endl;
                        
                    if (m_lastStatistics.Id(i) == m_elected && !m_lastStatistics.IsDead(i))
                    {
                        if (m_elected > m_id)
                        {
                            lllog(4) << "SP: Found elected node with higher id than me, not starting election!" << std::endl;
                            return;
                        }
                    }
                }
            }
            
            
            lllog(4) << "SP: Starting election" << std::endl;
            m_currentElectionId = LlufId_GenerateRandom64();
            
            m_pendingInquiries = m_nonLightNodeTypes;
            SendPendingElectionMessages();
            
            m_electionTimer.expires_from_now(boost::chrono::seconds(6));
            m_electionTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& error)
                                                     {
                                                         if (!error)
                                                         {
                                                             ElectionTimeout();
                                                         }
                                                     }));
        }));
    }

    //not in strand
    void Coordinator::GotData(const int64_t from, 
                              const int64_t nodeTypeId, 
                              const boost::shared_ptr<char[]>& data, 
                              size_t size)
    {
        ElectionMessage message;
        message.ParseFromArray(data.get(), static_cast<int>(size));
        lllog(5) << "SP: Got ElectionMessage (" 
                 << ToString(message.action())
                 << ", " 
                 << message.election_id() 
                 << ") from " << from << std::endl;
        m_strand.dispatch([this,message,from, nodeTypeId]
        {
            switch (message.action())
            {
            case INQUIRY: 
                {
                    //if we got an inquiry from someone smaller than us we send an alive
                    //and start a new election
                    if (from < m_id)
                    {
                        lllog(5) << "SP: Got an inquiry from someone smaller than us, sending alive and starting election" << std::endl;
                        m_pendingAlives.insert(std::make_pair(from, std::make_pair(nodeTypeId, message.election_id())));
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

                        m_rawHandler.SetElectionId(message.election_id());

                        //cancel any ongoing elections
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
    void Coordinator::ElectionTimeout()
    {
        lllog(4) << "SP: There can be only one! Will send VICTORY to everyone!" << std::endl;
        
        m_elected = m_id;

        m_pendingVictories = m_nonLightNodeTypes;
        SendPendingElectionMessages();
    }

    //must be called in strand
    void Coordinator::SendPendingElectionMessages()
    {
        m_sendMessageTimer.cancel();

        { //ALIVE
            const auto pending = m_pendingAlives;
            m_pendingAlives.clear();
            for (const auto& it : pending)
            {
                ElectionMessage aliveMsg;
                aliveMsg.set_action(ALIVE);
                aliveMsg.set_election_id(it.second.second);
                const auto size = aliveMsg.ByteSize();
                boost::shared_ptr<char[]> data = boost::make_shared<char[]>(size);
                aliveMsg.SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
                
                const bool sent = m_communication.SendToNode(it.first, it.second.first, std::move(data), size, m_dataIdentifier);
                
                if (!sent)
                {
                    lllog(9) << "SP: Coordinator: Overflow when sending ALIVE to node " 
                             << it.first << std::endl;
                    m_pendingAlives.insert(it);
                }
            }
        }

        { //VICTORY
            const auto pending = m_pendingVictories;
            m_pendingVictories.clear();
            for (const auto& it : pending)
            {
                ElectionMessage victoryMsg;
                victoryMsg.set_action(VICTORY);
                victoryMsg.set_election_id(m_currentElectionId);
                const auto size = victoryMsg.ByteSize();
                boost::shared_ptr<char[]> data = boost::make_shared<char[]>(size);
                victoryMsg.SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
                
                const bool sent = m_communication.SendToNodeType(it, std::move(data), size, m_dataIdentifier);

                if (!sent)
                {
                    lllog(9) << "SP: Coordinator: Overflow when sending VICTORY to node type " 
                             << m_nodeTypes.find(it)->second.name.c_str() << std::endl;
                    m_pendingVictories.insert(it);
                }
            }
        }

        { //INQUIRY
            const auto pending = m_pendingInquiries;
            m_pendingInquiries.clear();
            for (const auto it : pending)
            {
                ElectionMessage message;
                message.set_action(INQUIRY);
                message.set_election_id(m_currentElectionId);
                const auto size = message.ByteSize();
                boost::shared_ptr<char[]> data = boost::make_shared<char[]>(size);
                message.SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));

                const bool sent = m_communication.SendToNodeType(it, std::move(data), size, m_dataIdentifier);
                if (!sent)
                {
                    lllog(7) << "SP: Coordinator: Overflow when sending INQUIRY to node type " 
                             << m_nodeTypes.find(it)->second.name.c_str() << std::endl;
                    m_pendingInquiries.insert(it);
                }
                else
                {
                    lllog(9) << "SP: Coordinator: sent INQUIRY to node type " 
                             << m_nodeTypes.find(it)->second.name.c_str() << std::endl;
                }
            }
        }

        //Handle retry
        if (!m_pendingAlives.empty() || !m_pendingVictories.empty() || !m_pendingInquiries.empty())
        {
            m_sendMessageTimer.expires_from_now(boost::chrono::milliseconds(10)); 
            m_sendMessageTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& error)
                                                        {
                                                            if (!error)
                                                            {
                                                                SendPendingElectionMessages();
                                                            }
                                                        }));
        }
    }
}
}
}
}

