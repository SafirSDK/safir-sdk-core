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
}

    Coordinator::Coordinator(const boost::shared_ptr<boost::asio::io_service>& ioService,
                             const boost::shared_ptr<Com::Communication>& communication,
                             const int64_t id,
                             const char* const dataIdentifier,
                             const boost::shared_ptr<RawHandler>& rawHandler)
        : m_strand (*ioService)
        , m_communication(communication)
        , m_dataIdentifier(LlufId_Generate64(dataIdentifier))
        , m_id(id)
        , m_elected(std::numeric_limits<boost::int64_t>::min())
        , m_electionTimer(*ioService)
    {
        rawHandler->SetStatisticsChangedCallback(m_strand.wrap([this](const RawStatistics& statistics)
                                                               {
                                                                   StatisticsChanged(statistics);
                                                               }));

        communication->SetDataReceiver([this](const boost::int64_t from, 
                                              const boost::shared_ptr<char[]>& data, 
                                              const size_t size)
                                       {
                                           GotData(from,data,size);
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
    void Coordinator::UpdateMyState()
    {
        if (!IsElected())
        {
            return;
        }
        /* TODO: update our state!
        //Exclude nodes!

        //currently we just copy the raw data... mucho stupido...

        lllog(6) << "Collating" << std::endl;
        if (!m_stateMessage)
        {
            m_stateMessage = boost::make_shared<SystemStateMessage>();

            m_stateMessage->set_name(statistics.Name());
            m_stateMessage->set_id(statistics.Id());
            m_stateMessage->set_control_address(statistics.Address());
        }

        m_stateMessage->clear_node_info(); //don't care about efficiency...

        for (int i = 0; i < statistics.Size(); ++i)
        {
            if (!statistics.IsDead(i))
            {
                auto node = m_stateMessage->add_node_info();
                node->set_name(statistics.Name(i));
                node->set_id(statistics.Id(i));
                node->set_control_address(statistics.Address(i));
                node->set_multicast_enabled(statistics.MulticastEnabled(i));
            }
        }
        */

    }
    
    void Coordinator::PerformOnStateMessage(const boost::function<void(const boost::shared_ptr<char []>& data, 
                                                                       const size_t size)> & fn,
                                            const size_t extraSpace)
    {
        m_strand.dispatch([this,fn,extraSpace]
                          {
                              if (IsElected())
                              {
                                  UpdateMyState();
                              }

                              const size_t size = m_stateMessage.ByteSize() + extraSpace;
                              auto data = boost::make_shared<char[]>(size);
                              m_stateMessage.SerializeWithCachedSizesToArray
                                  (reinterpret_cast<google::protobuf::uint8*>(data.get()));
                              fn(data, size);
                          });
    }

    void Coordinator::NewSystemState(const boost::int64_t from, 
                                     const boost::shared_ptr<char[]>& data, 
                                     const size_t size)
    {
        if (IsElected())
        {
            //TODO: should I just ignore this, or maybe start a new election?
            throw std::logic_error("Got SystemState from someone else, but I'm the elected coordinator!");
        }
        else
        {
            m_stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
            //TODO: act on information, such as exclude nodes.
        }
    }


    //must be called in strand
    void Coordinator::StartElection()
    {
        //cancel any other pending elections
        m_electionTimer.cancel(); //
        
        m_electionTimer.expires_from_now(boost::chrono::milliseconds(200)); 
        m_electionTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& error)
        {
            if (!!error)
            {
                return;
            }
            lllog(4) << "Checking if I should start election" << std::endl;
            
            if (!m_lastStatistics.Valid())
            {
                lllog(5) << "Haven't heard from any other nodes, electing myself!" << std::endl;
                m_elected = m_id;
                return;
            }
            else
            {
                for (int i = 0; i < m_lastStatistics.Size(); ++i)
                {
                    if (m_lastStatistics.Id(i) == m_elected)
                    {
                        if (m_elected > m_id)
                        {
                            lllog(5) << "Found elected node with higher id than me, not starting election!" << std::endl;
                            return;
                        }
                    }
                }
            }
            
            
            lllog(4) << "Starting election" << std::endl;
            m_currentElectionId = LlufId_GenerateRandom64();
            
            ElectionMessage message;
            message.set_action(INQUIRY);
            message.set_election_id(m_currentElectionId);
            const auto size = message.ByteSize();
            boost::shared_ptr<char[]> data = boost::make_shared<char[]>(size);
            message.SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
            m_communication->SendAll(data, size, m_dataIdentifier);
            
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
    void Coordinator::GotData(const boost::int64_t from, 
                              const boost::shared_ptr<char[]>& data, 
                              size_t size)
    {
        ElectionMessage message;
        message.ParseFromArray(data.get(), static_cast<int>(size));
        lllog(5) << "Got ElectionMessage (" 
                 << ToString(message.action())
                 << ", " 
                 << message.election_id() 
                 << ") from " << from << std::endl;
        
        m_strand.dispatch([this,message,from]
        {
            switch (message.action())
            {
            case INQUIRY: 
                {
                    //if we got an inquiry from someone smaller than us we send an alive
                    //and start a new election
                    if (from < m_id)
                    {
                        lllog(5) << "Got an inquiry from someone smaller than us, sending alive and starting election" << std::endl;
                        ElectionMessage aliveMsg;
                        aliveMsg.set_action(ALIVE);
                        aliveMsg.set_election_id(message.election_id());
                        const auto size = aliveMsg.ByteSize();
                        boost::shared_ptr<char[]> data = boost::make_shared<char[]>(size);
                        aliveMsg.SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
                        m_communication->SendTo(from, data, size, m_dataIdentifier);
            
                        StartElection();
                    }
                    
                }
                break;
            case ALIVE: 
                {
                    //if we got an alive from someone bigger than us we abandon the election
                    if (message.election_id() == m_currentElectionId &&
                        from > m_id)
                    {
                        lllog(5) << "Got alive from someone bigger than me (" 
                                 << from << "), abandoning election." << std::endl;
                        m_electionTimer.cancel();
                        m_currentElectionId = 0;
                    }
                }
                break;
            case VICTORY: 
                {
                    if (from > m_id)
                    {
                        lllog(4) << "New controller elected: " << from << std::endl;
                        //graciously accept their victory
                        m_elected = from;

                        //cancel any ongoing elections
                        m_electionTimer.cancel();
                        m_currentElectionId = 0;
                    }
                    else //No! We're going to usurp him! restart election
                    {
                        lllog(5) << "Got victory from someone smaller than me (" 
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
        lllog(4) << "Haha! There can be only one! Victory!" << std::endl;
        m_elected = m_id;
        m_currentElectionId = 0;

        ElectionMessage victoryMsg;
        victoryMsg.set_action(VICTORY);
        victoryMsg.set_election_id(m_currentElectionId);
        const auto size = victoryMsg.ByteSize();
        boost::shared_ptr<char[]> data = boost::make_shared<char[]>(size);
        victoryMsg.SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
        m_communication->SendAll(data, size, m_dataIdentifier);
    }

}
}
}
}

