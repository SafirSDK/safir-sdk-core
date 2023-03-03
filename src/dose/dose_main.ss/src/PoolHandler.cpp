/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#include <boost/make_shared.hpp>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/StateDeleter.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/ServiceTypes.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/Internal/EndStates.h>

#include "PoolHandler.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
    PoolHandler::PoolHandler(boost::asio::io_service::strand& strand,
                             Distribution& distribution,
                             const std::function<void(int64_t)>& checkPendingReg,
                             const std::function<void(const std::string& str)>& logStatus)
        :m_strand(strand)
        ,m_endStatesTimer(m_strand.get_io_service())
        ,m_distribution(distribution)
        ,m_log(logStatus)
        ,m_poolDistributor(m_strand.get_io_service(), m_distribution)
        ,m_poolDistributionRequests(m_strand.get_io_service(), m_distribution.GetCommunication())
        ,m_waitingStatesSanityTimer(m_strand.get_io_service())
        ,m_persistenceReady(false)
    {
        m_persistHandler.reset(new PersistHandler(m_strand.get_io_service(),
                                                  distribution,
                                                  logStatus,
                                                  [=] // persistentDataReadyCb
                                                  {
                                                      OnPersistenceReady();
                                                  },
                                                  [=] // persistentDataAllowedCb
                                                  {
                                                      Connections::Instance().AllowConnect(-1);
                                                  }));

        auto injectNode=[=](const std::string&, int64_t id, int64_t nt, const std::string&)
        {
            m_strand.dispatch([this, id, nt]
            {
                if (m_nodes.insert(std::make_pair(id, nt)).second)
                {
                    lllog(5) << L"DoseMain.PoolHandler InjectNode and send PD-request, nodeId=" << id << std::endl;
                    m_poolDistributionRequests.RequestPoolDistribution(id, nt);
                }
            });
        };

        auto excludeNode=[=](int64_t id, int64_t)
        {
            m_strand.post([this, id]
            {
                if (m_nodes.erase(id)>0)
                {
                    m_poolDistributionRequests.PoolDistributionFinished(id);
                    m_poolDistributor.RemovePoolDistribution(id);
                }

                m_waitingStates.NodeDown(id);
            });
        };

        //subscribe for included and excluded nodes
        distribution.SubscribeNodeEvents(injectNode, excludeNode);

        //set data receiver for pool distribution information
        m_distribution.GetCommunication().SetDataReceiver([=](int64_t fromNodeId, int64_t fromNodeType, const char *data, size_t size)
                                                          {
                                                              OnPoolDistributionInfo(fromNodeId, fromNodeType, data, size);
                                                          },
                                                          PoolDistributionInfoDataTypeId,
                                                          [=](size_t s){return new char[s];},
                                                          [](const char* data){ delete[] data;});

        //set data receiver for registration states
        m_distribution.GetCommunication().SetDataReceiver([=](int64_t fromNodeId, int64_t fromNodeType, const char *data, size_t size)
                                                          {
                                                              OnRegistrationState(fromNodeId, fromNodeType, data, size);
                                                          },
                                                          RegistrationStateDataTypeId,
                                                          [=](size_t s){return DistributionData::NewData(s);},
                                                          [](const char* data){DistributionData::DropReference(data);});

        //set data receiver for entity states
        m_distribution.GetCommunication().SetDataReceiver([=](int64_t fromNodeId, int64_t fromNodeType, const char *data, size_t size)
                                                          {
                                                              OnEntityState(fromNodeId, fromNodeType, data, size);
                                                          },
                                                          EntityStateDataTypeId,
                                                          [=](size_t s){return DistributionData::NewData(s);},
                                                          [](const char* data){DistributionData::DropReference(data);});

        //create one StateDistributor per nodeType
        for (auto nt = distribution.GetNodeTypeConfiguration().nodeTypesParam.cbegin();
             nt != distribution.GetNodeTypeConfiguration().nodeTypesParam.cend(); ++nt)
        {
            auto sd=std::unique_ptr<StateDistributorType>(new StateDistributorType(nt->id,
                                                                                   distribution,
                                                                                   m_strand,
                                                                                   checkPendingReg));
            m_stateDistributors.insert(std::make_pair(nt->id, std::move(sd)));
        }
    }

    void PoolHandler::OnPersistenceReady()
    {
        m_strand.dispatch([this]
        {
            if (m_persistenceReady)
            {
                return; //already got this event
            }

            m_persistenceReady = true;

            lllog(5) << L"DoseMain.PoolHandler Persistence data ready. Allow connections on all contexts." << std::endl;

            Connections::Instance().AllowConnect(-1);
            Connections::Instance().AllowConnect(0);

            m_nodeInfoHandler.reset(new NodeInfoHandler(m_strand.get_io_service(), m_distribution));
            m_poolDistributor.Start();
        });
    }

    void PoolHandler::Start()
    {
        m_strand.post([=]
        {
            m_persistHandler->Start();

            RunEndStatesTimer();
            RunWaitingStatesSanityCheckTimer();

            //request pool distributions
            m_poolDistributionRequests.Start();

            //start distributing states to other nodes
            for (auto vt = m_stateDistributors.cbegin(); vt != m_stateDistributors.cend(); ++vt)
            {
                vt->second->Start();
            }
        });
    }

    void PoolHandler::Stop()
    {
        m_strand.post([=]
        {
            if (m_nodeInfoHandler != nullptr)
            {
                m_nodeInfoHandler->Stop();
            }

            m_endStatesTimer.cancel();
            m_waitingStatesSanityTimer.cancel();

            m_persistHandler->Stop();

            //We are stopping so we dont care about receiving pool distributions anymore
            m_poolDistributionRequests.Stop();

            //Stop ongoing pool distributions
            m_poolDistributor.Stop();

            //stop distributing states to others
            for (auto vt = m_stateDistributors.cbegin(); vt != m_stateDistributors.cend(); ++vt)
            {
                vt->second->Stop();
            }
        });
    }

    void PoolHandler::HandleConnect(const ConnectionId& connId)
    {
        m_strand.post([this, connId]
        {
            auto this_ = this;

            m_waitingStates.PerformStatesWaitingForConnection(connId,
                                                              [this_](const DistributionData& state,
                                                                     int64_t fromNodeType)
            {
                if (state.GetType() == DistributionData::RegistrationState)
                {
                    this_->HandleRegistrationState(state, fromNodeType);
                }
                else if (state.GetType() == DistributionData::EntityState)
                {
                    this_->HandleEntityState(state, fromNodeType);
                }
                else
                {
                    ENSURE (false, <<
                            "PoolHandler::HandleConnect Expected a registration state or entity state!");
                }
            });
        });
    }

    void PoolHandler::HandleDisconnect(const ConnectionId& connId)
    {
        m_strand.post([this, connId]
        {
            m_waitingStates.Disconnect(connId);
        });
    }

    void PoolHandler::OnPoolDistributionInfo(int64_t fromNodeId, int64_t fromNodeType, const char *data, size_t /*size*/)
    {
        const PoolDistributionInfo pdInfo=*reinterpret_cast<const PoolDistributionInfo*>(data);
        delete[] data;

        m_strand.dispatch([=]
        {
            switch (pdInfo)
            {
            case PdRequest:
                {
                    lllog(5)<<"PoolHandler: got PdRequest from "<<fromNodeId<<std::endl;
                    //start new pool distribution to node
                    m_poolDistributor.AddPoolDistribution(fromNodeId, fromNodeType);
                }
                break;

            case PdComplete:
                {
                    
                    lllog(5)<<"PoolHandler: got PdComplete from "<<fromNodeId<<std::endl;
                    if (!m_persistenceReady)
                    {
                        m_persistHandler->SetPersistentDataReady(); //persistence is ready as soon as we have received one pdComplete
                    }
                    
                    m_poolDistributionRequests.PoolDistributionFinished(fromNodeId);
                }
                break;

            default:
                break;
            }
        });
    }

    void PoolHandler::OnRegistrationState(int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t /*size*/)
    {
        m_strand.post([=]
        {
            const auto state=DistributionData::ConstConstructor(new_data_tag, data);
            DistributionData::DropReference(data);

            ENSURE(!m_distribution.IsLocal(state.GetTypeId()),
                   << "Received Local RegistrationState of type " << state.GetTypeId()
                   << " from node " << fromNodeId << ", system configuration is bad!");

            ENSURE (state.GetType() == DistributionData::RegistrationState, <<
                    "PoolHandler::OnRegistrationState received DistributionData that is not a RegistrationState!");

            HandleRegistrationState(state, fromNodeType);
        });
    }

    void PoolHandler::OnEntityState(int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t /*size*/)
    {
        m_strand.post([=]
        {
            const auto state=DistributionData::ConstConstructor(new_data_tag, data);
            DistributionData::DropReference(data);

            ENSURE (state.GetType() == DistributionData::EntityState, <<
                    "PoolHandler::OnEntityState received DistributionData that is not a EntityState!");

            ENSURE(!m_distribution.IsLocal(state.GetTypeId()),
                   << "Received Local EntityState of type " << state.GetTypeId()
                   << " from node " << fromNodeId << ", system configuration is bad!");

            HandleEntityState(state, fromNodeType);
        });
    }

    void PoolHandler::RunEndStatesTimer()
    {
        Safir::Dob::Internal::EndStates::Instance().HandleTimeout();

        m_endStatesTimer.expires_from_now(boost::chrono::seconds(60));
        m_endStatesTimer.async_wait(m_strand.wrap([=](const boost::system::error_code& error)
        {
            if (!error)
            {
                RunEndStatesTimer();
            }
        }));
    }

    void PoolHandler::RunWaitingStatesSanityCheckTimer()
    {
        m_waitingStates.SanityCheck();

        /* We run this timer fairly infrequently, to reduce false warnings */
        m_waitingStatesSanityTimer.expires_from_now(boost::chrono::minutes(6));
        m_waitingStatesSanityTimer.async_wait(m_strand.wrap([=](const boost::system::error_code& error)
        {
            if (!error)
            {
                RunWaitingStatesSanityCheckTimer();
            }
        }));
    }

    void PoolHandler::HandleStatesWaitingForRegistration(const DistributionData& registrationState)
    {
        m_waitingStates.PerformStatesWaitingForRegistration
            (registrationState,
             [this](const DistributionData& state, int64_t fromNodeType)
             {
                 ENSURE(state.GetType() == DistributionData::EntityState, <<
                                "PoolHandler::HandleStatesWaitingForRegistration Expected an EntityState!");

                 HandleEntityState(state, fromNodeType);
             });
    }

    void PoolHandler::HandleRegistrationState(const DistributionData& state, int64_t fromNodeType)
    {
        if (state.IsRegistered()) //is a registration state
        {
            const ConnectionId senderId=state.GetSenderId();

            ENSURE(senderId.m_id != -1, << "Registration states are expected to have ConnectionId != -1! Reg for type "
                   << Safir::Dob::Typesystem::Operations::GetName(state.GetTypeId()));

            const ConnectionPtr connection = Connections::Instance().GetConnection(senderId, std::nothrow);

            bool remoteSetOk = false;

            if (Typesystem::Operations::IsOfType(state.GetTypeId(),Safir::Dob::Service::ClassTypeId))
            {
                remoteSetOk = ServiceTypes::Instance().RemoteSetRegistrationState(connection,state);
            }
            else
            {
                remoteSetOk = EntityTypes::Instance().RemoteSetRegistrationState(connection,state);
            }

            if (remoteSetOk)
            {
                HandleStatesWaitingForRegistration(state);
            }
            else
            {
                lllog(9) << "RemoteSetRegistrationState not successful, adding to waiting states" << std::endl;
                m_waitingStates.Add(state, fromNodeType);
            }
        }
        else //is an unregistration state
        {
            ENSURE(state.GetSenderId().m_id == -1, << "Unregistration states are expected to have ConnectionId == -1! Unreg for type "
                   << Safir::Dob::Typesystem::Operations::GetName(state.GetTypeId()));

            //unregistration states bypass all waitingstates handling and go straight
            //into shared memory
            if (Typesystem::Operations::IsOfType(state.GetTypeId(),Safir::Dob::Service::ClassTypeId))
            {
                ServiceTypes::Instance().RemoteSetRegistrationState(ConnectionPtr(), state);
            }
            else
            {
                EntityTypes::Instance().RemoteSetRegistrationState(ConnectionPtr(), state);
            }

            HandleStatesWaitingForRegistration(state);

            auto sdIt=m_stateDistributors.find(fromNodeType);
            if (sdIt!=m_stateDistributors.end())
            {
                sdIt->second->CheckForPending(state.GetTypeId());
            }
        }

        m_waitingStates.CleanUp(state);
    }

    void PoolHandler::HandleEntityState(const DistributionData& state, int64_t fromNodeType)
    {
        switch (state.GetEntityStateKind())
        {
        case DistributionData::Ghost:
            {
                ENSURE(state.GetSenderId().m_id == -1, << "Ghost states are expected to have ConnectionId == -1! Ghost for "
                       << state.GetEntityId());

                const RemoteSetResult result =
                        EntityTypes::Instance().RemoteSetRealEntityState(ConnectionPtr(), // Null connection for ghosts
                                                                         state);
                if (result == RemoteSetNeedRegistration)
                {
                    lllog(9) << "RemoteSetRealEntityState (missing reg) not successful,"
                             << " adding to waiting states" << std::endl;
                    m_waitingStates.Add(state, fromNodeType);
                }

            }
            break;
        case DistributionData::Injection:
            {
                ENSURE(state.GetSenderId().m_id == -1,
                       << "Injection states are expected to have ConnectionId == -1! Injection for "
                       << state.GetEntityId());
                EntityTypes::Instance().RemoteSetInjectionEntityState(state);
            }
            break;
        case DistributionData::Real:
            {
                if (state.IsCreated()) //handle created and delete states differently
                {
                    const ConnectionId senderId=state.GetSenderId();

                    ENSURE(senderId.m_id != -1, << "Entity states are expected to have ConnectionId != -1! State for "
                           << state.GetEntityId());

                    const ConnectionPtr connection = Connections::Instance().GetConnection(senderId, std::nothrow);

                    const RemoteSetResult result =
                            EntityTypes::Instance().RemoteSetRealEntityState(connection, state);

                    if (result == RemoteSetNeedRegistration || result == RemoteSetNeedConnection)
                    {
                        lllog(9) << "RemoteSetRealEntityState (result = " << result << ") not successful,"
                                 << " adding to waiting states" << std::endl;

                        m_waitingStates.Add(state, fromNodeType);
                    }
                }
                else
                {
                    ENSURE(state.GetSenderId().m_id == -1, << "Delete states are expected to have ConnectionId == -1! Delete for "
                           << state.GetEntityId());

                    RemoteSetResult result = EntityTypes::Instance().RemoteSetDeleteEntityState(state);

                    if (result == RemoteSetNeedRegistration)
                    {
                        lllog(9) << "RemoteSetRealEntityState (missing reg) not successful,"
                                 << " adding to waiting states" << std::endl;
                        m_waitingStates.Add(state, fromNodeType);
                    }
                }
            }
            break;
        }
    }
}
}
}
