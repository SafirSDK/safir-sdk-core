/******************************************************************************
*
* Copyright Saab AB, 2023 (http://safirsdkcore.com)
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
#include <Safir/Dob/Internal/DistributionScopeReader.h>

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
        ,m_endStatesTimer(m_strand.context())
        ,m_distribution(distribution)
        ,m_log(logStatus)
        ,m_poolDistributor(m_strand.context(), m_distribution)
        ,m_poolDistributionRequests(m_strand.context(), m_distribution, [this]{OnAllPoolsReceived();})
        ,m_waitingStatesSanityTimer(m_strand.context())
        ,m_persistenceReady(false)
        ,m_nodeState(m_distribution.IsLightNode() ? Safir::Dob::NodeState::Detached : Safir::Dob::NodeState::Normal)
        ,m_running(false)
    {
        if (!m_distribution.IsLightNode())
        {
            m_persistHandler.reset(new PersistHandler(m_strand.context(),
                                                      distribution,
                                                      logStatus,
                                                      [this](bool fromDope) // persistentDataReadyCb
                                                      {
                                                          OnPersistenceReady(fromDope);
                                                      },
                                                      [] // persistentDataAllowedCb
                                                      {
                                                          Connections::Instance().AllowConnect(-1);
                                                      }));
        }

        auto injectNode=[this](const std::string&, int64_t id, int64_t nt, const std::string&)
        {
            m_strand.dispatch([this, id, nt]
            {
                if (m_nodes.insert(std::make_pair(id, nt)).second)
                {
                    lllog(5)<< L"DoseMain.PoolHandler InjectNode and send PD-request, nodeId=" << id << std::endl;
                    m_poolDistributionRequests.RequestPoolDistribution(id, nt);
                }
            });
        };

        auto excludeNode=[this](int64_t id, int64_t)
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
        m_distribution.GetCommunication().SetDataReceiver([this](int64_t fromNodeId, int64_t fromNodeType, const char *data, size_t size)
                                                          {
                                                              OnPoolDistributionInfo(fromNodeId, fromNodeType, data, size);
                                                          },
                                                          PoolDistributionInfoDataTypeId,
                                                          [](size_t s){return new char[s];},
                                                          [](const char* data){ delete[] data;});

        //set data receiver for registration states
        m_distribution.GetCommunication().SetDataReceiver([this](int64_t fromNodeId, int64_t fromNodeType, const char *data, size_t size)
                                                          {
                                                              OnRegistrationState(fromNodeId, fromNodeType, data, size);
                                                          },
                                                          RegistrationStateDataTypeId,
                                                          [](size_t s){return DistributionData::NewData(s);},
                                                          [](const char* data){DistributionData::DropReference(data);});

        //set data receiver for entity states
        m_distribution.GetCommunication().SetDataReceiver([this](int64_t fromNodeId, int64_t fromNodeType, const char *data, size_t size)
                                                          {
                                                              OnEntityState(fromNodeId, fromNodeType, data, size);
                                                          },
                                                          EntityStateDataTypeId,
                                                          [](size_t s){return DistributionData::NewData(s);},
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

    void PoolHandler::Start()
    {
        m_strand.post([this]
        {
            if (m_running)
            {
                return;
            }
            lllog(5)<< L"DoseMain.PoolHandler Start" << std::endl;
            m_running = true;

            if (m_distribution.IsLightNode())
            {
                // This will immediately allow connections and start the poolDistributor.
                m_nodeInfoHandler.reset(new NodeInfoHandler(m_strand.context(), m_distribution, Safir::Dob::NodeState::Detached));
                m_poolDistributor.Start();
                m_persistenceReady=true;
                Connections::Instance().AllowConnect(-1);
                Connections::Instance().AllowConnect(0);
            }
            else
            {
                // For normal nodes we must wait for persistence.
                m_persistHandler->Start();
            }

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

    void PoolHandler::Stop(const std::function<void()>& onPoolDistributionsCancelled)
    {
#if (!defined NDEBUG && !defined SAFIR_DISABLE_CHECK_STRAND)
        ENSURE(m_strand.running_in_this_thread(), << "PoolHandler::Stop must be called from within the correct strand!");
#endif
        if (!m_running)
        {
            return;
        }
        m_running = false;

        if (m_nodeInfoHandler != nullptr)
        {
            m_nodeInfoHandler->Stop();
        }

        m_endStatesTimer.cancel();
        m_waitingStatesSanityTimer.cancel();

        if (m_persistHandler != nullptr)
        {
            m_persistHandler->Stop();
        }

        //We are stopping so we dont care about receiving pool distributions anymore
        m_poolDistributionRequests.Stop();

        //stop distributing states to others
        for (auto vt = m_stateDistributors.cbegin(); vt != m_stateDistributors.cend(); ++vt)
        {
            vt->second->Stop();
        }

        //Stop ongoing pool distributions
        m_poolDistributor.Stop(onPoolDistributionsCancelled);
    }

    void PoolHandler::SetDetached(bool detach)
    {
        m_strand.post([this, detach]
        {
            lllog(5)<< L"DoseMain.PoolHandler set detached to " << std::boolalpha << detach << std::endl;

            auto isCurrentlyDetached = m_nodeState == Safir::Dob::NodeState::Detached;
            if (detach == isCurrentlyDetached)
            {
                // No change of state
                return;
            }

            m_nodeState = detach ? Safir::Dob::NodeState::Detached : Safir::Dob::NodeState::Attaching;

            // When a lightNode attach to a System it can not keep EndStates since they
            // will prevent pooldistribution of Entities that previously existed on this node.
            EndStates::Instance().ClearAllEndstates();

            // If the node is detached from the start, nodeInfoHandler is not created here.
            if (m_nodeInfoHandler)
            {
                m_nodeInfoHandler->SetNodeState(m_nodeState); // Update state flag in NodeInfo
            }
        });
    }

    void PoolHandler::OnPersistenceReady(bool fromDope)
    {
        ENSURE(!m_distribution.IsLightNode(), <<"DoseMain.PoolHandler: Got OnPersistenceReady. Should not happen on a light node!")
        m_strand.dispatch([this, fromDope]
        {
            if (m_persistenceReady)
            {
                return; //already got this event
            }

            m_persistenceReady=true;

            if (fromDope)
            {
                lllog(5)<< L"DoseMain.PoolHandler Persistence data ready, got Persistence from Dope. Allow connections on all contexts." << std::endl;
            }
            else
            {
                lllog(5)<< L"DoseMain.PoolHandler Persistence data ready, got Persistence from other node via PD. Allow connections on all contexts." << std::endl;
            }

            Connections::Instance().AllowConnect(-1);
            Connections::Instance().AllowConnect(0);
            m_nodeInfoHandler.reset(new NodeInfoHandler(m_strand.context(), m_distribution, Safir::Dob::NodeState::Normal));
            m_poolDistributor.Start();
        });
    }

    void PoolHandler::HandleConnect(const ConnectionId& connId)
    {
        m_strand.post([this, connId]
        {
            m_waitingStates.PerformStatesWaitingForConnection(connId,
                                                              [this](const DistributionData& state,
                                                                     int64_t fromNodeType)
            {
                if (state.GetType() == DistributionData::RegistrationState)
                {
                    HandleRegistrationState(state, fromNodeType);
                }
                else if (state.GetType() == DistributionData::EntityState)
                {
                    HandleEntityState(state, fromNodeType);
                }
                else
                {
                    ENSURE (false, <<"PoolHandler::HandleConnect Expected a registration state or entity state!");
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

    void PoolHandler::OnPoolDistributionInfo(int64_t fromNodeId, int64_t fromNodeType, const char *data, size_t size)
    {
        auto pdInfo = std::make_shared<Pd::PoolSyncInfo>();
        bool parsedOk = pdInfo->ParseFromArray(static_cast<const void*>(data), static_cast<int>(size));
        delete[] data;

        if (!parsedOk)
        {
            lllog(4) << L"PoolHandler: Received corrupt poolDistributionInfo data."<<std::endl;
            return;
        }

        m_strand.dispatch([this, fromNodeType, fromNodeId, pdInfo]
        {
            switch (pdInfo->messagetype())
            {
            case Pd::PoolSyncInfo_PdMsgType::PoolSyncInfo_PdMsgType_PdRequest:
                {
                    lllog(5)<<"PoolHandler: got PdRequest from "<<fromNodeId<<std::endl;
                    auto syncState = std::make_shared<SmartSyncState>();
                    for (const auto& c : pdInfo->connections())
                    {
                        syncState->connections.push_back(SmartSyncState::Connection{c.connection_id(), c.context(), c.counter(), {}, c.name()});
                        auto& smCon = syncState->connections.back();
                        for (const auto& r : c.registrations())
                        {
                            smCon.registrations.push_back(SmartSyncState::Registration{r.type_id(), r.handler_id(), r.registration_time(), {}, &smCon});
                            auto& smReg = smCon.registrations.back();

                            for (const auto& e : r.entities())
                            {
                                smReg.entities.push_back(SmartSyncState::Entity{ e.instance_id(), e.version(), e.creation_time(), &smReg });
                            }
                        }
                    }
                     
                    //start new pool distribution to node
                    m_poolDistributor.AddPoolDistribution(fromNodeId, fromNodeType, syncState);
                }
                break;

            case Pd::PoolSyncInfo_PdMsgType::PoolSyncInfo_PdMsgType_PdComplete:
                {
                    lllog(5)<<"PoolHandler: got PdComplete from "<<fromNodeId<<std::endl;
                    if (!m_distribution.IsLightNode() && !m_distribution.IsLightNode(fromNodeType) && !m_persistenceReady)
                    {
                        //persistence is ready as soon as we have received one pdComplete from a non-lightNode
                        // This will trigger the chain OnPersistenDataReady
                        m_persistHandler->SetPersistentDataReady(false); // false indicates that we did not get Persistence from Dope
                    }

                    // If this node is a light node, the smartSync has made sure the other node only sent the changes during PD.
                    // Any unchanged states must now be updated to not be marked as detached anymore.
                    if (m_distribution.IsLightNode())
                    {
                        Connections::Instance().SetDetachFlagForConnectionsFromNode(fromNodeId, false);
                    }

                    m_poolDistributionRequests.PoolDistributionFinished(fromNodeId);
                }
                break;

            default:
                break;
            }
        });
    }

    void PoolHandler::OnAllPoolsReceived()
    {
        lllog(5) << L"PoolHandler - all pools received" << std::endl;

        if (m_nodeState == Safir::Dob::NodeState::Attaching)
        {
            // Clear all detached states that are still left. If there still exists any detached states after all pools have been received, those states must
            // belong to nodes that were part of the system before we became detached, but they left the system while we were detached and is not part of
            // the system anymore.
            Connections::Instance().RemoveDetachedConnections();

            m_nodeState = Safir::Dob::NodeState::Attached;
            if (m_nodeInfoHandler)
            {
                lllog(5) << L"PoolHandler - change NodeState from Attaching to Attached" << std::endl;
                m_nodeInfoHandler->SetNodeState(Safir::Dob::NodeState::Attached);
            }
        }
    }

    void PoolHandler::OnRegistrationState(int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t /*size*/)
    {
        m_strand.post([this,data,fromNodeId,fromNodeType]
        {
            const auto state=DistributionData::ConstConstructor(new_data_tag, data);
            DistributionData::DropReference(data);

            ENSURE(!DistributionScopeReader::Instance().IsLocal(state.GetTypeId()),
                   << "Received Local RegistrationState of type " << state.GetTypeId()
                   << " from node " << fromNodeId << ", system configuration is bad!");

            ENSURE (state.GetType() == DistributionData::RegistrationState, <<
                    "PoolHandler::OnRegistrationState received DistributionData that is not a RegistrationState!");

            lllog(7) << "PoolHandler - Received RegistrationState from nodeId=" << fromNodeId << std::endl << state.Image() << std::endl;
            HandleRegistrationState(state, fromNodeType);
        });
    }

    void PoolHandler::OnEntityState(int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t /*size*/)
    {
        m_strand.post([this,data,fromNodeId,fromNodeType]
        {
            const auto state=DistributionData::ConstConstructor(new_data_tag, data);
            DistributionData::DropReference(data);

            ENSURE (state.GetType() == DistributionData::EntityState, <<
                    "PoolHandler::OnEntityState received DistributionData that is not a EntityState!");

            ENSURE(!DistributionScopeReader::Instance().IsLocal(state.GetTypeId()),
                   << "Received Local EntityState of type " << state.GetTypeId()
                   << " from node " << fromNodeId << ", system configuration is bad!");

            HandleEntityState(state, fromNodeType);
        });
    }

    void PoolHandler::RunEndStatesTimer()
    {
        Safir::Dob::Internal::EndStates::Instance().HandleTimeout();

        m_endStatesTimer.expires_from_now(boost::chrono::seconds(60));

        m_endStatesTimer.async_wait([this](const boost::system::error_code& error)
        {
            if (!error && m_running)
            {
                RunEndStatesTimer();
            }
        });
    }

    void PoolHandler::RunWaitingStatesSanityCheckTimer()
    {
        m_waitingStates.SanityCheck();

        /* We run this timer fairly infrequently, to reduce false warnings */
        m_waitingStatesSanityTimer.expires_from_now(boost::chrono::minutes(6));
        m_waitingStatesSanityTimer.async_wait([this](const boost::system::error_code& error)
        {
            if (!error && m_running)
            {
                RunWaitingStatesSanityCheckTimer();
            }
        });
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
                lllog(9) << "PoolHandler: RemoteSetRegistrationState not successful, adding to waiting states" << std::endl;
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
                    lllog(9) << "PoolHandler: RemoteSetRealEntityState (missing reg) not successful,"
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
                        lllog(9) << "PoolHandler: RemoteSetRealEntityState (result = " << result << ") not successful,"
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
                        lllog(9) << "PoolHandler: RemoteSetRealEntityState (missing reg) not successful,"
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
