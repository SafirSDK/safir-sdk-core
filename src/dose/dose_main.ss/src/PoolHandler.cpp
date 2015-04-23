/******************************************************************************
*
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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
        ,m_persistHandler(m_strand.get_io_service(), distribution, logStatus, [=]{OnPersistenceReady();}, // persistentDataReadyCb
                                                       [=]{Connections::Instance().AllowConnect(-1);}) // persistentDataAllowedCb
    {
        auto injectNode=[=](const std::string&, int64_t id, int64_t nt, const std::string&)
        {
            m_strand.dispatch([=]
            {
                if (m_nodes.insert(std::make_pair(id, nt)).second)
                {
                    if (!m_poolDistributionComplete || !m_persistensReady)
                    {
                        m_poolDistributionComplete=false;
                        m_poolDistributionRequests.RequestPoolDistribution(id, nt);
                    }
                }
            });
        };

        auto excludeNode=[=](int64_t id, int64_t)
        {
            m_strand.dispatch([=]
            {
                if (m_nodes.erase(id)>0)
                {
                    m_poolDistributionRequests.PoolDistributionFinished(id);
                    m_poolDistributor.RemovePoolDistribution(id);
                }
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
                                                          [=](size_t s){return new char[s];});

        //set data receiver for registration states
        m_distribution.GetCommunication().SetDataReceiver([=](int64_t fromNodeId, int64_t fromNodeType, const char *data, size_t size)
                                                          {
                                                              OnRegistrationState(fromNodeId, fromNodeType, data, size);
                                                          },
                                                          RegistrationStateDataTypeId,
                                                          [=](size_t s){return DistributionData::NewData(s);});

        //set data receiver for entity states
        m_distribution.GetCommunication().SetDataReceiver([=](int64_t fromNodeId, int64_t fromNodeType, const char *data, size_t size)
                                                          {
                                                              OnEntityState(fromNodeId, fromNodeType, data, size);
                                                          },
                                                          EntityStateDataTypeId,
                                                          [=](size_t s){return DistributionData::NewData(s);});

        //create one StateDistributor per nodeType
        for (auto& nt : distribution.GetNodeTypeConfiguration().nodeTypesParam)
        {
            auto sd=std::unique_ptr<StateDistributorType >(new StateDistributorType(nt.id, distribution, m_strand, checkPendingReg));
            m_stateDistributors.emplace(nt.id, std::move(sd));
        }
    }

    void PoolHandler::OnPersistenceReady()
    {
        m_strand.dispatch([=]
        {
            if (m_persistensReady)
            {
                return; //already got this event
            }

            if (m_numReceivedPdComplete==0) //got persistence from Dope
            {
                m_poolDistributionRequests.PoolDistributionFinished(0); //clear all, we dont care anymore for other nodes pools
            }
            else //got persistence from other node
            {
                m_poolDistributor.SetHaveNothing(); //tell poolDistributor that until we are started, we have no pool or persistence to provide
            }
            m_persistensReady=true;
            SignalPdComplete();

        });
    }

    void PoolHandler::Start()
    {
        m_strand.dispatch([=]
        {
            m_persistHandler.Start();

            RunEndStatesTimer();

            //request pool distributions
            m_poolDistributionRequests.Start(m_strand.wrap([=]
            {
                m_poolDistributionComplete=true;
                SignalPdComplete();
            }));

            //start distributing states to other nodes
            for (auto& vt : m_stateDistributors)
            {
                vt.second->Start();
            }
        });
    }

    void PoolHandler::Stop()
    {
        m_strand.post([=]
        {
            m_endStatesTimer.cancel();

            m_persistHandler.Stop();

            //We are stopping so we dont care about receiving pool distributions anymore
            m_poolDistributionRequests.Stop();

            //Stop ongoing pool distributions
            m_poolDistributor.Stop();

            //stop distributing states to others
            for (auto& vt : m_stateDistributors)
            {
                vt.second->Stop();
            }
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
            case PoolDistributionInfo::PdRequest:
            {
                lllog(5)<<"PoolHandler: got PdRequest from "<<fromNodeId<<std::endl;
                //start new pool distribution to node
                m_poolDistributor.AddPoolDistribution(fromNodeId, fromNodeType);
            }
                break;

            case PoolDistributionInfo::PdComplete:
            {
                lllog(5)<<"PoolHandler: got PdComplete from "<<fromNodeId<<std::endl;
                ++m_numReceivedPdComplete;
                m_persistHandler.SetPersistentDataReady(); //persistens is ready as soon as we have received one pdComplete
                m_poolDistributionRequests.PoolDistributionFinished(fromNodeId);
            }
                break;

            case PoolDistributionInfo::PdHaveNothing:
            {
                lllog(5)<<"PoolHandler: got PdHaveNothing from "<<fromNodeId<<std::endl;
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

            if (state.IsRegistered()) //is a registration state
            {
                lllog(1)<<"OnRegistrationState - "<<Safir::Dob::Typesystem::Operations::GetName(state.GetTypeId())<<", handler "<<state.GetHandlerId()<<std::endl;
                const ConnectionId senderId=state.GetSenderId();

                ENSURE(senderId.m_id != -1, << "Registration states are expected to have ConnectionId != -1! Reg for type "
                       << Safir::Dob::Typesystem::Operations::GetName(state.GetTypeId()));

                const ConnectionPtr connection = Connections::Instance().GetConnection(senderId, std::nothrow);

                if (connection!=nullptr)
                {
                    if (Typesystem::Operations::IsOfType(state.GetTypeId(),Safir::Dob::Service::ClassTypeId))
                    {
                        ServiceTypes::Instance().RemoteSetRegistrationState(connection,state);
                    }
                    else
                    {
                        EntityTypes::Instance().RemoteSetRegistrationState(connection,state);
                    }
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

                auto sdIt=m_stateDistributors.find(fromNodeType);
                if (sdIt!=m_stateDistributors.end())
                {
                    sdIt->second->CheckForPending(state.GetTypeId());
                }
            }
        });
    }

    void PoolHandler::OnEntityState(int64_t fromNodeId, int64_t /*fromNodeType*/, const char* data, size_t /*size*/)
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

            switch (state.GetEntityStateKind())
            {
            case DistributionData::Ghost:
            {
                ENSURE(state.GetSenderId().m_id == -1, << "Ghost states are expected to have ConnectionId == -1! Ghost for "
                       << state.GetEntityId());

                EntityTypes::Instance().RemoteSetRealEntityState(ConnectionPtr(), // Null connection for ghosts
                                                                 state);
            }
                break;
            case DistributionData::Injection:
            {
                ENSURE(state.GetSenderId().m_id == -1, << "Injection states are expected to have ConnectionId == -1! Injection for "
                       << state.GetEntityId());
                EntityTypes::Instance().RemoteSetInjectionEntityState(state);
            }
                break;
            case DistributionData::Real:
            {
                lllog(1)<<"OnEntityState - "<<state.GetEntityId().ToString()<<", handler "<<state.GetHandlerId()<<std::endl;
                if (state.IsCreated()) //handle created and delete states differently
                {
                    const ConnectionId senderId=state.GetSenderId();

                    ENSURE(senderId.m_id != -1, << "Entity states are expected to have ConnectionId != -1! State for "
                           << state.GetEntityId());

                    const ConnectionPtr connection = Connections::Instance().GetConnection(senderId, std::nothrow);
                    if (connection!=nullptr)
                    {
                        EntityTypes::Instance().RemoteSetRealEntityState(connection, state);
                    }
                    else
                    {
                        //auto name=Safir::Dob::Typesystem::Operations::GetName(state.GetTypeId());
                        //throw std::logic_error(Safir::Dob::Typesystem::Utilities::ToUtf8(name)+", unknown con");
                        //throw std::logic_error("Received acked entity with an unknown connection ");
                    }
                }
                else
                {
                    ENSURE(state.GetSenderId().m_id == -1, << "Delete states are expected to have ConnectionId == -1! Delete for "
                           << state.GetEntityId());

                    EntityTypes::Instance().RemoteSetDeleteEntityState(state);
                }
            }
                break;
            }
        });
    }

    void PoolHandler::SignalPdComplete()
    {
        if (m_persistensReady && m_poolDistributionComplete && !m_pdCompleteSignaled)
        {
            lllog(1)<<"PD complete"<<std::endl;
            m_pdCompleteSignaled=true;
            m_poolDistributor.Start();
            Connections::Instance().AllowConnect(-1);
            Connections::Instance().AllowConnect(0);
        }
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
}
}
}
