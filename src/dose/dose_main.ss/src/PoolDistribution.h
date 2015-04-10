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
#pragma once
#include <queue>
#include <functional>
#include <boost/make_shared.hpp>
#include <boost/asio.hpp>
#include <boost/chrono.hpp>
#include <boost/asio/steady_timer.hpp>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/ConnectionAspectInjector.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    ///
    /// Handles a single pool distribution to a specific node. This class is only to be used by
    /// PoolDistributor. To start a new pool distribution to a node, use PoolDistributor.AddPoolDistribution.
    ///
    template <class DistributionT>
    class PoolDistribution
            :public Safir::Dob::EntitySubscriber
            ,public Safir::Dob::RegistrationSubscriber
            ,public Safir::Dob::Dispatcher
    {
    public:
        PoolDistribution(int64_t nodeId,
                         int64_t nodeType,
                         boost::asio::io_service::strand& strand,
                         DistributionT& distribution,
                         const std::function<void(int64_t)>& completionHandler)
            :m_nodeId(nodeId)
            ,m_nodeType(nodeType)
            ,m_strand(strand)
            ,m_timer(strand.get_io_service())
            ,m_distribution(distribution)
            ,m_completionHandler(completionHandler)
        {
        }

        void Run()
        {
            m_strand.dispatch([=]
            {
                m_running=true;
                std::wcout<<L"Run PD to "<<m_nodeId<<std::endl;
                //collect all connections on this node
                Connections::Instance().ForEachConnection([=](const Connection& connection)
                {
                    //auto notDoseConnection=std::string(connection.NameWithoutCounter()).find(";dose_main;")==std::string::npos;
                    auto localContext=Safir::Dob::NodeParameters::LocalContexts(connection.Id().m_contextId);
                    auto connectionOnThisNode=connection.IsLocal();

                    //std::wcout<<std::boolalpha<<L"Pd_con id="<<connection.Id().m_id<<L", notLocalContext="<<notLocalContext<<L" notDoseCon="<<notDoseConnection<<L" conOnThis="<<connectionOnThisNode<<std::endl;
                    if (!localContext && connectionOnThisNode)
                    {
                        std::wcout<<L"PD_CON: "<<connection.NameWithoutCounter()<<std::endl;
                        m_connections.push(DistributionData(connect_message_tag,
                                                            connection.Id(),
                                                            connection.NameWithoutCounter(),
                                                            connection.Counter()));
                    }
                    else
                        std::wcout<<L"PD_CON SKIP: "<<connection.NameWithoutCounter()<<std::endl;
                });

                SendConnections(); //will trigger the sequence SendConnections -> SendStates() -> SendPdComplete()
            });
        }

        void Cancel()
        {
            std::wcout<<"Cancel a pd"<<std::endl;
            m_strand.dispatch([=]
            {
                m_running=false;
                m_timer.cancel();
            });
        }

        int64_t Id() const {return m_nodeId;}

    private:
        static const int64_t PoolDistributionInfoDataTypeId=-3446507522969672286; //DoseMain.PoolDistributionInfo
        static const int64_t ConnectionMessageDataTypeId=4477521173098643793; //DoseMain.ConnectionMessage
        static const int64_t RegistrationStateDataTypeId=6915466164769792349; //DoseMain.RegistrationState
        static const int64_t EntityStateDataTypeId=5802524208372516084; //DoseMain.EntityState

        int64_t m_nodeId;
        int64_t m_nodeType;
        boost::asio::io_service::strand& m_strand;
        boost::asio::steady_timer m_timer;
        DistributionT& m_distribution;
        std::function<void(int64_t)> m_completionHandler;
        std::queue<DistributionData> m_connections;
        Safir::Dob::Connection m_dobConnection;

        bool m_running=false;

        void SendConnections()
        {
            if (!m_running)
            {
                std::wcout<<L"SendConnections not running"<<std::endl;
                return;
            }

            std::wcout<<L"SendConnections"<<std::endl;
            while (CanSend() && !m_connections.empty())
            {
                const DistributionData& d=m_connections.front();
                boost::shared_ptr<const char[]> p(d.GetReference(), [=](const char* ptr){DistributionData::DropReference(ptr);});

                if (m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, p, d.Size(), ConnectionMessageDataTypeId, true))
                {
                    m_connections.pop();
                }
                else
                {
                    break;
                }
            }

            if (m_connections.empty())
            {
                m_strand.post([=]
                {
                    SendStates(0);
                });
            }
            else
            {
                SetTimer([=]{SendConnections();});
            }
        }

        void SendStates(int context)
        {
            if (!m_running)
            {
                std::wcout<<L"SendStates not running"<<std::endl;
                return;
            }

            std::wcout<<L"SendStates"<<std::endl;
            if (context>=Safir::Dob::NodeParameters::NumberOfContexts())
            {
                std::wcout<<L"SendStates 0"<<std::endl;
                m_strand.post([=]{SendPdComplete();});
                return;
            }

            std::wcout<<L"SendStates 1"<<std::endl;
            if (m_dobConnection.IsOpen())
            {
                std::wcout<<L"SendStates 2"<<std::endl;
                m_dobConnection.Close();
            }

            std::wcout<<L"SendStates 3"<<std::endl;

            //Note the connection name, we do NOT want to get special treatment for being
            //in dose_main, since we're a different thread. But we want to be allowed in
            //always, so don't change connection string "dose_main_pd" because it is always allowed to connect,
            // even before we let -1 connections in.
            m_dobConnection.Open(L"dose_main_pd",L"pool_distribution", context, NULL, this);

            std::wcout<<L"SendStates 4"<<std::endl;


            m_dobConnection.SubscribeRegistration(Entity::ClassTypeId,
                                                  Typesystem::HandlerId::ALL_HANDLERS,
                                                  true, //includeSubclasses
                                                  false, //restartSubscription
                                                  this);

            std::wcout<<L"SendStates 5"<<std::endl;

            m_dobConnection.SubscribeRegistration(Service::ClassTypeId,
                                                  Typesystem::HandlerId::ALL_HANDLERS,
                                                  true, //includeSubclasses
                                                  false, //restartSubscription
                                                  this);

            std::wcout<<L"SendStates 6"<<std::endl;

            ConnectionAspectInjector(m_dobConnection).SubscribeEntity(Entity::ClassTypeId,
                                                                      false, //includeUpdates,
                                                                      true,  //includeSubclasses
                                                                      false, //restartSubscription
                                                                      false, //wantsGhostDelete
                                                                      false, //wantsLastState
                                                                      false, //doesntWantSourceIsPermanentStore
                                                                      true,  //wantsAllStateChanges
                                                                      false, //timestampChangeInfo
                                                                      this);

            std::wcout<<L"SendStates 7"<<std::endl;

            auto connectionName=ConnectionAspectMisc(m_dobConnection).GetConnectionName();

            std::wcout<<L"SendStates 8"<<std::endl;

            auto conPtr=Connections::Instance().GetConnectionByName(Typesystem::Utilities::ToUtf8(connectionName));

            std::wcout<<L"SendStates 9"<<std::endl;

            DispatchStates(conPtr, context);

            std::wcout<<L"SendStates 10"<<std::endl;
        }

        void DispatchStates(const Safir::Dob::Internal::ConnectionPtr& conPtr, int context)
        {
            if (!m_running)
            {
                std::wcout<<L"DispStates not running"<<std::endl;
                return;
            }

            std::wcout<<L"DispatchStates"<<std::endl;
            bool overflow=false;
            conPtr->GetDirtySubscriptionQueue().Dispatch([this, conPtr, context, &overflow](const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove)
            {
                dontRemove=false;
                DistributionData realState = subscription->GetState()->GetRealState();
                if (!realState.IsNoState() && !m_distribution.IsLocal(realState.GetTypeId()))
                {
                    if (realState.GetType()==DistributionData::RegistrationState)
                    {
                        // Registration state
                        dontRemove=!subscription->DirtyFlag().Process([this, &subscription]{return ProcessRegistrationState(subscription);});
                    }
                    else
                    {
                        // Entity state
                        dontRemove=!subscription->DirtyFlag().Process([this, &subscription]{return ProcessEntityState(subscription);});
                    }
                }
                //dontRemove is true if we got an overflow, and if we did we dont want to keep sending anything to dose_com.
                exitDispatch = dontRemove;
                overflow=exitDispatch;
            });

            if (overflow)
            {
                SetTimer([=]{DispatchStates(conPtr, context);});
            }
            else //continue with next context
            {
                m_strand.dispatch([=]{SendStates(context+1);});
            }
        }

        void SendPdComplete()
        {
            if (!m_running)
            {
                std::wcout<<L"SendPdComp not running"<<std::endl;
                return;
            }

            std::wcout<<L"SendPdComplete"<<std::endl;
            m_dobConnection.Close();

            auto req=boost::make_shared<char[]>(sizeof(PoolDistributionInfo));
            (*reinterpret_cast<PoolDistributionInfo*>(req.get()))=PoolDistributionInfo::PdComplete;

            if (!m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, req, sizeof(PoolDistributionInfo), PoolDistributionInfoDataTypeId, true))
            {
                SetTimer([=]{SendPdComplete();});
            }
        }

        bool ProcessEntityState(const SubscriptionPtr& subscription)
        {
            if (!m_running)
            {
                std::wcout<<L"ProcessEntityState not running"<<std::endl;
                return true;
            }
            // All nodes send ghost and injection data on PD!
            // Do not send updates

            std::wcout<<L"ProcessEntityState"<<std::endl;
            bool success=true;

            //Real state
            if (subscription->GetLastRealState().IsNoState())
            {
                const DistributionData currentState = subscription->GetCurrentRealState();

                if (!currentState.IsNoState())
                {
                    //Send all local states
                    //send all ghosts (the owner node is probably down...)
                    //send all delete states (so that new nodes get the correct timestamps)
                    if (currentState.GetSenderId().m_node==m_distribution.GetCommunication().Id() ||
                            currentState.GetEntityStateKind() == DistributionData::Ghost ||
                            !currentState.HasBlob())
                    {
                        if (!CanSend() || !m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, ToPtr(currentState), currentState.Size(), EntityStateDataTypeId, true))
                        {
                            success=false;
                        }
                    }
                }
                subscription->SetLastRealState(currentState);
            }

            //injection state
            if (subscription->GetLastInjectionState().IsNoState())
            {
                const DistributionData currentState = subscription->GetCurrentInjectionState();

                if (!currentState.IsNoState())
                {
                    if (!CanSend() || !m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, ToPtr(currentState), currentState.Size(), EntityStateDataTypeId, true))
                    {
                        success=false;
                    }
                    subscription->SetLastInjectionState(currentState);
                }
            }

            return success;
        }

        bool ProcessRegistrationState(const SubscriptionPtr& subscription)
        {
            bool success=true;

            if (subscription->GetLastRealState().IsNoState())
            {
                const DistributionData state = subscription->GetCurrentRealState();


                if (!state.IsNoState())
                {
                    //Local states are to be sent to other nodes.
                    //Unregistration states (regardless of whether they are local or not) are always sent.
                    if (state.GetSenderId().m_node==m_distribution.GetCommunication().Id() ||
                            !state.IsRegistered())
                    {
                        if (!CanSend() || !m_distribution.GetCommunication().Send(0, m_nodeType, ToPtr(state), state.Size(), RegistrationStateDataTypeId, true))
                        {
                            success=false;
                        }
                    }
                }
                subscription->SetLastRealState(state); //update this so we only dispatch this state once (see "if" above)
            }
            return success;
        }

        bool CanSend() const
        {
            static const size_t threshold=m_distribution.GetCommunication().SendQueueCapacity(m_nodeType)/2;
            return m_distribution.GetCommunication().NumberOfQueuedMessages(m_nodeType)<threshold;
        }

        void SetTimer(const std::function<void()>& completionHandler)
        {
            if (!m_running)
                return;

            m_timer.expires_from_now(boost::chrono::milliseconds(10));
            m_timer.async_wait(m_strand.wrap([=](const boost::system::error_code&)
            {
                if (m_running)
                    completionHandler();
            }));
        }

        static inline boost::shared_ptr<const char[]> ToPtr(const DistributionData& d)
        {
            boost::shared_ptr<const char[]> p(d.GetReference(), [=](const char* ptr){DistributionData::DropReference(ptr);});
            return p;
        }

        //Dummy consumer
        void OnDoDispatch() override {}
        void OnRegistered(const Safir::Dob::Typesystem::TypeId, const Safir::Dob::Typesystem::HandlerId&) override {}
        void OnUnregistered(const Safir::Dob::Typesystem::TypeId, const Safir::Dob::Typesystem::HandlerId&) override {}
        void OnNewEntity(const Safir::Dob::EntityProxy) override {}
        void OnUpdatedEntity(const Safir::Dob::EntityProxy) override {}
        void OnDeletedEntity(const Safir::Dob::EntityProxy, const bool) override {}
    };
}
}
}
