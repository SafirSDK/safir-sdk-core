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
#pragma once
#include <queue>
#include <functional>
#include <boost/lexical_cast.hpp>
#include <boost/asio.hpp>
#include <boost/chrono.hpp>
#include <boost/asio/steady_timer.hpp>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/ConnectionAspectInjector.h>
#include <Safir/Utilities/Internal/SharedCharArray.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/Internal/DistributionScopeReader.h>

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
            ,m_receiverIsLightNode(distribution.IsLightNode(nodeType))
            ,m_strand(strand)
            ,m_timer(strand.context())
            ,m_distribution(distribution)
            ,m_completionHandler(completionHandler)
            ,m_running(false)
        {
        }

        void Run() SAFIR_GCC_VISIBILITY_BUG_WORKAROUND
        {
            m_strand.dispatch([this]
            {
                if (m_running)
                    return;

                lllog(5)<<"PoolHandler: Start PoolDistribution to "<<m_nodeId<<std::endl;

                m_running=true;
                //collect all connections on this node
                Connections::Instance().ForEachConnection([this](const Connection& connection)
                {
                    //auto notDoseConnection=std::string(connection.NameWithoutCounter()).find(";dose_main;")==std::string::npos;
                    const auto localContext=Safir::Dob::NodeParameters::LocalContexts(connection.Id().m_contextId);
                    const auto connectionOnThisNode=connection.IsLocal();
                    const auto detachedConnection = connection.IsDetached();

                    if (!detachedConnection && !localContext && connectionOnThisNode)
                    {
                        m_connections.push(DistributionData(connect_message_tag,
                                                            connection.Id(),
                                                            connection.NameWithoutCounter(),
                                                            connection.Counter()));
                    }
                });

                SendConnections(); //will trigger the sequence SendConnections -> SendStates() -> SendPdComplete()
            });
        }

        void Cancel()
        {
            m_strand.dispatch([this]
            {
                m_running=false;
            });
        }

        int64_t NodeId() const {return m_nodeId;}
        int64_t NodeType() const {return m_nodeType;}

    private:
        static const int64_t PoolDistributionInfoDataTypeId=-3446507522969672286; //DoseMain.PoolDistributionInfo
        static const int64_t ConnectionMessageDataTypeId=4477521173098643793; //DoseMain.ConnectionMessage
        static const int64_t RegistrationStateDataTypeId=6915466164769792349; //DoseMain.RegistrationState
        static const int64_t EntityStateDataTypeId=5802524208372516084; //DoseMain.EntityState

        const int64_t m_nodeId;
        const int64_t m_nodeType;
        const bool m_receiverIsLightNode;
        boost::asio::io_service::strand& m_strand;
        boost::asio::steady_timer m_timer;
        DistributionT& m_distribution;
        std::function<void(int64_t)> m_completionHandler;
        std::queue<DistributionData> m_connections;
        Safir::Dob::Connection m_dobConnection;

        bool m_running;

        void SendConnections()
        {
            if (!m_running)
            {
                return;
            }

            while (CanSend() && !m_connections.empty())
            {
                const DistributionData& d=m_connections.front();
                Safir::Utilities::Internal::SharedConstCharArray p(d.GetReference(), [=](const char* ptr){DistributionData::DropReference(ptr);});

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
                m_strand.post([this]
                {
                    SendStates(0);
                });
            }
            else
            {
                SetTimer([this]{SendConnections();});
            }
        }

        void SendStates(int context)
        {
            if (!m_running)
            {
                return;
            }

            if (context>=Safir::Dob::NodeParameters::NumberOfContexts())
            {
                m_strand.post([this]{SendPdComplete();});
                return;
            }

            if (m_dobConnection.IsOpen())
            {
                m_dobConnection.Close();
            }

            m_dobConnection.Open(L"dose_pool_distribution", L"pool_distribution"+boost::lexical_cast<std::wstring>(m_distribution.GetNodeId()), context, NULL, this);

            m_dobConnection.SubscribeRegistration(Entity::ClassTypeId,
                                                  Typesystem::HandlerId::ALL_HANDLERS,
                                                  true, //includeSubclasses
                                                  false, //restartSubscription
                                                  this);


            m_dobConnection.SubscribeRegistration(Service::ClassTypeId,
                                                  Typesystem::HandlerId::ALL_HANDLERS,
                                                  true, //includeSubclasses
                                                  false, //restartSubscription
                                                  this);

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

            auto connectionName=ConnectionAspectMisc(m_dobConnection).GetConnectionName();

            auto conPtr=Connections::Instance().GetConnectionByName(Typesystem::Utilities::ToUtf8(connectionName));

            DispatchStates(conPtr, context);
        }

        void DispatchStates(const Safir::Dob::Internal::ConnectionPtr& conPtr, int context) SAFIR_GCC_VISIBILITY_BUG_WORKAROUND
        {
            if (!m_running)
            {
                return;
            }

            bool overflow=false;
            conPtr->GetDirtySubscriptionQueue().Dispatch([this, conPtr, &overflow](const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove)
            {
                dontRemove=false;
                DistributionData realState = subscription->GetState()->GetRealState();
                if (!realState.IsNoState() && !DistributionScopeReader::Instance().IsLocal(realState.GetTypeId()))
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
                //dontRemove is true if we got an overflow, and if we did we dont want to keep sending anything to communication.
                exitDispatch = dontRemove;
                overflow=exitDispatch;
            });

            if (overflow)
            {
                SetTimer([this,conPtr,context]{DispatchStates(conPtr, context);});
            }
            else //continue with next context
            {
                m_strand.dispatch([this,context]{SendStates(context+1);});
            }
        }

        void SendPdComplete()
        {
            if (!m_running)
            {
                return;
            }

            m_dobConnection.Close();

            auto req=Safir::Utilities::Internal::MakeSharedArray(sizeof(PoolDistributionInfo));
            (*reinterpret_cast<PoolDistributionInfo*>(req.get()))=PdComplete;

            if (m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, req, sizeof(PoolDistributionInfo), PoolDistributionInfoDataTypeId, true))
            {
                lllog(5)<<"PoolHandler: Completed PoolDistribution to "<<m_nodeId<<std::endl;
                m_completionHandler(m_nodeId);
            }
            else
            {
                SetTimer([this]{SendPdComplete();});
            }
        }

        bool ProcessEntityState(const SubscriptionPtr& subscription)
        {
            if (!m_running)
            {
                return true;
            }
            // All nodes send ghost and injection data on PD!
            // Do not send updates

            if (subscription->GetState()->IsDetached())
            {
                // never send detached states in PD
                return true;
            }

            bool success=true;

            //Real state
            if (subscription->GetLastRealState().IsNoState())
            {
                const DistributionData currentState = subscription->GetCurrentRealState();

                if (m_distribution.IsLightNode() || m_receiverIsLightNode) // dealing with light nodes
                {
                    //Send all states owned by someone on this node
                    //It is a real state, not ghost or other junk
                    //It is an existing state, not a deleted state
                    if (currentState.GetSenderId().m_node==m_distribution.GetCommunication().Id() &&
                        currentState.GetEntityStateKind() == DistributionData::Real &&
                            currentState.HasBlob())
                    {
                        if (!CanSend() || !m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, ToPtr(currentState), currentState.Size(), EntityStateDataTypeId, true))
                        {
                            success=false;
                        }
                        else
                        {
                            lllog(5)<<L"PoolHandler: Send state"<<currentState.Image()<<std::endl;
                        }
                    }
                    else
                    {
                        lllog(5)<<L"PoolHandler: Dont send state"<<currentState.Image()<<std::endl;
                    }
                }
                else // sender and receiver are normal nodes
                {
                    //Send all states owned by someone on this node
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

                if (success)
                {
                    subscription->SetLastRealState(currentState);
                }
            }

            //injection state
            if (subscription->GetLastInjectionState().IsNoState() && !m_distribution.IsLightNode() && !m_receiverIsLightNode)
            {
                const DistributionData currentState = subscription->GetCurrentInjectionState();

                if (!currentState.IsNoState())
                {
                    if (!CanSend() || !m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, ToPtr(currentState), currentState.Size(), EntityStateDataTypeId, true))
                    {
                        success=false;
                    }
                    else
                    {
                        subscription->SetLastInjectionState(currentState);
                    }
                }
            }

            return success;
        }

        bool ProcessRegistrationState(const SubscriptionPtr& subscription)
        {
            bool success=true;

            if (subscription->GetState()->IsDetached())
            {
                // never send detached states in PD
                return true;
            }

            if (subscription->GetLastRealState().IsNoState())
            {
                const DistributionData state = subscription->GetCurrentRealState();

                if (!state.IsNoState())
                {
                    const auto normalNode = !m_distribution.IsLightNode();
                    const auto ownerOnThisNode = state.GetSenderId().m_node==m_distribution.GetCommunication().Id();
                    const auto isUnregistration =!state.IsRegistered();

                    //States owned by someone on this node are to be sent to other nodes.
                    //Unregistration states are always sent, since ghosts may be in WaitingStates
                    //waiting for the unreg (only applies to normal nodes).
                    if (ownerOnThisNode || (normalNode && isUnregistration))
                    {
                        if (!CanSend() || !m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, ToPtr(state), state.Size(), RegistrationStateDataTypeId, true))
                        {
                            success=false;
                        }
                    }
                }

                if (success)
                {
                    subscription->SetLastRealState(state);
                }
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
            m_timer.async_wait(m_strand.wrap([this,completionHandler](const boost::system::error_code&)
            {
                if (m_running)
                    completionHandler();
            }));
        }

        static inline Safir::Utilities::Internal::SharedConstCharArray ToPtr(const DistributionData& d)
        {
            Safir::Utilities::Internal::SharedConstCharArray p(d.GetReference(), [](const char* ptr){DistributionData::DropReference(ptr);});
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
