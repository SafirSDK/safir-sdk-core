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
#include <unordered_map>
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
    static const int64_t PoolDistributionInfoDataTypeId=-3446507522969672286; //DoseMain.PoolDistributionInfo
    enum class PoolDistributionInfo : uint8_t
    {
        RequestPd = 1,
        PdComplete = 2
    };
    //---------------------------------------------------------

    ///
    /// Responsible for sending poolDistributionRequests to all nodes at start-up.
    /// When all poolDistributions are received, this class calls the pdComplete callback.
    ///
    template <class CommunicationT>
    class PoolDistributionRequestor
    {
    public:
        PoolDistributionRequestor(boost::asio::io_service& io,
                                  CommunicationT& communication)
            :m_strand(io)
            ,m_communication(communication)
        {
        }

        void Start(const std::function<void()>& pdComplete)
        {
            m_strand.dispatch([=]
            {
                m_pdComplete=pdComplete;
                m_running=true;
                if (m_requests.empty())
                {
                    m_pdComplete();
                }
                else
                {
                    SendPoolDistributionRequests();
                }
            });

        }

        void Stop()
        {
            m_strand.post([=] //use post to prevent that stop is executed before start
            {
                m_running=false;
                m_requests.clear();
            });
        }

        void RequestPoolFrom(int64_t nodeId, int64_t nodeTypeId)
        {
            m_strand.post([=]
            {
                m_requests.push_back({nodeId, nodeTypeId, false});
                SendPoolDistributionRequests();
            });
        }

        void ReceivedPoolDistributionCompleteFrom(int64_t nodeId)
        {
            m_strand.dispatch([=]
            {
                auto it=std::find_if(m_requests.begin(), m_requests.end(), [nodeId](const PdReq& r){return r.nodeId==nodeId;});
                if (it!=m_requests.end())
                {
                    m_requests.erase(it);
                }

                if (m_requests.empty())
                {
                    m_pdComplete();
                }
            });
        }

#ifndef SAFIR_TEST
    private:
#endif
        struct PdReq
        {
            int64_t nodeId;
            int64_t nodeType;
            bool sent;
        };

        bool m_running=false;

        boost::asio::io_service::strand m_strand;
        CommunicationT& m_communication;
        std::function<void()> m_pdComplete; //signal pdComplete after all our pdRequests has reported pdComplete
        std::vector<PdReq> m_requests;

        void SendPoolDistributionRequests()
        {
            if (!m_running)
            {
                return;
            }

            auto req=boost::make_shared<char[]>(sizeof(PoolDistributionInfo));
            (*reinterpret_cast<PoolDistributionInfo*>(req.get()))=PoolDistributionInfo::RequestPd;

            bool unsentRequests=false;

            for (auto& r : m_requests)
            {
                if (!r.sent)
                {
                    if (m_communication.Send(r.nodeId, r.nodeType, req, sizeof(PoolDistributionInfo), PoolDistributionInfoDataTypeId, true))
                    {
                        r.sent=true;
                    }
                    else
                    {
                        unsentRequests=true;
                    }
                }
            }

            if (unsentRequests)
            {
                //some requests could not be sent, retry
                m_strand.post([=]{SendPoolDistributionRequests();}); //a bit aggressive, maybe we should set a timer instead
            }
        }
    };

    ///
    /// This class is responsible for handling all pool distributions to other nodes.
    /// It will keep a queue of PoolDistributionRequests and make sure that only one
    /// pd is active at the same time. It will also ensure that pool distributions will
    /// be sending in a pace that wont flood the network and cause starvation in other parts
    /// of the system.
    ///
    template <class CommunicationT> class PoolDistribution; //forward
    template <class CommunicationT>
    class PoolDistributor : private boost::noncopyable
    {
    public:
        PoolDistributor(boost::asio::io_service& io, CommunicationT& com)
            :m_strand(io)
            ,m_communication(com)
        {
        }

        //make sure that start is not called before persistent data is ready
        void Start()
        {
            m_strand.dispatch([=]
            {
                m_running=true;
                StartNextPoolDistribution();
            });
        }

        void Stop()
        {
            m_strand.post([=]
            {
                m_running=false;
                //TODO: m_timer.cancel();
            });
        }

        //Called when a new pdRequest is received. Will result in a pd to the node with specified id.
        void AddPoolDistribution(int64_t nodeId, int64_t nodeTypeId)
        {
            m_strand.dispatch([this, nodeId, nodeTypeId]
            {
                auto pd=PdPtr(new PoolDistribution<CommunicationT>(nodeId, nodeTypeId, m_strand, m_communication, [=](int64_t nodeId){StartNextPoolDistribution();}));
                m_pendingPoolDistributions.push(std::move(pd));

                if (m_pendingPoolDistributions.size()==1) //if queue was empty we have to manually start the pd. If not the completion handler of the ongoing pd will start the next one.
                {
                    StartNextPoolDistribution();
                }
            });
        }

    private:
        boost::asio::io_service::strand m_strand;
        CommunicationT& m_communication;
        bool m_running=false;

        using PdPtr = std::unique_ptr< PoolDistribution<CommunicationT> >;
        std::queue<PdPtr> m_pendingPoolDistributions;

        void StartNextPoolDistribution()
        {
            if (m_running && !m_pendingPoolDistributions.empty())
            {
                m_pendingPoolDistributions.front()->Run();
            }
        }
    };

    ///
    /// Handles a single pool distribution to a specific node. This class is only to be used by
    /// PoolDistributor. To start a new pool distribution to a node, use PoolDistributor.AddPoolDistribution.
    ///
    template <class CommunicationT>
    class PoolDistribution
            :public Safir::Dob::EntitySubscriber
            ,public Safir::Dob::RegistrationSubscriber
            ,public Safir::Dob::Dispatcher
    {
    public:
        PoolDistribution(int64_t nodeId,
                         int64_t nodeType,
                         boost::asio::io_service::strand& strand,
                         CommunicationT& communication,
                         const std::function<void(int64_t)>& completionHandler)
            :m_nodeId(nodeId)
            ,m_nodeType(nodeType)
            ,m_strand(strand)
            ,m_timer(strand.get_io_service())
            ,m_communication(communication)
            ,m_completionHandler(completionHandler)
        {
        }

        void Run()
        {
            m_strand.dispatch([=]
            {
                //collect all connections on this node
                Connections::Instance().ForEachConnection([=](const Connection& connection)
                {
                    auto notLocalContext=!Safir::Dob::NodeParameters::LocalContexts(connection.Id().m_contextId);
                    auto notDoseConnection=std::string(connection.NameWithoutCounter()).find(";dose_main;")==std::string::npos;
                    auto connectionOnThisNode=connection.IsLocal();
                    if (notLocalContext && notDoseConnection && connectionOnThisNode)
                    {
                        m_connections.push(DistributionData(connect_message_tag,
                                                            connection.Id(),
                                                            connection.NameWithoutCounter(),
                                                            connection.Counter()));
                        SendConnections(); //will trigger the sequence SendConnections -> SendStates() -> SendPdComplete()
                    }
                });
            });
        }

    private:
        static const int64_t ConnectionMessageDataTypeId=4477521173098643793; //DoseMain.ConnectionMessage
        static const int64_t RegistrationStateDataTypeId=6915466164769792349; //DoseMain.RegistrationState
        static const int64_t EntityStateDataTypeId=5802524208372516084; //DoseMain.EntityState

        int64_t m_nodeId;
        int64_t m_nodeType;
        boost::asio::io_service::strand& m_strand;
        boost::asio::steady_timer m_timer;
        CommunicationT& m_communication;
        std::function<void(int64_t)> m_completionHandler;
        std::queue<DistributionData> m_connections;
        Safir::Dob::Connection m_dobConnection;

        void SendConnections()
        {
            while (CanSend() && !m_connections.empty())
            {
                const DistributionData& d=m_connections.front();
                boost::shared_ptr<const char[]> p(d.GetReference(), [=](const char* ptr){DistributionData::DropReference(ptr);});

                if (m_communication.Send(m_nodeId, m_nodeType, p, d.Size(), ConnectionMessageDataTypeId, true))
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
                SendStates(0);
            }
            else
            {
                m_timer.expires_from_now(boost::chrono::milliseconds(10));
                m_timer.async_wait(m_strand.wrap([=](const boost::system::error_code& /*error*/){SendConnections();}));
            }
        }

        void SendStates(int context)
        {
            if (context>=Safir::Dob::NodeParameters::NumberOfContexts())
            {
                SendPdComplete();
                return;
            }

            if (m_dobConnection.IsOpen())
            {

                m_dobConnection.Close();
            }

            //Note the connection name, we do NOT want to get special treatment for being
            //in dose_main, since we're a different thread. But we want to be allowed in
            //always, so don't change connection string "dose_main_pd" because it is always allowed to connect,
            // even before we let -1 connections in.
            m_dobConnection.Open(L"dose_main_pd",L"pool_distribution", context, NULL, this);
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

        void DispatchStates(const Safir::Dob::Internal::ConnectionPtr& conPtr, int context)
        {
            bool overflow=false;
            conPtr->GetDirtySubscriptionQueue().Dispatch([this, conPtr, context, &overflow](const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove)
            {
                DistributionData realState = subscription->GetState()->GetRealState();
                if (!realState.IsNoState() && realState.GetType()==DistributionData::RegistrationState)
                {
                    // Registration state
                    dontRemove=!subscription->DirtyFlag().Process([this, &subscription]{return ProcessRegistrationState(subscription);});
                }
                else
                {
                    // Entity state
                    dontRemove=!subscription->DirtyFlag().Process([this, &subscription]{return ProcessEntityState(subscription);});
                }
                //dontRemove is true if we got an overflow, and if we did we dont want to keep sending anything to dose_com.
                exitDispatch = dontRemove;
                overflow=exitDispatch;
            });

            if (overflow)
            {
                m_timer.expires_from_now(boost::chrono::milliseconds(10));
                m_timer.async_wait(m_strand.wrap([=](const boost::system::error_code& /*error*/){DispatchStates(conPtr, context);}));
            }
            else //continue with next context
            {
                m_strand.dispatch([=]{SendStates(context+1);});
            }
        }

        void SendPdComplete()
        {
            m_dobConnection.Close();

            auto req=boost::make_shared<char[]>(sizeof(PoolDistributionInfo));
            (*reinterpret_cast<PoolDistributionInfo*>(req.get()))=PoolDistributionInfo::PdComplete;

            if (m_communication.Send(m_nodeId, m_nodeType, req, sizeof(PoolDistributionInfo), PoolDistributionInfoDataTypeId, true))
            {
                m_timer.expires_from_now(boost::chrono::milliseconds(10));
                m_timer.async_wait(m_strand.wrap([=](const boost::system::error_code& /*error*/){SendPdComplete();}));
            }
        }

        bool ProcessEntityState(const SubscriptionPtr& subscription)
        {
            // All nodes send ghost and injection data on PD!
            // Do not send updates

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
                    if (currentState.GetSenderId().m_node==m_communication.Id() ||
                            currentState.GetEntityStateKind() == DistributionData::Ghost ||
                            !currentState.HasBlob())
                    {
                        if (!CanSend() || !m_communication.Send(m_nodeId, m_nodeType, ToPtr(currentState), currentState.Size(), EntityStateDataTypeId, true))
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
                    if (!CanSend() || !m_communication.Send(m_nodeId, m_nodeType, ToPtr(currentState), currentState.Size(), EntityStateDataTypeId, true))
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
                    if (state.GetSenderId().m_node==m_communication.Id() ||
                            !state.IsRegistered())
                    {
                        if (!CanSend() || !m_communication.Send(0, m_nodeType, ToPtr(state), state.Size(), RegistrationStateDataTypeId, true))
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
            static const size_t threshold=m_communication.SendQueueCapacity(m_nodeType)/2;
            return m_communication.NumberOfQueuedMessages(m_nodeType)<threshold;
        }

        static inline boost::shared_ptr<const char[]> ToPtr(const DistributionData& d)
        {
            boost::shared_ptr<const char[]> p(d.GetReference(), [=](const char* ptr){DistributionData::DropReference(ptr);});
            return p;
        }

        //Dummy consumer
        virtual void OnDoDispatch() {}
        virtual void OnRegistered(const Safir::Dob::Typesystem::TypeId, const Safir::Dob::Typesystem::HandlerId&) {}
        virtual void OnUnregistered(const Safir::Dob::Typesystem::TypeId, const Safir::Dob::Typesystem::HandlerId&) {}
        virtual void OnNewEntity(const Safir::Dob::EntityProxy) {}
        virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy) {}
        virtual void OnDeletedEntity(const Safir::Dob::EntityProxy, const bool) {}
    };
}
}
}
