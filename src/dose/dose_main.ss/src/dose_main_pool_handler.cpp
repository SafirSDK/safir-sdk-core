/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / stlrha
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

#include "dose_main_pool_handler.h"

#include "dose_main_blocking_handler.h"
#include "dose_main_communication.h"
#include "dose_main_connection_handler.h"
#include "dose_main_pending_registration_handler.h"
#include "dose_main_persist_handler.h"
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/ConnectionAspectInjector.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/Internal/ServiceTypes.h>
#include <Safir/Dob/Internal/State.h>
#include <Safir/Dob/Internal/ContextIdComposer.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <boost/bind.hpp>
#include <Safir/Utilities/Internal/SystemLog.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    const Dob::Typesystem::Int32 g_thisNode = Safir::Dob::ThisNodeParameters::NodeNumber();

    class PoolHandler::ExceptionInfo
    {
    public:
        ExceptionInfo(const Dob::Typesystem::TypeId typeId,
                      const std::string& description)
            : m_typeId(typeId)
            , m_description(description)
        {
                
        }
            
        void Rethrow() const
        {
            Dob::Typesystem::LibraryExceptions::Instance().Throw(m_typeId, m_description);
        }
    private:
        const Dob::Typesystem::TypeId m_typeId;
        const std::string m_description;
    };

    PoolHandler::PoolHandler(boost::asio::io_service::strand& strand):
#if 0 //stewart
        m_ecom(NULL),
#endif
        m_pendingRegistrationHandler(NULL),
        m_persistHandler(NULL),
        m_connectionHandler(NULL),
        m_stateSubscriptionConnections(Safir::Dob::NodeParameters::NumberOfContexts()),
        m_pdThread(),
        m_strand(strand)
    {
        m_stateDispatcher.reset(new StateDispatcher([this]()
                                                    {
                                                        DistributeStates();
                                                    },
                                                    strand));
    }

    PoolHandler::~PoolHandler()
    {
        if (m_pdThread.get_id() != boost::thread::id())
        {
            m_pdThread.interrupt();
            m_pdThread.join();
            m_pdThread = boost::thread();
        }
    }

    void StartSubscriptions(const Safir::Dob::Connection & connection,
                            DummySubscriber & subscriber,
                            const bool subscribeEntities,
                            const bool includeUpdates)
    {

        connection.SubscribeRegistration(Entity::ClassTypeId,
                                         Typesystem::HandlerId::ALL_HANDLERS,
                                         true, //includeSubclasses
                                         false, //restartSubscription
                                         &subscriber);

        connection.SubscribeRegistration(Service::ClassTypeId,
                                         Typesystem::HandlerId::ALL_HANDLERS,
                                         true, //includeSubclasses
                                         false, //restartSubscription
                                         &subscriber);

        if (subscribeEntities)
        {
            ConnectionAspectInjector(connection).SubscribeEntity(Entity::ClassTypeId,
                                                                 includeUpdates,
                                                                 true,  //includeSubclasses
                                                                 false, //restartSubscription
                                                                 false, //wantsGhostDelete
                                                                 false, //wantsLastState
                                                                 false, //doesntWantSourceIsPermanentStore
                                                                 true,  //wantsAllStateChanges
                                                                 false, //timestampChangeInfo
                                                                 &subscriber);
        }
    }

    void PoolHandler::Init(
#if 0 //stewart
                           ExternNodeCommunication & ecom,
#endif
                           PendingRegistrationHandler & pendingHandler,
                           PersistHandler & persistHandler,
                           ConnectionHandler & connectionHandler)
    {
#if 0 //stewart
        m_ecom = &ecom;
#endif
        m_pendingRegistrationHandler = &pendingHandler;
        m_persistHandler = &persistHandler;
        m_connectionHandler = &connectionHandler;

        // dose_main must subscribe for states in all contexts
        for (ContextId context = 0; context < Safir::Dob::NodeParameters::NumberOfContexts(); ++context)
        {
            m_stateSubscriptionConnections[context].m_connection.Open(L"dose_main",L"states", context, NULL, m_stateDispatcher.get());

            StartSubscriptions(m_stateSubscriptionConnections[context].m_connection,
                               m_dummySubscriber,
#if 0 //stewart
                               !m_ecom->GetQualityOfServiceData().IsStandalone(), //only include entity states when we're in a multinode system
#else
                               false,
#endif
                               true);

            const std::wstring connectionName = ConnectionAspectMisc(m_stateSubscriptionConnections[context].m_connection).GetConnectionName();
            m_stateSubscriptionConnections[context].m_connectionPtr =
                Connections::Instance().GetConnectionByName(Typesystem::Utilities::ToUtf8(connectionName));
        }
    }

    void PoolHandler::Stop()
    {
        for (ContextId context = 0; context < Safir::Dob::NodeParameters::NumberOfContexts(); ++context)
        {
            m_stateSubscriptionConnections[context].m_connection.Close();
        }
    }

    void PoolHandler::StartPoolDistribution()
    {
        lllout << "Starting pool distribution thread" << std::endl;
        ENSURE(m_pdThread.get_id() == boost::thread::id(), << "There is already a pd thread running! m_pdThread id = "
                                              << m_pdThread.get_id());
        m_pdThread = boost::thread(boost::bind(&PoolHandler::PoolDistributionWorker,this));
    }

    void PoolHandler::RequestPoolDistribution(const int nodeId)
    {
        lllout << "PoolHandler::RequestPoolDistribution(" << nodeId << ") called." << std::endl;

        // Startup sequence, we must wait for persistence.
        // Ok, if nodes hang in state NEW at this stage.
        if (!m_persistHandler->IsPersistentDataReady())
        {
            lllout << "PoolHandler::RequestPoolDistribution(" << nodeId << "). Waiting for persistence - request ignored." << std::endl;
            return;
        }

        lllog(1) << "PoolHandler::RequestPoolDistribution: request sent to node " << nodeId << "." << std::endl;
        DistributionData PDRequest
            (request_pool_distribution_request_tag,
            ConnectionId(ThisNodeParameters::NodeNumber(), 0, -1), //Sender - use context 0 for this request
            ConnectionId(nodeId, 0, -1)); //Receiver - use context 0 for this request
#if 0 //stewart
        m_ecom->Send(PDRequest);
#endif
    }

    void PoolHandler::HandleMessageFromDoseCom(const DistributionData& data)
    {
        if (data.GetType() == DistributionData::Action_RequestPoolDistribution)
        {
            lllog(1) << "Forcing pool distribution on request from node " << data.GetSenderId().m_node << "." << std::endl;
#if 0 //stewart
            m_ecom->ForcePoolDistribution(data.GetSenderId().m_node);
#endif
        }
    }

    void PoolHandler::PDConnection(const Connection & connection)
    {
        if (connection.IsLocal())
        {
            if (std::string(connection.NameWithoutCounter()).find(";dose_main;") != std::string::npos)
            {
                return;
            }

            //We're only interested in local node connections
            DistributionData msg(connect_message_tag,
                                 connection.Id(),
                                 connection.NameWithoutCounter(),
                                 connection.Counter());

            // send later to avoid locking up shmem
            ConnectionMsgsToSend.push_back(msg);
        }
    }

 
    void PoolHandler::PoolDistributionWorker()
    {
#if 0 //stewart
        boost::shared_ptr<ExceptionInfo> exceptionInfo;
        
        try
        {
            lllout << "Pool distribution thread started" << std::endl;
            
        m_poolDistributionThreadId = boost::this_thread::get_id();
        m_threadMonitor->StartWatchdog(m_poolDistributionThreadId, L"pool distribution thread");

            // Wait until persistent data is ready.
            if (!m_persistHandler->IsPersistentDataReady())
            {
                lllout << "Pool distribution thread is waiting for persistence data from DOPE" << std::endl;
                while (!m_persistHandler->IsPersistentDataReady())
                {
                    boost::this_thread::sleep_for(boost::chrono::milliseconds(10)); //sleep is interruption point
                
                m_threadMonitor->KickWatchdog(m_poolDistributionThreadId);
                }
                lllout << "Pool distribution thread thinks DOPE is done." << std::endl;
            }


            lllout << "Distributing connections" << std::endl;
            // get connections
            ConnectionMsgsToSend.clear();
            Connections::Instance().ForEachConnection(boost::bind(&PoolHandler::PDConnection,this,_1));
            // send connections
            ConnectionMsgs::const_iterator msgIter;
            for ( msgIter = ConnectionMsgsToSend.begin( ) ; msgIter != ConnectionMsgsToSend.end( ) ; msgIter++ )
            {
                boost::this_thread::interruption_point();
                m_ecom->SendPoolDistributionData(*msgIter, *m_threadMonitor, m_poolDistributionThreadId);
            }
            ConnectionMsgsToSend.clear();
            lllout << "Setting up connection in each context for pool distribution" << std::endl;

            DummyDispatcher dispatcher;
            DummySubscriber dummySubscriber;

            // Distribute all contexts
            for (ContextId context = 0; context < Safir::Dob::NodeParameters::NumberOfContexts(); ++context)
            {
                boost::this_thread::interruption_point();
                Safir::Dob::Connection pdConnection;

                //Note the connection name, we do NOT want to get special treatment for being
                //in dose_main, since we're a different thread. But we want to be allowed in
                //always, so don't change connection string "dose_main_pd" because it is always allowed to connect,
                // even before we let -1 connections in.
                pdConnection.Open(L"dose_main_pd",L"pool_distribution", context,NULL, &dispatcher);
                StartSubscriptions(pdConnection, dummySubscriber,true,false);
                const std::wstring connectionName = ConnectionAspectMisc(pdConnection).GetConnectionName();
                ConnectionPtr connection = Connections::Instance().GetConnectionByName(Typesystem::Utilities::ToUtf8(connectionName));

                connection->GetDirtySubscriptionQueue().Dispatch(boost::bind(&PoolHandler::PDDispatchSubscription,this,_1,_2,_3));
            }

            lllout << "Pool distribution completed (calling ecom::PoolDistributionCompleted" << std::endl;

            m_ecom->PoolDistributionCompleted(*m_threadMonitor, m_poolDistributionThreadId);
        }
        catch (const boost::thread_interrupted&)
        {
            //do nothing but exit the thread.
            return;
        }
        catch (const Dob::Typesystem::Internal::CommonExceptionBase& e)
        {
            exceptionInfo.reset
                (new ExceptionInfo(e.GetTypeId(),
                                   Dob::Typesystem::Utilities::ToUtf8(e.GetExceptionInfo())));
        }
        catch (const std::exception & e)
        {
            exceptionInfo.reset(new ExceptionInfo(0,
                                                  e.what()));
        }
        catch (...)
        {
            exceptionInfo.reset(new ExceptionInfo(0,
                                                  "Unknown exception (caught as ...)"));
        }

        if (exceptionInfo != NULL)
        {
            SEND_SYSTEM_LOG(Alert,
                            << "An exception was caught inside PoolHandler::PoolDistributionWorker!"
                            << "The exception will be rethrown in the main thread");
        }

        lllout << "Signalling pool distribution completed"<< std::endl;

        //No need for m_isNotified flag guard, since this happens very seldom.
        m_ioService.post(boost::bind(&PoolHandler::PDCompletedHandler,this,exceptionInfo));

        m_threadMonitor->StopWatchdog(m_poolDistributionThreadId);
#endif
    }

    void PoolHandler::HandleRegistrationStateFromDoseCom(const DistributionData& state, const bool isAckedData)
    {
        if (state.IsRegistered())
        {
            const ConnectionId senderId=state.GetSenderId();

            ENSURE(senderId.m_id != -1, << "Registration states are expected to have ConnectionId != -1! Reg for type "
                   << Safir::Dob::Typesystem::Operations::GetName(state.GetTypeId()));

            const ConnectionPtr connection = Connections::Instance().GetConnection(senderId, std::nothrow);

            if (connection == NULL)
            {
                ENSURE(isAckedData, << "Got a registration state on an unacked priority channel! " << state.Image());
            }
            else
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
        else
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

            m_pendingRegistrationHandler->CheckForPending(state.GetTypeId());
        }
    }

    void PoolHandler::HandleEntityStateFromDoseCom(const DistributionData& state, const bool isAckedData)
    {
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
                if (state.IsCreated()) //handle created and delete states differently
                {
                    const ConnectionId senderId=state.GetSenderId();

                    ENSURE(senderId.m_id != -1, << "Entity states are expected to have ConnectionId != -1! State for "
                           << state.GetEntityId());

                    const ConnectionPtr connection = Connections::Instance().GetConnection(senderId, std::nothrow);
                    if (connection == NULL)
                    {
                        if (isAckedData)
                        {
                            SEND_SYSTEM_LOG(Alert,
                                            << "Got an acked entity with a sender connection "
                                            << "(Id:" << senderId.m_id << " Node:" << senderId.m_node << " Ctx:"
                                            << senderId.m_contextId << ") that is unknown on this node.");
                            throw std::logic_error("Received acked entity with an unknown connection");
                        }
                        else
                        {
                            lllog(3) << "Discarding an unacked entity state since we don't have its connection" << std::endl;
                        }
                    }
                    else
                    {
                        EntityTypes::Instance().RemoteSetRealEntityState(connection, state);
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
    }

    void PoolHandler::HandleStateFromDoseCom(const DistributionData & state, const bool isAckedData)
    {
        switch (state.GetType())
        {
        case DistributionData::RegistrationState:
            {

                HandleRegistrationStateFromDoseCom(state,isAckedData);
            }
            break;

        case DistributionData::EntityState:
            {
                HandleEntityStateFromDoseCom(state,isAckedData);
            }
            break;
        default:
            ENSURE(false,<<"Illegal type of DistributionData in HandleStateFromDoseCom");
        }
    }

    void PoolHandler::PDCompletedHandler(const boost::shared_ptr<ExceptionInfo>& exceptionInfo)
    {
        lllout << "Pool distribution is completed"<< std::endl;
        m_pdThread.join();
        m_pdThread = boost::thread();

        if (exceptionInfo != NULL)
        {
            SEND_SYSTEM_LOG(Alert,
                            << "An exception occurred in pool distribution, rethrowing.");
            exceptionInfo->Rethrow();
        }
        m_connectionHandler->MaybeSignalConnectSemaphore();
    }


    void PoolHandler::DistributeStates()
    {
        for (ContextId context = 0; context < Safir::Dob::NodeParameters::NumberOfContexts(); ++context)
        {
            m_stateSubscriptionConnections[context].m_connectionPtr->
                GetDirtySubscriptionQueue().Dispatch(boost::bind(&PoolHandler::DispatchSubscription,this,_1,_2,_3));
        }
    }

    bool IsLocal(const DistributionData& state)
    {
        return state.GetSenderId().m_node == g_thisNode;
    }

    bool ProcessEntityState(
#if 0 //stewart
            ExternNodeCommunication* /*ecom*/,
#endif
            const SubscriptionPtr& subscription)
    {
        bool complete = true;

        //Real state
        {
            const DistributionData currentState = subscription->GetCurrentRealState();
            const DistributionData lastState = subscription->GetLastRealState();

            if (currentState != lastState && !currentState.IsNoState())
            {
                //if sender is not local node we dont send to dosecom, and just ignore it.
                if (!IsLocal(currentState))
                {
                    subscription->SetLastRealState(currentState);
                }
                else
                {
#if 0 //stewart
                    if (ecom->Send(currentState))
#endif
                    {
                        subscription->SetLastRealState(currentState);
                    }
#if 0 //stewart
                    else
                    {
                        complete = false;
                    }
#endif
                }
            }
        }

        //injection state
        {
            const DistributionData currentState = subscription->GetCurrentInjectionState();
            const DistributionData lastState = subscription->GetLastInjectionState();

            if (currentState != lastState && !currentState.IsNoState())
            {
                //if sender is not local node we dont send to dosecom, and just ignore it.
                if (!IsLocal(currentState))
                {
                    subscription->SetLastInjectionState(currentState);
                }
                else
                {
#if 0 //stewart
                    if (ecom->Send(currentState))
#endif
                    {
                        subscription->SetLastInjectionState(currentState);
                    }
#if 0 //stewart
                    else
                    {
                        complete = false;
                    }
#endif
                }
            }
        }

        return complete;
    }

    bool PoolHandler::PDProcessEntityState(const SubscriptionPtr & /*subscription*/)
    {
#if 0 //stewart
        // All nodes send ghost and injection data on PD!
        // Do not send updates

        //Real state
        if (subscription->GetLastRealState().IsNoState())
        {
            const DistributionData currentState = subscription->GetCurrentRealState();

            if (!currentState.IsNoState())
            {
                //Send all local states
                //send all ghosts (the owner node is probably down...)
                //send all delete states (so that new nodes get the correct timestamps)
                if (IsLocal(currentState) ||
                    currentState.GetEntityStateKind() == DistributionData::Ghost ||
                    !currentState.HasBlob())
                {
                    m_ecom->SendPoolDistributionData(currentState, *m_threadMonitor, m_poolDistributionThreadId);
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
                m_ecom->SendPoolDistributionData(currentState, *m_threadMonitor, m_poolDistributionThreadId);
                subscription->SetLastInjectionState(currentState);
            }
        }
#endif
        return true; //PD send always succeeds
    }

    bool ProcessRegistrationState(
#if 0 //stewart
                                  ExternNodeCommunication * ecom,
#endif
                                  PendingRegistrationHandler * pendingHandler,
                                  const SubscriptionPtr & subscription)
    {

        const DistributionData currentState = subscription->GetCurrentRealState();
        const DistributionData lastState = subscription->GetLastRealState();

        if (currentState == lastState)
        {
            return true;
        }

        //if sender is not local node we dont send to dosecom, and just ignore it.
        if (currentState.GetSenderId().m_node != g_thisNode)
        {
            return true;
        }
#if 0 //stewart
        const bool success = ecom->Send(currentState);

        if (success)
#endif
        {
            pendingHandler->CheckForPending(currentState.GetTypeId());
            subscription->SetLastRealState(currentState);
        }

#if 0 //stewart
        return success;
#else
        return true;
#endif
    }

    bool PoolHandler::PDProcessRegistrationState(const SubscriptionPtr & /*subscription*/)
    {
#if 0 //stewart
        if (subscription->GetLastRealState().IsNoState())
        {
            const DistributionData state = subscription->GetCurrentRealState();


            if (!state.IsNoState())
            {
                //Local states are to be sent to other nodes.
                //Unregistration states (regardless of whether they are local or not) are always sent.
                if (IsLocal(state) ||
                    !state.IsRegistered())
                {
                    m_ecom->SendPoolDistributionData(state, *m_threadMonitor, m_poolDistributionThreadId);
                }
            }
            subscription->SetLastRealState(state); //update this so we only dispatch this state once (see "if" above)
        }
#endif

        return true; //PD send always succeeds
    }

    void PoolHandler::DispatchSubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove)
    {
        DistributionData realState = subscription->GetState()->GetRealState();


        if (!realState.IsNoState() && realState.GetType() == DistributionData::RegistrationState)
        {
            // Registration state
            dontRemove = !subscription->DirtyFlag().Process(boost::bind(ProcessRegistrationState,
#if 0 //stewart
                                                                        m_ecom,
#endif
                                                                        m_pendingRegistrationHandler,
                                                                        boost::cref(subscription)));
        }
        else
        {
            // Entity state
            dontRemove = !subscription->DirtyFlag().Process(boost::bind(ProcessEntityState,
#if 0 //stewart
                                                                        m_ecom,
#endif
                                                                        boost::cref(subscription)));
        }
        //dontRemove is true if we got an overflow, and if we did we dont want to keep sending anything to dose_com.
        exitDispatch = dontRemove;
    }

    void PoolHandler::PDDispatchSubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove)
    {
        boost::this_thread::interruption_point();

        exitDispatch = false;
        dontRemove = false;

        DistributionData realState = subscription->GetState()->GetRealState();

        if (!realState.IsNoState() && realState.GetType() == DistributionData::RegistrationState)
        {
            // Registration state
            subscription->DirtyFlag().Process(boost::bind(&PoolHandler::PDProcessRegistrationState,
                                                          this,
                                                          boost::cref(subscription)));
        }
        else
        {
            // Entity state
            subscription->DirtyFlag().Process(boost::bind(&PoolHandler::PDProcessEntityState,
                                                          this,
                                                          boost::cref(subscription)));
        }
    }

}
}
}
