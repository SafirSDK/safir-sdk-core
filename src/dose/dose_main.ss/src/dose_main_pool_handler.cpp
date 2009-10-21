/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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
#include "dose_main_end_states_handler.h"
#include "dose_main_communication.h"
#include "dose_main_connection_handler.h"
#include "dose_main_pending_registration_handler.h"
#include "dose_main_persist_handler.h"
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/ConnectionAspectInjector.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/Internal/ServiceTypes.h>
#include <Safir/Dob/Internal/State.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <ace/Thread.h>
#include <boost/bind.hpp>
#include <ace/OS_NS_unistd.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    const Dob::Typesystem::Int32 g_thisNode = Safir::Dob::ThisNodeParameters::NodeNumber();

    PoolHandler::PoolHandler():
        m_ecom(NULL),
        m_blockingHandler(NULL),
        m_pendingRegistrationHandler(NULL),
        m_persistHandler(NULL),
        m_endStates(NULL),
        m_connectionHandler(NULL),
        m_connection(NULL),
        m_pdThreadHandle(NULL)
    {
        m_stateDispatcher.reset(new StateDispatcher(m_stateSubscriptionConnection,boost::bind(&PoolHandler::DistributeStates,this)));
    }

    PoolHandler::~PoolHandler()
    {
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

    void PoolHandler::Init(BlockingHandlers & blockingHandler,
                           ExternNodeCommunication & ecom,
                           PendingRegistrationHandler & pendingHandler,
                           PersistHandler & persistHandler,
                           EndStatesHandler& endStates,
                           ConnectionHandler & connectionHandler)
    {
        m_blockingHandler = &blockingHandler;
        m_ecom = &ecom;
        m_pendingRegistrationHandler = &pendingHandler;
        m_persistHandler = &persistHandler;
        m_endStates = &endStates;
        m_connectionHandler = &connectionHandler;

        m_stateSubscriptionConnection.Open(L"dose_main",L"states",0,NULL,m_stateDispatcher.get());

        //only include entity states when we're in a multinode system
        StartSubscriptions(m_stateSubscriptionConnection,
                           m_dummySubscriber,
                           !m_ecom->GetQualityOfServiceData().IsStandalone(),
                           true);

        const std::wstring connectionName = ConnectionAspectMisc(m_stateSubscriptionConnection).GetConnectionName();
        m_connection = Connections::Instance().GetConnectionByName(Typesystem::Utilities::ToUtf8(connectionName)).get();
    }



    ACE_THR_FUNC_RETURN PoolHandler::PoolDistributionThreadFunc(void * _this)
    {
        static_cast<PoolHandler *>(_this)->PoolDistributionWorker();
        return NULL;
    }

    void PoolHandler::StartPoolDistribution()
    {
        lllout << "Starting pool distribution thread" << std::endl;
        ENSURE(m_pdThreadHandle == 0, << "It appears that there are multiple pool distribution threads running! m_pdThreadHandle = 0x"
                                      <<std::hex<< (void*)m_pdThreadHandle<<std::dec);
        ACE_Thread::spawn(&PoolHandler::PoolDistributionThreadFunc,this,THR_NEW_LWP|THR_JOINABLE,NULL,&m_pdThreadHandle);
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

            m_ecom->SendPoolDistributionData(msg);
        }
    }


    void PoolHandler::PoolDistributionWorker()
    {
        lllout << "Pool distribution thread started" << std::endl;

        // Wait until persistent data is ready if we're the persistance node
        if (PersistHandler::SystemHasPersistence() &&  PersistHandler::ThisNodeIsPersistanceNode())
        {
            lllout << "Pool distribution thread is waiting for persistence data from DOPE (or from other node)" << std::endl;
            while (!m_persistHandler->IsPersistentDataReady())
            {
                ACE_OS::sleep(ACE_Time_Value(0,10000)); //10ms
            }
            lllout << "Pool distribution thread thinks DOPE is done." << std::endl;
        }

        try
        {
            lllout << "Distributing connections" << std::endl;
            Connections::Instance().ForEachConnection(boost::bind(&PoolHandler::PDConnection,this,_1));

            lllout << "Setting up connection for pool distribution" << std::endl;
            DummyDispatcher dispatcher;
            DummySubscriber dummySubscriber;
            Safir::Dob::Connection pdConnection;

            //Note the connection name, we do NOT want to get special treatment for being
            //in dose_main, since we're a different thread. But we want to be allowed in
            //early, so we use -1 for context.
            pdConnection.Open(L"dose_main_pd",L"pool_distribution",-1,NULL, &dispatcher);
            StartSubscriptions(pdConnection, dummySubscriber,true,false);

            const std::wstring connectionName = ConnectionAspectMisc(pdConnection).GetConnectionName();
            ConnectionPtr connection = Connections::Instance().GetConnectionByName(Typesystem::Utilities::ToUtf8(connectionName)).get();

            connection->GetDirtySubscriptionQueue().Dispatch(boost::bind(&PoolHandler::PDDispatchSubscription,this,_1,_2,_3));

            lllout << "Pool distribution completed (calling ecom::PoolDistributionCompleted" << std::endl;

            m_ecom->PoolDistributionCompleted();
        }
        catch (const std::exception & exc)
        {
            std::wostringstream ostr;
            ostr << "dose_main std::exception in PoolHandler::PoolDistributionWorker:" << std::endl
                 << exc.what();

            std::wcout << ostr.str() << std::endl;
            lllout << ostr.str() << std::endl;
            exit(-1);
        }
        catch (...)
        {
            std::wostringstream ostr;
            ostr << "dose_main ... in PoolHandler::PoolDistributionWorker!";

            std::wcout << ostr.str() << std::endl;
            lllout << ostr.str() << std::endl;
            exit(-1);
        }
        lllout << "Signalling pool distribution completed"<< std::endl;

        //No need for m_isNotified flag guard, since this happens very seldom.
        ACE_Reactor::instance()->notify(this);
    }

    void PoolHandler::HandleRegistrationStateFromDoseCom(const DistributionData& state, const bool isAckedData)
    {
        if (state.IsRegistered())
        {
            const ConnectionId senderId=state.GetSenderId();

            ENSURE(senderId.m_id != -1, << "Registration states are expected to have ConnectionId != -1! Reg for type "
                   << Safir::Dob::Typesystem::Operations::GetName(state.GetTypeId()));

            if (m_endStates->IsDisconnected(senderId))
            {
                //The state belongs to a recently disconnected connection, so we
                //just discard it.
                return;
            }

            const ConnectionPtr connection = Connections::Instance().GetConnection(senderId, std::nothrow);

            if (connection == NULL)
            {
                ENSURE(isAckedData, << "Got a registration state on an unacked priority channel! " << state.Image());
                m_waitingStates.Add(state);
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

                PerformStatesWaitingForRegistration(state);
            }
        }
        else
        {
            ENSURE(state.GetSenderId().m_id == -1, << "Unregistration states are expected to have ConnectionId == -1! Unreg for type "
                   << Safir::Dob::Typesystem::Operations::GetName(state.GetTypeId()));

            //unregistration states bypass all waitingstates handling and go straight
            //into shared memory, and we keep a reference in endstates for a while
            if (Typesystem::Operations::IsOfType(state.GetTypeId(),Safir::Dob::Service::ClassTypeId))
            {
                ServiceTypes::Instance().RemoteSetUnregistrationState(state);
            }
            else
            {
                EntityTypes::Instance().RemoteSetUnregistrationState(state);
            }

            m_pendingRegistrationHandler->CheckForPending(state.GetTypeId());
        }

        m_waitingStates.CleanUp(state);
    }

    void PoolHandler::HandleEntityStateFromDoseCom(const DistributionData& state, const bool isAckedData)
    {
        switch (state.GetEntityStateKind())
        {
        case DistributionData::Ghost:
            {
                ENSURE(state.GetSenderId().m_id == -1, << "Ghost states are expected to have ConnectionId == -1! Ghost for "
                       << state.GetEntityId());
                EntityTypes::Instance().RemoteSetGhostEntityState(state);
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

                    if (m_endStates->IsDisconnected(senderId))
                    {
                        //The state belongs to a recently disconnected connection, so we
                        //just discard it.
                        return;
                    }

                    const ConnectionPtr connection = Connections::Instance().GetConnection(senderId, std::nothrow);
                    if (connection == NULL)
                    {
                        if (isAckedData)
                        {
                            m_waitingStates.Add(state);
                        }
                        else
                        {
                            lllout << "Discarding an unacked entity state since we don't have its connection" << std::endl;
                        }
                    }
                    else
                    {
                        const RemoteSetResult result = EntityTypes::Instance().RemoteSetRealEntityState(connection, state);

                        if (result == RemoteSetNeedRegistration)
                        {
                            m_waitingStates.Add(state);
                        }
                    }
                }
                else
                {
                    //delete states bypass all waitingstates handling and go straight
                    //into shared memory, and we keep a reference in endstates for a while

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

    int PoolHandler::handle_exception(ACE_HANDLE)
    {
        lllout << "Pool distribution is completed"<< std::endl;
        ACE_THR_FUNC_RETURN exitStatus;
        const int ret = ACE_Thread::join(m_pdThreadHandle,&exitStatus);
        ENSURE(exitStatus == NULL, << "Pool distribution thread exited with an unexpected status! code = " << exitStatus);
        ENSURE(ret == 0, << "PoolHandler::handle_exception: Failed to join PoolDistribution thread! return value = " << ret);
        m_pdThreadHandle = 0;

        m_connectionHandler->MaybeSignalConnectSemaphore();
        return 0;
    }


    void PoolHandler::DistributeStates()
    {
        m_connection->GetDirtySubscriptionQueue().Dispatch(boost::bind(&PoolHandler::DispatchSubscription,this,_1,_2,_3));
    }

    bool IsLocal(const DistributionData& state)
    {
        return state.GetSenderId().m_node == g_thisNode;
    }

    bool ProcessEntityState(ExternNodeCommunication * ecom,
                            const SubscriptionPtr & subscription)
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
                    if (ecom->Send(currentState))
                    {
                        subscription->SetLastRealState(currentState);
                    }
                    else
                    {
                        complete = false;
                    }
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
                    if (ecom->Send(currentState))
                    {
                        subscription->SetLastInjectionState(currentState);
                    }
                    else
                    {
                        complete = false;
                    }
                }
            }
        }

        return complete;
    }

    bool PDProcessEntityState(ExternNodeCommunication * ecom,
                              const SubscriptionPtr & subscription)
    {
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
                    ecom->SendPoolDistributionData(currentState);
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
                ecom->SendPoolDistributionData(currentState);
                subscription->SetLastInjectionState(currentState);
            }
        }

        return true; //PD send always succeeds
    }

    bool ProcessRegistrationState(ExternNodeCommunication * ecom,
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

        const bool success = ecom->Send(currentState);

        if (success)
        {
            pendingHandler->CheckForPending(currentState.GetTypeId());
            subscription->SetLastRealState(currentState);
        }

        return success;
    }

    bool PDProcessRegistrationState(ExternNodeCommunication * ecom,
                                    const SubscriptionPtr & subscription)
    {
        if (subscription->GetLastRealState().IsNoState())
        {
            const DistributionData state = subscription->GetCurrentRealState();


            if (!state.IsNoState())
            {
                //local states are to be sent to other nodes
                //unregistrationstates (regardless of whether they are local or not) must
                //  be sent, so that new nodes get the correct timestamps etc.
                if (IsLocal(state) ||
                    !state.IsRegistered())
                {
                    ecom->SendPoolDistributionData(state);
                }
            }
            subscription->SetLastRealState(state); //update this so we only dispatch this state once (see "if" above)
        }
        return true; //PD send always succeeds
    }

    void PoolHandler::DispatchSubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove)
    {
        DistributionData realState = subscription->GetState()->GetRealState();

        if (!realState.IsNoState() && realState.GetType() == DistributionData::RegistrationState)
        {
            // Registration state
            dontRemove = !subscription->DirtyFlag().Process(boost::bind(ProcessRegistrationState,
                                                                        m_ecom,
                                                                        m_pendingRegistrationHandler,
                                                                        boost::cref(subscription)));
        }
        else
        {
            // Entity state
            dontRemove = !subscription->DirtyFlag().Process(boost::bind(ProcessEntityState,
                                                                        m_ecom,
                                                                        boost::cref(subscription)));
        }

        //dontRemove is true if we got an overflow, and if we did we dont want to keep sending anything to dose_com.
        exitDispatch = dontRemove;
    }

    void PoolHandler::PDDispatchSubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove)
    {
        exitDispatch = false;
        dontRemove = false;

        DistributionData realState = subscription->GetState()->GetRealState();

        if (!realState.IsNoState() && realState.GetType() == DistributionData::RegistrationState)
        {
            // Registration state
            subscription->DirtyFlag().Process(boost::bind(PDProcessRegistrationState,
                                                          m_ecom,
                                                          boost::cref(subscription)));
        }
        else
        {
            // Entity state
            subscription->DirtyFlag().Process(boost::bind(PDProcessEntityState,
                                                          m_ecom,
                                                          boost::cref(subscription)));
        }
    }

    void PoolHandler::PerformStatesWaitingForConnection(const ConnectionId & connId)
    {
        //Note that all waiting states are acked states, hence the "true" at the end.
        m_waitingStates.PerformStatesWaitingForConnection
            (connId,
             boost::bind(&PoolHandler::HandleStateFromDoseCom,this,_1,true));
    }

    void PoolHandler::PerformStatesWaitingForRegistration(const DistributionData & registrationState)
    {
        //Note that all waiting states are acked states, hence the "true" at the end.
        m_waitingStates.PerformStatesWaitingForRegistration
            (registrationState,
             boost::bind(&PoolHandler::HandleStateFromDoseCom,this,_1,true));
    }

    void PoolHandler::HandleDisconnectFromDoseCom(const ConnectionId & connId)
    {
        m_waitingStates.Disconnect(connId);
    }

    void PoolHandler::RemoveStatesWaitingForNode(const Typesystem::Int32 node)
    {
        m_waitingStates.NodeDown(node);
        m_endStates->ClearDisconnectsFromNode(node);
    }
}
}
}
