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

#ifndef _dose_main_pool_handler_h
#define _dose_main_pool_handler_h

#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Internal/ConnectionId.h>
#include <ace/Event_Handler.h>
#include <map>
#include <deque>
#include <boost/function.hpp>
#include <ace/Reactor.h>
#include "dose_main_waiting_states.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
    //forward declarations
    class BlockingHandlers;
    class ExternNodeCommunication;
    class PendingRegistrationHandler;
    class PersistHandler;
    class ConnectionHandler;
    class EndStatesHandler;

    class DummySubscriber:
        public RegistrationSubscriber,
        public EntitySubscriber
    {
    private:
        virtual void OnRegistered(const Safir::Dob::Typesystem::TypeId     typeId,
                                  const Safir::Dob::Typesystem::HandlerId&  handlerId)
        {ENSURE(false, << "DummySubscriber got an OnRegistered for type = " << typeId << ", handler = " << handlerId);}

        virtual void OnUnregistered(const Safir::Dob::Typesystem::TypeId    typeId,
                                    const Safir::Dob::Typesystem::HandlerId&  handlerId)
        {ENSURE(false, << "DummySubscriber got an OnUnregistered for type = " << typeId << ", handler = " << handlerId);}

        virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
        {ENSURE(false, << "DummySubscriber got an OnNewEntity for entity = " << entityProxy.GetEntityId() << ".");}

        virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
        {ENSURE(false, << "DummySubscriber got an OnUpdatedEntity for entity = " << entityProxy.GetEntityId() << ".");}

        virtual void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool /*deletedByOwner*/)
        {ENSURE(false, << "DummySubscriber got an OnDeletedEntity for entity = " << entityProxy.GetEntityId() << ".");}

    };

    class StateDispatcher:
        public Safir::Dob::Dispatcher,
        public ACE_Event_Handler,
        private boost::noncopyable
    {
    public:
        StateDispatcher(const Safir::Dob::Connection & connection,
                      const boost::function <void(void)> & dispatchFunc):
            ACE_Event_Handler(ACE_Reactor::instance()),
            m_connection(connection),
            m_dispatchFunc(dispatchFunc),
            m_isNotified(0){}

    private:
        virtual int handle_exception(ACE_HANDLE)
        {
            m_isNotified = 0;
            m_dispatchFunc();
            return 0;
        }

        virtual void OnDoDispatch()
        {
            if (m_isNotified == 0)
            {
                m_isNotified = 1;
                reactor()->notify(this);
            }
        }


        const Safir::Dob::Connection&       m_connection;
        const boost::function <void(void)>  m_dispatchFunc;
        AtomicUint32                        m_isNotified;
    };

    class DummyDispatcher:
        public Safir::Dob::Dispatcher
    {
        virtual void OnDoDispatch() {}
    };


    class PoolHandler:
        public ACE_Event_Handler
    {
    public:
        PoolHandler();
        virtual ~PoolHandler();

        void Init(BlockingHandlers & blockingHandler,
                  ExternNodeCommunication & ecom,
                  PendingRegistrationHandler & pendingHandler,
                  PersistHandler & persistHandler,
                  EndStatesHandler& endStates,
                  ConnectionHandler & connectionHandler);

        void StartPoolDistribution();

        void DistributeStates();

        /** Handle a state from another node.
         * This must only be called with EntityState or RegistrationState DDs.
         */
        void HandleStateFromDoseCom(const DistributionData & state, const bool isAckedData);

        /** when a new connection arrives a list of waiting states need to be checked
          * to see if there are any states that need to be "set".
          */
        void HandleConnectFromDoseCom(const ConnectionId & connId)
        {PerformStatesWaitingForConnection(connId);}

        /** When a connection is removed we need to clear stuff from the waitingstates structure */
        void HandleDisconnectFromDoseCom(const ConnectionId & connId);

        void RemoveStatesWaitingForNode(const Typesystem::Int32 node);
    private:
        void PerformStatesWaitingForConnection(const ConnectionId & connId);

        //when a new registration arrives a list of waiting states need to be checked
        //to see if there are any states that need to be "set".
        void PerformStatesWaitingForRegistration(const DistributionData & registrationState);

        void HandleRegistrationStateFromDoseCom(const DistributionData& state, const bool isAckedData);
        void HandleEntityStateFromDoseCom(const DistributionData& state, const bool isAckedData);

        //handle pool distribution complete
        virtual int handle_exception(ACE_HANDLE);

        static ACE_THR_FUNC_RETURN PoolDistributionThreadFunc(void * _this);
        void PoolDistributionWorker();

        void DispatchSubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove);

        void DispatchInjectedEntityId(const Typesystem::EntityId & entityId, bool & remove);

        void PDConnection(const Connection & connection);
        void PDDispatchSubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove);

        ExternNodeCommunication* m_ecom;
        BlockingHandlers* m_blockingHandler;
        PendingRegistrationHandler * m_pendingRegistrationHandler;
        PersistHandler * m_persistHandler;
        EndStatesHandler* m_endStates;
        ConnectionHandler * m_connectionHandler;

        Safir::Dob::Connection m_stateSubscriptionConnection;
        boost::shared_ptr<StateDispatcher> m_stateDispatcher;
        DummySubscriber m_dummySubscriber;

        //This is the Internal representation of the m_stateSubscriptionConnection.
        Safir::Dob::Internal::Connection * m_connection;

        ACE_hthread_t m_pdThreadHandle;

        WaitingStates m_waitingStates;

        typedef std::vector<DistributionData> ConnectionMsgs;
        ConnectionMsgs ConnectionMsgsToSend;

    };
}
}
}
#endif
