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
#include "Distribution.h"
#include "PoolDistribution.h"
#include "StateDistributor.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
    ///
    /// PoolHandler is responsible for keeping the pool synced with all other nodes.
    /// It handles pool distribution to other nodes, and it request pools from other
    /// existing nodes at start-up.
    ///
    ///
    class PoolHandler : private boost::noncopyable
    {
    public:
        PoolHandler(boost::asio::io_service::strand& strand, Distribution& distribution);

        void Start(const std::function<void()>& poolDistributionComplete);
        void Stop();

    private:
        boost::asio::io_service::strand& m_strand;
        Com::Communication& m_communication;
        std::unordered_map<int64_t, int64_t> m_nodes; //map<nodeId, nodeType>
        std::unique_ptr<PoolDistributionRequestor<Com::Communication> > m_poolDistributionRequests;

        //called from Distribution when new nodes are injected or excluded
        void OnInjectNode(const std::string& nodeName, int64_t nodeId, int64_t nodeTypeId, const std::string& dataAddress);
        void OnExcludeNode(int64_t nodeId, int64_t nodeTypeId);

        //other node is requesting a pd or report pdComplete
        void OnPoolDistributionInfo(int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t size);
    };
}
}
}

//#ifndef _dose_main_pool_handler_h
//#define _dose_main_pool_handler_h

//#include <Safir/Dob/Internal/InternalFwd.h>
//#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
//#include <Safir/Dob/Connection.h>
//#include <Safir/Dob/Internal/ConnectionId.h>
//#include <Safir/Utilities/Array.h>
//#include <map>
//#include <deque>
//#include <boost/array.hpp>
//#include <boost/thread.hpp>
//#include <boost/function.hpp>
//#include <Safir/Utilities/Internal/LowLevelLogger.h>

//#ifdef _MSC_VER
//#pragma warning (push)
//#pragma warning (disable: 4267)
//#endif

//#include <boost/asio.hpp>

//#ifdef _MSC_VER
//#pragma warning (pop)
//#endif

//namespace Safir
//{
//namespace Dob
//{
//namespace Internal
//{
//    //forward declarations
//    class BlockingHandlers;
//#if 0 //stewart
//    class ExternNodeCommunication;
//#endif
//    class PendingRegistrationHandler;
//    class PersistHandler;
//    class ConnectionHandler;

//    class DummySubscriber:
//        public RegistrationSubscriber,
//        public EntitySubscriber
//    {
//    private:
//        virtual void OnRegistered(const Safir::Dob::Typesystem::TypeId     typeId,
//                                  const Safir::Dob::Typesystem::HandlerId&  handlerId)
//        {ENSURE(false, << "DummySubscriber got an OnRegistered for type = " << typeId << ", handler = " << handlerId);}

//        virtual void OnUnregistered(const Safir::Dob::Typesystem::TypeId    typeId,
//                                    const Safir::Dob::Typesystem::HandlerId&  handlerId)
//        {ENSURE(false, << "DummySubscriber got an OnUnregistered for type = " << typeId << ", handler = " << handlerId);}

//        virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
//        {ENSURE(false, << "DummySubscriber got an OnNewEntity for entity = " << entityProxy.GetEntityId() << ".");}

//        virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
//        {ENSURE(false, << "DummySubscriber got an OnUpdatedEntity for entity = " << entityProxy.GetEntityId() << ".");}

//        virtual void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool /*deletedByOwner*/)
//        {ENSURE(false, << "DummySubscriber got an OnDeletedEntity for entity = " << entityProxy.GetEntityId() << ".");}

//    };

//    class StateDispatcher:
//        public Safir::Dob::Dispatcher,
//        private boost::noncopyable
//    {
//    public:
//        StateDispatcher(const boost::function <void(void)> & dispatchFunc,
//                        boost::asio::io_service::strand& strand):
//            m_dispatchFunc(dispatchFunc),
//            m_isNotified(0),
//            m_strand(strand)
//        {}

//    private:

//        virtual void OnDoDispatch()
//        {
//            if (m_isNotified == 0)
//            {
//                m_isNotified = 1;
//                m_strand.post([this]()
//                              {
//                                  m_isNotified = 0;
//                                  m_dispatchFunc();
//                              });
//            }
//        }

//        const boost::function <void(void)>       m_dispatchFunc;
//        Safir::Utilities::Internal::AtomicUint32 m_isNotified;
//        boost::asio::io_service::strand&         m_strand;
//    };

//    class DummyDispatcher:
//        public Safir::Dob::Dispatcher
//    {
//        virtual void OnDoDispatch() {}
//    };


//    class PoolHandler
//    {
//    public:
//        explicit PoolHandler(boost::asio::io_service::strand& strand);
//        virtual ~PoolHandler();

//        void Init(
//#if 0 //stewart
//                  ExternNodeCommunication & ecom,
//#endif
//                  PendingRegistrationHandler & pendingHandler,
//                  PersistHandler & persistHandler,
//                  ConnectionHandler & connectionHandler);


//        void Stop();
        
//        void StartPoolDistribution();

//        // Request pooldistribution from nodeId
//        void RequestPoolDistribution(const int nodeId);

//        void DistributeStates();

//        /** Handle a state from another node.
//         * This must only be called with EntityState or RegistrationState DDs.
//         */
//        void HandleStateFromDoseCom(const DistributionData & state, const bool isAckedData);


//        /** Handle a pooldistribution request from another node.
//         */
//        void HandleMessageFromDoseCom(const DistributionData& data);

//        /** When a connection is removed we need to clear stuff from the waitingstates structure */
//        void HandleDisconnectFromDoseCom(const ConnectionId & connId);

//        void RemoveStatesWaitingForNode(const Typesystem::Int32 node);
//    private:
//        class ExceptionInfo;

//        void HandleRegistrationStateFromDoseCom(const DistributionData& state, const bool isAckedData);
//        void HandleEntityStateFromDoseCom(const DistributionData& state, const bool isAckedData);

//        //handle pool distribution complete
//        void PDCompletedHandler(const boost::shared_ptr<ExceptionInfo>& exceptionInfo);

//        void PoolDistributionWorker();

//        void DispatchSubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove);

//        void DispatchInjectedEntityId(const Typesystem::EntityId & entityId, bool & remove);

//        bool PDProcessRegistrationState(const SubscriptionPtr & subscription);
//        bool PDProcessEntityState(const SubscriptionPtr & subscription);

//        void PDConnection(const Connection & connection);
//        void PDDispatchSubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove);

//#if 0 //stewart
//        ExternNodeCommunication* m_ecom;
//#endif
//        PendingRegistrationHandler * m_pendingRegistrationHandler;
//        PersistHandler * m_persistHandler;
//        ConnectionHandler * m_connectionHandler;

//        boost::thread::id m_poolDistributionThreadId;

//        typedef std::vector<DistributionData> ConnectionMsgs;
//        ConnectionMsgs ConnectionMsgsToSend;

//        struct SubcriptionConnection
//        {
//            Safir::Dob::Connection m_connection;

//            //This is the Internal representation of the m_stateSubscriptionConnection.
//            Safir::Dob::Internal::ConnectionPtr m_connectionPtr;
//        };

//        //Safir::Dob::Utilities::Array<SubcriptionConnection> m_stateSubscriptionConnections;

//        Safir::Utilities::Array<SubcriptionConnection> m_stateSubscriptionConnections;
        
//        boost::shared_ptr<StateDispatcher> m_stateDispatcher;
//        DummySubscriber m_dummySubscriber;

//        boost::thread m_pdThread;

//        boost::asio::io_service::strand& m_strand;
//    };
//}
//}
//}
//#endif
