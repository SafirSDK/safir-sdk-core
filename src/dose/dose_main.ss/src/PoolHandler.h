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
#pragma once
#include <unordered_map>
#include "WaitingStates.h"
#include "Distribution.h"
#include "PoolDistributionRequestSender.h"
#include "PoolDistributionHandler.h"
#include "PoolDistribution.h"
#include "StateDistributor.h"
#include "PersistHandler.h"
#include "NodeInfoHandler.h"

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
    class PoolHandler : private boost::noncopyable
    {
    public:
        PoolHandler(boost::asio::io_context::strand& strand,
                    Distribution& distribution,
                    const std::function<void(int64_t)>& checkPendingReg,
                    const std::function<void(const std::string& str)>& logStatus);

        void Start();
        void Stop(const std::function<void()>& onPoolDistributionsCancelled);

        // The pool handler must be aware of connections/disconnections arriving from external nodes
        // in order to properly handle the "waiting states".
        void HandleConnect(const ConnectionId& connId);
        void HandleDisconnect(const ConnectionId& connId);

        void SetDetached(bool detach);

    private:
        typedef PoolDistribution<Distribution> PoolDistributionType;
        typedef PoolDistributionHandler<Distribution, PoolDistributionType> PoolDistributionHandlerType;
        typedef PoolDistributionRequestSender<Distribution, Connections> PoolDistributionRequestSenderType;
        typedef StateDistributor<Distribution> StateDistributorType;

        boost::asio::io_context::strand& m_strand;
        Distribution& m_distribution;
        std::function<void(const std::string& str)> m_log;
        PoolDistributionHandlerType m_poolDistributor;
        PoolDistributionRequestSenderType m_poolDistributionRequests;
        std::unique_ptr<PersistHandler> m_persistHandler;
        WaitingStates m_waitingStates;
        boost::asio::steady_timer m_waitingStatesSanityTimer;

        std::unordered_map<int64_t, int64_t> m_nodes; //map<nodeId, nodeType>
        std::unordered_map<int64_t, std::unique_ptr<StateDistributorType> > m_stateDistributors; //map<nodeType, StateDistributor>

        std::function<void()> m_poolDistributionCompleteCallback;
        bool m_persistenceReady;

        //The NodeInfoHandler can not be started until we have pd complete
        //so the PoolHandler has to own it.
        std::unique_ptr<NodeInfoHandler> m_nodeInfoHandler;
        Safir::Dob::NodeState::Enumeration m_nodeState;
        bool m_running;

        void RunWaitingStatesSanityCheckTimer();

        //when a new registration arrives a list of waiting states need to be checked
        //to see if there are any states that need to be "set".
        void HandleStatesWaitingForRegistration(const DistributionData& registrationState);

        void HandleRegistrationState(const DistributionData& state, int64_t fromNodeType);
        void HandleEntityState(const DistributionData& state, int64_t fromNodeType);

        //PersistenceHandler reports Persistet data ready
        void OnPersistenceReady(bool fromDope);

        //other node is requesting a pd or report pdComplete
        void OnPoolDistributionInfo(int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t size);
        void OnAllPoolsReceived();

        //received registration state from other node
        void OnRegistrationState(int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t size);

        //received entity state from other node
        void OnEntityState(int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t size);

        //convert a string to HandlerId by first trying to cast the string as int64_t
        Safir::Dob::Typesystem::HandlerId ToHandlerId(const std::string& idStr) const;
    };
}
}
}
