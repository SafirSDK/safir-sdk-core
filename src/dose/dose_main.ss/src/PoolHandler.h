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
#include "PersistHandler.h"

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
        PoolHandler(boost::asio::io_service& io,
                    Distribution& distribution,
                    const std::function<void(int64_t)>& checkPendingReg);

        void Start(const std::function<void()>& poolDistributionComplete);
        void Stop();

    private:
        boost::asio::io_service::strand m_strand;
        boost::asio::steady_timer m_endStatesTimer;
        Com::Communication& m_communication;
        PoolDistributor<Com::Communication> m_poolDistributor;
        PoolDistributionRequestor<Com::Communication> m_poolDistributionRequests;
        PersistHandler m_persistHandler;
        std::unordered_map<int64_t, int64_t> m_nodes; //map<nodeId, nodeType>
        std::unordered_map<int64_t, std::unique_ptr<StateDistributor<Com::Communication> > > m_stateDistributors; //map<nodeType, StateDistributor>

        bool m_poolDistributionComplete=false;

        void RunEndStatesTimer();

        //other node is requesting a pd or report pdComplete
        void OnPoolDistributionInfo(int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t size);

        //received registration state from other node
        void OnRegistrationState(int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t size);

        //received entity state from other node
        void OnEntityState(int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t size);
    };
}
}
}
