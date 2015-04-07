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
#include <boost/asio.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    ///
    /// This class is responsible for handling all pool distributions to other nodes.
    /// It will keep a queue of PoolDistributionRequests and make sure that only one
    /// pd is active at the same time. It will also ensure that pool distributions will
    /// be sending in a pace that wont flood the network and cause starvation in other parts
    /// of the system.
    ///
    template <class CommunicationT, class PoolDistributionT>
    class PoolDistributionHandler : private boost::noncopyable
    {
    public:
        PoolDistributionHandler(boost::asio::io_service& io, CommunicationT& com)
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
            //std::wcout<<L"AddPoolDistribution "<<nodeId<<std::endl;
            m_strand.dispatch([this, nodeId, nodeTypeId]
            {
                auto pd=PdPtr(new PoolDistributionT(nodeId, nodeTypeId, m_strand, m_communication, [=](int64_t /*nodeId*/)
                {
                    if (!m_pendingPoolDistributions.empty())
                        m_pendingPoolDistributions.pop();
                    StartNextPoolDistribution();
                }));

                m_pendingPoolDistributions.push(std::move(pd));

                if (m_pendingPoolDistributions.size()==1) //if queue was empty we have to manually start the pd. If not the completion handler of the ongoing pd will start the next one.
                {
                    StartNextPoolDistribution();
                }
            });
        }

#ifndef SAFIR_TEST
    private:
#endif
        boost::asio::io_service::strand m_strand;
        CommunicationT& m_communication;
        bool m_running=false;

        using PdPtr = std::unique_ptr< PoolDistributionT >;
        std::queue<PdPtr> m_pendingPoolDistributions;

        void StartNextPoolDistribution()
        {
            if (m_running && !m_pendingPoolDistributions.empty())
            {
                m_pendingPoolDistributions.front()->Run();
            }
        }
    };
}
}
}
