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
#include <deque>
#include <functional>
#include <Safir/Utilities/Internal/VisibilityHelpers.h>
#include <Safir/Utilities/Internal/SharedCharArray.h>
#include <Safir/Dob/Internal/SmartSyncState.h>
#include "PoolDistributionRequestSender.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

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
    template <class DistributionT, class PoolDistributionT>
    class PoolDistributionHandler
    {
    public:
        PoolDistributionHandler(boost::asio::io_service& io, DistributionT& distribution)
            :m_strand(io)
            ,m_distribution(distribution)
            ,m_running(false)
        {
        }

        PoolDistributionHandler(const PoolDistributionHandler&) = delete;
        const PoolDistributionHandler& operator=(const PoolDistributionHandler&) = delete;

        // Make sure that start is not called before persistent data is ready
        void Start()
        {
            lllog(5) << "PoolHandler: Start called on PooDistributionHandler" << std::endl;
            boost::asio::dispatch(m_strand, [this]
            {
                if (!m_running) //dont call start if its already started
                {
                    m_running=true;
                    StartNextPoolDistribution();
                }
            });
        }

        void Stop(const std::function<void()>& onPoolDistributionsCancelled)
        {
            lllog(5) << "PoolHandler: Stop called on PooDistributionHandler" << std::endl;
            boost::asio::post(m_strand, [this, onPoolDistributionsCancelled]
            {
                lllog(5) << "PoolHandler: PooDistributionHandler calls cancel on all ongoing and pending PDs" << std::endl;
                m_running=false;
                if (!m_pendingPoolDistributions.empty() && m_pendingPoolDistributions.front()->IsStarted())
                {
                    // The first pd in list is currently running.
                    auto i = 0;
                    for (auto& pendingPd : m_pendingPoolDistributions)
                    {
                        if (i++ == 0)
                        {
                            pendingPd->Cancel(onPoolDistributionsCancelled);
                        }
                        else
                        {
                            pendingPd->Cancel();
                        }
                    }
                    m_pendingPoolDistributions.clear();
                }
                else
                {
                    for (auto& pendingPd : m_pendingPoolDistributions)
                    {
                        pendingPd->Cancel();
                    }
                    m_pendingPoolDistributions.clear();

                    onPoolDistributionsCancelled();
                }
            });
        }

        // Called when a new pdRequest is received. Will result in a pd to the node with specified id.
        // This method can be called before Start, but no PD will begin to run before Start is called.
        void AddPoolDistribution(int64_t nodeId, int64_t nodeTypeId, const std::shared_ptr<SmartSyncState>& syncState) SAFIR_GCC_VISIBILITY_BUG_WORKAROUND
        {
            lllog(5) << "PoolHandler: AddPoolDistribution to " << nodeId << std::endl;
            boost::asio::post(m_strand, [this, nodeId, nodeTypeId, syncState]
            {
                auto pd = std::make_shared<PoolDistributionT> (nodeId,
                                                               nodeTypeId,
                                                               syncState,
                                                               m_strand,
                                                               m_distribution,
                                                               [this](int64_t nodeId)
                                    {
                                        lllog(5) << "PoolHandler: PD completionHandler nodeId=" << nodeId << std::endl;
                                        if (!m_pendingPoolDistributions.empty() && m_pendingPoolDistributions.front()->NodeId()==nodeId)
                                        {
                                            m_pendingPoolDistributions.pop_front();
                                            StartNextPoolDistribution();
                                        }
                                    });

                m_pendingPoolDistributions.push_back(std::move(pd));

                StartNextPoolDistribution();
            });
        }

        void RemovePoolDistribution(int64_t nodeId)
        {
            boost::asio::post(m_strand, [this, nodeId]
            {
                for (auto it = std::begin(m_pendingPoolDistributions); it != std::end(m_pendingPoolDistributions); ++it)
                {
                    if ((*it)->NodeId()==nodeId)
                    {
                        lllog(5) << "PoolHandler: RemovePoolDistribution to nodeId=" << nodeId << std::endl;
                        (*it)->Cancel();
                        m_pendingPoolDistributions.erase(it);
                        break; //finished
                    }
                }
                StartNextPoolDistribution();
            });
        }

#ifndef SAFIR_TEST
    private:
#endif
        boost::asio::io_service::strand m_strand;
        DistributionT& m_distribution;
        bool m_running;

        typedef std::shared_ptr<PoolDistributionT> PdPtr;
        std::deque<PdPtr> m_pendingPoolDistributions;


        void StartNextPoolDistribution()
        {
            // This is always called from m_strand
            if (m_running && !m_pendingPoolDistributions.empty())
            {
                lllog(5) << "PoolHandler: StartNextPoolDistribution to " << m_pendingPoolDistributions.front()->NodeId() << std::endl;
                m_pendingPoolDistributions.front()->Run();
            }
        }
    };
}
}
}
