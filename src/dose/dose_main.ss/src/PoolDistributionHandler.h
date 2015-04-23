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
#include <deque>
#include <functional>

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
    class PoolDistributionHandler : private boost::noncopyable
    {
    public:
        PoolDistributionHandler(boost::asio::io_service& io, DistributionT& distribution)
            :m_strand(io)
            ,m_distribution(distribution)
        {
        }

        //make sure that start is not called before persistent data is ready
        void Start()
        {
            m_strand.dispatch([=]
            {
                if (!m_running) //dont call start if its already started
                {
                    m_haveNothing=false;
                    m_running=true;
                    StartNextPoolDistribution();
                }
            });
        }

        void Stop()
        {
            m_strand.post([=]
            {
                m_running=false;
                if (!m_pendingPoolDistributions.empty())
                {
                    m_pendingPoolDistributions.front()->Cancel();
                }
            });
        }

        void SetHaveNothing()
        {
            m_strand.dispatch([=]
            {
                if (m_running)
                    return; //not allowed if we are started

                m_haveNothing=true;
                while (!m_pendingPoolDistributions.empty())
                {
                    SendHaveNothing(m_pendingPoolDistributions.front()->NodeId(), m_pendingPoolDistributions.front()->NodeType());
                    m_pendingPoolDistributions.pop_front();
                }
            });
        }

        //Called when a new pdRequest is received. Will result in a pd to the node with specified id.
        void AddPoolDistribution(int64_t nodeId, int64_t nodeTypeId)
        {
            m_strand.dispatch([this, nodeId, nodeTypeId]
            {
                if (m_haveNothing)
                {
                    SendHaveNothing(nodeId, nodeTypeId);
                    return;
                }

                auto pd=PdPtr(new PoolDistributionT(nodeId, nodeTypeId, m_strand, m_distribution, [=](int64_t nodeId)
                {
                    if (!m_pendingPoolDistributions.empty() && m_pendingPoolDistributions.front()->NodeId()==nodeId)
                    {
                        m_strand.post([=] //since this code is called from the m_pendingPoolDistributions.front object, we must post before we can pop_front
                        {
                            m_pendingPoolDistributions.pop_front();
                            StartNextPoolDistribution();
                        });
                    }
                }));

                m_pendingPoolDistributions.push_back(std::move(pd));

                if (m_pendingPoolDistributions.size()==1) //if queue was empty we have to manually start the pd. If not the completion handler of the ongoing pd will start the next one.
                {
                    StartNextPoolDistribution();
                }
            });
        }

        void RemovePoolDistribution(int64_t nodeId)
        {
            m_strand.post([this, nodeId]
            {
                if (m_pendingPoolDistributions.empty())
                {
                    return;
                }

                auto it=++m_pendingPoolDistributions.begin(); //start at second element since the first one is probably already running

                for (; it!=m_pendingPoolDistributions.end(); ++it)
                {
                    if ((*it)->NodeId()==nodeId)
                    {
                        m_pendingPoolDistributions.erase(it);
                        break; //finished
                    }
                }
            });
        }

#ifndef SAFIR_TEST
    private:
#endif
        boost::asio::io_service::strand m_strand;
        DistributionT& m_distribution;
        bool m_running=false;
        bool m_haveNothing=false;

        using PdPtr = std::unique_ptr< PoolDistributionT >;
        std::deque<PdPtr> m_pendingPoolDistributions;

        void StartNextPoolDistribution()
        {
            if (m_running && !m_pendingPoolDistributions.empty())
            {
                m_pendingPoolDistributions.front()->Run();
            }
        }

        void SendHaveNothing(int64_t nodeId, int64_t nodeType)
        {
            auto msg=boost::make_shared<char[]>(sizeof(PoolDistributionInfo));
            (*reinterpret_cast<PoolDistributionInfo*>(msg.get()))=PoolDistributionInfo::PdHaveNothing;

            if (!m_distribution.GetCommunication().Send(nodeId, nodeType, msg, sizeof(PoolDistributionInfo), PoolDistributionInfoDataTypeId, true))
            {
                m_strand.post([=]{SendHaveNothing(nodeId, nodeType);});
            }
        }
    };
}
}
}
