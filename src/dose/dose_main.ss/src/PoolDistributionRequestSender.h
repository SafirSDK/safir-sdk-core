/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
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
#include <vector>
#include <functional>
#include <Safir/Utilities/Internal/SharedCharArray.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

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
    static const int64_t PoolDistributionInfoDataTypeId=-3446507522969672286; //DoseMain.PoolDistributionInfo
    enum PoolDistributionInfo
    {
        PdRequest = 1,      //Request a pool distribution from another node
        PdComplete = 2,     //Answer to PdRequest: my pool distribution is completed including persistent data
        PdHaveNothing = 3   //Answer to PdRequest: you can ignore me, my pool is empty and I have no persistence ready
    };
    //---------------------------------------------------------

    ///
    /// Responsible for sending poolDistributionRequests to all nodes at start-up.
    /// When all poolDistributions are received, this class calls the pdComplete callback.
    ///
    template <class DistributionT>
    class PoolDistributionRequestSender
    {
    public:
        PoolDistributionRequestSender(boost::asio::io_service& io,
                                  DistributionT& distribution)
            :m_running(false)
            ,m_strand(io)
            ,m_distribution(distribution)
        {
        }

        void Start(const std::function<void()>& pdComplete)
        {
            m_strand.dispatch([this,pdComplete]
            {
                m_pdComplete=pdComplete;
                m_running=true;
                if (IsAllNormalNodesCompleted())
                {
                    m_pdComplete();
                }

                SendPoolDistributionRequests();
            });

        }

        void Stop()
        {
            m_strand.post([this] //use post to prevent that stop is executed before start
            {
                m_running=false;
                m_requests.clear();
            });
        }

        void RequestPoolDistribution(int64_t nodeId, int64_t nodeTypeId)
        {
            m_strand.post([this, nodeId, nodeTypeId]
            {
                lllog(5)<<"PoolHandler: Request pooldistribution from "<< nodeId <<std::endl;
                m_requests.push_back(PoolDistributionRequestSender<DistributionT>::PdReq(nodeId, nodeTypeId, false));
                SendPoolDistributionRequests();
            });
        }

        void ClearNonLightNodesPoolDistributions()
        {
            m_strand.dispatch([this]
            {
                for (auto it = std::cbegin(m_requests); it != std::cend(m_requests);)
                {
                    if (!m_distribution.IsLightNode(it->nodeType))
                    {
                        it = m_requests.erase(it);
                    }
                    else
                    {
                        ++it;
                    }
                }

                if (IsAllNormalNodesCompleted())
                {
                    m_pdComplete();
                }
            });
        }

        //fromNodeId=0 means all queued requests are finished
        void PoolDistributionFinished(int64_t fromNodeId)
        {
            m_strand.dispatch([this,fromNodeId]
            {
                lllog(5)<<"PoolHandler: PoolDistributionFinished from "<< fromNodeId <<std::endl;
                if (fromNodeId==0)
                {
                    m_requests.clear();
                }
                else
                {
                    auto it=std::find_if(m_requests.begin(), m_requests.end(), [fromNodeId](const PdReq& r){return r.nodeId==fromNodeId;});
                    if (it!=m_requests.end())
                    {
                        m_requests.erase(it);
                    }
                }

                if (IsAllNormalNodesCompleted())
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
            PdReq(int64_t nodeId_, int64_t nodeType_, bool sent_) 
                : nodeId(nodeId_)
                , nodeType(nodeType_)
                , sent(sent_)
            {
            }

            int64_t nodeId;
            int64_t nodeType;
            bool sent;
        };

        bool m_running;

        boost::asio::io_service::strand m_strand;
        DistributionT& m_distribution;

        std::function<void()> m_pdComplete; //signal pdComplete after all our pdRequests has reported pdComplete
        std::vector<PdReq> m_requests;

        void SendPoolDistributionRequests()
        {
            if (!m_running || m_requests.empty())
            {
                return;
            }

            auto req=Safir::Utilities::Internal::MakeSharedArray(sizeof(PoolDistributionInfo));
            (*reinterpret_cast<PoolDistributionInfo*>(req.get()))= PdRequest;

            bool unsentRequests=false;

            for (auto r = m_requests.begin(); r != m_requests.end(); ++r)
            {
                if (!r->sent)
                {
                    if (m_distribution.GetCommunication().Send(r->nodeId, r->nodeType, req, sizeof(PoolDistributionInfo), PoolDistributionInfoDataTypeId, true))
                    {
                        r->sent=true;
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
                m_strand.post([this]{SendPoolDistributionRequests();}); //a bit aggressive, maybe we should set a timer instead
            }
        }

        bool IsAllNormalNodesCompleted() const
        {
            auto it = std::find_if(std::cbegin(m_requests), std::cend(m_requests), [this](const PdReq& r){return !m_distribution.IsLightNode(r.nodeType);});
            return it == std::cend(m_requests);
        }
    };
}
}
}
