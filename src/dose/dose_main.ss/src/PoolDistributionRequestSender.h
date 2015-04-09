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
#include <vector>
#include <functional>
#include <boost/make_shared.hpp>
#include <boost/asio.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    static const int64_t PoolDistributionInfoDataTypeId=-3446507522969672286; //DoseMain.PoolDistributionInfo
    enum class PoolDistributionInfo : uint8_t
    {
        RequestPd = 1,
        PdComplete = 2
    };
    //---------------------------------------------------------

    ///
    /// Responsible for sending poolDistributionRequests to all nodes at start-up.
    /// When all poolDistributions are received, this class calls the pdComplete callback.
    ///
    template <class CommunicationT>
    class PoolDistributionRequestSender
    {
    public:
        PoolDistributionRequestSender(boost::asio::io_service& io,
                                  CommunicationT& communication)
            :m_strand(io)
            ,m_communication(communication)
        {
        }

        void Start(const std::function<void()>& pdComplete)
        {
            m_strand.dispatch([=]
            {
                m_pdComplete=pdComplete;
                m_running=true;
                if (m_requests.empty())
                {
                    m_pdComplete();
                }
                else
                {
                    SendPoolDistributionRequests();
                }
            });

        }

        void Stop()
        {
            m_strand.post([=] //use post to prevent that stop is executed before start
            {
                m_running=false;
                m_requests.clear();
            });
        }

        void RequestPoolFrom(int64_t nodeId, int64_t nodeTypeId)
        {
            m_strand.post([=]
            {
                m_requests.push_back({nodeId, nodeTypeId, false});
                SendPoolDistributionRequests();
            });
        }

        void ReceivedPoolDistributionCompleteFrom(int64_t nodeId)
        {
            m_strand.dispatch([=]
            {
                auto it=std::find_if(m_requests.begin(), m_requests.end(), [nodeId](const PdReq& r){return r.nodeId==nodeId;});
                if (it!=m_requests.end())
                {
                    m_requests.erase(it);
                }

                if (m_requests.empty())
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
            int64_t nodeId;
            int64_t nodeType;
            bool sent;
        };

        bool m_running=false;

        boost::asio::io_service::strand m_strand;
        CommunicationT& m_communication;
        std::function<void()> m_pdComplete; //signal pdComplete after all our pdRequests has reported pdComplete
        std::vector<PdReq> m_requests;

        void SendPoolDistributionRequests()
        {
            if (!m_running)
            {
                return;
            }

            auto req=boost::make_shared<char[]>(sizeof(PoolDistributionInfo));
            (*reinterpret_cast<PoolDistributionInfo*>(req.get()))=PoolDistributionInfo::RequestPd;

            bool unsentRequests=false;

            for (auto& r : m_requests)
            {
                if (!r.sent)
                {
                    if (m_communication.Send(r.nodeId, r.nodeType, req, sizeof(PoolDistributionInfo), PoolDistributionInfoDataTypeId, true))
                    {
                        r.sent=true;
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
                m_strand.post([=]{SendPoolDistributionRequests();}); //a bit aggressive, maybe we should set a timer instead
            }
        }
    };
}
}
}