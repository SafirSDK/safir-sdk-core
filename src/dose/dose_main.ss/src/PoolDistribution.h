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
#include <functional>
#include <boost/make_shared.hpp>
#include <boost/asio.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    static const int64_t PoolDistributionInfoDataTypeId=879351346; //TODO: "DoseMain.PoolDistributionInfo"
    enum class PoolDistributionInfo : uint8_t {RequestPd = 1, PdComplete = 2};

    ///
    /// This class handles a pool distribution to a single node.
    ///
    class PoolDistribution
    {
    public:



    private:

    };

    //********************************************************************************************************

    ///
    /// Responsible for sending poolDistributionRequests to all nodes at start-up.
    /// When all poolDistributions are received, this class calls the pdComplete callback.
    ///
    template <class CommunicationT>
    class PoolDistributionRequestor
    {
    public:
        PoolDistributionRequestor(boost::asio::io_service::strand& strand,
                                  CommunicationT& communication,
                                  std::unordered_map<int64_t, int64_t>& nodes,
                                  std::function<void()> pdComplete)
            :m_strand(strand)
            ,m_communication(communication)
            ,m_nodes(nodes)
            ,m_pdComplete(pdComplete)
        {
            for (auto& vt : m_nodes)
            {
                m_poolDistributionRequests.insert({vt.first, false});
            }
            m_strand.post([=]{SendPoolDistributionRequests();});
        }

        void ReceivedPoolDistributionCompleteFrom(int64_t nodeId)
        {
            m_poolDistributionRequests.erase(nodeId);
            if (m_poolDistributionRequests.empty())
            {
                m_pdComplete();
            }
        }

    private:
        boost::asio::io_service::strand& m_strand;
        CommunicationT& m_communication;
        std::unordered_map<int64_t, int64_t>& m_nodes;
        std::function<void()> m_pdComplete; //signal pdComplete after all our pdRequests has reported pdComplete
        std::unordered_map<int64_t, bool> m_poolDistributionRequests;

        void SendPoolDistributionRequests()
        {
            auto req=boost::make_shared<char[]>(sizeof(PoolDistributionInfo));
            (*reinterpret_cast<PoolDistributionInfo*>(req.get()))=PoolDistributionInfo::RequestPd;

            bool unsentRequests=false;

            for (auto it=m_poolDistributionRequests.begin(); it!=m_poolDistributionRequests.end();)
            {
                auto nodeIt=m_nodes.find(it->first);
                if (nodeIt==m_nodes.end())
                {
                    //node does no longer exist, remove it since we no longer expect to get a pd from that node.
                    it=m_poolDistributionRequests.erase(it);
                    continue;
                }

                if (it->second)
                {
                    //pd request has already been successfully sent in previous calls to this method
                    ++it;
                    continue;
                }

                //try to send pd request to the node
                if (m_communication.Send(nodeIt->first, nodeIt->second, req, sizeof(PoolDistributionInfo), PoolDistributionInfoDataTypeId, true))
                {
                    it->second=true; //pd request has been sent
                    ++it;
                }
                else
                {
                    unsentRequests=true; //could not send request right now due to overflow
                    ++it;
                }
            }

            if (unsentRequests)
            {
                //there are nodes that has not got an pd request yet, retry later
                m_strand.post([=]{SendPoolDistributionRequests();}); //set timer instead
            }
        }
    };
}
}
}
