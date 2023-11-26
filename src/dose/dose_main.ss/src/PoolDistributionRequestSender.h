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
#include <vector>
#include <functional>
#include <Safir/Utilities/Internal/SharedCharArray.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/SmartSyncState.h>

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4267)
#  pragma warning (disable: 4127)
#  pragma warning (disable: 4100)
#endif

#include <boost/asio.hpp>
#include "PoolSyncInfo.pb.h"

#ifdef _MSC_VER
#  pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
    static const int64_t PoolDistributionInfoDataTypeId=-3446507522969672286; //DoseMain.PoolDistributionInfo
    //---------------------------------------------------------

    ///
    /// Responsible for sending poolDistributionRequests to all nodes at start-up.
    /// When all poolDistributions are received, this class calls the pdComplete callback.
    ///
    template <class DistributionT, class ConnectionsT>
    class PoolDistributionRequestSender
    {
    public:
        typedef ConnectionsT Connections;

        PoolDistributionRequestSender(boost::asio::io_service& io, DistributionT& distribution,
                                      const std::function<void()>& allPoolsReceived)
            :m_running(false)
            ,m_strand(io)
            ,m_distribution(distribution)
            ,m_allPoolsReceivedCb(allPoolsReceived)
        {
        }

        void Start()
        {
            boost::asio::dispatch(m_strand, [this]
            {
                m_running=true;
                SendPoolDistributionRequests();
            });

        }

        void Stop()
        {
            boost::asio::post(m_strand, [this] //use post to prevent that stop is executed before start
            {
                m_running=false;
                m_requests.clear();
            });
        }

        void RequestPoolDistribution(int64_t nodeId, int64_t nodeTypeId)
        {
            boost::asio::post(m_strand, [this, nodeId, nodeTypeId]
            {
                lllog(5)<<"PoolHandler: Request pooldistribution from "<< nodeId <<std::endl;
                m_requests.push_back(PoolDistributionRequestSender<DistributionT, ConnectionsT>::PdReq(nodeId, nodeTypeId, false));
                SendPoolDistributionRequests();
            });
        }

        //fromNodeId=0 means all queued requests are finished
        void PoolDistributionFinished(int64_t fromNodeId)
        {
            boost::asio::dispatch(m_strand, [this,fromNodeId]
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

                if (m_requests.empty())
                {
                    m_allPoolsReceivedCb();
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
        std::function<void()> m_allPoolsReceivedCb;

        std::vector<PdReq> m_requests;

        void SendPoolDistributionRequests()
        {
            if (!m_running || m_requests.empty())
            {
                return;
            }

            bool unsentRequests=false;

            for (auto request = m_requests.begin(); request != m_requests.end(); ++request)
            {
                if (!request->sent)
                {                    
                    SmartSyncState syncState;
                    ConnectionsT::Instance().PrepareSmartSync(request->nodeId, syncState);

                    if (Safir::Utilities::Internal::Internal::LowLevelLogger::Instance().LogLevel() >= 5)
                    {
                        std::wostringstream os;
                        syncState.ToString(os);
                        lllog(5) << L"Send PD request with smartSyncState: " << std::endl << os.str() << std::endl;
                    }

                    // Convert smartSyncState to protobuf poolSyncInfo
                    Pd::PoolSyncInfo poolSyncInfo;
                    poolSyncInfo.set_messagetype(Pd::PoolSyncInfo_PdMsgType::PoolSyncInfo_PdMsgType_PdRequest);
                    for (const auto& connection : syncState.connections)
                    {
                        auto c = poolSyncInfo.mutable_connections()->Add();
                        c->set_connection_id(connection.connectionId);
                        c->set_context(connection.context);
                        c->set_counter(connection.counter);
                        for (const auto& registration : connection.registrations)
                        {
                            auto r = c->mutable_registrations()->Add();
                            r->set_type_id(registration.typeId);
                            r->set_registration_time(registration.registrationTime);
                            if (registration.handlerId.GetRawString().empty())
                            {
                                r->set_handler_id(std::to_string(registration.handlerId.GetRawValue()));
                            }
                            else
                            {
                                r->set_handler_id(registration.handlerId.Utf8String());
                            }

                            for (const auto& entity : registration.entities)
                            {
                                auto e = r->mutable_entities()->Add();
                                e->set_instance_id(entity.instanceId);
                                e->set_version(entity.version);
                                e->set_creation_time(entity.creationTime);
                            }
                        }
                        c->set_name(connection.name);
                    }

                    const auto size = poolSyncInfo.ByteSizeLong();
                    Safir::Utilities::Internal::SharedCharArray payload = Safir::Utilities::Internal::MakeSharedArray(size);
                    google::protobuf::uint8* buf=reinterpret_cast<google::protobuf::uint8*>(const_cast<char*>(payload.get()));
                    poolSyncInfo.SerializeWithCachedSizesToArray(buf);

                    if (m_distribution.GetCommunication().Send(request->nodeId, request->nodeType, payload, size, PoolDistributionInfoDataTypeId, true))
                    {
                        request->sent=true;
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
                boost::asio::post(m_strand, [this]{SendPoolDistributionRequests();}); //a bit aggressive, maybe we should set a timer instead
            }
        }
    };
}
}
}
