/******************************************************************************
*
* Copyright Saab AB, 2007-2023 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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

#include <Safir/Dob/SecondaryConnection.h>
#include <Safir/Control/GetConnectionStatistics.h>
#include <Safir/Control/GetConnectionStatisticsAllNodes.h>
#include <Safir/Control/ConnectionStatisticsAllNodesResponse.h>

#include <boost/asio.hpp>
#include <boost/noncopyable.hpp>

namespace Safir
{
namespace Control
{

class ConnectionStatisticsServiceHandler :
        public Safir::Dob::ServiceHandler,
        public Safir::Dob::ServiceHandlerPending,
        public Safir::Dob::Requestor,
        private boost::noncopyable
{
public:
    ConnectionStatisticsServiceHandler(boost::asio::io_context& io);
    void Start();
    void Stop();

private:
    boost::asio::io_context& m_io;
    Safir::Dob::SecondaryConnection m_dobConnection;
    int64_t m_nodeId;

    void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId& handlerId) override;
    void OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId& handlerId) override;
    void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy, Safir::Dob::ResponseSenderPtr responseSender) override;

    void OnResponse(const Safir::Dob::ResponseProxy responseProxy) override;
    void OnNotRequestOverflow() override;

    void SendNodeRequests();

    void HandleNodeStatisticRequest(const Safir::Dob::ResponseSenderPtr& responseSender) ;
    void HandleAllNodesStatisticRequest(const Safir::Dob::ResponseSenderPtr& responseSender);

    struct AllNodesStatisticsState
    {
        bool isActive;
        Safir::Control::ConnectionStatisticsAllNodesResponsePtr response;
        std::vector<Safir::Dob::ResponseSenderPtr> waitingResponseSenders;
        struct NodeRequests
        {
            int64_t nodeId;
            bool isSent;
            bool hasResponse;
        };
        std::vector<NodeRequests> nodeRequests;
    };

    AllNodesStatisticsState m_allNodesStatisticsState;
    void SetupAllNodesStatisticsState();
    void ClearAllNodesStatisticsState();
};

}
}
