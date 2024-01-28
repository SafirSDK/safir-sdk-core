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
#include "ConnectionStatisticsServiceHandler.h"
#include "ConnectionStatisticsCollector.h"

#include <Safir/Control/ConnectionStatisticsResponse.h>
#include <Safir/Control/ConnectionStatisticsAllNodesResponse.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Logging/Log.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/NodeInfo.h>
#include <Safir/Dob/LowMemoryException.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/ConnectionAspectMisc.h>

namespace
{
    std::pair<int, int> GetReqInQAccumulated(const std::vector<ConnectionStatisticsCollector::ReqQStat>& reqInQStat)
    {
        int recv = 0;
        int overflows = 0;
        for (const auto& v : reqInQStat)
        {
            recv += v.noPushedRequests;
            overflows += v.noOverflows;
        }
        return std::make_pair(recv, overflows);
    }

    std::pair<int, int> GetMsgInQAccumulated(const std::vector<ConnectionStatisticsCollector::MsgQStat>& msgQStat)
    {
        int recv = 0;
        int overflows = 0;
        for (const auto& v : msgQStat)
        {
            recv += v.noPushedMsg;
            overflows += v.noOverflows;
        }
        return std::make_pair(recv, overflows);
    }
}

namespace Safir
{
namespace Control
{

ConnectionStatisticsServiceHandler::ConnectionStatisticsServiceHandler(boost::asio::io_context& io)
    : m_io(io)
{
}

void ConnectionStatisticsServiceHandler::Start()
{
    m_dobConnection.Attach();

    Safir::Dob::ConnectionAspectMisc misc(m_dobConnection);
    m_nodeId = misc.GetNodeId();

    // Service that provides connection stats from this node only
    lllog(5) << L"Status ConnectionsStats - Register GetConnectionStatistics service" << std::endl;
    m_dobConnection.RegisterServiceHandler(Safir::Control::GetConnectionStatistics::ClassTypeId, Safir::Dob::Typesystem::HandlerId(m_nodeId), this);

    if (!misc.IsLightNode())
    {
        ClearAllNodesStatisticsState();
        // Service that provides connection stats from all system nodes
        lllog(5) << L"Status ConnectionsStats - Register GetConnectionStatisticsAllNodes service" << std::endl;
        m_dobConnection.RegisterServiceHandlerPending(Safir::Control::GetConnectionStatisticsAllNodes::ClassTypeId, Safir::Dob::Typesystem::HandlerId(), this);
    }
}

void ConnectionStatisticsServiceHandler::Stop()
{
    m_dobConnection.Detach();
}


void ConnectionStatisticsServiceHandler::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId& handlerId)
{
    lllog(5) << L"Status ConnectionsStats - OnRevokedRegistration of " << Safir::Dob::Typesystem::Operations::GetName(typeId) << L", handler=" << handlerId << std::endl;
}

void ConnectionStatisticsServiceHandler::OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId& handlerId)
{
    lllog(5) << L"Status ConnectionsStats - OnCompletedRegistration of " << Safir::Dob::Typesystem::Operations::GetName(typeId) << L", handler=" << handlerId << std::endl;
}

void ConnectionStatisticsServiceHandler::OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    if (serviceRequestProxy.GetTypeId() == Safir::Control::GetConnectionStatistics::ClassTypeId)
    {
        HandleNodeStatisticRequest(responseSender);
    }
    else if (serviceRequestProxy.GetTypeId() == Safir::Control::GetConnectionStatisticsAllNodes::ClassTypeId)
    {
        HandleAllNodesStatisticRequest(responseSender);
    }
    else
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Error, L"Status ConnectionsStats - Received request of type I am not handler of. TypeId=" + std::to_wstring(serviceRequestProxy.GetTypeId()));
    }
}

void ConnectionStatisticsServiceHandler::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
{
    auto fromNodeId = responseProxy.GetRequestHandlerId().GetRawValue();

    if (!responseProxy.IsSuccess())
    {
        std::wostringstream os;
        os << L"Status ConnectionsStats - Received error response on a from nodeId: " << fromNodeId << L". Receive response: "
           << Safir::Dob::Typesystem::Serialization::ToJson(responseProxy.GetResponse()) << std::endl;
        Safir::Logging::SendSystemLog(Safir::Logging::Error, os.str());
    }

    bool allResultsReceived = true;
    for (auto& nr : m_allNodesStatisticsState.nodeRequests)
    {
        if (nr.nodeId == fromNodeId)
        {
            nr.hasResponse = true;

            auto response = std::dynamic_pointer_cast<Safir::Control::ConnectionStatisticsResponse>(responseProxy.GetResponse());
            if (response)
            {
                m_allNodesStatisticsState.response->NodeConnectionStatistics().push_back(response);
                lllog(5) << L"Status ConnectionsStats - Received node statistics from " << response->NodeName().GetVal() << std::endl;
            }
        }
        else if (!nr.hasResponse)
        {
            allResultsReceived = false;
        }
    }

    // We have received node statistics from all nodes. Send collected response.
    if (allResultsReceived)
    {
        for (auto& respSender : m_allNodesStatisticsState.waitingResponseSenders)
        {
            respSender->Send(m_allNodesStatisticsState.response);
        }

        // Clear state, from now we don't have an active request. Next time we get an GetConnectionStatisticsAllNodes we start a new request to each node.
        ClearAllNodesStatisticsState();
    }


}

void ConnectionStatisticsServiceHandler::OnNotRequestOverflow()
{
    lllog(5) << L"Status ConnectionsStats - OnNotRequestOverflow" << std::endl;
    SendNodeRequests();
}

void ConnectionStatisticsServiceHandler::SendNodeRequests()
{
    if (m_allNodesStatisticsState.isActive)
    {
        for (auto& req : m_allNodesStatisticsState.nodeRequests)
        {
            try
            {
                auto serviceReq = Safir::Control::GetConnectionStatistics::Create();
                m_dobConnection.ServiceRequest(serviceReq, Safir::Dob::Typesystem::HandlerId(req.nodeId), this);
                req.isSent = true;
                lllog(5) << L"Status ConnectionsStats - Send GetConnectionStatistics to " << req.nodeId << std::endl;
            }
            catch (const Safir::Dob::OverflowException&)
            {
                lllog(5) << L"Status ConnectionsStats - OverflowException when sending node request to nodeId " << req.nodeId << std::endl;
                return; // Continue when we get OnNotOverflow callback
            }
        }
    }
}

void ConnectionStatisticsServiceHandler::HandleNodeStatisticRequest(const Safir::Dob::ResponseSenderPtr& responseSender)
{
    std::vector<std::pair<std::wstring, ConnectionStatisticsCollector::Stat>> connectionStatistics;
    Safir::Dob::Internal::Connections::Instance().ForEachConnectionPtr([&connectionStatistics](const Safir::Dob::Internal::ConnectionPtr& con)
    {
        if (con->IsLocal())
        {
            connectionStatistics.emplace_back();
            auto& stat = connectionStatistics.back();
            stat.first = Safir::Dob::Typesystem::Utilities::ToWstring(con->NameWithCounter());
            ConnectionStatisticsCollector::GetStatistics(con, stat.second);
        }
    });

    Safir::Control::ConnectionStatisticsResponsePtr response = Safir::Control::ConnectionStatisticsResponse::Create();
    response->NodeId() = m_nodeId;
    response->NodeName() = Safir::Dob::ThisNodeParameters::Name();

    for (const auto& s : connectionStatistics)
    {
        auto item = Safir::Control::ConnectionStatisticsItem::Create();
        response->ConnectionStatistics().push_back(item);

        // name
        item->ConnectionName() = s.first;

        // req in
        auto reqInQ = GetReqInQAccumulated(s.second.reqInQStat);
        item->NumberOfReceivedRequests() = reqInQ.first;
        item->NumberOfReceiveRequestOverflows() = reqInQ.second;

        // req out
        item->NumberOfSentRequests() = s.second.reqOutQStat.noPushedRequests;
        item->NumberOfSendRequestOverflows() = s.second.reqOutQStat.noOverflows;
        item->NumberOfSendRequestTimeouts() = s.second.reqOutQStat.noTimeouts;

        // msg in
        auto msgInQ = GetMsgInQAccumulated(s.second.msgInQStat);
        item->NumberOfReceivedMessages() = msgInQ.first;
        item->NumberOfReceiveMessageOverflows() = msgInQ.second;

        // msg out
        item->NumberOfSentMessages() = s.second.msgOutQStat.noPushedMsg;
        item->NumberOfSendMessageOverflows() = s.second.msgOutQStat.noOverflows;
    }

    try
    {
        responseSender->Send(response);
    }
    catch (const Safir::Dob::LowMemoryException&)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                      L"Failed to send response due to low shared memory. Skipping.");
        responseSender->Discard();
    }
}

void ConnectionStatisticsServiceHandler::HandleAllNodesStatisticRequest(const Safir::Dob::ResponseSenderPtr& responseSender)
{
    lllog(5) << L"Status ConnectionsStats - Received GetConnectionStatisticsAllNodes" << std::endl;

    if (m_allNodesStatisticsState.isActive)
    {
        // There is already an active request. Just save the responseSender until the response is ready.
        m_allNodesStatisticsState.waitingResponseSenders.push_back(responseSender);
        return;
    }

    // No active request, start collecting stats from all system nodes.
    SetupAllNodesStatisticsState();
    m_allNodesStatisticsState.waitingResponseSenders.push_back(responseSender);
    SendNodeRequests();
}

void ConnectionStatisticsServiceHandler::SetupAllNodesStatisticsState()
{
    lllog(5) << L"Status ConnectionsStats - Send SetupAllNodesStatisticsState"<< std::endl;
    m_allNodesStatisticsState.isActive = true;
    m_allNodesStatisticsState.response = Safir::Control::ConnectionStatisticsAllNodesResponse::Create();
    m_allNodesStatisticsState.waitingResponseSenders.clear();
    for (auto it = m_dobConnection.GetEntityIterator(Safir::Dob::NodeInfo::ClassTypeId, false); it != Safir::Dob::EntityIterator(); ++it)
    {
        auto nodeId = it->GetInstanceId().GetRawValue();
        lllog(5) << L"Status ConnectionsStats - Send SetupAllNodesStatisticsState add nodeId " << nodeId << std::endl;
        m_allNodesStatisticsState.nodeRequests.push_back(AllNodesStatisticsState::NodeRequests{nodeId, false, false});
    }
}

void ConnectionStatisticsServiceHandler::ClearAllNodesStatisticsState()
{
    lllog(5) << L"Status ConnectionsStats - Send ClearAllNodesStatisticsState add nodeId " << std::endl;
    m_allNodesStatisticsState.isActive = false;
    m_allNodesStatisticsState.response.reset();
    m_allNodesStatisticsState.waitingResponseSenders.clear();
    m_allNodesStatisticsState.nodeRequests.clear();
}

}
}
