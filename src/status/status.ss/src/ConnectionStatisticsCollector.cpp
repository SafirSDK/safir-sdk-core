/******************************************************************************
*
* Copyright Saab AB, 2023 (http://safirsdkcore.com)
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
#include "ConnectionStatisticsCollector.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{

inline void StatisticsCollector(Safir::Dob::Internal::RequestOutQueue& requestOutQueue, void* ptr)
{
    auto p = static_cast<ConnectionStatisticsCollector::ReqQStat*>(ptr);
    p->noTimeouts = requestOutQueue.m_noTimeouts;
    p->noPushedRequests = requestOutQueue.m_noPushedRequests;
    p->noOverflows = requestOutQueue.m_noOverflows;
    p->noDispatchedRequests = requestOutQueue.m_noDispatchedRequests;
    p->noAttachedResponses = requestOutQueue.m_noAttachedResponses;
    p->noDispatchedResponses = requestOutQueue.m_noDispatchedResponses;
    p->size = requestOutQueue.size();
    p->capacity = requestOutQueue.capacity();
}

inline void StatisticsCollector(Safir::Dob::Internal::MessageQueue& messageQueue, void* ptr)
{
    auto p = static_cast<ConnectionStatisticsCollector::MsgQStat*>(ptr);
    p->noPushedMsg = messageQueue.m_noPushed;
    p->noOverflows = messageQueue.m_noOverflows;
    p->size = messageQueue.size();
    p->capacity = messageQueue.capacity();
}

inline void StatisticsCollector(Safir::Dob::Internal::RequestInQueue& requestInQueue, void* ptr)
{
    auto p = static_cast<ConnectionStatisticsCollector::ReqQStat*>(ptr);
    p->noPushedRequests = requestInQueue.m_noPushedRequests;
    p->noOverflows = requestInQueue.m_noOverflows;
    p->noDispatchedRequests = requestInQueue.m_noDispatchedRequests;
    p->noAttachedResponses = requestInQueue.m_noAttachedResponses;
    p->noDispatchedResponses = requestInQueue.m_noDispatchedResponses;
    p->size = requestInQueue.size();
    p->capacity = requestInQueue.capacity();
}
}
}
}

namespace ConnectionStatisticsCollector
{
void GetStatistics(const Safir::Dob::Internal::ConnectionPtr& connection, Stat& stat)
{
    stat.connectionName = connection->NameWithCounter();
    stat.detached = connection->IsDetached();

    Safir::Dob::Internal::RequestOutQueue& reqOutQ = connection->GetRequestOutQueue();
    Safir::Dob::Internal::StatisticsCollector(reqOutQ, &stat.reqOutQStat);

    connection->ForEachRequestInQueue([&stat](const auto& consumer, auto& queue)
    {
        ReqQStat reqInQStat;
        reqInQStat.consumerId = consumer;
        Safir::Dob::Internal::StatisticsCollector(queue, &reqInQStat);
        stat.reqInQStat.push_back(reqInQStat);
    });

    Safir::Dob::Internal::MessageQueue& msgOutQ = connection->GetMessageOutQueue();
    Safir::Dob::Internal::StatisticsCollector(msgOutQ, &stat.msgOutQStat);

    connection->ForEachMessageInQueue([&stat](const auto& consumer, auto& queue)
    {
        MsgQStat msgInQStat;
        msgInQStat.consumerId = consumer;
        Safir::Dob::Internal::StatisticsCollector(queue, &msgInQStat);
        stat.msgInQStat.push_back(msgInQStat);
    });
}
}

