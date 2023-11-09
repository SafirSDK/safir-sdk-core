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
#pragma once
#include <Safir/Dob/Internal/Connection.h>

namespace ConnectionStatisticsCollector
{
    struct ReqQStat
    {
        Safir::Dob::Typesystem::Int32 noPushedRequests;
        Safir::Dob::Typesystem::Int32 noOverflows;
        Safir::Dob::Typesystem::Int32 noDispatchedRequests;
        Safir::Dob::Typesystem::Int32 noAttachedResponses;
        Safir::Dob::Typesystem::Int32 noDispatchedResponses;
        size_t capacity;
        size_t size;

        Safir::Dob::Internal::ConsumerId    consumerId; // valid only for in queues
        Safir::Dob::Typesystem::Int32       noTimeouts; // valid only for out queues
    };

    struct MsgQStat
    {
        Safir::Dob::Typesystem::Int32 noPushedMsg;
        Safir::Dob::Typesystem::Int32 noOverflows;
        size_t capacity;
        size_t size;

        Safir::Dob::Internal::ConsumerId    consumerId; // valid only for in queues
    };

    struct Stat
    {
        ReqQStat                    reqOutQStat;
        std::vector<ReqQStat>       reqInQStat;
        MsgQStat                    msgOutQStat;
        std::vector<MsgQStat>       msgInQStat;
        bool detached;

    };

    void GetStatistics(const Safir::Dob::Internal::ConnectionPtr& connection, Stat& stat);
};
