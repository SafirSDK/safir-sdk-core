/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / stlrha
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

#ifndef _dose_main_request_timers_h
#define _dose_main_request_timers_h

#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/ConnectionId.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    struct RequestTimerInfo
    {
    public:
        RequestTimerInfo(const Identifier connectionIdentifier,
                         const InternalRequestId& reqId,
                         const Typesystem::TypeId typeId,
                         const Typesystem::HandlerId& handlerId)
            : m_connectionIdentifier(connectionIdentifier),
              m_requestId(reqId),
              m_typeId(typeId),
              m_handlerId(handlerId){}

        RequestTimerInfo(const Identifier connectionIdentifier,
                         const InternalRequestId& reqId)
            : m_connectionIdentifier(connectionIdentifier),
              m_requestId(reqId),
              m_typeId(-1),
              m_handlerId(-1){}

        bool operator<(const RequestTimerInfo& rhs) const;

        Identifier    m_connectionIdentifier;
        InternalRequestId m_requestId;
        Typesystem::TypeId m_typeId;
        Typesystem::HandlerId m_handlerId;
    };

    typedef TimerInfo<RequestTimerInfo> ReqTimer;

    struct RequestTimers
    {
        static TimerId m_localReqTimerId;
        static TimerId m_externalReqTimerId;
    };
}
}
}
#endif
