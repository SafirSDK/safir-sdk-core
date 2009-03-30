/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagstr�m / stlrha
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

#include "dose_main_request_timers.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
    TimerId RequestTimers::m_localReqTimerId;
    TimerId RequestTimers::m_externalReqTimerId;

    bool RequestTimerInfo::operator<(const RequestTimerInfo& rhs) const
    {
        if (m_connectionIdentifier < rhs.m_connectionIdentifier)
        {
            return true;
        }
        else if (rhs.m_connectionIdentifier < m_connectionIdentifier)
        {
            return false;
        }

        if (m_requestId < rhs.m_requestId)
        {
            return true;
        }
        else
        {
            return false;
        }
    }
}
}
}
