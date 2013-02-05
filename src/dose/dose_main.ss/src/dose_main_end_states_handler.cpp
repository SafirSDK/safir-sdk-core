/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#include "dose_main_end_states_handler.h"

#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Internal/EndStates.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    EndStatesHandler::EndStatesHandler()
    {
        m_timerId = TimerHandler::Instance().RegisterTimeoutHandler(L"End States Timer", *this);

        TimerInfoPtr timerInfo(new EmptyTimerInfo(m_timerId));
        TimerHandler::Instance().Set(Discard,
                                     timerInfo,
                                     GetUtcTime() + 60.0); //time out in 60 seconds
    }


    void EndStatesHandler::HandleTimeout(const TimerInfoPtr& timer)
    {
        //TimerInfoPtr timerInfo(new EmptyTimerInfo(m_timerId));
        TimerHandler::Instance().Set(Discard,
                                     timer,
                                     GetUtcTime() + 60.0); //time out again in 60 seconds

        Safir::Dob::Internal::EndStates::Instance().HandleTimeout();
    }
}
}
}
