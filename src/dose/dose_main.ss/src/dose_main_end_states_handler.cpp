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

// #include "dose_main_blocking_handler.h"
// #include "dose_main_communication.h"
// #include "dose_main_connection_handler.h"
// #include "dose_main_pending_registration_handler.h"
// #include "dose_main_persist_handler.h"
// #include <Safir/Dob/Internal/Connections.h>
// #include <Safir/Dob/ConnectionAspectMisc.h>
// #include <Safir/Dob/ConnectionAspectInjector.h>
// #include <Safir/Dob/Internal/EntityTypes.h>
// #include <Safir/Dob/Internal/ServiceTypes.h>
// #include <Safir/Dob/Internal/State.h>
// #include <Safir/Dob/ThisNodeParameters.h>
// #include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Internal/EndStates.h>
// #include <ace/Thread.h>
// #include <boost/bind.hpp>


namespace Safir
{
namespace Dob
{
namespace Internal
{
    EndStatesHandler::EndStatesHandler():
        m_lastTimestamp(0)
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

        for (ConnectionTable::iterator it = m_connections.begin();
             it != m_connections.end(); )//iterator increment below
        {
            if (it->second < m_lastTimestamp)
            {
                m_connections.erase(it++);
            }
            else
            {
                ++it;
            }
        }

        Safir::Dob::Internal::EndStates::Instance().HandleTimeout();

        ++m_lastTimestamp;
    }


    void EndStatesHandler::AddDisconnect(const ConnectionId & connection)
    {
        const std::pair<ConnectionTable::iterator,bool> insertResult =
            m_connections.insert(std::make_pair(connection,m_lastTimestamp));

        if (!insertResult.second)
        {
            lllerr << "Duplicate disconnect added to EndStatesHandler! Keeping the old one a little bit longer" << std::endl;
            insertResult.first->second = m_lastTimestamp;
        }
    }
}
}
}
