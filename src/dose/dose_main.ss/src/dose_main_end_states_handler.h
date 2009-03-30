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

#ifndef _dose_main_end_states_h
#define _dose_main_end_states_h

// #include <Safir/Dob/Internal/InternalFwd.h>
// #include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
// #include <Safir/Dob/Connection.h>
// #include <Safir/Dob/Internal/ConnectionId.h>
// #include <ace/Event_Handler.h>
// #include <map>
// #include <deque>
// #include <boost/function.hpp>
// #include <ace/Reactor.h>
// #include "dose_main_waiting_states.h"
#include "dose_main_timers.h"
#include <boost/noncopyable.hpp>
namespace Safir
{
namespace Dob
{
namespace Internal
{
    class EndStatesHandler:
        public TimeoutHandler,
        private boost::noncopyable
    {
    public:
        EndStatesHandler();

        /** Add a connection endstate */
        void AddDisconnect(const ConnectionId& connection);

        bool IsDisconnected(const ConnectionId & connection) const
        {return m_connections.find(connection) != m_connections.end();}
    private:
        virtual void HandleTimeout(const TimerInfoPtr & timer);

        //a table over connections that have disconnected. The second item in the pair
        //is the timestamp when we got the disconnect.
        typedef std::map<ConnectionId,Typesystem::Int64> ConnectionTable;

        ConnectionTable m_connections;

        Typesystem::Int64 m_lastTimestamp;
        TimerId m_timerId;
    };
}
}
}
#endif
