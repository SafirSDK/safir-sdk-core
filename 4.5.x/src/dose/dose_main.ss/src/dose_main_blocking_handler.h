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

#ifndef _dose_main_blocking_handler_h
#define _dose_main_blocking_handler_h

#include <Safir/Dob/Internal/ConnectionId.h>
#include "dose_main_defs.h"
#include <boost/noncopyable.hpp>
namespace Safir
{
namespace Dob
{
namespace Internal
{
    //--------------------------------------------------------
    // AWI ??
    //--------------------------------------------------------
    class BlockingHandler:
        private boost::noncopyable
    {
    public:

        //Add waitingConn to the list of applications waiting for blockingConn
        void AddWaitingConnection(const Identifier& blockingConn, const Identifier& waitingConn);

        // Gets and removes the waiting connections for a specific blocking connection
        bool GetWaitingConnections(const Identifier& blockingApp, IdentifierSet& waitingApps);

        // The given connection is removed (both as blocking connection and as waiting connection)
        void RemoveConnection(const Identifier& id);

    private:
        typedef unordered_map<Identifier, IdentifierSet> ClientMap;
        typedef std::pair<Identifier, IdentifierSet> ClientPair;
        ClientMap m_clientMap;
    };

    class BlockingHandlers:
        private boost::noncopyable
    {
    public:
        BlockingHandler& Request() {return m_requestQ;}
        BlockingHandler& Response() {return m_responseQ;}
        BlockingHandler& State() {return m_stateQ;}
        BlockingHandler& Message() {return m_messageQ;}

        void RemoveConnection(const Identifier& id);
    private:
        BlockingHandler m_requestQ;
        BlockingHandler m_responseQ;   //only dose_com may block this
        BlockingHandler m_stateQ;   //only dose_com may block this
        BlockingHandler m_messageQ; //only dose_com may block this
    };

}
}
}

#endif

