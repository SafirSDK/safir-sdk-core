/******************************************************************************
*
* Copyright Saab AB, 2007-2013,2015 (http://safir.sourceforge.net)
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
#pragma once

#include <Safir/Dob/Internal/ConnectionId.h>
#include <boost/noncopyable.hpp>
#include <boost/unordered_map.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    /**
     * This class holds info about connections that are waiting for free space on another connection's
     * in queue.
     */
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
        typedef boost::unordered_map<Identifier, IdentifierSet> ClientMap;
        typedef std::pair<Identifier, IdentifierSet> ClientPair;
        ClientMap m_clientMap;
    };

    class BlockingHandlers:
        private boost::noncopyable
    {
    public:
        BlockingHandler& Request() {return m_requestQ;}

        void RemoveConnection(const Identifier& id);
    private:
        BlockingHandler m_requestQ;
    };

}
}
}

