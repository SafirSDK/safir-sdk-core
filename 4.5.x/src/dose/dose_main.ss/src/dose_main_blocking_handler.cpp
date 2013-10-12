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

#include "dose_main_blocking_handler.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
    void BlockingHandler::AddWaitingConnection(const Identifier& blockingConn, const Identifier& waitingConn)
    {
        ClientMap::iterator it = m_clientMap.find(blockingConn);
        if(it != m_clientMap.end())
        {
            it->second.insert(waitingConn);
        }
        else
        {
            IdentifierSet appIds;
            appIds.insert(waitingConn);
            m_clientMap.insert(ClientPair(blockingConn, appIds));
        }
    }


    bool BlockingHandler::GetWaitingConnections(const Identifier& blockingApp, IdentifierSet& waitingApps)
    {
        ClientMap::iterator it = m_clientMap.find(blockingApp);
        if(it != m_clientMap.end())
        {
            waitingApps = (*it).second;
            m_clientMap.erase(it);
            return true;
        }
        return false;
    }

    void BlockingHandler::RemoveConnection(const Identifier& id)
    {
        m_clientMap.erase(id);

        for (ClientMap::iterator it = m_clientMap.begin();
            it != m_clientMap.end(); ++it)
        {
            it->second.erase(id);
        }
    }

    void BlockingHandlers::RemoveConnection(const Identifier& id)
    {
        m_requestQ.RemoveConnection(id);
        m_responseQ.RemoveConnection(id);
        m_stateQ.RemoveConnection(id);
        m_messageQ.RemoveConnection(id);
    }

}
}
}

