/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include "../../src/Signals.h"

using namespace Safir::Dob::Internal;

void user(const ConnectionId& connection)
{
    Signals::Instance().GetConnectionSignalWaiter(connection)();
    Signals::Instance().SignalConnectOrOut();    
}

int main()
{
    std::set<ConnectionId> connids;
    boost::thread_group threads;
    for (int i = 0; i < 100; ++i)
    {
        ConnectionId id(0,0,i);
        Signals::Remove(id);
        connids.insert(id);
        threads.create_thread(boost::bind(user,id));
    }

    for (std::set<ConnectionId>::iterator it = connids.begin();
         it != connids.end(); ++it)
    {
        Signals::Instance().SignalIn(*it);
        Signals::Instance().WaitForConnectOrOut();
    }

    threads.join_all();

    return 0;
}


