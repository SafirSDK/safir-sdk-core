/******************************************************************************
*
* Copyright Saab AB, 2016 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#include <iostream>
#include <algorithm>
#include <boost/thread.hpp>
#include <thread>
#include "WebsocketServer.h"

#include <Safir/Dob/Typesystem/Serialization.h>

#include "Typesystem.h"
#include <Safir/Dob/Typesystem/ToolSupport/Serialization.h>

int main(int /*argc*/, const char** /*argv*/)
{    
    boost::asio::io_service ioService;
    WebsocketServer ws(ioService);
    ioService.post([&]{ws.Run();});
    boost::thread_group threads;
    auto numberOfThreads=std::max(static_cast<unsigned int>(3), boost::thread::hardware_concurrency());
    for (unsigned int i=0; i<numberOfThreads; ++i)
    {
        threads.create_thread([&]{ioService.run();});
    }

    threads.join_all();

    std::cout<<"WebsocketServer stopped"<<std::endl;

    return 0;
}


// TODO
// ------
// requests
// ping at interval
// logging
// send only if id
// breakpad


// batch
// dob-object with stats
// max connections
