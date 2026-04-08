/******************************************************************************
*
* Copyright Saab AB, 2026 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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
#include <memory>
#include <Safir/Application/CrashReporter.h>
#include "ApiServer.h"
#include "DobConnectionRegistry.h"
#include <Safir/Dob/Typesystem/Serialization.h>

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4100)
#endif

#include <boost/thread.hpp>

#ifdef _MSC_VER
#pragma warning(pop)
#endif


int main(int /*argc*/, const char** /*argv*/)
{
    Safir::Application::ScopedCrashReporter scopedStartStop;

    lllog(5)<<"safir_web started"<<std::endl;

    boost::asio::io_context io;
    auto dobConnectionRegistry = std::make_shared<DobConnectionRegistry>();
    ApiServer ws(io, dobConnectionRegistry);
    boost::asio::post(io, [&ws]{ws.Run();});
    boost::thread_group threads;
    auto numberOfThreads=std::max(static_cast<unsigned int>(3), boost::thread::hardware_concurrency());
    for (unsigned int i=0; i<numberOfThreads; ++i)
    {
        threads.create_thread([&]{io.run();});
    }

    threads.join_all();

    lllog(5)<<"safir_web stopped"<<std::endl;

    return 0;
}
