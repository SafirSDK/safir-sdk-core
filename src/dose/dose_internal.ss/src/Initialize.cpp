/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safir.sourceforge.net)
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
#include <Safir/Dob/Internal/Initialize.h>

#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/ContextSharedTable.h>
#include <Safir/Dob/Internal/MessageTypes.h>
#include <Safir/Dob/Internal/EndStates.h>
#include <Safir/Dob/Internal/ServiceTypes.h>
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{

void InitializeDoseInternalFromDoseMain(const int64_t nodeId)
{
    lllog(1) << "Initializing dose_internal from dose_main" << std::endl;
    Connections::Initialize(true,nodeId);
    ContextSharedTable::Initialize();
    MessageTypes::Initialize(true);
    EndStates::Initialize();
    ServiceTypes::Initialize(true,nodeId);
    InjectionKindTable::Initialize();
    EntityTypes::Initialize(true,nodeId);

    auto sem = SharedMemoryObject::GetSharedMemory().find_or_construct<boost::interprocess::interprocess_semaphore>
        ("InitializationGateKeeper")(0);

    sem->post();

    lllog(1) << "Initialization complete" << std::endl;
}

void InitializeDoseInternalFromApp()
{
    auto sem = SharedMemoryObject::GetSharedMemory().find_or_construct<boost::interprocess::interprocess_semaphore>
        ("InitializationGateKeeper")(0);

    lllog(1) << "Waiting for dose_main to initialize dose_internal" << std::endl;

    for(;;)
    {
        if (sem->try_wait())
        {
            break;
        }

        //sleep_for contains an interruption point, which makes it possible to interrupt the thread
        //if it is hanging in here. Useful in dobexplorer, for example.
        boost::this_thread::sleep_for(boost::chrono::milliseconds(100));
    }
    sem->post();

    lllog(1) << "Connecting to dose_internal from app" << std::endl;
    Connections::Initialize(false,0);
    ContextSharedTable::Initialize();
    MessageTypes::Initialize(false);
    EndStates::Initialize();
    ServiceTypes::Initialize(false,0);
    InjectionKindTable::Initialize();
    EntityTypes::Initialize(false,0);
}

}
}
}
