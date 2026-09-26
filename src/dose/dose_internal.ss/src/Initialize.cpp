/******************************************************************************
*
* Copyright Saab AB, 2015, 2026 (http://safirsdkcore.com)
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
#include <Safir/Dob/Internal/LowMemoryOperationsTable.h>
#include <Safir/Dob/Internal/MessageTypes.h>
#include <Safir/Dob/Internal/ServiceTypes.h>
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <atomic>
#include <mutex>
#include <thread>

namespace Safir
{
namespace Dob
{
namespace Internal
{

namespace
{
    //Both Initialize functions below set the m_instance pointers of the shared memory
    //singletons, and every other thread reads those pointers without a lock through
    //Instance(). They must therefore run exactly once per process: Controller::Connect
    //calls InitializeDoseInternalFromApp for each connection it opens, and rewriting a
    //pointer while another thread is reading it is a data race even when the value
    //written is identical.
    //
    //Once per process is the right scope, since the pointers are this process's own
    //view of the shared memory and each process has to set up its own. The flag is not
    //a substitute for the gatekeeper semaphore further down, which gates one process
    //against another and still has to be waited on for every connection.
    //
    //One flag covers both functions, because dose_main runs the dose_main variant at
    //startup and then opens ordinary connections of its own, which must not
    //reinitialise anything. The cost of sharing it is that the first caller decides
    //which variant runs, and the two are not interchangeable: only the dose_main
    //variant calls Signals::RemoveConnectOrOut and posts the gatekeeper semaphore that
    //every application waits on. dose_main must therefore initialise before it opens
    //any connection, and initializedFromDoseMain is what makes a breach of that fail
    //where it happens, rather than as every application in the system hanging at
    //startup for want of a semaphore post.
    //
    //An initialization that throws leaves the flag unset, so the next caller retries.
    std::once_flag initializeOnce;
    std::atomic<bool> initializedFromDoseMain{false};
}

void InitializeDoseInternalFromDoseMain(const int64_t nodeId)
{
    std::call_once(initializeOnce, [nodeId]
    {
        lllog(1) << "Initializing dose_internal from dose_main" << std::endl;
        Connections::Initialize(true,nodeId);
        ContextSharedTable::Initialize();
        LowMemoryOperationsTable::Initialize();
        MessageTypes::Initialize(true);
        ServiceTypes::Initialize(true,nodeId);
        InjectionKindTable::Initialize();
        EntityTypes::Initialize(true,nodeId);

        auto sem = SharedMemoryObject::GetSharedMemory().find_or_construct<boost::interprocess::interprocess_semaphore>
            ("InitializationGateKeeper")(0);

        sem->post();

        initializedFromDoseMain = true;

        lllog(1) << "Initialization complete" << std::endl;
    });

    ENSURE(initializedFromDoseMain.load(),
           << "dose_internal has already been initialized as an application in this "
              "process. InitializeDoseInternalFromDoseMain must be called before "
              "anything in dose_main opens a Connection.");
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
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }
    sem->post();

    std::call_once(initializeOnce, []
    {
        lllog(1) << "Connecting to dose_internal from app" << std::endl;
        Connections::Initialize(false,0);
        ContextSharedTable::Initialize();
        LowMemoryOperationsTable::Initialize();
        MessageTypes::Initialize(false);
        ServiceTypes::Initialize(false,0);
        InjectionKindTable::Initialize();
        EntityTypes::Initialize(false,0);
    });
}

}
}
}
