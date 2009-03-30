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

#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

#include <Safir/Dob/NodeParameters.h>
#include <ace/Process_Mutex.h>
#include <ace/Guard_T.h>
#include <ace/Process_Semaphore.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{

    /**
     * @name Stuff for the class SharedMemoryObject::SharedMemory
     */

    /** @{ */

    static const char * SHARED_MEMORY_NAME = "DOSE_SHARED_MEMORY";


    SharedMemoryObject::SharedMemoryHolder * volatile SharedMemoryObject::SharedMemoryHolder::m_instance = NULL;
    ACE_Recursive_Thread_Mutex SharedMemoryObject::SharedMemoryHolder::m_instantiationLock;

    SharedMemoryObject::SharedMemoryHolder & SharedMemoryObject::SharedMemoryHolder::Instance()
    {
        if (m_instance == NULL)
        {
            ACE_Guard<ACE_Recursive_Thread_Mutex> lck(m_instantiationLock);
            if (m_instance == NULL)
            {
                m_instance = new SharedMemoryObject::SharedMemoryHolder();
                m_instance->m_startupSynchronizer.Start();
            }
        }
        return *m_instance;
    }


#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4355)
    //disable using this in constructor warning, since we're not using the startupsynchronizer until later this is safe.
#endif

    SharedMemoryObject::SharedMemoryHolder::SharedMemoryHolder():
        m_startupSynchronizer("DOSE_INITIALIZATION", this)
    {
        /*
        static ACE_Process_Mutex mutex("CREATE_DOSE_SHARED_MEMORY_MTX");
        static ACE_Process_Semaphore sem(0,"CREATE_DOSE_SHARED_MEMORY_SEM",0,1);

        if (mutex.tryacquire() == 0)
        {
            lllout << "Creating dose shared memory" << std::endl;
            boost::interprocess::shared_memory_object::remove(SHARED_MEMORY_NAME);
            m_shmem.reset(new boost::interprocess::managed_shared_memory
                          (boost::interprocess::create_only,
                           SHARED_MEMORY_NAME,
                           Safir::Dob::NodeParameters::SharedMemorySize() * 1024 * 1024));

            sem.release();
        }
        else
        {
            ACE_Guard<ACE_Process_Semaphore> lck(sem);
            lllout << "Opening dose shared memory" << std::endl;
            m_shmem.reset(new boost::interprocess::managed_shared_memory
                          (boost::interprocess::open_only,
                           SHARED_MEMORY_NAME));
                           }*/
    }

#ifdef _MSC_VER
#pragma warning(pop)
#endif

    void SharedMemoryObject::SharedMemoryHolder::Create()
    {
        lllout << "Creating dose shared memory" << std::endl;
        boost::interprocess::shared_memory_object::remove(SHARED_MEMORY_NAME);

        boost::shared_ptr<boost::interprocess::managed_shared_memory> shmem
            (new boost::interprocess::managed_shared_memory
             (boost::interprocess::create_only,
              SHARED_MEMORY_NAME,
              Safir::Dob::NodeParameters::SharedMemorySize() * 1024 * 1024));
    }

    void SharedMemoryObject::SharedMemoryHolder::Use()
    {
        lllout << "Opening dose shared memory" << std::endl;
        m_shmem.reset(new boost::interprocess::managed_shared_memory
                      (boost::interprocess::open_only,
                       SHARED_MEMORY_NAME));
    }

    void SharedMemoryObject::SharedMemoryHolder::Destroy()
    {
        boost::interprocess::shared_memory_object::remove(SHARED_MEMORY_NAME);
    }

    SharedMemoryObject::SharedMemoryHolder::~SharedMemoryHolder()
    {

    }

    /*
    SharedMemoryObject::SharedMemoryObject()
    {

    }


    SharedMemoryObject::~SharedMemoryObject()
    {

    }
    */
    /*
    SharedMemory & SharedMemoryObject::GetSharedMemory()
    {
        if (m_sharedMemory != NULL)
        {
            Safir::Utilities::ScopedLock<ACE_Recursive_Thread_Mutex> lck(m_sharedMemoryInstantiationLock);
            if (m_sharedMemory != NULL)
            {
                m_sharedMemory = new SharedMemory();
            }
        }
        return *m_sharedMemory;
    }
    */

}
}
}
