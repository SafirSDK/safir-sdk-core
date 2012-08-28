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
#include <boost/bind.hpp>

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

    boost::once_flag SharedMemoryObject::SharedMemoryHolder::SingletonHelper::m_onceFlag = BOOST_ONCE_INIT;

    SharedMemoryObject::SharedMemoryHolder & SharedMemoryObject::SharedMemoryHolder::SingletonHelper::Instance()
    {
        static SharedMemoryHolder instance;
        return instance;
    }

    SharedMemoryObject::SharedMemoryHolder & SharedMemoryObject::SharedMemoryHolder::Instance()
    {
        boost::call_once(SingletonHelper::m_onceFlag,boost::bind(SingletonHelper::Instance));
        return SingletonHelper::Instance();
    }

    SharedMemoryObject::SharedMemoryHolder::SharedMemoryHolder():
        m_startupSynchronizer("DOSE_INITIALIZATION")
    {
        m_startupSynchronizer.Start(this);
    }

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
}
}
}
