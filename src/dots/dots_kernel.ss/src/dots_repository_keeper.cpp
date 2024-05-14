/******************************************************************************
*
* Copyright Saab AB, 2004-2023 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joot
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
#include <Safir/Utilities/Internal/Expansion.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "dots_repository_keeper.h"

namespace //anonymous namespace for internal stuff
{
    const std::string shmName("SAFIR_TYPESYSTEM_DATA" +
                              Safir::Utilities::Internal::Expansion::GetSafirInstanceSuffix());
    const char* DOTS_SHM_NAME = shmName.c_str();

    const std::string repName("SAFIR_DOTS_REPOSITORY" +
                              Safir::Utilities::Internal::Expansion::GetSafirInstanceSuffix());
    const char* DOTS_REPOSITORY_NAME = repName.c_str();
}

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    std::once_flag RepositoryKeeper::SingletonHelper::m_onceFlag;

    RepositoryKeeper & RepositoryKeeper::SingletonHelper::Instance()
    {
        static RepositoryKeeper instance;
        return instance;
    }

    RepositoryKeeper& RepositoryKeeper::Instance()
    {
        std::call_once(SingletonHelper::m_onceFlag,[]{SingletonHelper::Instance();});
        return SingletonHelper::Instance();
    }

    void RepositoryKeeper::Initialize(size_t sharedMemorySize, const std::vector<boost::filesystem::path>& paths)
    {
        try
        {
            RepositoryKeeper* instance=&RepositoryKeeper::Instance();
            instance->m_sharedMemorySize=sharedMemorySize;
            instance->m_paths.insert(instance->m_paths.begin(), paths.begin(), paths.end());
            instance->m_startupSynchronizer.Start(instance);
            if (instance->m_repository!=nullptr)
            {
                return; //loaded ok
            }
        }
        catch (const std::exception & exc)
        {
            SEND_SYSTEM_LOG(Error, << "Loading of dots_kernel failed with exception description: " << exc.what());
        }
        catch (...)
        {
            SEND_SYSTEM_LOG(Error,  << "Loading of dots_kernel failed with ... exception.");
        }
        exit(20);
    }

    bool RepositoryKeeper::RepositoryCreatedByThisProcess()
    {
        return Instance().m_repositoryCreatedByThisProcess;
    }

    void RepositoryKeeper::MemoryInfo(DotsC_Int32& capacity, DotsC_Int32& used)
    {
        capacity = -1;
        used = -1;

        if (Instance().m_sharedMemory == nullptr)
        {
            return;
        }

        const size_t tmpCapacity = Instance().m_sharedMemory->get_size();
        const size_t tmpUsed = tmpCapacity - Instance().m_sharedMemory->get_free_memory();
        const size_t maxInt = static_cast<size_t>(std::numeric_limits<DotsC_Int32>::max());
        if (tmpCapacity > maxInt || tmpUsed > maxInt)
        {
            std::ostringstream ostr;
            ostr <<"Numeric overflow in MemoryInfo.";
            std::cerr << ostr.str() << std::endl;
            SEND_SYSTEM_LOG(Error, << ostr.str().c_str());

            return;
        }

        capacity = static_cast<DotsC_Int32>(tmpCapacity);
        used = static_cast<DotsC_Int32>(tmpUsed);
    }

    const RepositoryShm* RepositoryKeeper::GetRepository()
    {
        return Instance().m_repository;
    }

    RepositoryKeeper::RepositoryKeeper()
        :m_sharedMemorySize(0)
        ,m_paths()
        ,m_repositoryCreatedByThisProcess(false)
        ,m_startupSynchronizer("SAFIR_DOTS_INITIALIZATION")
    {
    }

    RepositoryKeeper::~RepositoryKeeper()
    {
    }

    void RepositoryKeeper::Create()
    {
        //Mandatory cleanup of old shared memories before we start!
        Destroy();

        m_repositoryCreatedByThisProcess=true;

        //-------------------------------------------------
        //Parse dou and dom files into local repository
        //-------------------------------------------------
        std::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository> localRepository;
        try
        {
            localRepository=Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(m_paths);
        }
        catch(const Safir::Dob::Typesystem::ToolSupport::ParseError& err)
        {
            std::ostringstream ostr;
            ostr <<"********** Parse Error **********************************************\n"
                 <<"* Label: "<<err.Label() << "\n"
                 <<"* Descr: "<<err.Description() << "\n"
                 <<"* File:  "<<err.File() << "\n"
                 <<"* ErrId: "<<err.ErrorId() << "\n"
                 <<"*********************************************************************";
            std::cerr << ostr.str() << std::endl;
            SEND_SYSTEM_LOG(Error,
                            << ostr.str().c_str());
            Destroy(); //cleanup
            return;
        }

        try
        {
            //-------------------------------------------------
            //Create shared memory with appropriate permissions
            //-------------------------------------------------
            boost::interprocess::permissions perms;
            perms.set_unrestricted();

            boost::interprocess::shared_memory_object::remove(DOTS_SHM_NAME);
            m_sharedMemory.reset(new boost::interprocess::managed_shared_memory
                                 (boost::interprocess::create_only,
                                  DOTS_SHM_NAME,
                                  m_sharedMemorySize,
                                  0,
                                  perms));
        }
        catch (const boost::interprocess::interprocess_exception& e)
        {
            std::ostringstream ostr;
            ostr << "Encountered an error when setting up the typesystem shared memory (in RepositoryKeeper). "
                 << "This could be a sign of permissions problems in the directory used by boost interprocess. "
                 << "Exception information: " << e.what();
            std::cerr << ostr.str()<< std::endl;
            SEND_SYSTEM_LOG(Error,
                            << ostr.str().c_str());
            Destroy();
            return;
        }
        catch (const std::exception& ex)
        {
            std::ostringstream ostr;
            ostr << "Failure while creating dots shared memory: "<< ex.what();
            std::cerr << ostr.str() << std::endl;
            SEND_SYSTEM_LOG(Error,
                            << ostr.str().c_str());
            Destroy(); //cleanup
            return;
        }

        //-------------------------------------------------
        //Copy localRepository into shared memory
        //-------------------------------------------------
        try
        {
            RepositoryShm::CreateShmCopyOfRepository(*localRepository, DOTS_REPOSITORY_NAME, *m_sharedMemory);
        }
        catch (const boost::interprocess::bad_alloc& ex)
        {
            std::ostringstream ostr;
            ostr << "Encountered an error when copying typesystem information to shared memory. "
                 << "This is probably a sign that you should increase the shared memory size specified "
                 << "in typesystem.ini. ";
            std::cerr << ostr.str() << "Exception information: " << ex.what() << std::endl;
            SEND_SYSTEM_LOG(Error,
                            << ostr.str().c_str());
            Destroy(); //cleanup
            return;
        }
    }

    void RepositoryKeeper::Use()
    {
        try
        {
            m_sharedMemory.reset(new boost::interprocess::managed_shared_memory
                                 (boost::interprocess::open_read_only,
                                  DOTS_SHM_NAME));
        }
        catch (...)
        {
            m_repository=nullptr;
            m_sharedMemory.reset();;
            return;
        }

        m_repository=m_sharedMemory->find<RepositoryShm>(DOTS_REPOSITORY_NAME).first;
    }

    void RepositoryKeeper::Destroy()
    {
        m_repository=nullptr;
        m_sharedMemory.reset();
        boost::interprocess::shared_memory_object::remove(DOTS_SHM_NAME);
    }
}
}
}
}
