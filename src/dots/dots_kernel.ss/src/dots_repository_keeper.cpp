/******************************************************************************
*
* Copyright Saab AB, 2004-2015 (http://safir.sourceforge.net)
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
#include <Safir/Utilities/Internal/ConfigReader.h>
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

    RepositoryKeeper& RepositoryKeeper::Instance()
    {
        static RepositoryKeeper instance;
        return instance;
    }

    void RepositoryKeeper::Initialize(size_t sharedMemorySize, const std::vector<boost::filesystem::path>& paths)
    {
        try
        {
            RepositoryKeeper* instance=&RepositoryKeeper::Instance();
            instance->m_sharedMemorySize=sharedMemorySize;
            instance->m_paths.insert(instance->m_paths.begin(), paths.begin(), paths.end());
            instance->m_startupSynchronizer.Start(instance);
            if (instance->m_repository!=NULL)
            {
                //instance->m_blobLayout.reset(new Safir::Dob::Typesystem::ToolSupport::BlobLayout<RepositoryShm>(instance->m_repository));
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

    const RepositoryShm* RepositoryKeeper::GetRepository()
    {
        return Instance().m_repository;
    }

    RepositoryKeeper::RepositoryKeeper()
        :m_startupSynchronizer("SAFIR_DOTS_INITIALIZATION")
        ,m_sharedMemorySize(0)
        ,m_paths()
        ,m_repositoryCreatedByThisProcess(false)
    {
    }

    RepositoryKeeper::~RepositoryKeeper()
    {
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
            m_repository=NULL;
            return;
        }

        m_repository=m_sharedMemory->find<RepositoryShm>(DOTS_REPOSITORY_NAME).first;
    }

    void RepositoryKeeper::Destroy()
    {
        boost::interprocess::shared_memory_object::remove(DOTS_SHM_NAME);
    }

    void RepositoryKeeper::Create()
    {
        m_repositoryCreatedByThisProcess=true;

        //-------------------------------------------------
        //Parse dou and dom files into local repository
        //-------------------------------------------------
        boost::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository> localRepository;
        try
        {
            localRepository=Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(m_paths);
        }
        catch(const Safir::Dob::Typesystem::ToolSupport::ParseError& err)
        {
            std::cout<<"********** Parse Error **********************************************"<<std::endl;
            std::cout<<"* Label: "<<err.Label()<<std::endl;
            std::cout<<"* Descr: "<<err.Description()<<std::endl;
            std::cout<<"* File:  "<<err.File()<<std::endl;
            std::cout<<"* ErrId: "<<err.ErrorId()<<std::endl;
            std::cout<<"*********************************************************************"<<std::endl;
            localRepository.reset();
            m_repository=NULL;
            return;
        }

        try
        {
            //-------------------------------------------------
            //Copy localRepository into shared memory
            //-------------------------------------------------
            boost::interprocess::shared_memory_object::remove(DOTS_SHM_NAME);
            m_sharedMemory.reset(new boost::interprocess::managed_shared_memory
                                 (boost::interprocess::create_only,
                                  DOTS_SHM_NAME,
                                  m_sharedMemorySize));
        }
        catch (const boost::interprocess::interprocess_exception&)
        {
            SEND_SYSTEM_LOG(Error, << "Ran out of shared memory while loading types and parameters." <<std::endl
                            << "Please increase the shared memory size specified in typesystem.ini");
            localRepository.reset();
            m_repository=NULL;
            return;
        }
        catch (const std::exception& ex)
        {
            SEND_SYSTEM_LOG(Error, << "Failure while creating dots shared memory" <<std::endl << ex.what());
            localRepository.reset();
            m_repository=NULL;
            return;
        }

        RepositoryShm::CreateShmCopyOfRepository(*localRepository, DOTS_REPOSITORY_NAME, *m_sharedMemory);
    }
}
}
}
}
