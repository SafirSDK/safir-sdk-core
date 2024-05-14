/******************************************************************************
*
* Copyright Saab AB, 2004-2015 (http://safirsdkcore.com)
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
#ifndef __DOTS_KERNEL_REPOSITORY_KEEPER_H__
#define __DOTS_KERNEL_REPOSITORY_KEEPER_H__

#include <boost/filesystem.hpp>
#include <Safir/Utilities/StartupSynchronizer.h>
#include "dots_shm_repository.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**This class creates the shared memory. It initiates the dou-parsing and then
     * copies the data into the shared memory.
     */
    class RepositoryKeeper :
            public Safir::Utilities::Synchronized,
            private boost::noncopyable
    {
    public:
        static void Initialize(size_t sharedMemorySize, const std::vector<boost::filesystem::path>& paths);
        static const RepositoryShm* GetRepository();
        static bool RepositoryCreatedByThisProcess();
        static void MemoryInfo(DotsC_Int32& capacity, DotsC_Int32& used); //in bytes
    private:
        static RepositoryKeeper& Instance();
        RepositoryKeeper();
        ~RepositoryKeeper();

        //StartupSynchronizer stuff
        void Create() override;
        void Use() override;
        void Destroy() override;

        size_t m_sharedMemorySize;
        std::vector<boost::filesystem::path> m_paths;
        std::unique_ptr<boost::interprocess::managed_shared_memory> m_sharedMemory;
        RepositoryShm* m_repository;
        bool m_repositoryCreatedByThisProcess;

        //must be last member for Destroy to work!
        Safir::Utilities::StartupSynchronizer m_startupSynchronizer;

        /**
         * This class is here to ensure that only the Instance method can get at the
         * instance, so as to be sure that std call_once is used correctly.
         * Also makes it easier to grep for singletons in the code, if all
         * singletons use the same construction and helper-name.
         */
        struct SingletonHelper
        {
        private:
            friend RepositoryKeeper& RepositoryKeeper::Instance();

            static RepositoryKeeper& Instance();
            static std::once_flag m_onceFlag;
        };
    };
}
}
}
}

#endif
