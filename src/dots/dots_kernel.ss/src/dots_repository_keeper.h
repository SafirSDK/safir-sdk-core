/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
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

#include <boost/scoped_ptr.hpp>
#include <boost/filesystem.hpp>
#include <Safir/Utilities/StartupSynchronizer.h>
#include <Safir/Dob/Typesystem/ToolSupport/BlobLayout.h>
#include "dots_shm_repository.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    typedef Safir::Dob::Typesystem::ToolSupport::MemberStatus MemberStatus; //shorter name, used a lot in dots_kernel.cpp

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
        static const Safir::Dob::Typesystem::ToolSupport::BlobLayout<RepositoryShm>* GetBlobLayout();
        static bool RepositoryCreatedByThisProcess();

    private:
        Safir::Utilities::StartupSynchronizer m_startupSynchronizer;
        size_t m_sharedMemorySize;
        std::vector<boost::filesystem::path> m_paths;
        boost::scoped_ptr<boost::interprocess::managed_shared_memory> m_sharedMemory;
        RepositoryShm* m_repository;
        bool m_repositoryCreatedByThisProcess;
        boost::scoped_ptr< Safir::Dob::Typesystem::ToolSupport::BlobLayout<RepositoryShm> > m_blobLayout;

        static RepositoryKeeper& Instance();
        RepositoryKeeper();
        ~RepositoryKeeper();

        //StartupSynchronizer stuff
        virtual void Create();
        virtual void Use();
        virtual void Destroy();
    };
}
}
}
}

#endif
