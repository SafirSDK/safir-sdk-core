/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joot
*
*******************************************************************************/
#include <iostream>
#include <algorithm>
#include <set>
#include <Safir/Dob/Typesystem/ToolSupport/Serialization.h>
#include "../src/dots_shm_repository.h"

int main(int argc, char* argv[])
{
    if (argc!=2)
    {
        std::wcout<<"Wrong number of arguments, no dou path"<<std::endl;
        return 1;
    }

    std::string path(argv[1]);
    std::vector<boost::filesystem::path> paths;
    paths.push_back(path);

    std::ostringstream lom;
    boost::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository> local=Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(path);

    if (local->GetNumberOfClasses() <= 1) //object is predefined
    {
        std::wcout << "No classes in that path! Try again!" << std::endl;
        return 1;
    }
    Safir::Dob::Typesystem::ToolSupport::RepositoryToString(local.get(), false, lom);

    boost::interprocess::shared_memory_object::remove("DOTS_SHM_TEST");
    boost::interprocess::managed_shared_memory sharedMemory(boost::interprocess::create_only, "DOTS_SHM_TEST", 5000000);
    Safir::Dob::Typesystem::Internal::RepositoryShm::CreateShmCopyOfRepository(*local, "DOTS_TEST_REPO", sharedMemory);
    Safir::Dob::Typesystem::Internal::RepositoryShm* shm=sharedMemory.find<Safir::Dob::Typesystem::Internal::RepositoryShm>("DOTS_TEST_REPO").first;

    std::ostringstream som;
    Safir::Dob::Typesystem::ToolSupport::RepositoryToString(shm, false, som);


    if (som.str()!=lom.str())
    {
        std::wcout<<"=====LOCAL====="<<std::endl;
        std::wcout<<lom.str().c_str()<<std::endl;
        std::wcout<<"=====SHM====="<<std::endl;
        std::wcout<<som.str().c_str()<<std::endl;
        std::wcout<<"Local repository and shared repository differ!"<<std::endl;

        return 1;
    }
    else
    {
        std::wcout<<"Local repository and shared repository are identical!"<<std::endl;
        std::wcout<<"Passed!"<<std::endl;
    }



    return 0;
}
