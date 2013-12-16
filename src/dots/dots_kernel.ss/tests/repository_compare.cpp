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
#include <Safir/Dob/Typesystem/Internal/Serialization.h>
#include "../src/dots_shm_repository.h"

int main(int argc, char* argv[])
{
    if (argc!=2)
    {
        std::cout<<"Wrong number of arguments, no dou path"<<std::endl;
        return 1;
    }

    std::string path(argv[1]);
    std::vector<boost::filesystem::path> paths;
    paths.push_back(path);

    std::ostringstream lom;
    boost::shared_ptr<const Safir::Dob::Typesystem::Internal::TypeRepository> local=Safir::Dob::Typesystem::Internal::ParseTypeDefinitions(path);
    Safir::Dob::Typesystem::Internal::RepositoryToString(local.get(), false, lom);

    boost::interprocess::shared_memory_object::remove("DOTS_SHM_TEST");
    boost::interprocess::managed_shared_memory sharedMemory(boost::interprocess::create_only, "DOTS_SHM_TEST", 5000000);
    Safir::Dob::Typesystem::Internal::RepositoryShm::CreateShmCopyOfRepository(*local, "DOTS_TEST_REPO", sharedMemory);
    Safir::Dob::Typesystem::Internal::RepositoryShm* shm=sharedMemory.find<Safir::Dob::Typesystem::Internal::RepositoryShm>("DOTS_TEST_REPO").first;

    std::ostringstream som;
    Safir::Dob::Typesystem::Internal::RepositoryToString(shm, false, som);


    if (som.str()!=lom.str())
    {
        std::cout<<"=====LOCAL====="<<std::endl;
        std::cout<<lom.str()<<std::endl;
        std::cout<<"=====SHM====="<<std::endl;
        std::cout<<som.str()<<std::endl;
        std::cout<<"Local repository and shared repository differ!"<<std::endl;

        return 1;
    }
    else
    {
        std::cout<<"Local repository and shared repository are identical!"<<std::endl;
        std::cout<<"Passed!"<<std::endl;
    }



    return 0;
}
