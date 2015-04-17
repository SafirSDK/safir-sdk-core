/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
*
* Created by: Joel Ottosson / joot
*
*******************************************************************************/
#include <iostream>
#include <algorithm>
#include <set>
#include <Safir/Utilities/Internal/ConfigReader.h>
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

    const std::string dotsShmTest("SAFIR_DOTS_SHM_TEST" + Safir::Utilities::Internal::Expansion::GetSafirInstanceSuffix());
    const std::string dotsTestRepo("SAFIR_DOTS_TEST_REPO" + Safir::Utilities::Internal::Expansion::GetSafirInstanceSuffix());

    boost::interprocess::shared_memory_object::remove(dotsShmTest.c_str());
    boost::interprocess::managed_shared_memory sharedMemory(boost::interprocess::create_only, dotsShmTest.c_str(), 5000000);
    Safir::Dob::Typesystem::Internal::RepositoryShm::CreateShmCopyOfRepository(*local, dotsTestRepo.c_str(), sharedMemory);
    Safir::Dob::Typesystem::Internal::RepositoryShm* shm=sharedMemory.find<Safir::Dob::Typesystem::Internal::RepositoryShm>(dotsTestRepo.c_str()).first;

    std::ostringstream som;
    Safir::Dob::Typesystem::ToolSupport::RepositoryToString(shm, false, som);

    std::string sharedString=som.str();
    std::string localString=lom.str();

    if (sharedString!=localString)
    {
        std::cout<<"Local repository and shared repository differ!"<<std::endl;
        std::cout<<"Shared size: "<<sharedString.size()<<", local size: "<<localString.size()<<std::endl;

        std::cout<<"=====LOCAL====="<<std::endl;
        std::cout<<localString<<std::endl;
        std::cout<<"=====SHM====="<<std::endl;
        std::cout<<sharedString<<std::endl;

        std::cout<<"Local repository and shared repository differ!"<<std::endl;
        return 1;
    }
    else
    {
        std::wcout<<"Local repository and shared repository are identical!"<<std::endl;
        std::wcout<<"Passed!"<<std::endl;
    }



    return 0;
}
