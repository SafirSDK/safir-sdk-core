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
#include "dots_shm_repository.h"

int main(int argc, char* argv[])
{
    if (argc!=3)
    {
        std::cout<<"Wrong number of arguments"<<std::endl;
        std::cout<<"Usage: dots_shm_test [-s, -l, -c] dou_path"<<std::endl;
        return 0;
    }

    std::string repoType(argv[1]);
    std::string path(argv[2]);
    std::vector<boost::filesystem::path> paths;
    paths.push_back(path);

    if (repoType=="-s")
    {
        Safir::Dob::Typesystem::Internal::RepositoryKeeper::Initialize(paths);
        const Safir::Dob::Typesystem::Internal::RepositoryShm* shm=Safir::Dob::Typesystem::Internal::RepositoryKeeper::GetRepository();
        Safir::Dob::Typesystem::Internal::RepositoryToString(shm, false, std::cout);
    }
    else if (repoType=="-l")
    {        
        boost::shared_ptr<const Safir::Dob::Typesystem::Internal::TypeRepository> local=Safir::Dob::Typesystem::Internal::ParseTypeDefinitions(path);
        Safir::Dob::Typesystem::Internal::RepositoryToString(local.get(), false, std::cout);
    }
    else if (repoType=="-c")
    {
        std::ostringstream som;
        Safir::Dob::Typesystem::Internal::RepositoryKeeper::Initialize(paths);
        const Safir::Dob::Typesystem::Internal::RepositoryShm* shm=Safir::Dob::Typesystem::Internal::RepositoryKeeper::GetRepository();
        Safir::Dob::Typesystem::Internal::RepositoryToString(shm, false, som);

        std::ostringstream lom;
        boost::shared_ptr<const Safir::Dob::Typesystem::Internal::TypeRepository> local=Safir::Dob::Typesystem::Internal::ParseTypeDefinitions(path);
        Safir::Dob::Typesystem::Internal::RepositoryToString(local.get(), false, lom);

        if (som.str()!=lom.str())
        {
            std::cout<<"Local repository and shared repository differ!"<<std::endl;
            return 1;
        }
        else
        {
            std::cout<<"Local repository and shared repository are identical!"<<std::endl;
            std::cout<<"Passed!"<<std::endl;
        }

    }
    else
    {
        std::cout<<"Illegal repository type '"<<repoType<<"'"<<std::endl;
        std::cout<<"Usage: dots_shm_test [s, l] <dou_path>"<<std::endl;
    }

    return 0;
}
