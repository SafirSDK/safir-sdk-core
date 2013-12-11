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
#include <iostream>
#include <boost/program_options.hpp>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <Safir/Dob/Typesystem/Internal/TypeParser.h>
#include <Safir/Dob/Typesystem/Internal/Serialization.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4702)
#endif

class CmdLine
{
public:
    CmdLine(int argc, char * argv[])
        :summary(false)
        ,details(false)
        ,typeName()
        ,paths()
    {
        boost::program_options::options_description desc("Command line options");
        desc.add_options()
                ("help,h", "Produce help message")
                ("summary,s", "Output a brief summary of the type system'")
                ("details,d", "Output a full information about the entire type system")
                ("type,t", boost::program_options::value<std::string>(), "Output info about a specific type")
                ("path", boost::program_options::value< std::vector<std::string> >()->multitoken(), "Parse specified path(s) into a local memory type repostionry. If same type exists in more than one path, the latter wins.");

        boost::program_options::variables_map vm;
        boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
        boost::program_options::notify(vm);
        if (vm.count("help"))
        {
            std::cout<<desc<<std::endl;
            exit(0);
        }

        summary=vm.count("summary");
        details=vm.count("details");
        if (vm.count("type"))
        {
            typeName=vm["type"].as<std::string>();
        }

        if (vm.count("path"))
        {
            paths=vm["path"].as< std::vector<std::string> >();
        }
    }

    bool summary;
    bool details;
    std::string typeName;
    std::vector<std::string> paths;
};

class CheckConfigurationDotsKernel
{
public:
    static void Run(const CmdLine& cmd)
    {
        if (!cmd.typeName.empty())
        {
            ShowType(DotsC_TypeIdFromName(cmd.typeName.c_str()));
        }
        else if (cmd.summary)
        {
            ShowSummary();
        }
        else if (cmd.details)
        {
            ShowDetails();
        }
        else
        {
            SimpleCheck();
        }
    }

private:

    static void SimpleCheck()
    {
        std::wcout<<"Checking configuration..."<<std::endl;
        DotsC_NumberOfTypeIds();
        std::wcout<<"Success!"<<std::endl;
    }

    static void ShowSummary()
    {
        if (DotsC_TypeRepositoryLoadedByThisProcess())
        {
            std::wcout<<L"Type system created by this process."<<std::endl;
        }
        else
        {
            std::wcout<<L"Type system loaded, was already created."<<std::endl;

        }
        std::wcout<<L"Number of classes:     "<<DotsC_NumberOfClasses()<<std::endl;
        std::wcout<<L"Number of enums:       "<<DotsC_NumberOfEnumerations()<<std::endl;
        std::wcout<<L"Number of properties:  "<<DotsC_NumberOfProperties()<<std::endl;
        std::wcout<<L"Number of exceptions:  "<<DotsC_NumberOfExceptions()<<std::endl;
        std::wcout<<L"Total number of types: "<<DotsC_NumberOfTypeIds()<<std::endl;
    }

    static void ShowType(DotsC_TypeId tid)
    {
        std::vector<char> v;
        v.resize(1000000);
        DotsC_Int32 resultSize=0;
        DotsC_GetTypeDescription(tid, &v[0], static_cast<DotsC_Int32>(v.size()), resultSize);
        if (resultSize>static_cast<DotsC_Int32>(v.size()))
        {
            v.resize(static_cast<size_t>(resultSize));
            DotsC_GetTypeDescription(tid, &v[0], static_cast<DotsC_Int32>(v.size()), resultSize);
        }

        std::wcout<<&v[0]<<std::endl;
    }

    static void ShowDetails()
    {
        ShowType(0); //shows entire repository
        std::wcout<<"===== Summary ====="<<std::endl;
        ShowSummary();
    }
};

class CheckConfigurationLocal
{
public:
    typedef boost::shared_ptr<const Safir::Dob::Typesystem::Internal::TypeRepository> RepPtr;
    static void Run(const CmdLine& cmd)
    {
        RepPtr rep;
        try
        {
            std::vector<boost::filesystem::path> paths;
            for (std::vector<std::string>::const_iterator it=cmd.paths.begin(); it!=cmd.paths.end(); ++it)
            {
                paths.push_back(boost::filesystem::path(*it));
            }
            rep=Safir::Dob::Typesystem::Internal::ParseTypeDefinitions(paths);
        }
        catch(const Safir::Dob::Typesystem::Internal::ParseError& err)
        {
            std::wcout<<"********** Parse Error **********************************************"<<std::endl;
            std::wcout<<"* Label: "<<err.Label().c_str()<<std::endl;
            std::wcout<<"* Descr: "<<err.Description().c_str()<<std::endl;
            std::wcout<<"* File:  "<<err.File().c_str()<<std::endl;
            std::wcout<<"* ErrId: "<<err.ErrorId()<<std::endl;
            std::wcout<<"*********************************************************************"<<std::endl;
            exit(1);
        }

        if (!cmd.typeName.empty())
        {
            ShowType(rep, Safir::Dob::Typesystem::Internal::TypeUtilities::CalculateTypeId(cmd.typeName));
        }
        else if (cmd.summary)
        {
            ShowSummary(rep);
        }
        else if (cmd.details)
        {
            ShowDetails(rep);
        }
        else //SimpleCheck, already succeeded if we get here
        {
            std::wcout<<"Checking configuration..."<<std::endl;
            std::wcout<<"Success!"<<std::endl;
        }
    }

private:
    static void ShowSummary(const RepPtr& rep)
    {
        std::wcout<<L"Type system created by this process in local memory"<<std::endl;
        std::wcout<<L"Number of classes:     "<<rep->GetNumberOfClasses()<<std::endl;
        std::wcout<<L"Number of enums:       "<<rep->GetNumberOfEnums()<<std::endl;
        std::wcout<<L"Number of properties:  "<<rep->GetNumberOfProperties()<<std::endl;
        std::wcout<<L"Number of exceptions:  "<<rep->GetNumberOfExceptions()<<std::endl;
        std::wcout<<L"Total number of types: "<<(rep->GetNumberOfClasses()+rep->GetNumberOfEnums()+rep->GetNumberOfExceptions()+rep->GetNumberOfProperties())<<std::endl;
    }

    static void ShowType(const RepPtr& rep, DotsC_TypeId tid)
    {
        std::ostringstream os;
        Safir::Dob::Typesystem::Internal::TypeToString(rep.get(), tid, os);
        std::wcout<<os.str().c_str()<<std::endl;
    }

    static void ShowDetails(const RepPtr& rep)
    {
        std::ostringstream os;
        Safir::Dob::Typesystem::Internal::RepositoryToString(rep.get(), true, os); //include createRoutines
        std::wcout<<os.str().c_str()<<std::endl;
        std::wcout<<"===== Summary ====="<<std::endl;
        ShowSummary(rep);
    }
};

int main(int argc, char* argv[])
{
    CmdLine cmd(argc, argv);

    try
    {
        if (cmd.paths.empty())
        {
            CheckConfigurationDotsKernel::Run(cmd);
        }
        else
        {
            CheckConfigurationLocal::Run(cmd);
        }
    }
    catch (const std::exception & exc)
    {
        std::wcout << "Failed with exception description: " << exc.what() << std::endl;
        exit(1);
    }
    catch (...)
    {
        std::wcout << "Failed with ... exception." << std::endl;
        exit(1);
    }

    return 0;
}
