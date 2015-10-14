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
#include <iostream>
#include <boost/program_options.hpp>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeParser.h>
#include <Safir/Dob/Typesystem/ToolSupport/Serialization.h>

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
        ,typeIdLookup()
    {
        boost::program_options::options_description desc("Command line options");
        desc.add_options()
                ("help,h", "Produce help message")
                ("summary,s", "Output a brief summary of the type system'")
                ("details,d", "Output a full information about the entire type system")
                ("type,t", boost::program_options::value<std::string>(), "Output info about a specific type")
                ("path,p", boost::program_options::value< std::vector<std::string> >()->multitoken(), "Parse specified path(s) into a local memory type repository. If same type exists in more than one path, the latter wins.")
                ("type-id",boost::program_options::value<std::string>(), "If argument is a string the typeId is calculated. If argument is a numeric typeId or part of a typeId, the matching type name(s) are looked up." );

        boost::program_options::variables_map vm;
        boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
        boost::program_options::notify(vm);
        if (vm.count("help"))
        {
            std::cout<<desc<<std::endl;
            exit(0);
        }

        summary=vm.count("summary")>0;
        details=vm.count("details")>0;
        if (vm.count("type"))
        {
            typeName=vm["type"].as<std::string>();
        }

        if (vm.count("path"))
        {
            paths=vm["path"].as< std::vector<std::string> >();
        }

        if (vm.count("type-id"))
        {
            typeIdLookup=vm["type-id"].as<std::string>();
        }
    }

    bool summary;
    bool details;
    std::string typeName;
    std::vector<std::string> paths;
    std::string typeIdLookup;
};

void TypeIdLookup(const std::string& typeIdString, const std::map<std::string, DotsC_TypeId>& typeTable)
{
    DotsC_TypeId typeId;
    bool numeric=true;
    try
    {
        typeId=boost::lexical_cast<DotsC_TypeId>(typeIdString);
    }
    catch (const boost::bad_lexical_cast&)
    {
        typeId=Safir::Dob::Typesystem::ToolSupport::TypeUtilities::CalculateTypeId(typeIdString);
        numeric=false;
    }

    if (numeric)
    {
        int numberOfMatches=0;
        for (std::map<std::string, DotsC_TypeId>::const_iterator it=typeTable.begin(); it!=typeTable.end(); ++it)
        {
            std::string tmp=boost::lexical_cast<std::string>(it->second);
            size_t found=tmp.find(typeIdString);
            if (found!=std::string::npos)
            {
                std::cout<<it->first<<", typeId: "<<it->second<<std::endl;
                ++numberOfMatches;
            }
        }

        std::cout<<"Number of matches: "<<numberOfMatches<<std::endl;
    }
    else
    {
        std::map<std::string, DotsC_TypeId>::const_iterator it=typeTable.find(typeIdString);

        if (it!=typeTable.end())
        {
            if (it->second!=typeId) throw std::logic_error("TypeIds supposed to match");

            std::cout<<"The string '"<<typeIdString<<"' is an existing type and has typeId: "<<typeId<<std::endl;
        }
        else
        {
            std::cout<<"The string '"<<typeIdString<<"' is NOT an existing type. Calculated typeId: "<<typeId<<std::endl;
        }
    }
}

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
        else if (!cmd.typeIdLookup.empty())
        {
            TypeIdLookup(cmd.typeIdLookup);
        }
        else
        {
            SimpleCheck();
        }
    }

private:

    static void SimpleCheck()
    {
        std::cout<<"Checking configuration..."<<std::endl;
        DotsC_NumberOfTypeIds();
        std::cout<<"Success!"<<std::endl;
    }

    static void ShowSummary()
    {
        if (DotsC_TypeRepositoryLoadedByThisProcess())
        {
            std::cout<<"Type system created by this process."<<std::endl;
        }
        else
        {
            std::cout<<"Type system loaded, was already created."<<std::endl;

        }
        std::cout<<"Number of classes:     "<<DotsC_NumberOfClasses()<<std::endl;
        std::cout<<"Number of enums:       "<<DotsC_NumberOfEnumerations()<<std::endl;
        std::cout<<"Number of properties:  "<<DotsC_NumberOfProperties()<<std::endl;
        std::cout<<"Number of exceptions:  "<<DotsC_NumberOfExceptions()<<std::endl;
        std::cout<<"Total number of types: "<<DotsC_NumberOfTypeIds()<<std::endl;
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

        std::cout<<&v[0]<<std::endl;
    }

    static void ShowDetails()
    {
        ShowType(0); //shows entire repository
        std::cout<<"===== Summary ====="<<std::endl;
        ShowSummary();
    }

    static void TypeIdLookup(const std::string& typeIdString)
    {
        DotsC_Int32 numTypes=DotsC_NumberOfTypeIds();
        DotsC_Int32 count=0;
        std::vector<DotsC_TypeId> types(numTypes, 0);
        DotsC_GetAllTypeIds(&types[0], numTypes, count);
        std::map<std::string, DotsC_TypeId> typeTable;
        for (std::vector<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            std::string typeName=DotsC_GetTypeName(*it);
            typeTable.insert(std::make_pair(typeName, *it));
        }
        ::TypeIdLookup(typeIdString, typeTable);
    }
};

class CheckConfigurationLocal
{
public:
    typedef boost::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository> RepPtr;
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
            rep=Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(paths);
        }
        catch(const Safir::Dob::Typesystem::ToolSupport::ParseError& err)
        {
            std::cout<<"********** Parse Error **********************************************"<<std::endl;
            std::cout<<"* Label: "<<err.Label()<<std::endl;
            std::cout<<"* Descr: "<<err.Description()<<std::endl;
            std::cout<<"* File:  "<<err.File()<<std::endl;
            std::cout<<"* ErrId: "<<err.ErrorId()<<std::endl;
            std::cout<<"*********************************************************************"<<std::endl;
            exit(1);
        }

        if (!cmd.typeName.empty())
        {
            ShowType(rep, Safir::Dob::Typesystem::ToolSupport::TypeUtilities::CalculateTypeId(cmd.typeName));
        }
        else if (cmd.summary)
        {
            ShowSummary(rep);
        }
        else if (cmd.details)
        {
            ShowDetails(rep);
        }
        else if (!cmd.typeIdLookup.empty())
        {
            TypeIdLookup(rep, cmd.typeIdLookup);
        }
        else //SimpleCheck, already succeeded if we get here
        {
            std::cout<<"Checking configuration..."<<std::endl;
            std::cout<<"Success!"<<std::endl;
        }
    }

private:
    static void ShowSummary(const RepPtr& rep)
    {
        std::cout<<"Type system created by this process in local memory"<<std::endl;
        std::cout<<"Number of classes:     "<<rep->GetNumberOfClasses()<<std::endl;
        std::cout<<"Number of enums:       "<<rep->GetNumberOfEnums()<<std::endl;
        std::cout<<"Number of properties:  "<<rep->GetNumberOfProperties()<<std::endl;
        std::cout<<"Number of exceptions:  "<<rep->GetNumberOfExceptions()<<std::endl;
        std::cout<<"Total number of types: "<<(rep->GetNumberOfClasses()+rep->GetNumberOfEnums()+rep->GetNumberOfExceptions()+rep->GetNumberOfProperties())<<std::endl;
    }

    static void ShowType(const RepPtr& rep, DotsC_TypeId tid)
    {
        std::ostringstream os;
        Safir::Dob::Typesystem::ToolSupport::TypeToString(rep.get(), tid, os);
        std::cout<<os.str()<<std::endl;
    }

    static void ShowDetails(const RepPtr& rep)
    {
        std::ostringstream os;
        Safir::Dob::Typesystem::ToolSupport::RepositoryToString(rep.get(), true, os); //include createRoutines
        std::cout<<os.str()<<std::endl;
        std::cout<<"===== Summary ====="<<std::endl;
        ShowSummary(rep);
    }

    static void TypeIdLookup(const RepPtr& rep, const std::string& typeIdString)
    {
        std::set<DotsC_TypeId> types;
        rep->GetAllClassTypeIds(types);
        rep->GetAllEnumTypeIds(types);
        rep->GetAllExceptionTypeIds(types);
        rep->GetAllPropertyTypeIds(types);
        std::map<std::string, DotsC_TypeId> typeTable;
        for (std::set<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            std::string typeName=Safir::Dob::Typesystem::ToolSupport::TypeUtilities::GetTypeName(rep.get(), *it);
            typeTable.insert(std::make_pair(typeName, *it));
        }
        ::TypeIdLookup(typeIdString, typeTable);
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
        std::cout << "Failed with exception description: " << exc.what() << std::endl;
        exit(1);
    }
    catch (...)
    {
        std::cout << "Failed with ... exception." << std::endl;
        exit(1);
    }

    return 0;
}
