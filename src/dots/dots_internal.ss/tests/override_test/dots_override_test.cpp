///******************************************************************************
//*
//* Copyright Saab AB, 2004-2013 (http://safirsdkcore.com)
//*
//* Created by: Joel Ottosson / joot
//*
//*******************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <set>
#include <boost/filesystem.hpp>
#include <Safir/Dob/Typesystem/ToolSupport/TypeParser.h>

#include <boost/circular_buffer.hpp>

void GetFiles(const boost::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository>& rep,
              std::set<std::string>& parsedFiles);
void TestFailed(std::set<std::string>& parsedFiles);

int main(int argc, char* argv[])
{
    if (argc!=2)
    {
        std::cout<<"wrong number of args"<<std::endl;
        return 1;
    }

    std::cout<<"--- Start override test with test data:"<<std::endl;
    std::cout<<"--- "<<argv[1]<<std::endl;
    boost::filesystem::path base=boost::filesystem::path(argv[1]).remove_filename();
    std::ifstream is;
    is.open(argv[1]);

    std::vector<boost::filesystem::path> roots;
    std::string line;
    while (std::getline(is, line))
    {
        if (line.empty())
            break;
        roots.push_back(base/line);
    }

    std::set<std::string> expectedToBeParsed;
    while (std::getline(is, line))
    {
        if (!line.empty())
        {
            expectedToBeParsed.insert(boost::filesystem::path(line).string());
        }
    }    

    std::set<std::string> parsedFiles;
    try
    {
        boost::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository> rep=Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(roots);
        GetFiles(rep, parsedFiles);
    }
    catch (const Safir::Dob::Typesystem::ToolSupport::ParseError& err)
    {
        std::cout<<"********** Parse Error **********************************************"<<std::endl;
        std::cout<<"* Label: "<<err.Label()<<std::endl;
        std::cout<<"* Descr: "<<err.Description()<<std::endl;
        std::cout<<"* File:  "<<err.File()<<std::endl;
        std::cout<<"* ErrId: "<<err.ErrorId()<<std::endl;
        std::cout<<"*********************************************************************"<<std::endl;
    }

    //Built in types will have empty filename, we remove them
    parsedFiles.erase("");

    //Compare parsed files with expected
    if (expectedToBeParsed.size()!=parsedFiles.size())
    {
        //Error
        std::cout<<"There are less number of parsed files than expected files!"<<std::endl;
        TestFailed(parsedFiles);
    }

    std::set<std::string>::const_iterator expIt=expectedToBeParsed.begin();
    std::set<std::string>::const_iterator parsedIt=parsedFiles.begin();
    while (expIt!=expectedToBeParsed.end())
    {
        std::string parsed = *parsedIt;
        std::replace(parsed.begin(), parsed.end(), L'\\', L'/');
        if (parsed.find(*expIt) == std::string::npos)
        {
            std::cout << *expIt << " does not match " << parsed << std::endl;
            //Error
            TestFailed(parsedFiles);
        }

        ++expIt;
        ++parsedIt;
    }

    std::cout<<"Test passed!"<<std::endl;
    return 0;
}

template <class T>
std::string FileName(const T& t)
{
    //Old boost versions doesn't have generic_string()
#if ((BOOST_VERSION / 100000) < 2) && ((BOOST_VERSION / 100 % 1000) < 44)
    std::string tmp=boost::filesystem::path(t->FileName()).string();
#else
    std::string tmp=boost::filesystem::path(t->FileName()).generic_string();
#endif
    return tmp;
}

//get files parsed
void GetFiles(const boost::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository>& rep,
              std::set<std::string>& parsedFiles)
{
    std::set<DotsC_TypeId> types;
    rep->GetAllClassTypeIds(types);
    for (std::set<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
    {
        const Safir::Dob::Typesystem::ToolSupport::ClassDescription* c=rep->GetClass(*it);
        parsedFiles.insert(FileName(c));
        std::set<DotsC_TypeId> pmIds;
        c->GetPropertyIds(pmIds);
        for (std::set<DotsC_TypeId>::const_iterator pmIt=pmIds.begin(); pmIt!=pmIds.end(); ++pmIt)
        {
            bool dummy=false;
            parsedFiles.insert(FileName(c->GetPropertyMapping(*pmIt, dummy)));
        }
    }

    types.clear();
    rep->GetAllEnumTypeIds(types);
    for (std::set<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
    {        
        parsedFiles.insert(FileName(rep->GetEnum(*it)));
    }

    types.clear();
    rep->GetAllExceptionTypeIds(types);
    for (std::set<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
    {
        parsedFiles.insert(FileName(rep->GetException(*it)));
    }

    types.clear();
    rep->GetAllPropertyTypeIds(types);
    for (std::set<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
    {
        parsedFiles.insert(FileName(rep->GetProperty(*it)));
    }
}

void TestFailed(std::set<std::string>& parsedFiles)
{
    std::cout<<"************* Test failed ******************"<<std::endl;
    std::cout<<"The parsed files was not the ones that was expected to be parsed!"<<std::endl;
    std::cout<<"Following "<<parsedFiles.size()<<" files were parsed:"<<std::endl;
    for (std::set<std::string>::const_iterator it=parsedFiles.begin(); it!=parsedFiles.end(); ++it)
    {
        std::cout<<*it<<std::endl;
    }
    exit(1);
}
