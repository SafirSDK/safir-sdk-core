/******************************************************************************
*
* Copyright Saab AB, 2004-2012 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / joot
*
*******************************************************************************/
#include <iostream>
#include <boost/timer.hpp>
#include <Safir/Dob/Typesystem/Internal/TypeDefinitionParser.h>
#include <boost/lexical_cast.hpp>
#include <boost/limits.hpp>

void RunTests(const boost::filesystem::path& testRoot, int firstTest, int lastTest);
void RunSingleTest(const boost::filesystem::path& testDir, const std::string& expectedResult);
void ParseDir(const boost::filesystem::path& dir);
bool CheckResult(const std::string& expected, const std::string& result);
bool TestInfoFromPath(const boost::filesystem::path& testDir, int& testNumber, std::string& testName, std::string& expectedResult);

int main(int argc, char* argv[])
{
    //ParseDir(boost::filesystem::path("/home/joel/dev/safir_open/src/dots/dots_parser.ss/tests/dou_test_files/100.General"));
    //ParseDir(boost::filesystem::path("/home/joel/dev/safir_open/src/dots/dots_parser.ss/tests/dou_test_files/400.Properties"));
    //ParseDir(boost::filesystem::path("/home/joel/dev/safir_open/src/dots/dots_parser.ss/tests/dou_test_files/200.CreateRoutines"));
    //ParseDir(boost::filesystem::path("/home/joel/safir/runtime/data/text/dots/classes"));
    //ParseDir(boost::filesystem::path("/home/joel/Dropbox/Dev/Dots parser/dots_generated"));
    //return 0;

    boost::filesystem::path douDir;  //= boost::filesystem::path("/home/joel/dev/safir_open/src/dots/dots_parser.ss/tests/dou_test_files");
    if (argc>1)
    {
        douDir = boost::filesystem::path(argv[1]);
    }
    else
    {
        //For testing locally on developer machine without args.
#ifdef _MSC_VER
        douDir = boost::filesystem::path("C:/dev/safir_sdk_core/src/dots/dots_parser.ss/tests/dou_test_files");
#else
        douDir = boost::filesystem::path("/home/joel/dev/safir_open/src/dots/dots_parser.ss/tests/dou_test_files");
#endif
    }

    boost::timer timer;
    try
    {
        RunTests(douDir, 0, 1000);
    }
    catch(const Safir::Dob::Typesystem::Internal::ParseError& err)
    {
        std::cout<<"********** Parse Error **********************************************"<<std::endl;
        std::cout<<"* Label: "<<err.Label()<<std::endl;
        std::cout<<"* Descr: "<<err.Description()<<std::endl;
        std::cout<<"* File:  "<<err.File()<<std::endl;
        std::cout<<"*********************************************************************"<<std::endl;
        return 1; //Failed
    }
    catch(...)
    {
        std::cout<<"Unexpected exception occured"<<std::endl;
        return 1; //Failed
    }


    std::cout<<"All tests passed. Time elapsed: "<<timer.elapsed()<<std::endl;
    return 0;
}

void RunTests(const boost::filesystem::path& testRoot, int firstTest, int lastTest)
{
    if (!boost::filesystem::is_directory(testRoot))
    {
        std::cout<<"Test root directory does not exist: "<<testRoot.string()<<std::endl;
        throw;
    }

    boost::filesystem::directory_iterator it(testRoot), end;
    for (; it!=end; ++it)
    {
        if (boost::filesystem::is_directory(it->path()))
        {
            int testNumber=-1;
            std::string testName;
            std::string expectedResult;
            if (TestInfoFromPath(it->path(), testNumber, testName, expectedResult))
            {
                std::cout<<"Test: "<<testNumber<<" "<<testName<<", Expect: "<<expectedResult<<std::endl;
                if (testNumber>=firstTest && testNumber<=lastTest)
                {
                    RunSingleTest(it->path(), expectedResult);
                }
            }
        }
    }
}

void RunSingleTest(const boost::filesystem::path& testDir, const std::string& expectedResult)
{
    //name consists of a test number followed by the expected result, 'ok' for success, or arbitrary
    //prelusive part of ParseError.Label if an error is expected to occur.    
    try
    {
        Safir::Dob::Typesystem::Internal::TypeDefinitionParser parser(testDir);
        boost::shared_ptr<const Safir::Dob::Typesystem::Internal::TypeRepository> rep=parser.GetRepository();
        //std::cout<<Safir::Dob::Typesystem::Internal::TypeDefinitionParser::ToString(rep)<<std::endl;
        //std::cout<<Safir::Dob::Typesystem::Internal::TypeDefinitionParser::ToString(parser->GetRepository())<<std::endl;
        //std::cout<<Safir::Dob::Typesystem::Internal::TypeDefinitionParser::ToString(parser->GetRawResult())<<std::endl;

        if (!CheckResult("ok", expectedResult))
        {
            throw Safir::Dob::Typesystem::Internal::ParseError("Unexpected parse result", "Did not expect this test to succeed", testDir.string());
        }

    }
    catch (const Safir::Dob::Typesystem::Internal::ParseError& err)
    {
        std::cout<<err.Label()<<std::endl;
        if (!CheckResult(err.Label(), expectedResult))
        {
            throw err;
        }
    }
    catch (...)
    {
        throw Safir::Dob::Typesystem::Internal::ParseError("Unexpected error", "An unhandled exception occured in test.", testDir.string());
    }
}

bool CheckResult(const std::string& expected, const std::string& result)
{
    bool ok = expected.find_first_of(result)!=expected.npos;
    if (ok)
    {
        std::cout<<"Success!"<<std::endl;
    }
    else
    {
        std::cout<<"Failure - Expected: "<<expected<<", Result: "<<result<<std::endl;
    }
    return ok;
}

bool TestInfoFromPath(const boost::filesystem::path& testDir, int& testNumber, std::string& testName, std::string& expectedResult)
{
#if BOOST_FILESYSTEM_VERSION == 2
    std::string name = testDir.filename();
#else
    std::string name = testDir.filename().string();
#endif
    size_t ix = name.find_first_of('.');
    if (ix==name.npos || ix==0)
    {
        return false;
    }

    try
    {
        testNumber=boost::lexical_cast<int>(name.substr(0, ix));

        //check if we have an expected error message
        size_t errIx = name.find_first_of('-');
        if (errIx!=name.npos)
        {
            testName=name.substr(ix+1, errIx);
            expectedResult=name.substr(errIx+1);
        }
        else
        {
            testName=name.substr(ix+1);
            expectedResult="ok";
        }

        return true;
    }
    catch(...)
    {
        return false;
    }
}

//Just parse a directory and print errors if any
void ParseDir(const boost::filesystem::path& dir)
{
    boost::timer timer;
    try
    {
        Safir::Dob::Typesystem::Internal::TypeDefinitionParser parser(dir);
        //std::cout<<Safir::Dob::Typesystem::Internal::TypeDefinitionParser::ToString(parser.GetRepository())<<std::endl;
        std::cout<<"Parsed ok!"<<std::endl;
    }
    catch(const Safir::Dob::Typesystem::Internal::ParseError& err)
    {
        std::cout<<"********** Parse Error **********************************************"<<std::endl;
        std::cout<<"* Label: "<<err.Label()<<std::endl;
        std::cout<<"* Descr: "<<err.Description()<<std::endl;
        std::cout<<"* File:  "<<err.File()<<std::endl;
        std::cout<<"*********************************************************************"<<std::endl;
    }
    catch(...)
    {
        std::cout<<"Unexpected exception occured"<<std::endl;
    }


    std::cout<<"Time elapsed: "<<timer.elapsed()<<std::endl;
}
