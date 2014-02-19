/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joot
*
*******************************************************************************/
#ifdef _MSC_VER
#pragma warning(disable:4913)
#endif

#include <iostream>
#include <set>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/timer.hpp>
#include <Safir/Dob/Typesystem/ToolSupport/TypeParser.h>
#include <Safir/Dob/Typesystem/ToolSupport/Serialization.h>
#include <boost/lexical_cast.hpp>
#include <boost/limits.hpp>

static bool StopOnError=false;
static bool PrintRepository=false;

struct TestCase
{
    static const int SuccessCode=1000000;
    boost::filesystem::path path;
    int testNumber;
    std::string name;
    int expectedResult;
};
struct CompareTestCase
{
    bool operator()(const TestCase& a, const TestCase& b) {return a.testNumber<=b.testNumber;}
};
typedef  std::set<TestCase, CompareTestCase> TestSet;

void RunTests(const boost::filesystem::path& testRoot, int firstTest, int lastTest);
bool RunSingleTest(const TestCase& test);
void ParseDir(const boost::filesystem::path& dir);
bool CheckResult(int expected, const std::string& result);
bool TestInfoFromPath(const boost::filesystem::path& testDir, int& testNumber, std::string& testName, int& expectedErrorCode);
bool TestInfoFromPath(const boost::filesystem::path& testDir, TestCase& test);
void PrintTestFailMessage(const std::string& result, const std::string& shortInfo, const std::string& description, const std::string& file);

int main(int argc, char* argv[])
{
    //ParseDir("/home/joot/safir/runtime/data/text/dots/classes");
    //ParseDir("/home/joot/dev/slb_dou/dots/classes");
    //ParseDir("/home/joot/Dropbox/Dev/DotsParser/dots_generated_conv");
    //ParseDir("/home/joot/dev/safir_open/dots_internal_rewrite/src/dots/dots_test_dou.ss/data");
    //return 0;

    if (argc<2)
    {
        std::cout<<"Too few arguments!"<<std::endl;
        std::cout<<"Usage: 'dots_parser_test <testRoot> firstTest lastTest print'"<<std::endl;
        std::cout<<"   or just 'dots_parser_test <testRoot>''  to run all tests."<<std::endl;
        return 1;
    }

    boost::filesystem::path douDir(argv[1]);
    int first=0;
    int last=10000;
    if (argc>2)
    {
        first=boost::lexical_cast<int>(argv[2]);
    }
    if (argc>3)
    {
        last=boost::lexical_cast<int>(argv[3]);
    }
    if (argc>4)
    {
        PrintRepository=true;
    }

    std::cout<<"========= Test suite started ========"<<std::endl;
    boost::timer timer;
    try
    {
        RunTests(douDir, first, last);
    }
    catch(...)
    {
        std::cout<<"Test failed with unexpected exception!"<<std::endl;
        return 1; //Failed
    }

    std::cout<<"- Time elapsed: "<<timer.elapsed()<<std::endl;
    std::cout<<"====================================="<<std::endl;
    return 0;
}

void RunTests(const boost::filesystem::path& testRoot, int firstTest, int lastTest)
{
    int passed=0;
    int failed=0;

    if (!boost::filesystem::is_directory(testRoot))
    {
        std::cout<<"Test root directory does not exist: "<<testRoot.string()<<std::endl;
        throw;
    }

    TestSet testCases;

    //Order test cases after test number
    boost::filesystem::directory_iterator it(testRoot), end;
    for (; it!=end; ++it)
    {
        if (boost::filesystem::is_directory(it->path()))
        {
            TestCase tc;
            if (TestInfoFromPath(it->path(), tc))
            {
                if (tc.testNumber>=firstTest && tc.testNumber<=lastTest)
                {
                    testCases.insert(tc);
                }
            }
        }
    }

    //run the tests
    for (TestSet::const_iterator it=testCases.begin(); it!=testCases.end(); ++it)
    {
        std::cout<<"  -------------------------------------------------"<<std::endl;
        std::cout<<"  Test "<<it->testNumber<<" "<<it->name<<std::endl;
        if (it->expectedResult==TestCase::SuccessCode) std::cout<<"        Expect: success"<<std::endl;
        else std::cout<<"        Expect: "<<it->expectedResult<<std::endl;

        if (RunSingleTest(*it))
        {
            std::cout<<"        Test Passed"<<std::endl;
            ++passed;
        }
        else
        {
            std::cout<<"        Test Failed"<<std::endl;
            ++failed;
            if (StopOnError)
            {
                break;
            }
        }
    }

    std::cout<<"-------------- Summary --------------"<<std::endl;
    std::cout<<"- Number of executed test cases: "<<(passed+failed)<<std::endl;
    std::cout<<"- Passed: "<<passed<<std::endl;
    std::cout<<"- Failed: "<<failed<<std::endl;
    if (failed==0)
    {
        std::cout<<"- Test suite passed with no errors!  "<<std::endl;
    }
    else
    {
        std::cout<<"- Test suite finished with ERRORS!  "<<std::endl;
        exit(1);
    }

}

bool RunSingleTest(const TestCase& test)
{
    //name consists of a test number followed by the expected result, 'ok' for success, or arbitrary
    //prelusive part of ParseError.Label if an error is expected to occur.    
    try
    {
        boost::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository> rep=Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(test.path);
        if (PrintRepository)
        {
            Safir::Dob::Typesystem::ToolSupport::RepositoryToString(rep.get(), true, std::cout);
        }

        if (test.expectedResult!=TestCase::SuccessCode)
        {
            std::ostringstream os;
            os<<"Parsing succeeded but was expected to generate ParseError with code "<<test.expectedResult;
            PrintTestFailMessage(os.str(), "Unexpected parse result", "Did not expect this test to succeed", test.path.string());
            return false;
        }
        std::cout<<"        Result:  success"<<std::endl;

    }
    catch (const Safir::Dob::Typesystem::ToolSupport::ParseError& err)
    {
        std::cout<<"        Result:  "<<err.ErrorId()<<std::endl;
        std::cout<<"        Message: "<<err.Label()<<std::endl;
        if (err.ErrorId()!=test.expectedResult)
        {
            std::ostringstream os;
            os<<"Parsing failed with error id "<<err.ErrorId()<<" but was expected to ";
            if (test.expectedResult==TestCase::SuccessCode)
            {
                os<<"succeed.";
            }
            else
            {
                os<<"generate error id "<<test.expectedResult;
            }
            PrintTestFailMessage(os.str(), err.Label(), err.Description(), err.File());
            return false;
        }
    }
    catch (...)
    {
        throw Safir::Dob::Typesystem::ToolSupport::ParseError("Unexpected error", "An unhandled exception occured in test.", test.path.string(), -2);
    }

    return true;
}

bool TestInfoFromPath(const boost::filesystem::path& testDir, TestCase& test)
{
#if !defined(BOOST_FILESYSTEM_VERSION) || (BOOST_FILESYSTEM_VERSION == 2)
    std::string name=testDir.filename();
#else
    std::string name=testDir.filename().string();
#endif
    size_t ix=name.find_first_of('.');
    if (ix==name.npos || ix==0)
    {
        return false;
    }

    try
    {
        test.path=testDir;
        test.testNumber=boost::lexical_cast<int>(name.substr(0, ix));

        //check if we have an expected error message
        size_t errIx=name.find_last_of('-');
        if (errIx!=name.npos)
        {
            test.name=name.substr(ix+1, errIx);
            test.expectedResult=boost::lexical_cast<int>(name.substr(errIx+1));
        }
        else
        {
            test.name=name.substr(ix+1);
            test.expectedResult=TestCase::SuccessCode;
        }

        return true;
    }
    catch(...)
    {
        return false;
    }
}

void PrintTestFailMessage(const std::string& result, const std::string& shortInfo, const std::string& description, const std::string& file)
{
    std::cout<<"********** Test case failed *****************************************"<<std::endl;
    std::cout<<"* "<<result<<std::endl;
    std::cout<<"* Label: "<<shortInfo<<std::endl;
    std::cout<<"* Descr: "<<description<<std::endl;
    std::cout<<"* File:  "<<file<<std::endl;
    std::cout<<"*********************************************************************"<<std::endl;
}

//Just parse a directory and print errors if any
void ParseDir(const boost::filesystem::path& dir)
{
    std::cout<<"Start parsing"<<std::endl;
    boost::timer timer;
    try
    {
        boost::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository> rep=Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(dir);
        //Safir::Dob::Typesystem::ToolSupport::RepositoryToString(rep.get(), std::cout);
        std::cout<<"Parsed successfully! NumTypes="<<(rep->GetNumberOfClasses()+rep->GetNumberOfEnums()+rep->GetNumberOfExceptions()+rep->GetNumberOfProperties())<<std::endl;
    }
    catch(const Safir::Dob::Typesystem::ToolSupport::ParseError& err)
    {
        std::cout<<"********** Parse Error **********************************************"<<std::endl;
        std::cout<<"* Label: "<<err.Label()<<std::endl;
        std::cout<<"* Descr: "<<err.Description()<<std::endl;
        std::cout<<"* File:  "<<err.File()<<std::endl;
        std::cout<<"* ErrorCode: "<<err.ErrorId()<<std::endl;
        std::cout<<"*********************************************************************"<<std::endl;
    }
    catch(...)
    {
        std::cout<<"Unexpected exception occured"<<std::endl;
    }


    std::cout<<"Time elapsed: "<<timer.elapsed()<<std::endl;
}

#ifdef _MSC_VER
#pragma warning(default:4913)
#endif
