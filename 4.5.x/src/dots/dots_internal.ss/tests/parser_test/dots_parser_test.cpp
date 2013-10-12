/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joot
*
*******************************************************************************/
#include <iostream>
#include <set>
#include <boost/timer.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <Safir/Dob/Typesystem/Internal/TypeRepositoryOperations.h>
#include <boost/lexical_cast.hpp>
#include <boost/limits.hpp>

static bool PrintRepository=true;

struct MyTimer
{
    MyTimer() : m_last(boost::posix_time::microsec_clock().universal_time()) {}

    double Elapsed() const
    {
        boost::posix_time::time_duration d=(boost::posix_time::microsec_clock().universal_time()-m_last);
        return d.total_milliseconds()/1000.0;
    }
    void Reset() {m_last=boost::posix_time::microsec_clock().universal_time();}
    boost::posix_time::ptime m_last;
};

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
    //ParseDir(boost::filesystem::path("/home/joot/Dropbox/Dev/Dots parser/dots_generated_conv"));
    //return 0;

    boost::filesystem::path douDir;  //= boost::filesystem::path("/home/joel/dev/safir_open/src/dots/dots_parser.ss/tests/dou_test_files");
    if (argc>1)
    {
        douDir=boost::filesystem::path(argv[1]);
    }
    else
    {
        //For testing locally on developer machine without args.
#ifdef _MSC_VER
        douDir=boost::filesystem::path("C:/dev/dots_internal.ss/tests/parser_test/dou_test_files");
#else
        douDir=boost::filesystem::path("/home/joel/dev/safir_open/src/dots/dots_internal.ss/tests/parser_test/dou_test_files");
        //douDir=boost::filesystem::path("/home/joot/dev/safir-svn/src/dots/dots_internal.ss/tests/parser_test/dou_test_files");
#endif
    }

    std::cout<<"========= Test suite started ========"<<std::endl;
    MyTimer timer;
    try
    {
        RunTests(douDir, 0, 10000);
    }
    catch(...)
    {
        std::cout<<"Test failed with unexpected exception!"<<std::endl;
        return 1; //Failed
    }

    std::cout<<"- Time elapsed: "<<timer.Elapsed()<<std::endl;
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
        boost::shared_ptr<const Safir::Dob::Typesystem::Internal::TypeRepository> rep=Safir::Dob::Typesystem::Internal::ParseTypeDefinitions(test.path);
        if (PrintRepository)
        {
            //std::cout<<Safir::Dob::Typesystem::Internal::TypeRepositoryToString(rep.get())<<std::endl;
            std::cout<<rep<<std::endl;
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
    catch (const Safir::Dob::Typesystem::Internal::ParseError& err)
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
        throw Safir::Dob::Typesystem::Internal::ParseError("Unexpected error", "An unhandled exception occured in test.", test.path.string(), -2);
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
    MyTimer timer;
    try
    {
        boost::shared_ptr<const Safir::Dob::Typesystem::Internal::TypeRepository> rep=Safir::Dob::Typesystem::Internal::ParseTypeDefinitions(dir);
        //std::cout<<Safir::Dob::Typesystem::Internal::TypeRepositoryToString(rep)<<std::endl;
        std::cout<<"Parsed successfully! NumTypes="<<(rep->GetNumberOfClasses()+rep->GetNumberOfEnums()+rep->GetNumberOfExceptions()+rep->GetNumberOfProperties())<<std::endl;
    }
    catch(const Safir::Dob::Typesystem::Internal::ParseError& err)
    {
        std::cout<<"********** Parse Error **********************************************"<<std::endl;
        std::cout<<"* Label: "<<err.Label()<<std::endl;
        std::cout<<"* Descr: "<<err.Description()<<std::endl;
        std::cout<<"* File:  "<<err.File()<<std::endl;
        std::cout<<"* ErrId: "<<err.ErrorId()<<std::endl;
        std::cout<<"*********************************************************************"<<std::endl;
    }
    catch(...)
    {
        std::cout<<"Unexpected exception occured"<<std::endl;
    }


    std::cout<<"Time elapsed: "<<timer.Elapsed()<<std::endl;
}
