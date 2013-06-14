/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / joot
*
*******************************************************************************/
#include <iostream>
#include <fstream>
#include <set>
#include <boost/lexical_cast.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include <Safir/Dob/Typesystem/Internal/TypeRepositoryOperations.h>
#include <Safir/Dob/Typesystem/Internal/BlobLayout.h>
#include <Safir/Dob/Typesystem/Internal/Id.h>
#include <Safir/Dob/Typesystem/Internal/Serialization.h>

using namespace Safir::Dob::Typesystem::Internal;

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

typedef boost::shared_ptr<const Safir::Dob::Typesystem::Internal::TypeRepository> RepositoryPtr;
const ClassDescription* GetClassByName(const RepositoryPtr& rep, const std::string& name);
bool TestInfoFromPath(const boost::filesystem::path& p, TestCase& test);

void RunTests(const TypeRepository* repository, const boost::filesystem::path& testRoot, int firstTest, int lastTest);
bool RunSingleTest(const TypeRepository* repository, const TestCase& test);
bool RunTestFromXml(const TypeRepository* repository, const std::string& fileName);
void RunTestFromJson(const TypeRepository* repository, const std::string& fileName);
void PrintTestFailMessage(const std::string& result, const std::string& shortInfo, const std::string& description, const std::string& file);

int main(int argc, char* argv[])
{    
    //-----------------------------------------------------------
    //Parse dou files for this test and create a type repository
    //-----------------------------------------------------------
    boost::filesystem::path douDir;  //= boost::filesystem::path("/home/joel/dev/safir_open/src/dots/dots_internal.ss/tests/dou_test_files");
    boost::filesystem::path testDir;
    if (argc>2)
    {
        douDir=boost::filesystem::path(argv[1]);
        testDir=boost::filesystem::path(argv[2]);
    }
    else
    {
        douDir=boost::filesystem::path("C:/dev/dots_internal.ss/tests/serialization_test/dou");
        testDir=boost::filesystem::path("C:/dev/dots_internal.ss/tests/serialization_test/testcases");

        //douDir=boost::filesystem::path("/home/joel/dev/safir_open/src/dots/dots_internal.ss/tests/serialization_test/dou");
        //testDir=boost::filesystem::path("/home/joel/dev/safir_open/src/dots/dots_internal.ss/tests/serialization_test/testcases");
        //douDir=boost::filesystem::path("/home/joot/dev/safir-svn/src/dots/dots_internal.ss/tests/serialization_test/dou");
        //testDir=boost::filesystem::path("/home/joot/dev/safir-svn/src/dots/dots_internal.ss/tests/serialization_test/testcases");
    }

    RepositoryPtr repository;
    try
    {
        repository=Safir::Dob::Typesystem::Internal::ParseTypeDefinitions(douDir);

    }
    catch (const Safir::Dob::Typesystem::Internal::ParseError& err)
    {
        std::cout<<err.what()<<std::endl;
        return 1;
    }

    std::cout<<"========= Test suite started ========"<<std::endl;
    try
    {
        RunTests(repository.get(), testDir, 0, 10000);
    }
    catch(...)
    {
        std::cout<<"Test failed with unexpected exception!"<<std::endl;
        return 1; //Failed
    }

    std::cout<<"====================================="<<std::endl;
    return 0;
}

//--------------
const ClassDescription* GetClassByName(const RepositoryPtr& rep, const std::string& name)
{
    std::vector<DotsC_TypeId> v;
    rep->GetAllClassTypeIds(v);
    for (std::vector<DotsC_TypeId>::const_iterator it=v.begin(); it!=v.end(); ++it)
    {
        const ClassDescription* c=rep->GetClass(*it);
        if (c->GetName()==name)
            return c;
    }
    return NULL;
}

bool TestInfoFromPath(const boost::filesystem::path& p, TestCase& test)
{
#if !defined(BOOST_FILESYSTEM_VERSION) || (BOOST_FILESYSTEM_VERSION == 2)
    std::string name=p.stem();
#else
    std::string name=p.stem().string();
#endif
    size_t ix=name.find_first_of('.');
    if (ix==name.npos || ix==0)
    {
        return false;
    }

    try
    {
        test.path=p;
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

void RunTests(const TypeRepository* repository, const boost::filesystem::path& testRoot, int firstTest, int lastTest)
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
        if (!boost::filesystem::is_directory(it->path()))
        {
            std::string file=it->path().string();
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

        if (RunSingleTest(repository, *it))
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

bool RunSingleTest(const TypeRepository* repository, const TestCase& test)
{
    //name consists of a test number followed by the expected result, 'ok' for success, or arbitrary
    //prelusive part of ParseError.Label if an error is expected to occur.
    try
    {
        if (test.path.extension()==".xml")
        {
            RunTestFromXml(repository, test.path.string());
        }
        else if (test.path.extension()==".json")
        {
            //TODO
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


bool RunTestFromXml(const TypeRepository* repository, const std::string& fileName)
{
    //read file into xml
    std::ostringstream xml1;
    {
        std::ifstream is(fileName.c_str());
        xml1<<is.rdbuf();
        is.close();
    }
    std::cout<<"---- RAW ---"<<std::endl<<xml1.str()<<std::endl;
    std::vector<char> bin1;
    Safir::Dob::Typesystem::Internal::XmlToBinary(repository, xml1.str().c_str(), bin1);    

    std::ostringstream os1;
    Safir::Dob::Typesystem::Internal::BinaryToBase64(&bin1[0], bin1.size(), os1);

    std::vector<char> bin2;
    Safir::Dob::Typesystem::Internal::Base64ToBinary(os1.str(), bin2);

    if (bin1.size()!=bin2.size() || memcmp(&bin1[0], &bin2[0], bin1.size())!=0)
    {
        throw std::logic_error("Not binary equal after base64 conversion");
    }

    std::ostringstream xml2;
    BinaryToXml(repository, &bin2[0], xml2);
    std::cout<<"---- XML2 ---"<<std::endl<<xml2.str()<<std::endl;

    std::vector<char> bin3;
    Safir::Dob::Typesystem::Internal::XmlToBinary(repository, xml2.str().c_str(), bin3);

    if (bin1.size()!=bin3.size() || memcmp(&bin1[0], &bin3[0], bin1.size())!=0)
    {
        throw std::logic_error("Not binary equal after xml conversion");
    }



    std::ostringstream json1;
    BinaryToJson(repository, &bin1[0], json1);

    std::cout<<"--- JSON 1 ---"<<std::endl<<json1.str()<<std::endl;

    std::vector<char> bin4;
    JsonToBinary(repository, json1.str().c_str(), bin4);
    if (bin1.size()!=bin4.size() || memcmp(&bin1[0], &bin4[0], bin1.size())!=0)
    {
        throw std::logic_error("Not binary equal after json conversion");
    }

    std::ostringstream json2;
    BinaryToJson(repository, &bin4[0], json2);
    std::cout<<"--- JSON 2 ---"<<std::endl<<json2.str()<<std::endl;


    return true;
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


//==============================================================================================
// Heres some code that maybe will be used later in a test case. It builds a blob from scratch
//==============================================================================================
//    boost::filesystem::recursive_directory_iterator it(testDir), end;
//    while (it!=end)
//    {
//        if (it->path().extension()==".xml")
//        {
//            RunTestFromXml(repository.get(), it->path().string());
//        }
//        else if (it->path().extension()==".json")
//        {

//        }
//        ++it;
//    }
//    return 0;

    //------ blob builder ------
//    //Inital declarations
//    BlobLayout bl(repository.get());
//    const ClassDescription* cd=GetClassByName(repository, "BlobTest.MyEntity");
//    int size=2*cd->InitialSize()+GetClassByName(repository, "BlobTest.MyItem")->InitialSize()*5;
//    std::cout<<"Create a blob sized "<<size<<std::endl;
//    char* blob=new char[size];
//    char* unused=NULL;
//    bl.FormatBlob(blob, size, cd->GetTypeId(), unused);
//    DotsC_MemberIndex memIx=-1;
//    DotsC_MemberStatus stat;

//    //-----------------------------------------------------------
//    // Start tests
//    //-----------------------------------------------------------

//    //int32
//    //-----------------
//    {
//        DotsC_Int32 int32val=0;
//        memIx=cd->GetMemberIndex("MyInt32Val");
//        stat=bl.GetMemberStatus(blob, memIx, 0);
//        bl.SetInt32Member(123, blob, memIx, 0);
//        bl.SetMemberStatus(false, true, blob, memIx, 0);
//        stat=bl.GetInt32Member(blob, memIx, 0, int32val);
//        std::cout<<"val="<<int32val<<std::endl;
//    }

//    //MyInt32Array
//    //------------------------
//    {
//        DotsC_Int32 int32arr=0;
//        memIx=cd->GetMemberIndex("MyInt32Array");
//        stat=bl.GetMemberStatus(blob, memIx, 0);
//        for (int ai=0; ai<3; ++ai)
//        {
//            bl.SetInt32Member(10+ai, blob, memIx, ai);
//            bl.SetMemberStatus(false, true, blob, memIx, ai);
//            stat=bl.GetInt32Member(blob, memIx, ai, int32arr);
//            std::cout<<"val="<<int32arr<<std::endl;
//        }
//    }

//    //stringVal
//    //-----------------
//    {
//        char* strVal=unused;
//        memIx=cd->GetMemberIndex("MyStringVal");
//        stat=bl.GetMemberStatus(blob, memIx, 0);

//        std::string toSet="Bill & Bull vet inte att 4<9,\\ 10>5 \"muhaha!\"";
//        //std::string toSet="Hello_JoelOttosson";
//        bl.CreateStringMember(blob, toSet.size()+1, memIx, 0, true, unused);
//        strcpy(strVal, toSet.c_str());

//        const char* result=NULL;
//        int dummy=0;
//        stat=bl.GetDynamicMember(blob, memIx, 0, result, dummy);
//        std::cout<<"val="<<result<<std::endl;
//    }

//    //item
//    //-------------------
//    {
//        const ClassDescription* itemCd=GetClassByName(repository, "BlobTest.MyItem");
//        int itemSize=2*itemCd->InitialSize();
//        char* itemBlob=new char[itemSize];
//        char* itemUnused=NULL;
//        bl.FormatBlob(itemBlob, itemSize, itemCd->GetTypeId(), itemUnused);
//        DotsC_MemberIndex itemMemIx=itemCd->GetMemberIndex("MyNumber");
//        bl.SetInt32Member(666, itemBlob, itemMemIx, 0);
//        bl.SetMemberStatus(false, true, itemBlob, itemMemIx, 0);


//        char* obj=unused;
//        memIx=cd->GetMemberIndex("MyItemVal");
//        stat=bl.GetMemberStatus(blob, memIx, 0);

//        bl.CreateObjectMember(blob, itemSize, bl.GetTypeId(itemBlob), memIx, 0, true, unused);
//        memcpy(obj, itemBlob, itemSize);

//    }


//    try
//    {
//        std::cout<<"--- BinaryToXml ---"<<std::endl;
//        std::ostringstream xml;
//        Safir::Dob::Typesystem::Internal::BinaryToXml(repository.get(), blob, xml);
//        std::cout<<xml.str()<<std::endl;

//        std::cout<<std::endl<<"--- XmlToBinary ---"<<std::endl;
//        std::vector<char> toBlob;
//        Safir::Dob::Typesystem::Internal::XmlToBinary(repository.get(), xml.str().c_str(), toBlob);

//        std::cout<<std::endl<<"--- BinaryToXml again---"<<std::endl;
//        std::ostringstream xml2;
//        Safir::Dob::Typesystem::Internal::BinaryToXml(repository.get(), &toBlob[0], xml2);
//        std::cout<<xml2.str()<<std::endl;
//    }
//    catch (const Safir::Dob::Typesystem::Internal::ParseError& err)
//    {
//        std::cout<<err.what()<<std::endl;
//        return 1;
//    }

//    //int32
//    //-----------------
//    memIx=cd->GetMemberIndex("MyInt32");
//    md=cd->GetMember(memIx);r
//    bl.SetInt32Member(123, blob, memIx, 0);
//    stat=bl.GetInt32Member(blob, memIx, 0, int32Val);

//    //int32
//    //-----------------
//    memIx=cd->GetMemberIndex("MyInt32");
//    md=cd->GetMember(memIx);
//    bl.SetInt32Member(123, blob, memIx, 0);
//    stat=bl.GetInt32Member(blob, memIx, 0, int32Val);

