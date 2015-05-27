/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joot
*
*******************************************************************************/
#include <iostream>
#include <fstream>
#include <set>
#include <boost/chrono.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include <Safir/Dob/Typesystem/ToolSupport/TypeParser.h>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Dob/Typesystem/ToolSupport/Serialization.h>
#include <Safir/Dob/Typesystem/ToolSupport/BlobWriter.h>
#include <Safir/Dob/Typesystem/ToolSupport/BlobReader.h>

using namespace Safir::Dob::Typesystem::ToolSupport;

#define CHECK(v) if(!v){std::cout<<"Test failed, file: "<<__FILE__<<", line: "<<__LINE__<<std::endl; exit(1);}

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

typedef boost::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository> RepositoryPtr;
const ClassDescription* GetClassByName(const RepositoryPtr& rep, const std::string& name);
bool TestInfoFromPath(const boost::filesystem::path& p, TestCase& test);

void RunTests(const TypeRepository* repository, const boost::filesystem::path& testRoot, int firstTest, int lastTest);
bool RunSingleTest(const TypeRepository* repository, const TestCase& test);
bool RunTestFromXml(const TypeRepository* repository, const std::string& fileName);
void RunTestFromJson(const TypeRepository* repository, const std::string& fileName);
void PrintTestFailMessage(const std::string& result, const std::string& shortInfo, const std::string& description, const std::string& file);

void CheckBlobSize(const std::vector<char>& blob)
{
    DotsC_Int32 size=*reinterpret_cast<const DotsC_Int32*>(&blob[0]);

    if (static_cast<size_t>(size)!=blob.size())
    {
        std::cout<<"Wrong blob size. BlobSize="<<size<<", blobVector.size()="<<blob.size()<<std::endl;
        exit(1);
    }

    std::cout<<"blob size="<<blob.size()<<", capacity="<<blob.capacity()<<std::endl;

}

void BlobArraySizeDiff(RepositoryPtr rep)
{
    const ClassDescription* cd=GetClassByName(rep, "BlobTest.SmallArray");
    const DotsC_MemberIndex intArrayMemberIndex=cd->GetMemberIndex("IntArray");
    const MemberDescription* md=cd->GetMember(intArrayMemberIndex);

    if (cd->GetNumberOfMembers()!=1)
    {
        throw std::logic_error("BlobTest.SmallArray is expected to have just one member.");
    }
    if (md->GetArraySize()!=3)
    {
        throw std::logic_error("BlobTest.SmallArray.IntArray is expected to have arraySize=3.");
    }

    //Manually create a blob with too few array elements.
    Internal::Blob blob(cd->GetTypeId(), cd->GetNumberOfMembers());
    blob.AddValue(intArrayMemberIndex, false);
    blob.AddValue(intArrayMemberIndex, false);
    blob.SetValueInt32(intArrayMemberIndex, 0, 1); //intArray[0]=1
    blob.SetValueInt32(intArrayMemberIndex, 1, 2); //intArray[1]=2

    //serialize to binary
    size_t size=static_cast<size_t>(blob.CalculateBlobSize());
    std::vector<char> bin(size);
    blob.Serialize(&bin[0]);

    //Deserialize the blob that has only 2 array elements and is expected to have 3
    int value=0;
    bool isNull=false;
    bool isChanged=false;

    BlobReader<Safir::Dob::Typesystem::ToolSupport::TypeRepository> r(rep.get(), &bin[0]);

    //read index 0
    r.ReadValue(intArrayMemberIndex, 0, value, isNull, isChanged);
    if (isNull || value!=1)
    {
        throw std::logic_error("BlobTest.SmallArray.IntArray[0] is expected to have value 1");
    }

    //read index 1
    r.ReadValue(intArrayMemberIndex, 1, value, isNull, isChanged);
    if (isNull || value!=2)
    {
        throw std::logic_error("BlobTest.SmallArray.IntArray[1] is expected to have value 2");
    }

    //read index 2, wich does not exist. Should return isNull=true and isChanged=false on ReadValue
    r.ReadValue(intArrayMemberIndex, 2, value, isNull, isChanged);
    //std::cout<<"BlobTest.SmallArray.IntArray[2]: isNull="<<std::boolalpha<<isNull<<", isChanged="<<isChanged<<", value="<<value<<std::endl;
    if (!isNull || isChanged )
    {
        throw std::logic_error("BlobTest.SmallArray.IntArray[2] is expected to be null and not changed");
    }
}

void BlobDiffTest(RepositoryPtr rep)
{
    std::cout<<"========= Blob Diff Test ========"<<std::endl;

    //BlobTest.MyItem
    const DotsC_TypeId tid=Safir::Dob::Typesystem::ToolSupport::TypeUtilities::CalculateTypeId("BlobTest.MySpecialItem");
    const ClassDescription* cd=rep->GetClass(tid);
    const DotsC_MemberIndex myNum=cd->GetMemberIndex("MyNumber");
    const DotsC_MemberIndex myStrings=cd->GetMemberIndex("MyStrings");
    const DotsC_MemberIndex myChildName=cd->GetMemberIndex("ChildName");
    const DotsC_MemberIndex mySeqInt32=cd->GetMemberIndex("SeqInt32");
    const DotsC_MemberIndex mySeqInt64=cd->GetMemberIndex("SeqInt64");
    const DotsC_MemberIndex mySeqBool=cd->GetMemberIndex("SeqBool");
    const DotsC_MemberIndex myDictStringInt32=cd->GetMemberIndex("DictStringInt32");
    const DotsC_MemberIndex myDictInt32Int32=cd->GetMemberIndex("DictInt32Int32");
    const DotsC_MemberIndex myDictInt32Bool=cd->GetMemberIndex("DictInt32Bool");
    const DotsC_MemberIndex myDictStringString=cd->GetMemberIndex("DictStringString");

    //Create First object
    //--------------------
    BlobWriter<Safir::Dob::Typesystem::ToolSupport::TypeRepository> first(rep.get(), tid);
    first.WriteValue(myNum, 0, 123, false, true);
    first.WriteValue(myStrings, 0, "Hej_1", false, true);
    first.WriteValue(myStrings, 3, "Hej_2", false, true);
    first.WriteValue(myStrings, 4, "Hej_3", false, true);
    first.WriteValue(myChildName, 0, "Svarre", false, true);
    first.WriteValue(mySeqInt32, 0, 1, false, false);
    first.WriteValue(mySeqInt32, 0, 2, false, false);
    first.WriteValue(mySeqInt64, 0, DotsC_Int64(1), false, false);
    first.WriteValue(mySeqInt64, 0, DotsC_Int64(2), false, false);
    first.WriteValue(mySeqBool, 0, true, false, false);
    first.WriteKey(myDictStringInt32, "first");
    first.WriteValue(myDictStringInt32, 0, 1, false, false);
    first.WriteKey(myDictStringInt32, "second");
    first.WriteValue(myDictStringInt32, 0, 2, false, false);
    first.WriteKey(myDictStringInt32, "third");
    first.WriteValue(myDictStringInt32, 0, 3, false, false);
    first.WriteKey(myDictStringInt32, "fourth");
    first.WriteValue(myDictStringInt32, 0, 4, true, false);
    first.WriteKey(myDictInt32Int32, 0);
    first.WriteValue(myDictInt32Int32, 0, 0, false, false);
    first.WriteKey(myDictInt32Int32, 1);
    first.WriteValue(myDictInt32Int32, 0, 1, false, false);
    first.WriteKey(myDictInt32Bool, 0);
    first.WriteValue(myDictInt32Bool, 0, true, false, false);
    first.WriteKey(myDictStringString, "first");
    first.WriteValue(myDictStringString, 0, "Ralph", false, false);
    first.WriteKey(myDictStringString, "second");
    first.WriteValue(myDictStringString, 0, "Florian", false, false);
    first.SetAllChangeFlags(false);

    //Create Second object
    //--------------------
    BlobWriter<Safir::Dob::Typesystem::ToolSupport::TypeRepository> second(rep.get(), tid);
    second.WriteValue(myNum, 0, 123, false, true);
    second.WriteValue(myStrings, 0, "Hej_1", false, true);
    second.WriteValue(myStrings, 3, "Hej_2", false, true);
    second.WriteValue(myStrings, 4, "Hej_4", false, true);
    second.WriteValue(myChildName, 0, "Pelle", false, true);
    second.WriteValue(mySeqInt32, 0, 2, false, false);
    second.WriteValue(mySeqInt32, 0, 1, false, false);
    second.WriteValue(mySeqInt64, 0, DotsC_Int64(1), false, false);
    second.WriteValue(mySeqInt64, 0, DotsC_Int64(2), false, false);
    second.WriteValue(mySeqBool, 0, true, false, false);
    second.WriteValue(mySeqBool, 0, true, false, false);
    second.WriteKey(myDictStringInt32, "first");
    second.WriteValue(myDictStringInt32, 0, 1, false, false);
    second.WriteKey(myDictStringInt32, "second");
    second.WriteValue(myDictStringInt32, 0, 22, false, false);
    second.WriteKey(myDictStringInt32, "third");
    second.WriteValue(myDictStringInt32, 0, 3, true, false);
    second.WriteKey(myDictStringInt32, "fourth");
    second.WriteValue(myDictStringInt32, 0, 4, false, false);
    second.WriteKey(myDictInt32Int32, 0);
    second.WriteValue(myDictInt32Int32, 0, 0, false, false);
    second.WriteKey(myDictInt32Int32, 2);
    second.WriteValue(myDictInt32Int32, 0, 1, false, false);
    second.WriteKey(myDictInt32Bool, 0);
    second.WriteValue(myDictInt32Bool, 0, true, false, false);
    second.WriteKey(myDictInt32Bool, 1);
    second.WriteValue(myDictInt32Bool, 0, true, false, false);
    second.WriteKey(myDictStringString, "second");
    second.WriteValue(myDictStringString, 0, "Florian", false, false);
    second.WriteKey(myDictStringString, "first");
    second.WriteValue(myDictStringString, 0, "Ralph", false, false);
    second.SetAllChangeFlags(false);

    bool isNull, isChanged;
    std::vector<char> firstBlob(static_cast<size_t>(first.CalculateBlobSize()));
    first.CopyRawBlob(&firstBlob[0]);
    std::vector<char> secondBlob(static_cast<size_t>(second.CalculateBlobSize()));
    second.CopyRawBlob(&secondBlob[0]);

    BlobReader<Safir::Dob::Typesystem::ToolSupport::TypeRepository> r(rep.get(), &firstBlob[0]);
    BlobWriter<Safir::Dob::Typesystem::ToolSupport::TypeRepository> diffWriter(r);
    BlobReader<Safir::Dob::Typesystem::ToolSupport::TypeRepository> origin(rep.get(), &secondBlob[0]);

    diffWriter.MarkChanges(origin);
    int diffSize=diffWriter.CalculateBlobSize();
    std::vector<char> diffBlob(static_cast<size_t>(diffSize));
    diffWriter.CopyRawBlob(&diffBlob[0]);

    std::cout<<"Diff\n---"<<std::endl;
    r.ReadStatus(myNum, 0, isNull, isChanged);
    std::cout<<"myNum - "<<std::boolalpha<<"isNull: "<<isNull<<", isChanged: "<<isChanged<<std::endl;
    CHECK(!isChanged)

    r.ReadStatus(myChildName, 0, isNull, isChanged);
    std::cout<<"myChildName - "<<std::boolalpha<<"isNull: "<<isNull<<", isChanged: "<<isChanged<<std::endl;
    CHECK(isChanged)

    std::cout<<"mySeqInt32 - changedTopLevel: "<<r.IsChangedTopLevel(mySeqInt32)<<std::endl;
    CHECK(r.IsChangedTopLevel(mySeqInt32))
    std::cout<<"mySeqInt64 - changedTopLevel: "<<r.IsChangedTopLevel(mySeqInt64)<<std::endl;
    CHECK(!r.IsChangedTopLevel(mySeqInt64))
    std::cout<<"mySeqBool - changedTopLevel: "<<r.IsChangedTopLevel(mySeqBool)<<std::endl;
    CHECK(r.IsChangedTopLevel(mySeqBool))

    for (int i=0; i<5; ++i)
    {
        r.ReadStatus(myStrings, i, isNull, isChanged);
        std::cout<<"myStrings["<<i<<"] - "<<std::boolalpha<<"isNull: "<<isNull<<", isChanged: "<<isChanged<<std::endl;
        if (i==4) {CHECK(isChanged)}
        else {CHECK(!isChanged)}
    }

    std::cout<<"myDictStringInt32 - changedTopLevel: "<<r.IsChangedTopLevel(myDictStringInt32)<<std::endl;
    CHECK(!r.IsChangedTopLevel(myDictStringInt32))
    for (int i=0; i<r.NumberOfValues(myDictStringInt32); ++i)
    {
        std::string key=r.ReadKey<const char*>(myDictStringInt32, i);
        r.ReadStatus(myDictStringInt32, i, isNull, isChanged);
        std::cout<<"    myDictStringInt32["<<key<<"] - "<<std::boolalpha<<"isNull: "<<isNull<<", isChanged: "<<isChanged<<std::endl;
        if (key=="first") {CHECK(!isChanged)}
        else {CHECK(isChanged)}
    }

    std::cout<<"myDictInt32Int32 - changedTopLevel: "<<r.IsChangedTopLevel(myDictInt32Int32)<<std::endl;
    CHECK(r.IsChangedTopLevel(myDictInt32Int32))
    for (int i=0; i<r.NumberOfValues(myDictInt32Int32); ++i)
    {
        DotsC_Int32 key=r.ReadKey<DotsC_Int32>(myDictInt32Int32, i);
        r.ReadStatus(myDictInt32Int32, i, isNull, isChanged);
        std::cout<<"    myDictInt32Int32["<<key<<"] - "<<std::boolalpha<<"isNull: "<<isNull<<", isChanged: "<<isChanged<<std::endl;
        if (key==1) {CHECK(isChanged)}
        else {CHECK(!isChanged)}
    }

    std::cout<<"myDictInt32Bool - changedTopLevel: "<<r.IsChangedTopLevel(myDictInt32Bool)<<std::endl;
    CHECK(r.IsChangedTopLevel(myDictInt32Bool))
    for (int i=0; i<r.NumberOfValues(myDictInt32Bool); ++i)
    {
        DotsC_Int32 key=r.ReadKey<DotsC_Int32>(myDictInt32Bool, i);
        r.ReadStatus(myDictInt32Bool, i, isNull, isChanged);
        std::cout<<"    myDictInt32Bool["<<key<<"] - "<<std::boolalpha<<"isNull: "<<isNull<<", isChanged: "<<isChanged<<std::endl;
        CHECK(!isChanged)
    }

    std::cout<<"myDictStringString - changedTopLevel: "<<r.IsChangedTopLevel(myDictStringString)<<std::endl;
    CHECK(!r.IsChangedTopLevel(myDictStringString))
    for (int i=0; i<r.NumberOfValues(myDictStringString); ++i)
    {
        std::string key=r.ReadKey<const char*>(myDictStringString, i);
        r.ReadStatus(myDictStringString, i, isNull, isChanged);
        std::cout<<"    myDictStringString["<<key<<"] - "<<std::boolalpha<<"isNull: "<<isNull<<", isChanged: "<<isChanged<<std::endl;
        CHECK(!isChanged)
    }


    std::cout<<"========= Blob Diff Test Done========"<<std::endl;
}

void BlobChangeTest(RepositoryPtr rep)
{
    std::cout<<std::boolalpha<<std::endl;
    std::cout<<"========= Blob Change Test ========"<<std::endl;
    //BlobTest.MyItem
    DotsC_TypeId tid=Safir::Dob::Typesystem::ToolSupport::TypeUtilities::CalculateTypeId("BlobTest.MySpecialItem");
    DotsC_TypeId collectionTid=Safir::Dob::Typesystem::ToolSupport::TypeUtilities::CalculateTypeId("BlobTest.MyCollections");
    std::cout<<"Class BlobTest.MySpecialItem "<<tid<<std::endl;

    const ClassDescription* cd=rep->GetClass(tid);
    DotsC_MemberIndex myNum=cd->GetMemberIndex("MyNumber");
    DotsC_MemberIndex myStrings=cd->GetMemberIndex("MyStrings");
    DotsC_MemberIndex myChild=cd->GetMemberIndex("Child");
    DotsC_MemberIndex myChildName=cd->GetMemberIndex("ChildName");
    DotsC_MemberIndex myDictStringInt32=cd->GetMemberIndex("DictStringInt32");
    DotsC_MemberIndex myDictAdvanced=cd->GetMemberIndex("DictAdvanced");
    const ClassDescription* collDesc=rep->GetClass(collectionTid);
    DotsC_MemberIndex mySeqInt32=collDesc->GetMemberIndex("SeqInt32");
    DotsC_MemberIndex mySeqObject=collDesc->GetMemberIndex("SeqObject");
    DotsC_MemberIndex myDictEntityIdObject=collDesc->GetMemberIndex("DictEntityIdObject");

    BlobWriter<Safir::Dob::Typesystem::ToolSupport::TypeRepository> w(rep.get(), tid);
    w.WriteValue(myNum, 0, 123, false, true);
    w.WriteValue(myStrings, 0, "Hej_1", false, true);
    w.WriteValue(myStrings, 3, "Hej_2", false, true);
    w.WriteValue(myStrings, 4, "Hej_3", false, true);
    w.WriteValue(myChildName, 0, "Svarre", false, true);
    w.WriteKey(myDictStringInt32, "first");
    w.WriteValue(myDictStringInt32, 0, 1, false, false);
    w.WriteKey(myDictStringInt32, "second");
    w.WriteValue(myDictStringInt32, 0, 2, true, true);
    w.WriteKey(myDictStringInt32, "third");
    w.WriteValue(myDictStringInt32, 0, 3, false, false);
    {
        BlobWriter<Safir::Dob::Typesystem::ToolSupport::TypeRepository> w2(rep.get(), tid);
        w2.WriteValue(myNum, 0, 456, false, true);
        w2.WriteValue(myStrings, 0, "In0", false, true);
        w2.WriteValue(myStrings, 3, "In3", false, true);
        w2.WriteValue(myStrings, 4, "In4", false, true);
        w.WriteValue(myChild, 0, w2, false, true);

        //Create a BlobTest.MyCollections object
        BlobWriter<Safir::Dob::Typesystem::ToolSupport::TypeRepository> wc(rep.get(), collectionTid);
        wc.WriteValue(mySeqInt32, 0, 100, false, false);
        wc.WriteValue(mySeqInt32, 0, 200, false, false);
        wc.WriteValue(mySeqObject, 0, w2, false, false);

        DotsC_EntityId eid={tid, 1};
        wc.WriteKey(myDictEntityIdObject, std::pair<DotsC_EntityId, const char*>(eid, NULL));
        wc.WriteValue(myDictEntityIdObject, 0, w2, false, false);

        w.WriteKey(myDictAdvanced, 0);
        w.WriteValue(myDictAdvanced, 0, wc, false, false);

        w.WriteKey(myDictAdvanced, 1);
        w.WriteValue(myDictAdvanced, 0, wc, false, false);
    }

    std::vector<char> blob(static_cast<size_t>(w.CalculateBlobSize()));
    w.CopyRawBlob(&blob[0]);
    std::cout<<"Blob created, with size "<<blob.size()<<std::endl;
    std::cout<<"BlobSize = "<<BlobReader<TypeRepository>::GetSize(&blob[0])<<std::endl;
    std::cout<<"BlobTypeId = "<<BlobReader<TypeRepository>::GetTypeId(&blob[0])<<std::endl;

//    std::ostringstream json;
//    BinaryToJson(rep.get(), &blob[0], json);
//    std::cout<<"======== JSON acvanded object ==========\n"<<json.str()<<std::endl;

    BlobReader<Safir::Dob::Typesystem::ToolSupport::TypeRepository> r(rep.get(), &blob[0]);

    {
        //---------- Print Object --------------------------
        bool isNull=false, isChanged=true;
        DotsC_Int32 myNumVal=0;
        std::cout<<"----- "<<rep->GetClass(r.TypeId())->GetName()<<" -----"<<std::endl;
        r.ReadValue(myNum, 0, myNumVal, isNull, isChanged);
        std::cout<<"- myNum isNull="<<isNull<<", isChanged="<<isChanged<<", val="<<myNumVal<<std::endl;

        const char* str=NULL;
        std::cout<<"- myStrings array"<<std::endl;
        for (DotsC_Int32 i=0; i<5; ++i)
        {
            r.ReadValue(myStrings, i, str, isNull, isChanged);
            std::cout<<"    isNull="<<isNull<<", isChanged="<<isChanged;
            if (isNull)
                std::cout<<std::endl;
            else
                std::cout<<", val="<<str<<std::endl;
        }

        std::cout<<"- myDictStringInt32, topChanged="<<r.IsChangedTopLevel(myDictStringInt32)<<std::endl;
        for (int i=0; i<r.NumberOfValues(myDictStringInt32); ++i)
        {
            DotsC_Int32 val=0;
            std::string key=r.ReadKey<const char*>(myDictStringInt32, i);
            r.ReadValue<DotsC_Int32>(myDictStringInt32, i, val, isNull, isChanged);
            std::cout<<"    key: "<<key<<" = {isNull="<<isNull<<", isChanged="<<isChanged;
            if (!isNull)
                std::cout<<", val="<<val;

            std::cout<<"}"<<std::endl;
        }

        r.ReadValue(myChildName, 0, str, isNull, isChanged);
        std::cout<<"- myChildName isNull="<<isNull<<", isChanged="<<isChanged<<", val="<<str<<std::endl;

        std::pair<const char*, DotsC_Int32> inner;
        r.ReadValue(myChild, 0, inner, isNull, isChanged);
        std::cout<<"- myChild isNull="<<isNull<<", isChanged="<<isChanged<<std::endl;
        BlobReader<Safir::Dob::Typesystem::ToolSupport::TypeRepository> innerReader(rep.get(), inner.first);
        innerReader.ReadValue(myNum, 0, myNumVal, isNull, isChanged);
        std::cout<<"  - myNum, isNull="<<isNull<<", isChanged="<<isChanged<<", val="<<myNumVal<<std::endl;
        std::cout<<"  - myStrings array"<<std::endl;
        for (DotsC_Int32 i=0; i<5; ++i)
        {
            innerReader.ReadValue(myStrings, i, str, isNull, isChanged);
            std::cout<<"      isNull="<<isNull<<", isChanged="<<isChanged;
            if (isNull)
                std::cout<<std::endl;
            else
                std::cout<<", val="<<str<<std::endl;
        }

        std::cout<<"- DictAdvanced, topChanged="<<r.IsChangedTopLevel(myDictAdvanced)<<std::endl;
        for (int i=0; i<r.NumberOfValues(myDictAdvanced); ++i)
        {
            int key=r.ReadKey<DotsC_Int32>(myDictAdvanced, i);
            std::pair<const char*, DotsC_Int32> obj;
            r.ReadValue(myDictAdvanced, i, obj, isNull, isChanged);
            std::cout<<"    key: "<<key<<" = {isNull="<<isNull<<", isChanged="<<isChanged<<"}"<<std::endl;
            if (!isNull)
            {
                BlobReader<Safir::Dob::Typesystem::ToolSupport::TypeRepository> wc(rep.get(), obj.first);
                wc.ReadStatus(mySeqInt32, 0, isNull, isChanged);
                std::cout<<"      - MyCollections.SeqInt32 isNull="<<isNull<<", isChanged="<<isChanged<<std::endl;
                for (int j=0; j<wc.NumberOfValues(mySeqInt32); ++j)
                {
                    DotsC_Int32 val=0;
                    wc.ReadValue(mySeqInt32, j, val, isNull, isChanged);
                    std::cout<<"          val["<<j<<"] = "<<val<<std::endl;
                }

                std::cout<<"      - MyCollections.SeqObject, topChanged="<<wc.IsChangedTopLevel(mySeqObject)<<std::endl;
                for (int j=0; j<wc.NumberOfValues(mySeqObject); ++j)
                {
                    std::pair<const char*, DotsC_Int32> val;
                    wc.ReadValue(mySeqObject, j, val, isNull, isChanged);
                    BlobReader<Safir::Dob::Typesystem::ToolSupport::TypeRepository> wcInnerObj(rep.get(), val.first);
                    DotsC_Int32 numVal=0;
                    wcInnerObj.ReadValue(myNum, 0, numVal, isNull, isChanged);
                    std::cout<<"        - MyCollections.SeqObject.MyNum isNull="<<isNull<<", isChanged="<<isChanged<<", val="<<numVal<<std::endl;
                    std::cout<<"        - MyCollections.SeqObject.MyStrings"<<std::endl;
                    for (DotsC_Int32 strIx=0; strIx<5; ++strIx)
                    {
                        innerReader.ReadValue(myStrings, strIx, str, isNull, isChanged);
                        std::cout<<"            isNull="<<isNull<<", isChanged="<<isChanged;
                        if (isNull)
                            std::cout<<std::endl;
                        else
                            std::cout<<", val="<<str<<std::endl;
                    }
                }

                std::cout<<"    - MyCollections.DictEntityIdObject, topChanged="<<wc.IsChangedTopLevel(myDictEntityIdObject)<<std::endl;
                for (int j=0; j<wc.NumberOfValues(myDictEntityIdObject); ++j)
                {
                    std::pair<DotsC_EntityId, const char*> key=wc.ReadKey< std::pair<DotsC_EntityId, const char*> >(myDictEntityIdObject, j);
                    std::pair<const char*, DotsC_Int32> val;
                    wc.ReadValue(myDictEntityIdObject, j, val, isNull, isChanged);
                    std::cout<<"        key: {"<<rep->GetClass(key.first.typeId)->GetName()<<", "<<key.first.instanceId<<
                               "}, isNull="<<isNull<<", isChanged="<<isChanged<<std::endl;

                    BlobReader<Safir::Dob::Typesystem::ToolSupport::TypeRepository> wcInnerObj(rep.get(), val.first);
                    DotsC_Int32 numVal=0;
                    wcInnerObj.ReadValue(myNum, 0, numVal, isNull, isChanged);
                    std::cout<<"          - MyCollections.DictEntityIdObject.MyNum isNull="<<isNull<<", isChanged="<<isChanged<<", val="<<numVal<<std::endl;
                    std::cout<<"          - MyCollections.DictEntityIdObject.MyStrings"<<std::endl;
                    for (DotsC_Int32 strIx=0; strIx<5; ++strIx)
                    {
                        innerReader.ReadValue(myStrings, strIx, str, isNull, isChanged);
                        std::cout<<"              isNull="<<isNull<<", isChanged="<<isChanged;
                        if (isNull)
                            std::cout<<std::endl;
                        else
                            std::cout<<", val="<<str<<std::endl;
                    }
                }
            }
        }
    }

    std::cout<<"--- SetAllChanged"<<std::endl;
    BlobWriter<Safir::Dob::Typesystem::ToolSupport::TypeRepository> w3(r);
    w3.SetAllChangeFlags(true);

    {
        //---------- Print Object --------------------------
        bool isNull=false, isChanged=true;
        DotsC_Int32 myNumVal=0;
        std::cout<<"----- "<<rep->GetClass(r.TypeId())->GetName()<<" -----"<<std::endl;
        r.ReadValue(myNum, 0, myNumVal, isNull, isChanged);
        if (!isChanged) throw std::logic_error(std::string("Change flag error, line: ")+boost::lexical_cast<std::string>(__LINE__));
        std::cout<<"- myNum isNull="<<isNull<<", isChanged="<<isChanged<<", val="<<myNumVal<<std::endl;

        const char* str=NULL;
        std::cout<<"- myStrings array"<<std::endl;
        for (DotsC_Int32 i=0; i<5; ++i)
        {
            r.ReadValue(myStrings, i, str, isNull, isChanged);
            if (!isChanged) throw std::logic_error(std::string("Change flag error, line: ")+boost::lexical_cast<std::string>(__LINE__));
            std::cout<<"    isNull="<<isNull<<", isChanged="<<isChanged;
            if (isNull)
                std::cout<<std::endl;
            else
                std::cout<<", val="<<str<<std::endl;
        }

        std::cout<<"- myDictStringInt32, topChanged="<<r.IsChangedTopLevel(myDictStringInt32)<<std::endl;
        for (int i=0; i<r.NumberOfValues(myDictStringInt32); ++i)
        {
            DotsC_Int32 val=0;
            std::string key=r.ReadKey<const char*>(myDictStringInt32, i);
            r.ReadValue<DotsC_Int32>(myDictStringInt32, i, val, isNull, isChanged);
            if (!isChanged) throw std::logic_error(std::string("Change flag error, line: ")+boost::lexical_cast<std::string>(__LINE__));
            std::cout<<"    key: "<<key<<" = {isNull="<<isNull<<", isChanged="<<isChanged;
            if (!isNull)
                std::cout<<", val="<<val;

            std::cout<<"}"<<std::endl;
        }

        r.ReadValue(myChildName, 0, str, isNull, isChanged);
        if (!isChanged) throw std::logic_error(std::string("Change flag error, line: ")+boost::lexical_cast<std::string>(__LINE__));
        std::cout<<"- myChildName isNull="<<isNull<<", isChanged="<<isChanged<<", val="<<str<<std::endl;

        std::pair<const char*, DotsC_Int32> inner;
        r.ReadValue(myChild, 0, inner, isNull, isChanged);
        if (!isChanged) throw std::logic_error(std::string("Change flag error, line: ")+boost::lexical_cast<std::string>(__LINE__));
        std::cout<<"- myChild isNull="<<isNull<<", isChanged="<<isChanged<<std::endl;
        BlobReader<Safir::Dob::Typesystem::ToolSupport::TypeRepository> innerReader(rep.get(), inner.first);
        innerReader.ReadValue(myNum, 0, myNumVal, isNull, isChanged);
        if (!isChanged) throw std::logic_error(std::string("Change flag error, line: ")+boost::lexical_cast<std::string>(__LINE__));
        std::cout<<"  - myNum, isNull="<<isNull<<", isChanged="<<isChanged<<", val="<<myNumVal<<std::endl;
        std::cout<<"  - myStrings array"<<std::endl;
        for (DotsC_Int32 i=0; i<5; ++i)
        {
            innerReader.ReadValue(myStrings, i, str, isNull, isChanged);
            if (!isChanged) throw std::logic_error(std::string("Change flag error, line: ")+boost::lexical_cast<std::string>(__LINE__));
            std::cout<<"      isNull="<<isNull<<", isChanged="<<isChanged;
            if (isNull)
                std::cout<<std::endl;
            else
                std::cout<<", val="<<str<<std::endl;
        }

        std::cout<<"- DictAdvanced, topChanged="<<r.IsChangedTopLevel(myDictAdvanced)<<std::endl;
        for (int i=0; i<r.NumberOfValues(myDictAdvanced); ++i)
        {
            int key=r.ReadKey<DotsC_Int32>(myDictAdvanced, i);
            std::pair<const char*, DotsC_Int32> obj;
            r.ReadValue(myDictAdvanced, i, obj, isNull, isChanged);
            if (!isChanged) throw std::logic_error(std::string("Change flag error, line: ")+boost::lexical_cast<std::string>(__LINE__));
            std::cout<<"    key: "<<key<<" = {isNull="<<isNull<<", isChanged="<<isChanged<<"}"<<std::endl;
            if (!isNull)
            {
                BlobReader<Safir::Dob::Typesystem::ToolSupport::TypeRepository> wc(rep.get(), obj.first);
                if (!wc.IsChangedTopLevel(mySeqInt32)) throw std::logic_error(std::string("Change flag error, line: ")+boost::lexical_cast<std::string>(__LINE__));
                std::cout<<"      - MyCollections.SeqInt32 topChanged="<<r.IsChangedTopLevel(mySeqInt32)<<std::endl;
                for (int j=0; j<wc.NumberOfValues(mySeqInt32); ++j)
                {
                    DotsC_Int32 val=0;
                    wc.ReadValue(mySeqInt32, j, val, isNull, isChanged);
                    std::cout<<"          val["<<j<<"] = "<<val<<std::endl;
                }

                if (!wc.IsChangedTopLevel(mySeqObject)) throw std::logic_error(std::string("Change flag error, line: ")+boost::lexical_cast<std::string>(__LINE__));
                std::cout<<"      - MyCollections.SeqObject, topChanged="<<wc.IsChangedTopLevel(mySeqObject)<<std::endl;
                for (int j=0; j<wc.NumberOfValues(mySeqObject); ++j)
                {
                    std::pair<const char*, DotsC_Int32> val;
                    wc.ReadValue(mySeqObject, j, val, isNull, isChanged);
                    BlobReader<Safir::Dob::Typesystem::ToolSupport::TypeRepository> wcInnerObj(rep.get(), val.first);
                    DotsC_Int32 numVal=0;
                    wcInnerObj.ReadValue(myNum, 0, numVal, isNull, isChanged);
                    if (!isChanged) throw std::logic_error(std::string("Change flag error, line: ")+boost::lexical_cast<std::string>(__LINE__));
                    std::cout<<"        - MyCollections.SeqObject.MyNum isNull="<<isNull<<", isChanged="<<isChanged<<", val="<<numVal<<std::endl;
                    std::cout<<"        - MyCollections.SeqObject.MyStrings"<<std::endl;
                    for (DotsC_Int32 strIx=0; strIx<5; ++strIx)
                    {
                        innerReader.ReadValue(myStrings, strIx, str, isNull, isChanged);
                        if (!isChanged) throw std::logic_error(std::string("Change flag error, line: ")+boost::lexical_cast<std::string>(__LINE__));
                        std::cout<<"            isNull="<<isNull<<", isChanged="<<isChanged;
                        if (isNull)
                            std::cout<<std::endl;
                        else
                            std::cout<<", val="<<str<<std::endl;
                    }
                }

                if (!wc.IsChangedTopLevel(myDictEntityIdObject)) throw std::logic_error(std::string("Change flag error, line: ")+boost::lexical_cast<std::string>(__LINE__));
                std::cout<<"    - MyCollections.DictEntityIdObject, topChanged="<<wc.IsChangedTopLevel(myDictEntityIdObject)<<std::endl;
                for (int j=0; j<wc.NumberOfValues(myDictEntityIdObject); ++j)
                {
                    std::pair<DotsC_EntityId, const char*> key=wc.ReadKey< std::pair<DotsC_EntityId, const char*> >(myDictEntityIdObject, j);
                    std::pair<const char*, DotsC_Int32> val;
                    wc.ReadValue(myDictEntityIdObject, j, val, isNull, isChanged);
                    if (!isChanged) throw std::logic_error(std::string("Change flag error, line: ")+boost::lexical_cast<std::string>(__LINE__));
                    std::cout<<"        key: {"<<rep->GetClass(key.first.typeId)->GetName()<<", "<<key.first.instanceId<<
                               "}, isNull="<<isNull<<", isChanged="<<isChanged<<std::endl;

                    BlobReader<Safir::Dob::Typesystem::ToolSupport::TypeRepository> wcInnerObj(rep.get(), val.first);
                    DotsC_Int32 numVal=0;
                    wcInnerObj.ReadValue(myNum, 0, numVal, isNull, isChanged);
                    if (!isChanged) throw std::logic_error(std::string("Change flag error, line: ")+boost::lexical_cast<std::string>(__LINE__));
                    std::cout<<"          - MyCollections.DictEntityIdObject.MyNum isNull="<<isNull<<", isChanged="<<isChanged<<", val="<<numVal<<std::endl;
                    std::cout<<"          - MyCollections.DictEntityIdObject.MyStrings"<<std::endl;
                    for (DotsC_Int32 strIx=0; strIx<5; ++strIx)
                    {
                        innerReader.ReadValue(myStrings, strIx, str, isNull, isChanged);
                        if (!isChanged) throw std::logic_error(std::string("Change flag error, line: ")+boost::lexical_cast<std::string>(__LINE__));
                        std::cout<<"              isNull="<<isNull<<", isChanged="<<isChanged;
                        if (isNull)
                            std::cout<<std::endl;
                        else
                            std::cout<<", val="<<str<<std::endl;
                    }
                }
            }
        }
    }

    std::cout<<"========= Blob Change Test Done ========"<<std::endl;
}

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
        std::cout<<"Too few arguments!"<<std::endl;
        return 1;
    }

    RepositoryPtr repository;
    try
    {
        repository=Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(douDir);

    }
    catch (const Safir::Dob::Typesystem::ToolSupport::ParseError& err)
    {
        std::cout<<err.what()<<std::endl;
        return 1;
    }

    BlobChangeTest(repository);
    BlobDiffTest(repository);
    BlobArraySizeDiff(repository);

    std::cout<<"========= Repository ========"<<std::endl;

    Safir::Dob::Typesystem::ToolSupport::RepositoryToString(repository.get(), true, std::cout);

    std::cout<<"========= Test suite started ========"<<std::endl;

    boost::chrono::high_resolution_clock::time_point startTime=boost::chrono::high_resolution_clock::now();
    try
    {
        RunTests(repository.get(), testDir, 0, 10000);
    }
    catch (const std::logic_error& err)
    {
        std::cout<<err.what()<<std::endl;
        return 1;
    }
    catch(...)
    {
        std::cout<<"Test failed with unexpected exception!"<<std::endl;
        return 1; //Failed
    }

    boost::chrono::high_resolution_clock::duration elapsed=boost::chrono::high_resolution_clock::now()-startTime;
    boost::chrono::milliseconds millis=boost::chrono::duration_cast<boost::chrono::milliseconds>(elapsed);
    std::cout<<"Elapsed time: "<<millis.count()<<std::endl;


    std::cout<<"====================================="<<std::endl;
    return 0;
}

//--------------
const ClassDescription* GetClassByName(const RepositoryPtr& rep, const std::string& name)
{
    std::set<DotsC_TypeId> v;
    rep->GetAllClassTypeIds(v);
    for (std::set<DotsC_TypeId>::const_iterator it=v.begin(); it!=v.end(); ++it)
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
        else
        {
            throw std::logic_error("Dots Serializaton Test only handles xml files");
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
    catch (const std::logic_error& err)
    {
        PrintTestFailMessage("Test failed", "", err.what(), test.path.string());
        return false;
    }
    catch (...)
    {
        throw Safir::Dob::Typesystem::ToolSupport::ParseError("Unexpected error", "An unhandled exception occured in test.", test.path.string(), -2);
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
    Safir::Dob::Typesystem::ToolSupport::XmlToBinary(repository, xml1.str().c_str(), bin1);
    CheckBlobSize(bin1);

    std::ostringstream os1;
    Safir::Dob::Typesystem::ToolSupport::BinaryToBase64(&bin1[0], bin1.size(), os1);

    std::vector<char> bin2;
    Safir::Dob::Typesystem::ToolSupport::Base64ToBinary(os1.str(), bin2);
    CheckBlobSize(bin2);

    if (bin1.size()!=bin2.size() || memcmp(&bin1[0], &bin2[0], bin1.size())!=0)
    {
        throw std::logic_error("Not binary equal after base64 conversion");
    }

    std::ostringstream xml2;
    BinaryToXml(repository, &bin2[0], xml2);
    std::cout<<"---- XML2 ---"<<std::endl<<xml2.str()<<std::endl;

    std::vector<char> bin3;
    Safir::Dob::Typesystem::ToolSupport::XmlToBinary(repository, xml2.str().c_str(), bin3);
    CheckBlobSize(bin3);

    if (bin1.size()!=bin3.size() || memcmp(&bin1[0], &bin3[0], bin1.size())!=0)
    {
        throw std::logic_error("Not binary equal after xml conversion");
    }



    std::ostringstream json1;
    BinaryToJson(repository, &bin1[0], json1);

    std::cout<<"--- JSON 1 ---"<<std::endl<<json1.str()<<std::endl;

    std::vector<char> bin4;
    JsonToBinary(repository, json1.str().c_str(), bin4);
    CheckBlobSize(bin4);
    if (bin1.size()!=bin4.size() || memcmp(&bin1[0], &bin4[0], bin1.size())!=0)
    {
        std::ostringstream dummyJson;
        BinaryToJson(repository, &bin4[0], dummyJson);
        std::cout<<"--- JSON DUMMY ---"<<std::endl<<dummyJson.str()<<std::endl;

        throw std::logic_error("Not binary equal after json conversion");
    }

    std::ostringstream json2;
    BinaryToJson(repository, &bin4[0], json2);
    std::cout<<"--- JSON 2 ---"<<std::endl<<json2.str()<<std::endl;

    if (json1.str()!=json2.str())
    {
        throw std::logic_error("JSON 1 differs from JSON 2");
    }

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
