/******************************************************************************
*
* Copyright Saab AB, 2016 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/StateDeleter.h>
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/Internal/TimestampOperations.h>
#include <DoseTest/InjectableEntity4.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Dob/Typesystem/Members.h>

#define BOOST_TEST_MODULE TimestampOperationsTest
#include <boost/test/unit_test.hpp>

using namespace DoseTest;
using namespace Safir;
using namespace Safir::Dob;
using namespace Safir::Dob::Internal;
using namespace Safir::Dob::Typesystem;
using namespace Safir::Dob::Typesystem::Internal;


DistributionData CreateDistributionData(InjectableEntity4Ptr object)
{
    //Ensure that we're initialized
    InjectionKindTable::Initialize();

    BinarySerialization bin;
    Serialization::ToBinary(object,bin);

#ifdef TEST_AGAINST_CORE_5
    static LamportClock clock;

    DistributionData dd(entity_state_tag,
                        ConnectionId(100,0,100),
                        InjectableEntity4::ClassTypeId,
                        HandlerId(),
                        clock.GetNewTimestamp(),
                        InstanceId(1),
                        clock.GetNewTimestamp(),
                        DistributionData::Real,
                        false,
                        false,
                        &bin[0]);
#else
    static LamportClock clock(0);
    BlobWriteHelper helper(&bin[0]);

    DistributionData dd(entity_state_tag,
                        ConnectionId(100,0,100),
                        InjectableEntity4::ClassTypeId,
                        HandlerId(),
                        clock.GetNewTimestamp(),
                        InstanceId(1),
                        clock.GetNewTimestamp(),
                        DistributionData::Real,
                        false,
                        false,
                        helper);
#endif

    return dd;
}

BOOST_AUTO_TEST_CASE(timestamp_initialization)
{
    auto dd = CreateDistributionData(InjectableEntity4::Create());

    BOOST_CHECK_EQUAL(dd.GetTopTimestamp(),0);
    BOOST_CHECK(dd.GetMemberTimestamps() != NULL);
    for (int i = 0; i < Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId); ++i)
    {
        BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],0);
    }
    //silly extra check
    BOOST_CHECK_EQUAL(Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId),20);

    //check that beyond the timestamps there is a blob, the beginning of which is very unlikely to be 0
    BOOST_CHECK_NE(dd.GetMemberTimestamps()[Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId) + 1],0);
}

BOOST_AUTO_TEST_CASE(SetTimestampForAllMembers)
{
    auto dd = CreateDistributionData(InjectableEntity4::Create());

    TimestampOperations::SetTimestampForAllMembers(dd, 100);

    BOOST_CHECK_EQUAL(dd.GetTopTimestamp(),100);
    BOOST_CHECK(dd.GetMemberTimestamps() != NULL);
    for (int i = 0; i < Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId); ++i)
    {
        BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],100);
    }
}


BOOST_AUTO_TEST_CASE(SetTimestampForChangedMembers_Simple)
{
    InjectableEntity4Ptr object = InjectableEntity4::Create();
    object->Int32Member() = 10;
    object->ItemMember1() = TestItem::Create();;

    auto dd = CreateDistributionData(object);

    TimestampOperations::SetTimestampForAllMembers(dd, 100);

    //without resetting timestamps
    TimestampOperations::SetTimestampForChangedMembers(dd, 200, false);

    BOOST_CHECK_EQUAL(dd.GetTopTimestamp(),200);
    BOOST_CHECK(dd.GetMemberTimestamps() != NULL);
    for (int i = 0; i < Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId); ++i)
    {
        if (i == 2 || i == 7)
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],200);
        }
        else
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],100);
        }
    }

    //with resetting timestamps
    TimestampOperations::SetTimestampForChangedMembers(dd, 300, true);

    BOOST_CHECK_EQUAL(dd.GetTopTimestamp(),300);
    BOOST_CHECK(dd.GetMemberTimestamps() != NULL);
    for (int i = 0; i < Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId); ++i)
    {
        if (i == 2 || i == 7)
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],300);
        }
        else
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],0);
        }
    }

}

BOOST_AUTO_TEST_CASE(SetTimestampForChangedMembers_SimpleArray)
{
    InjectableEntity4Ptr object = InjectableEntity4::Create();
    object->ArrayMember()[1].SetVal(1000);

    auto dd = CreateDistributionData(object);

    TimestampOperations::SetTimestampForAllMembers(dd, 100);

    //without resetting timestamps
    TimestampOperations::SetTimestampForChangedMembers(dd, 200, false);

    BOOST_CHECK_EQUAL(dd.GetTopTimestamp(),200);
    BOOST_CHECK(dd.GetMemberTimestamps() != NULL);
    for (int i = 0; i < Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId); ++i)
    {
        if (i == 4)
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],200);
        }
        else
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],100);
        }
    }

    //with resetting timestamps
    TimestampOperations::SetTimestampForChangedMembers(dd, 300, true);

    BOOST_CHECK_EQUAL(dd.GetTopTimestamp(),300);
    BOOST_CHECK(dd.GetMemberTimestamps() != NULL);
    for (int i = 0; i < Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId); ++i)
    {
        if (i == 4)
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],300);
        }
        else
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],0);
        }
    }
}


#ifndef TEST_AGAINST_CORE_5
BOOST_AUTO_TEST_CASE(SetTimestampForChangedMembers_SimpleNewCollections)
{
    InjectableEntity4Ptr object = InjectableEntity4::Create();
    object->SequenceMember().push_back(10);
    object->DictionaryMember().Insert(10,10);

    auto dd = CreateDistributionData(object);

    TimestampOperations::SetTimestampForAllMembers(dd, 100);

    //without resetting timestamps
    TimestampOperations::SetTimestampForChangedMembers(dd, 200, false);

    BOOST_CHECK_EQUAL(dd.GetTopTimestamp(),200);
    BOOST_CHECK(dd.GetMemberTimestamps() != NULL);
    for (int i = 0; i < Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId); ++i)
    {
        if (i == 5 || i == 6)
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],200);
        }
        else
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],100);
        }
    }

    //with resetting timestamps
    TimestampOperations::SetTimestampForChangedMembers(dd, 300, true);

    BOOST_CHECK_EQUAL(dd.GetTopTimestamp(),300);
    BOOST_CHECK(dd.GetMemberTimestamps() != NULL);
    for (int i = 0; i < Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId); ++i)
    {
        if (i == 5 || i == 6)
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],300);
        }
        else
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],0);
        }
    }

}
#endif

BOOST_AUTO_TEST_CASE(SetTimestampForChangedMembers_Items)
{
    InjectableEntity4Ptr object = InjectableEntity4::Create();
    object->ItemMember1() = DoseTest::TestItem::Create();
    object->ItemMember1()->MyInt() = 10;

    object->ItemMember2() = DoseTest::TestItem::Create();
    object->ItemMember2()->MyInt() = 20;
    object->ItemMember2().SetChangedHere(false);

    object->ItemMember3() = DoseTest::TestItem::Create();
    object->ItemMember3()->MyInt() = 30;
    object->ItemMember3()->MyInt().SetChanged(false);

    auto dd = CreateDistributionData(object);
    TimestampOperations::SetTimestampForChangedMembers(dd,1000,false);

    BOOST_CHECK_EQUAL(dd.GetTopTimestamp(),1000);
    for (int i = 0; i < Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId); ++i)
    {
        //std::wcout << "Checking member " << i << std::endl;
        if (i == 7 || i == 8 || i == 9)
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],1000);
        }
        else
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],0);
        }
    }
}


BOOST_AUTO_TEST_CASE(SetTimestampForChangedMembers_ItemArray)
{
    InjectableEntity4Ptr object = InjectableEntity4::Create();
    object->ItemArrayMember1()[0].SetPtr(DoseTest::TestItem::Create());
    object->ItemArrayMember1()[0]->MyInt() = 10;

    object->ItemArrayMember2()[0].SetPtr(DoseTest::TestItem::Create());
    object->ItemArrayMember2()[0]->MyInt() = 20;
    object->ItemArrayMember2()[0].SetChangedHere(false);

    object->ItemArrayMember3()[0].SetPtr(DoseTest::TestItem::Create());
    object->ItemArrayMember3()[0]->MyInt() = 30;
    object->ItemArrayMember3()[0]->MyInt().SetChanged(false);

    auto dd = CreateDistributionData(object);
    TimestampOperations::SetTimestampForChangedMembers(dd,1000,false);

    BOOST_CHECK_EQUAL(dd.GetTopTimestamp(),1000);
    for (int i = 0; i < Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId); ++i)
    {
        //std::wcout << "Checking member " << i << std::endl;
        if (i == 10 || i == 11 || i == 12)
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],1000);
        }
        else
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],0);
        }
    }
}

#ifndef TEST_AGAINST_CORE_5
BOOST_AUTO_TEST_CASE(SetTimestampForChangedMembers_ItemNewCollection)
{
    InjectableEntity4Ptr object = InjectableEntity4::Create();
    object->ItemSequenceMember1().push_back(DoseTest::TestItem::Create());

    object->ItemSequenceMember2().push_back(DoseTest::TestItem::Create());
    object->ItemSequenceMember2().at(0)->MyInt() = 20;
    object->ItemSequenceMember2().SetChangedHere(false);

    object->ItemSequenceMember3().push_back(DoseTest::TestItem::Create());
    object->ItemSequenceMember3().at(0)->MyInt() = 30;
    object->ItemSequenceMember3().at(0)->MyInt().SetChanged(false);

    object->ItemDictionaryMember1().Insert(0,DoseTest::TestItem::Create());
    object->ItemDictionaryMember1().at(0)->MyInt() = 20;
    object->ItemDictionaryMember1().SetChanged(false);
    object->ItemDictionaryMember1().SetChangedHere(true);

    object->ItemDictionaryMember2().Insert(0,DoseTest::TestItem::Create());
    object->ItemDictionaryMember2().at(0)->MyInt() = 20;
    object->ItemDictionaryMember2().SetChanged(false);
    object->ItemDictionaryMember2().at(0).SetChangedHere(true);

    object->ItemDictionaryMember3().Insert(0,DoseTest::TestItem::Create());
    object->ItemDictionaryMember3().SetChanged(false);
    object->ItemDictionaryMember3().at(0)->MyInt() = 20;

    object->ItemDictionaryMember4().Insert(0,DoseTest::TestItem::Create());
    object->ItemDictionaryMember4().at(0)->MyInt() = 20;

    auto dd = CreateDistributionData(object);
    TimestampOperations::SetTimestampForChangedMembers(dd,1000,false);

    BOOST_CHECK_EQUAL(dd.GetTopTimestamp(),1000);
    for (int i = 0; i < Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId); ++i)
    {
        //std::wcout << "Checking member " << i << std::endl;
        if (i >= 13 && i <= 19)
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],1000);
        }
        else
        {
            BOOST_CHECK_EQUAL(dd.GetMemberTimestamps()[i],0);
        }
    }

}

#endif
//HaveChanges never looks into any of the members, so we don't
//need to check new types. It only looks at member timestamps
BOOST_AUTO_TEST_CASE(HaveChanges)
{
    BOOST_CHECK(TimestampOperations::HaveChanges
                (CreateDistributionData(InjectableEntity4::Create()),
                 CreateDistributionData(InjectableEntity4::Create())));

    InjectableEntity4Ptr object = InjectableEntity4::Create();
    object->Int32Member() = 10;
    object->ItemMember1() = TestItem::Create();;

    //timestamps are still 0, so no we think we have all changes
    BOOST_CHECK(TimestampOperations::HaveChanges
                (CreateDistributionData(object),
                 CreateDistributionData(InjectableEntity4::Create())));

    auto dd1 = CreateDistributionData(object);
    TimestampOperations::SetTimestampForChangedMembers(dd1,100,false);
    BOOST_CHECK(TimestampOperations::HaveChanges
                (dd1,
                 CreateDistributionData(InjectableEntity4::Create())));
    BOOST_CHECK(!TimestampOperations::HaveChanges
                (CreateDistributionData(InjectableEntity4::Create()),
                 dd1));

    auto dd2 = CreateDistributionData(object);
    TimestampOperations::SetTimestampForChangedMembers(dd2,200,false);
    BOOST_CHECK(!TimestampOperations::HaveChanges(dd1,dd2));
    BOOST_CHECK(TimestampOperations::HaveChanges(dd2,dd1));

    object->SetChanged(false);
    object->Int64Member() = 10L;
    auto dd3 = CreateDistributionData(object);
    TimestampOperations::SetTimestampForChangedMembers(dd3,300,false);
    BOOST_CHECK(!TimestampOperations::HaveChanges(dd1,dd3));
    BOOST_CHECK(!TimestampOperations::HaveChanges(dd3,dd1));
    BOOST_CHECK(!TimestampOperations::HaveChanges(dd2,dd3));
    BOOST_CHECK(!TimestampOperations::HaveChanges(dd3,dd2));
}


//This is where we really need to start looking into the members change flags.


BOOST_AUTO_TEST_CASE(Merge_Simple)
{
    const auto dd1 = CreateDistributionData(InjectableEntity4::Create());

    InjectableEntity4Ptr before = InjectableEntity4::Create();
    before->Int32Member() = 10;
    auto dd2 = CreateDistributionData(before);
    TimestampOperations::SetTimestampForChangedMembers(dd2,100,false);

    const auto res = TimestampOperations::Merge(dd1,dd2);

    BOOST_CHECK(res.second);
    auto after = boost::static_pointer_cast<InjectableEntity4>
        (ObjectFactory::Instance().CreateObject(res.first.GetBlob()));
    BOOST_CHECK(after->IsChanged());
    BOOST_CHECK(after->Int32Member().IsChanged());

    //ensure that only that change flag got set.
    after->Int32Member().SetChanged(false);
    BOOST_CHECK(!after->IsChanged());

    BOOST_CHECK_EQUAL(res.first.GetTopTimestamp(),100);
    BOOST_CHECK(res.first.GetMemberTimestamps() != NULL);
    for (int i = 0; i < Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId); ++i)
    {
        if (i == 2)
        {
            BOOST_CHECK_EQUAL(res.first.GetMemberTimestamps()[i],100);
        }
        else
        {
            BOOST_CHECK_EQUAL(res.first.GetMemberTimestamps()[i],0);
        }
    }


}

BOOST_AUTO_TEST_CASE(Merge_Items)
{
    const auto dd1 = CreateDistributionData(InjectableEntity4::Create());

    InjectableEntity4Ptr before = InjectableEntity4::Create();
    before->ItemMember1() = DoseTest::TestItem::Create();
    before->ItemMember1()->MyInt() = 10;

    before->ItemMember2() = DoseTest::TestItem::Create();
    before->ItemMember2()->MyInt() = 20;
    before->ItemMember2().SetChangedHere(false);

    before->ItemMember3() = DoseTest::TestItem::Create();
    before->ItemMember3()->MyInt() = 30;
    before->ItemMember3()->MyInt().SetChanged(false);

    BOOST_CHECK(before->ItemMember2().IsChanged());

    auto dd2 = CreateDistributionData(before);
    TimestampOperations::SetTimestampForChangedMembers(dd2,1000,false);

    const auto res = TimestampOperations::Merge(dd1,dd2);

    BOOST_CHECK(res.second);
    auto after = boost::static_pointer_cast<InjectableEntity4>
        (ObjectFactory::Instance().CreateObject(res.first.GetBlob()));

    BOOST_CHECK_EQUAL(res.first.GetTopTimestamp(),1000);
    for (int i = 0; i < Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId); ++i)
    {
        if (i == 7 || i == 8 || i == 9)
        {
            BOOST_CHECK_EQUAL(res.first.GetMemberTimestamps()[i],1000);
        }
        else
        {
            BOOST_CHECK_EQUAL(res.first.GetMemberTimestamps()[i],0);
        }
    }

    BOOST_CHECK(after->IsChanged());
    BOOST_CHECK(after->ItemMember1().IsChanged());
    BOOST_CHECK_EQUAL(after->ItemMember1()->MyInt(), 10);

    BOOST_CHECK(after->ItemMember2().IsChanged());
    BOOST_CHECK(!after->ItemMember2().IsChangedHere());
    BOOST_CHECK_EQUAL(after->ItemMember2()->MyInt(), 20);
    BOOST_CHECK(after->ItemMember2()->MyInt().IsChanged());

    BOOST_CHECK(after->ItemMember3().IsChanged());
    BOOST_CHECK(after->ItemMember3().IsChangedHere());
    BOOST_CHECK(!after->ItemMember3()->MyInt().IsChanged());
    BOOST_CHECK_EQUAL(after->ItemMember3()->MyInt(),30);

}

BOOST_AUTO_TEST_CASE(Merge_SimpleArrays)
{
    const auto dd1 = CreateDistributionData(InjectableEntity4::Create());

    InjectableEntity4Ptr before = InjectableEntity4::Create();
    before->ArrayMember()[2].SetVal(10);

    auto dd2 = CreateDistributionData(before);
    TimestampOperations::SetTimestampForChangedMembers(dd2,100,false);

    const auto res = TimestampOperations::Merge(dd1,dd2);

    BOOST_CHECK(res.second);
    auto after = boost::static_pointer_cast<InjectableEntity4>
        (ObjectFactory::Instance().CreateObject(res.first.GetBlob()));
    BOOST_CHECK(after->IsChanged());
    BOOST_CHECK(!after->Int32Member().IsChanged());

    BOOST_CHECK(after->ArrayMember().IsChanged());
    BOOST_CHECK(!after->ArrayMember()[0].IsChanged());
    BOOST_CHECK(after->ArrayMember()[1].IsNull());
    BOOST_CHECK(after->ArrayMember()[2].IsChanged());
    BOOST_CHECK_EQUAL(after->ArrayMember()[2].GetVal(), 10);


}

#ifndef TEST_AGAINST_CORE_5
BOOST_AUTO_TEST_CASE(Merge_SimpleNewCollections)
{
    const auto dd1 = CreateDistributionData(InjectableEntity4::Create());

    InjectableEntity4Ptr before = InjectableEntity4::Create();
    before->SequenceMember().push_back(10);
    before->DictionaryMember().Insert(20,100);
    before->DictionaryMember().Insert(30,100);
    before->DictionaryMember()[30].SetChanged(false);

    auto dd2 = CreateDistributionData(before);
    TimestampOperations::SetTimestampForChangedMembers(dd2,100,false);

    const auto res = TimestampOperations::Merge(dd1,dd2);

    BOOST_CHECK(res.second);
    auto after = boost::static_pointer_cast<InjectableEntity4>
        (ObjectFactory::Instance().CreateObject(res.first.GetBlob()));
    BOOST_CHECK(after->IsChanged());
    BOOST_CHECK(!after->Int32Member().IsChanged());

    BOOST_CHECK(after->SequenceMember().IsChanged());
    BOOST_CHECK_EQUAL(after->SequenceMember().size(),1);

    BOOST_CHECK(after->DictionaryMember().IsChanged());
    BOOST_CHECK(after->DictionaryMember()[20].IsChanged());
    BOOST_CHECK_EQUAL(after->DictionaryMember()[20].GetVal(), 100);
    BOOST_CHECK(!after->DictionaryMember()[30].IsChanged());
    BOOST_CHECK(after->DictionaryMember().IsChangedHere());
}
#endif

BOOST_AUTO_TEST_CASE(Merge_ObjectArrays)
{
    const auto dd1 = CreateDistributionData(InjectableEntity4::Create());

    InjectableEntity4Ptr before = InjectableEntity4::Create();
    before->ItemArrayMember1()[0].SetPtr(DoseTest::TestItem::Create());
    before->ItemArrayMember1()[0]->MyInt() = 10;

    before->ItemArrayMember2()[0].SetPtr(DoseTest::TestItem::Create());
    before->ItemArrayMember2()[0]->MyInt() = 20;
    before->ItemArrayMember2()[0].SetChangedHere(false);

    before->ItemArrayMember3()[0].SetPtr(DoseTest::TestItem::Create());
    before->ItemArrayMember3()[0]->MyInt() = 30;
    before->ItemArrayMember3()[0]->MyInt().SetChanged(false);

    BOOST_CHECK(before->ItemArrayMember2().IsChanged());

    auto dd2 = CreateDistributionData(before);
    TimestampOperations::SetTimestampForChangedMembers(dd2,1000,false);

    const auto res = TimestampOperations::Merge(dd1,dd2);

    BOOST_CHECK(res.second);
    auto after = boost::static_pointer_cast<InjectableEntity4>
        (ObjectFactory::Instance().CreateObject(res.first.GetBlob()));

    BOOST_CHECK_EQUAL(res.first.GetTopTimestamp(),1000);
    for (int i = 0; i < Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId); ++i)
    {
        if (i == 10 || i == 11 || i == 12)
        {
            BOOST_CHECK_EQUAL(res.first.GetMemberTimestamps()[i],1000);
        }
        else
        {
            BOOST_CHECK_EQUAL(res.first.GetMemberTimestamps()[i],0);
        }
    }

    BOOST_CHECK(after->IsChanged());
    BOOST_CHECK(after->ItemArrayMember1()[0].IsChanged());
    BOOST_CHECK_EQUAL(after->ItemArrayMember1()[0]->MyInt(), 10);

    BOOST_CHECK(after->ItemArrayMember2()[0].IsChanged());
    BOOST_CHECK(!after->ItemArrayMember2()[0].IsChangedHere());
    BOOST_CHECK_EQUAL(after->ItemArrayMember2()[0]->MyInt(), 20);
    BOOST_CHECK(after->ItemArrayMember2()[0]->MyInt().IsChanged());

    BOOST_CHECK(after->ItemArrayMember3()[0].IsChanged());
    BOOST_CHECK(after->ItemArrayMember3()[0].IsChangedHere());
    BOOST_CHECK(!after->ItemArrayMember3()[0]->MyInt().IsChanged());
    BOOST_CHECK_EQUAL(after->ItemArrayMember3()[0]->MyInt(),30);
}

#ifndef TEST_AGAINST_CORE_5
BOOST_AUTO_TEST_CASE(Merge_ObjectSequence)
{
    const auto dd1 = CreateDistributionData(InjectableEntity4::Create());

    InjectableEntity4Ptr before = InjectableEntity4::Create();
    before->ItemSequenceMember1().push_back(DoseTest::TestItem::Create());
    before->ItemSequenceMember1().at(0)->MyInt() = 10;

    before->ItemSequenceMember2().push_back(DoseTest::TestItem::Create());
    before->ItemSequenceMember2().at(0)->MyInt() = 20;
    before->ItemSequenceMember2().SetChangedHere(false);

    before->ItemSequenceMember3().push_back(DoseTest::TestItem::Create());
    before->ItemSequenceMember3().at(0)->MyInt() = 30;
    before->ItemSequenceMember3().at(0)->MyInt().SetChanged(false);

    BOOST_CHECK(before->ItemSequenceMember2().IsChanged());

    auto dd2 = CreateDistributionData(before);
    TimestampOperations::SetTimestampForChangedMembers(dd2,1000,false);

    const auto res = TimestampOperations::Merge(dd1,dd2);

    BOOST_CHECK(res.second);
    auto after = boost::static_pointer_cast<InjectableEntity4>
        (ObjectFactory::Instance().CreateObject(res.first.GetBlob()));

    BOOST_CHECK_EQUAL(res.first.GetTopTimestamp(),1000);
    for (int i = 0; i < Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId); ++i)
    {
        if (i == 13 || i == 14 || i == 15)
        {
            BOOST_CHECK_EQUAL(res.first.GetMemberTimestamps()[i],1000);
        }
        else
        {
            BOOST_CHECK_EQUAL(res.first.GetMemberTimestamps()[i],0);
        }
    }

    BOOST_CHECK(after->IsChanged());
    BOOST_CHECK(after->ItemSequenceMember1().IsChanged());
    BOOST_CHECK_EQUAL(after->ItemSequenceMember1().at(0)->MyInt(), 10);

    BOOST_CHECK(after->ItemSequenceMember2().IsChanged());
    BOOST_CHECK(!after->ItemSequenceMember2().IsChangedHere());
    BOOST_CHECK_EQUAL(after->ItemSequenceMember2().at(0)->MyInt(), 20);
    BOOST_CHECK(after->ItemSequenceMember2().at(0)->MyInt().IsChanged());

    BOOST_CHECK(after->ItemSequenceMember3().IsChanged());
    BOOST_CHECK(after->ItemSequenceMember3().IsChangedHere());
    BOOST_CHECK(!after->ItemSequenceMember3().at(0)->MyInt().IsChanged());
    BOOST_CHECK_EQUAL(after->ItemSequenceMember3().at(0)->MyInt(),30);
}

BOOST_AUTO_TEST_CASE(Merge_ObjectDictionary)
{
    const auto dd1 = CreateDistributionData(InjectableEntity4::Create());

    InjectableEntity4Ptr before = InjectableEntity4::Create();
    before->ItemDictionaryMember1().Insert(0,DoseTest::TestItem::Create());
    before->ItemDictionaryMember1().at(0)->MyInt() = 10;

    before->ItemDictionaryMember2().Insert(0,DoseTest::TestItem::Create());
    before->ItemDictionaryMember2().SetChanged(false);
    before->ItemDictionaryMember2().at(0)->MyInt() = 20;

    before->ItemDictionaryMember3().Insert(0,DoseTest::TestItem::Create());
    before->ItemDictionaryMember3().at(0)->MyInt() = 30;
    before->ItemDictionaryMember3().SetChanged(false);
    before->ItemDictionaryMember3().at(0).SetChangedHere(true);

    before->ItemDictionaryMember4().Insert(0,DoseTest::TestItem::Create());
    before->ItemDictionaryMember4().at(0)->MyInt() = 40;
    before->ItemDictionaryMember4().SetChanged(false);
    before->ItemDictionaryMember4().SetChangedHere(true);

    BOOST_CHECK(before->ItemDictionaryMember2().IsChanged());

    auto dd2 = CreateDistributionData(before);
    TimestampOperations::SetTimestampForChangedMembers(dd2,1000,false);

    const auto res = TimestampOperations::Merge(dd1,dd2);

    BOOST_CHECK(res.second);
    auto after = boost::static_pointer_cast<InjectableEntity4>
        (ObjectFactory::Instance().CreateObject(res.first.GetBlob()));

    BOOST_CHECK_EQUAL(res.first.GetTopTimestamp(),1000);
    for (int i = 0; i < Members::GetNumberOfMembers(InjectableEntity4::ClassTypeId); ++i)
    {
        if (i == 16 || i == 17 || i == 18 || i == 19)
        {
            BOOST_CHECK_EQUAL(res.first.GetMemberTimestamps()[i],1000);
        }
        else
        {
            BOOST_CHECK_EQUAL(res.first.GetMemberTimestamps()[i],0);
        }
    }

    BOOST_CHECK(after->IsChanged());
    BOOST_CHECK(after->ItemDictionaryMember1().IsChanged());
    BOOST_CHECK_EQUAL(after->ItemDictionaryMember1().at(0)->MyInt(), 10);

    BOOST_CHECK(after->ItemDictionaryMember2().IsChanged());
    BOOST_CHECK(!after->ItemDictionaryMember2().IsChangedHere());
    BOOST_CHECK(!after->ItemDictionaryMember2().at(0).IsChangedHere());
    BOOST_CHECK_EQUAL(after->ItemDictionaryMember2().at(0)->MyInt(), 20);
    BOOST_CHECK(after->ItemDictionaryMember2().at(0)->MyInt().IsChanged());

    BOOST_CHECK(after->ItemDictionaryMember3().IsChanged());
    BOOST_CHECK(!after->ItemDictionaryMember3().IsChangedHere());
    BOOST_CHECK(after->ItemDictionaryMember3().at(0).IsChangedHere());
    BOOST_CHECK(!after->ItemDictionaryMember3().at(0)->MyInt().IsChanged());
    BOOST_CHECK_EQUAL(after->ItemDictionaryMember3().at(0)->MyInt(),30);

    BOOST_CHECK(after->ItemDictionaryMember4().IsChanged());
    BOOST_CHECK(after->ItemDictionaryMember4().IsChangedHere());
    BOOST_CHECK(!after->ItemDictionaryMember4().at(0).IsChangedHere());
    BOOST_CHECK(!after->ItemDictionaryMember4().at(0).IsChanged());
    BOOST_CHECK(!after->ItemDictionaryMember4().at(0)->MyInt().IsChanged());
    BOOST_CHECK_EQUAL(after->ItemDictionaryMember4().at(0)->MyInt(),40);
}
#endif

InjectableEntity4Ptr CallSetChangeFlags(const DistributionData& dd1,
                                        DistributionData& dd2)
{
#ifdef TEST_AGAINST_CORE_5
    boost::shared_ptr<char> blobHolder
        (Safir::Dob::Typesystem::Internal::CreateCopy(dd2.GetBlob()),
         Safir::Dob::Typesystem::Internal::Delete);
    TimestampOperations::SetChangeFlags(dd1,dd2,blobHolder.get());

    auto after = boost::static_pointer_cast<InjectableEntity4>
        (ObjectFactory::Instance().CreateObject(blobHolder.get()));
#else
    BlobWriteHelper helper(dd2.GetBlob());
    TimestampOperations::SetChangeFlags(dd1,dd2,helper);

    boost::shared_ptr<char> blobHolder
        (helper.ToBlob(),
         Safir::Dob::Typesystem::Internal::BlobOperations::Delete);

    auto after = boost::static_pointer_cast<InjectableEntity4>
        (ObjectFactory::Instance().CreateObject(blobHolder.get()));

#endif
    return after;
}

BOOST_AUTO_TEST_CASE(SetChangeFlags_Simple)
{
    auto dd1 = CreateDistributionData(InjectableEntity4::Create());
    auto dd2 = CreateDistributionData(InjectableEntity4::Create());
    dd2.SetTopTimestamp(100);
    dd2.GetMemberTimestamps()[2] = 100;
    dd2.GetMemberTimestamps()[4] = 100;

    const auto after = CallSetChangeFlags(dd1,dd2);

    BOOST_CHECK(after->IsChanged());
    BOOST_CHECK(after->Int32Member().IsChanged());
    BOOST_CHECK(after->ArrayMember()[0].IsChanged());
    BOOST_CHECK(after->ArrayMember()[1].IsChanged());
    BOOST_CHECK(after->ArrayMember()[8].IsChanged());

    //reset the change flags on the only things that should be changed and check just that
    after->ArrayMember().SetChanged(false);
    after->Int32Member().SetChanged(false);
    BOOST_CHECK(!after->IsChanged());
}

BOOST_AUTO_TEST_CASE(SetChangeFlags_Object)
{
    auto dd1 = CreateDistributionData(InjectableEntity4::Create());
    auto before = InjectableEntity4::Create();
    before->ItemMember1() = TestItem::Create();
    before->ItemMember1()->MyInt() = 1;
    before->ItemArrayMember1()[3].SetPtr(TestItem::Create());
    before->ItemArrayMember1()[3]->MyInt() = 2;
    before->SetChanged(false);

    auto dd2 = CreateDistributionData(before);
    dd2.SetTopTimestamp(100);
    dd2.GetMemberTimestamps()[InjectableEntity4::ItemMember1MemberIndex()] = 100;
    dd2.GetMemberTimestamps()[InjectableEntity4::ItemArrayMember1MemberIndex()] = 100;

    const auto after = CallSetChangeFlags(dd1,dd2);

    BOOST_CHECK(after->IsChanged());
    BOOST_CHECK(after->ItemMember1().IsChanged());
    BOOST_CHECK(!after->ItemMember1()->MyInt().IsChanged());
    BOOST_CHECK_EQUAL(after->ItemMember1()->MyInt(),1);
    BOOST_CHECK(after->ItemArrayMember1()[3].IsChanged());
    BOOST_CHECK(!after->ItemArrayMember1()[3]->MyInt().IsChanged());
    BOOST_CHECK_EQUAL(after->ItemArrayMember1()[3]->MyInt(), 2);
    BOOST_CHECK(after->ItemArrayMember1()[1].IsChanged());
    BOOST_CHECK(after->ItemArrayMember1()[8].IsChanged());

    //reset the change flags on the only things that should be changed and check just that
    after->ItemMember1().SetChanged(false);
    after->ItemArrayMember1().SetChanged(false);
    BOOST_CHECK(!after->IsChanged());
}

#ifndef TEST_AGAINST_CORE_5

BOOST_AUTO_TEST_CASE(SetChangeFlags_New)
{
    auto dd1 = CreateDistributionData(InjectableEntity4::Create());
    auto before = InjectableEntity4::Create();
    before->ItemSequenceMember1().push_back(TestItem::Create());
    before->ItemSequenceMember1()[0]->MyInt() = 1;
    before->ItemDictionaryMember1()[3].SetPtr(TestItem::Create());
    before->ItemDictionaryMember1()[3]->MyInt() = 2;
    before->SetChanged(false);

    auto dd2 = CreateDistributionData(before);
    dd2.SetTopTimestamp(100);
    dd2.GetMemberTimestamps()[InjectableEntity4::SequenceMemberMemberIndex()] = 100;
    dd2.GetMemberTimestamps()[InjectableEntity4::DictionaryMemberMemberIndex()] = 100;
    dd2.GetMemberTimestamps()[InjectableEntity4::ItemSequenceMember1MemberIndex()] = 100;
    dd2.GetMemberTimestamps()[InjectableEntity4::ItemDictionaryMember1MemberIndex()] = 100;

    const auto after = CallSetChangeFlags(dd1,dd2);

    BOOST_CHECK(after->IsChanged());
    BOOST_CHECK(after->SequenceMember().IsChanged());
    BOOST_CHECK(after->DictionaryMember().IsChanged());

    BOOST_CHECK(after->ItemSequenceMember1().IsChanged());
    BOOST_CHECK(after->ItemSequenceMember1().IsChangedHere());
    BOOST_CHECK(!after->ItemSequenceMember1()[0]->IsChanged());
    BOOST_CHECK_EQUAL(after->ItemSequenceMember1()[0]->MyInt(), 1);

    BOOST_CHECK(after->ItemDictionaryMember1().IsChanged());
    BOOST_CHECK(after->ItemDictionaryMember1().IsChangedHere());
    BOOST_CHECK(!after->ItemDictionaryMember1()[3]->IsChanged());
    BOOST_CHECK_EQUAL(after->ItemDictionaryMember1()[3]->MyInt(), 2);

    //reset the change flags on the only things that should be changed and check just that
    after->SequenceMember().SetChanged(false);
    after->DictionaryMember().SetChanged(false);
    after->ItemSequenceMember1().SetChanged(false);
    after->ItemDictionaryMember1().SetChanged(false);
    BOOST_CHECK(!after->IsChanged());
}

#endif
BOOST_AUTO_TEST_CASE(SetChanges)
{
//TODO: Test SetChanges, ie dose SetChanges on entity, the merge bit
}
