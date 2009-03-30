/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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

#include <Safir/Dob/Internal/TimestampOperations.h>
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Typesystem/Members.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <DoseTest/InjectableEntity.h>
#include <iostream>

using namespace Safir::Dob::Internal;
using namespace Safir::Dob;

int main(int , char ** )
{
    InjectionKindTable::Initialize();

    DoseTest::InjectableEntityPtr intoObject = DoseTest::InjectableEntity::Create();
    DoseTest::InjectableEntityPtr fromObject = DoseTest::InjectableEntity::Create();
    Typesystem::Int32 numMembers = Safir::Dob::Typesystem::Members::GetNumberOfMembers(DoseTest::InjectableEntity::ClassTypeId);
    std::vector<Typesystem::Int64> intoTimestamps(numMembers,0);
    std::vector<Typesystem::Int64> fromTimestamps(numMembers,0);

    //Idea is that all correct numeric results should be divisible by 100, so that it is easy to verify results.

    //from is newer than into (result should be 100)
    intoObject->Int32Member() = 10;
    fromObject->Int32Member() = 100;
    fromTimestamps[DoseTest::InjectableEntity::Int32MemberMemberIndex()] = 11;

    //from is older than into (result should be 200)
    intoObject->Int64Member() = 200;
    fromObject->Int64Member() = 20;
    intoTimestamps[DoseTest::InjectableEntity::Int64MemberMemberIndex()] = 10;

    //from is newer than into, but has no change flag (result should be 300)
    //TODO: this tc does not work in reverse!
    //the problem is that we check the timestamp before we check the change flag
    //since the timestamp in into is smaller than the one in from we will not
    //get to the part where the change flag is set. Is this even a realistic case?
    //
    intoObject->Float32Member() = 300;
    fromObject->Float32Member() = 30;
    fromObject->Float32Member().SetChanged(false);
    fromTimestamps[DoseTest::InjectableEntity::Float32MemberMemberIndex()] = 11;

    //from is newer than into (result should be null!)
    intoObject->Float64Member() = 40;
    fromObject->Float64Member().SetChanged(true);
    fromTimestamps[DoseTest::InjectableEntity::Float64MemberMemberIndex()] = 11;

    //from is newer than into, which is null (result should be 500)
    fromObject->Ampere32Member() = 500;
    fromTimestamps[DoseTest::InjectableEntity::Ampere32MemberMemberIndex()] = 11;

    Typesystem::BinarySerialization intoBin, fromBin;
    Typesystem::Serialization::ToBinary(intoObject,intoBin);
    Typesystem::Serialization::ToBinary(fromObject,fromBin);

    LamportClock clock;

    DistributionData intoDD(entity_state_tag,ConnectionId(),DoseTest::InjectableEntity::ClassTypeId,
        Typesystem::HandlerId(0),Typesystem::InstanceId(0),clock.GetNewTimestamp(),DistributionData::Real,false,&intoBin[0]);

    DistributionData fromDD(entity_state_tag,ConnectionId(),DoseTest::InjectableEntity::ClassTypeId,
        Typesystem::HandlerId(0),Typesystem::InstanceId(0),clock.GetNewTimestamp(),DistributionData::Real,false,&fromBin[0]);

    intoDD.SetTopTimestamp(10);
    fromDD.SetTopTimestamp(11);
    std::copy(intoTimestamps.begin(),intoTimestamps.end(),intoDD.GetMemberTimestamps());
    std::copy(fromTimestamps.begin(),fromTimestamps.end(),fromDD.GetMemberTimestamps());

    std::wcout << "Into object: " << std::endl << intoDD.Image() << std::endl;
    std::wcout << "From object: " << std::endl << fromDD.Image() << std::endl;

    {
        const DistributionData result = TimestampOperations::Merge(intoDD,fromDD);
        //TODO: test with nullblob states

        std::wcout << "------------ Merge result intoDD and fromDD ------------" << std::endl;
        std::wcout << result.Image() << std::endl;
    }

    {
        const DistributionData result = TimestampOperations::Merge(fromDD,intoDD);

        //TODO: test with nullblob states

        std::wcout << "------------ Merge result fromDD and intoDD ------------" << std::endl;
        std::wcout << result.Image() << std::endl;
    }


    DistributionData noBlobDD(entity_state_tag,ConnectionId(),DoseTest::InjectableEntity::ClassTypeId,
        Typesystem::HandlerId(0),Typesystem::InstanceId(0),clock.GetNewTimestamp(),DistributionData::Real,false,NULL);

    {
        const DistributionData result = TimestampOperations::Merge(intoDD,noBlobDD);

        //TODO: test with nullblob states

        std::wcout << "------------ Merge result intoDD and noBlobDD ------------" << std::endl;
        std::wcout << result.Image() << std::endl;
    }

    {
        const DistributionData result = TimestampOperations::Merge(noBlobDD,fromDD);

        //TODO: test with nullblob states

        std::wcout << "------------ Merge result noBlobDD and fromDD ------------" << std::endl;
        std::wcout << result.Image() << std::endl;
    }

    noBlobDD.SetTopTimestamp(12);

    {
        const DistributionData result = TimestampOperations::Merge(intoDD,noBlobDD);

        //TODO: test with nullblob states

        std::wcout << "------------ Merge result intoDD and noBlobDD ------------" << std::endl;
        std::wcout << result.Image() << std::endl;
    }

    {
        const DistributionData result = TimestampOperations::Merge(noBlobDD,fromDD);

        //TODO: test with nullblob states

        std::wcout << "------------ Merge result noBlobDD and fromDD ------------" << std::endl;
        std::wcout << result.Image() << std::endl;
    }

    return 0;
}

