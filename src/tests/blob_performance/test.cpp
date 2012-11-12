/******************************************************************************
*
* Copyright Saab AB, 2012 (http://www.safirsdk.com)
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
#include <iostream>
#include <DotsTest/MemberTypes.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <boost/timer.hpp>

int main()
{
    using namespace Safir::Dob::Typesystem;
    DotsTest::MemberTypesPtr obj = DotsTest::MemberTypes::Create();
    obj->StringMember() = L"hello sail"; //or! :-)
    obj->EntityIdMember() = EntityId(DotsTest::MemberTypes::ClassTypeId,
                                     InstanceId(L"foooofjklsakdfjlaksdjfklfjjfeijfijijasijfasdf"));
    obj->Int32Member() = 10;
    obj->Int64Member() = 10;
    obj->Float64Member() = 10.0;
    obj->Float32Member() = 10.0f;
    obj->ObjectMember() = DotsTest::TestItem::Create();

    std::wcout << "CalculateBlobSize: " << obj->CalculateBlobSize() << std::endl;

    std::vector<Binary> vec(100000);
    for (std::vector<Binary>::iterator it = vec.begin(); it != vec.end(); ++it)
    {
        it->reserve(2000);
    }

    boost::timer t;
    for (int i = 0; i < 5; ++i)
    {
        for (std::vector<Binary>::iterator it = vec.begin(); it != vec.end(); ++it)
        {
            Serialization::ToBinary(obj,*it);
        }
        
        for (std::vector<Binary>::reverse_iterator it = vec.rbegin(); it != vec.rend(); ++it)
        {
            ObjectPtr o = Serialization::ToObject(*it);
        }
    }
    std::wcout << "Elapsed time: " << t.elapsed() <<std::endl;
    return 0;
}
