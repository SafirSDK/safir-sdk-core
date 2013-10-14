/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#include <iostream>
#include <DotsTest/MemberTypes.h>


int tests = 0;
int failures = 0;


void Check(bool expr, const std::string & description = "")
{
    ++tests;
    if (!expr)
    {
        ++failures;
        std::wcout << "Testcase " << tests;
        if (!description.empty())
        {
            std::wcout << "'" << description.c_str() << "'";
        }
        std::wcout << " failed!" << std::endl;
    }
}


/*
class TestClass
{
public:
    TestClass() {}

    const ContainerProxy<Int32Container> Int32Member() const {return ContainerProxy<Int32Container>(const_cast<Int32Container&>(m_Int32Member));}
    ContainerProxy<Int32Container> Int32Member() {return ContainerProxy<Int32Container>(m_Int32Member);}

    const ContainerProxy<Float32Container> Float32Member() const {return ContainerProxy<Float32Container>(const_cast<Float32Container&>(m_Float32Member));}
    ContainerProxy<Float32Container> Float32Member() {return ContainerProxy<Float32Container>(m_Float32Member);}

    const ContainerProxy<TypeIdContainer> TypeIdMember() const {return ContainerProxy<TypeIdContainer>(const_cast<TypeIdContainer&>(m_TypeIdMember));}
    ContainerProxy<TypeIdContainer> TypeIdMember() {return ContainerProxy<TypeIdContainer>(m_TypeIdMember);}

    const ContainerProxy<InstanceIdContainer> InstanceIdMember() const {return ContainerProxy<InstanceIdContainer>(const_cast<InstanceIdContainer&>(m_InstanceIdMember));}
    ContainerProxy<InstanceIdContainer> InstanceIdMember() {return ContainerProxy<InstanceIdContainer>(m_InstanceIdMember);}

private:
    Int32Container m_Int32Member;
    Float32Container m_Float32Member;
    TypeIdContainer m_TypeIdMember;
    InstanceIdContainer m_InstanceIdMember;
};
*/

int main()
{
    using namespace Safir::Dob::Typesystem;
    using namespace DotsTest;

    //    HandlerId h;

    //container testing
    {
        Int32Container intCont;
        Check(intCont.IsNull());
        Check(!intCont.IsChanged());
        intCont.SetVal(10);
        Check(intCont.GetVal() == 10);
        Check(!intCont.IsNull());
        Check(intCont.IsChanged());


        BooleanContainer boolCont;
        Check(boolCont.IsNull());
        Check(!boolCont.IsChanged());
        boolCont.SetVal(true);
        Check(boolCont.GetVal() == true);
        Check(!boolCont.IsNull());
        Check(boolCont.IsChanged());

        TestEnum::EnumerationContainer enumCont;
        Check(enumCont.IsNull());
        Check(!enumCont.IsChanged());
        enumCont.SetVal(TestEnum::MyFirst);
        Check(enumCont.GetVal() == TestEnum::MyFirst);
        Check(!enumCont.IsNull());
        Check(enumCont.IsChanged());

        Float32Container floatCont;
        Check(floatCont.IsNull());
        Check(!floatCont.IsChanged());
        floatCont.SetVal(10);
        Check(floatCont.GetVal() == 10);
        Check(!floatCont.IsNull());
        Check(floatCont.IsChanged());

        TypeIdContainer typeIdCont;
        Check(typeIdCont.IsNull());
        Check(!typeIdCont.IsChanged());
        typeIdCont.SetVal(10);
        Check(typeIdCont.GetVal() == 10);
        Check(!typeIdCont.IsNull());
        Check(typeIdCont.IsChanged());

        InstanceIdContainer instanceIdCont;
        Check(instanceIdCont.IsNull());
        Check(!instanceIdCont.IsChanged());
        instanceIdCont.SetVal(InstanceId(10));
        Check(instanceIdCont.GetVal() == InstanceId(10));
        Check(!instanceIdCont.IsNull());
        Check(instanceIdCont.IsChanged());

        StringContainer strCont;
        Check(strCont.IsNull());
        Check(!strCont.IsChanged());
        strCont.SetVal(L"Kalle");
        Check(strCont.GetVal() == L"Kalle");
        Check(!strCont.IsNull());
        Check(strCont.IsChanged());

    }

#define CheckNullExc(expr) \
    try{bool b = expr;b;Check(false);}catch(Safir::Dob::Typesystem::NullException &){Check(true);}

    {
        MemberTypes t;
        Check(t.Int32Member().IsNull());
        Check(!t.Int32Member().IsChanged());
        CheckNullExc(t.Int32Member() == 10);
        CheckNullExc(10 == t.Int32Member());
        CheckNullExc(t.Int32Member() != 10);
        CheckNullExc(10 != t.Int32Member());
        t.Int32Member().SetVal(10);
        Check(!t.Int32Member().IsNull());
        Check(t.Int32Member().IsChanged());
        Check(t.Int32Member().GetVal() == 10);
        Check(t.Int32Member() == 10);
        const Int32 i = t.Int32Member();
        Check(i == 10);

        

        CheckNullExc(t.BooleanMember() == true);
        CheckNullExc(true == t.BooleanMember());
        CheckNullExc(t.BooleanMember());
        CheckNullExc(t.BooleanMember() != true);
        CheckNullExc(true != t.BooleanMember());
        CheckNullExc(t.BooleanMember());
        t.BooleanMember() = true;
        Check(!t.BooleanMember().IsNull());
        Check(t.BooleanMember().IsChanged());
        Check(t.BooleanMember() == true);
        Check(t.BooleanMember());
        t.BooleanMember().SetNull();
        t.BooleanMember().SetChanged(false);
        Check(t.BooleanMember().IsNull());
        Check(!t.BooleanMember().IsChanged());
        t.BooleanMember().SetVal(false);
        Check(!t.BooleanMember().IsNull());
        Check(t.BooleanMember().IsChanged());
        Check(t.BooleanMember() == false);
        Check(!t.BooleanMember());

        Check(t.Float32Member().IsNull());
        Check(!t.Float32Member().IsChanged());
        CheckNullExc(t.Float32Member() == 10);
        CheckNullExc(10 == t.Float32Member());
        CheckNullExc(t.Float32Member() != 10);
        CheckNullExc(10 != t.Float32Member());
        t.Float32Member().SetVal(10);
        Check(!t.Float32Member().IsNull());
        Check(t.Float32Member().IsChanged());
        Check(t.Float32Member().GetVal() == 10);
        Check(t.Float32Member() == 10.0);

        Check(t.TypeIdMember().IsNull());
        Check(!t.TypeIdMember().IsChanged());
        t.TypeIdMember().SetVal(10);
        Check(!t.TypeIdMember().IsNull());
        Check(t.TypeIdMember().IsChanged());
        Check(t.TypeIdMember().GetVal() == 10);
        Check(t.TypeIdMember() == 10);

        Check(t.InstanceIdMember().IsNull());
        Check(!t.InstanceIdMember().IsChanged());
        CheckNullExc(t.InstanceIdMember() == InstanceId(10));
        CheckNullExc(InstanceId(10) == t.InstanceIdMember());
        CheckNullExc(t.InstanceIdMember() != InstanceId(10));
        CheckNullExc(InstanceId(10) != t.InstanceIdMember());
        t.InstanceIdMember().SetVal(InstanceId(10));
        Check(!t.InstanceIdMember().IsNull());
        Check(t.InstanceIdMember().IsChanged());
        Check(t.InstanceIdMember().GetVal() == InstanceId(10));
        Check(InstanceId(10) == t.InstanceIdMember());
        Check(t.InstanceIdMember() == InstanceId(10));
        Check(InstanceId(11) != t.InstanceIdMember());
        Check(t.InstanceIdMember() != InstanceId(11));
        t.InstanceIdMember().SetVal(InstanceId(L"Kalle"));
        Check(InstanceId(L"Kalle") == t.InstanceIdMember());
        Check(t.InstanceIdMember() == InstanceId(L"Kalle")); 
        Check(InstanceId(L"Pelle") != t.InstanceIdMember());
        Check(t.InstanceIdMember() != InstanceId(L"Pelle")); 

        Check(t.StringMember().IsNull());
        Check(!t.StringMember().IsChanged());
        CheckNullExc(t.StringMember() == L"Kalle");
        CheckNullExc(L"Kalle" == t.StringMember());
        CheckNullExc(t.StringMember() != L"Kalle");
        CheckNullExc(L"Kalle" != t.StringMember());
        const std::wstring kalleS(L"Kalle");
        CheckNullExc(t.StringMember() == kalleS);
        CheckNullExc(kalleS == t.StringMember());
        CheckNullExc(t.StringMember() != kalleS);
        CheckNullExc(kalleS != t.StringMember());
        t.StringMember().SetVal(L"Kalle");
        Check(!t.StringMember().IsNull());
        Check(t.StringMember().IsChanged());
        Check(t.StringMember().GetVal() == L"Kalle");
        Check(t.StringMember() == L"Kalle");
        Check(L"Kalle" == t.StringMember());
        Check(t.StringMember() != L"Pelle");
        Check(L"Pelle" != t.StringMember());

        Check(t.BinaryMember().IsNull());
        Check(!t.BinaryMember().IsChanged());
        const char kalleC [] = "Kalle";
        const Safir::Dob::Typesystem::Binary kalle(kalleC,kalleC + sizeof(kalleC));
        CheckNullExc(t.BinaryMember() == kalle);
        CheckNullExc(kalle == t.BinaryMember());
        CheckNullExc(t.BinaryMember() != kalle);
        CheckNullExc(kalle != t.BinaryMember());
        t.BinaryMember().SetVal(kalle);
        Check(!t.BinaryMember().IsNull());
        Check(t.BinaryMember().IsChanged());
        Check(t.BinaryMember().GetVal() == kalle);
        Check(t.BinaryMember() == kalle);
        Check(kalle == t.BinaryMember());
        const char pelleC [] = "PellePlutt";
        const Safir::Dob::Typesystem::Binary pelle(pelleC,pelleC + sizeof(pelleC));
        Check(t.BinaryMember() != pelle);
        Check(pelle != t.BinaryMember());
    }

    {

        MemberTypes t;
        t.Int32Member() = 20;
        Check(!t.Int32Member().IsNull());
        Check(t.Int32Member().IsChanged());
        Check(t.Int32Member() == 20);
        t.Int32Member().SetNull();
        t.Int32Member().SetChanged(false);
        Check(t.Int32Member().IsNull());
        Check(!t.Int32Member().IsChanged());
        t.Int32Member().SetVal(30);
        Check(!t.Int32Member().IsNull());
        Check(t.Int32Member().IsChanged());
        Check(t.Int32Member() == 30);


        t.BooleanMember() = true;
        Check(!t.BooleanMember().IsNull());
        Check(t.BooleanMember().IsChanged());
        Check(t.BooleanMember() == true);
        Check(t.BooleanMember());
        t.BooleanMember().SetNull();
        t.BooleanMember().SetChanged(false);
        Check(t.BooleanMember().IsNull());
        Check(!t.BooleanMember().IsChanged());
        t.BooleanMember().SetVal(false);
        Check(!t.BooleanMember().IsNull());
        Check(t.BooleanMember().IsChanged());
        Check(t.BooleanMember() == false);
        Check(!t.BooleanMember());


        t.EnumerationMember() = TestEnum::MyFirst;
        Check(!t.EnumerationMember().IsNull());
        Check(t.EnumerationMember().IsChanged());
        Check(t.EnumerationMember() == TestEnum::MyFirst);
        t.EnumerationMember().SetNull();
        t.EnumerationMember().SetChanged(false);
        Check(t.EnumerationMember().IsNull());
        Check(!t.EnumerationMember().IsChanged());
        t.EnumerationMember().SetVal(TestEnum::MySecond);
        Check(!t.EnumerationMember().IsNull());
        Check(t.EnumerationMember().IsChanged());
        Check(t.EnumerationMember() == TestEnum::MySecond);


        t.Float32Member() = 20;
        Check(!t.Float32Member().IsNull());
        Check(t.Float32Member().IsChanged());
        Check(t.Float32Member() == 20);
        t.Float32Member().SetNull();
        t.Float32Member().SetChanged(false);
        Check(t.Float32Member().IsNull());
        Check(!t.Float32Member().IsChanged());
        t.Float32Member().SetVal(30);
        Check(!t.Float32Member().IsNull());
        Check(t.Float32Member().IsChanged());
        Check(t.Float32Member() == 30);


        t.TypeIdMember() = 20;
        Check(!t.TypeIdMember().IsNull());
        Check(t.TypeIdMember().IsChanged());
        Check(t.TypeIdMember() == 20);
        t.TypeIdMember().SetNull();
        t.TypeIdMember().SetChanged(false);
        Check(t.TypeIdMember().IsNull());
        Check(!t.TypeIdMember().IsChanged());
        t.TypeIdMember().SetVal(30);
        Check(!t.TypeIdMember().IsNull());
        Check(t.TypeIdMember().IsChanged());
        Check(t.TypeIdMember() == 30);


        t.InstanceIdMember() = InstanceId(20);
        Check(!t.InstanceIdMember().IsNull());
        Check(t.InstanceIdMember().IsChanged());
        Check(InstanceId(20) == t.InstanceIdMember());
        t.InstanceIdMember().SetNull();
        t.InstanceIdMember().SetChanged(false);
        Check(t.InstanceIdMember().IsNull());
        Check(!t.InstanceIdMember().IsChanged());
        t.InstanceIdMember().SetVal(InstanceId(30));
        Check(!t.InstanceIdMember().IsNull());
        Check(t.InstanceIdMember().IsChanged());
        Check(InstanceId(30) == t.InstanceIdMember());
        t.InstanceIdMember() = InstanceId(L"Kalle");
        Check(!t.InstanceIdMember().IsNull());
        Check(t.InstanceIdMember().IsChanged());
        Check(InstanceId(L"Kalle") == t.InstanceIdMember());
    }

    {
        MemberTypes t1;
        MemberTypes t2;
        //proxied container assign.
        t1.Int32Member() = 30;
        t1.Int32Member().SetChanged(false);
        t2.Int32Member() = t1.Int32Member();
        Check(t2.Int32Member() == 30);
        Check(t2.Int32Member().IsChanged());
        Check(!t1.Int32Member().IsChanged());

        //Operators
        ++t2.Int32Member();
        Check(t2.Int32Member() == 31);
        t2.Int32Member()++;
        Check(t2.Int32Member() == 32);
        t2.Int32Member() += 10;
        Check(t2.Int32Member() == 42);
        t2.Int32Member() *= 2;
        Check(t2.Int32Member() == 84);
        t2.Int32Member() /= 4;
        Check(t2.Int32Member() == 21);


        //proxied container assign.
        t1.BooleanMember() = false;
        t1.BooleanMember().SetChanged(false);
        t2.BooleanMember() = t1.BooleanMember();
        Check(!t2.BooleanMember());
        Check(t2.BooleanMember().IsChanged());
        Check(!t1.BooleanMember().IsChanged());

        //Operators N/A

        //proxied container assign.
        t1.EnumerationMember() = TestEnum::MyFirst;
        t1.EnumerationMember().SetChanged(false);
        t2.EnumerationMember() = t1.EnumerationMember();
        Check(t2.EnumerationMember() == TestEnum::MyFirst);
        Check(t2.EnumerationMember().IsChanged());
        Check(!t1.EnumerationMember().IsChanged());

        //Operators
        // Enum operator fail to compile, as should they
        //         ++t2.EnumerationMember();
        //         t2.EnumerationMember()++;
        //         t2.EnumerationMember() += TestEnum::MyFirst;
        //         t2.EnumerationMember() *= TestEnum::MySecond;
        //         t2.EnumerationMember() /= TestEnum::MyFirst;

        //proxied container assign.
        t1.Float32Member() = 30;
        t1.Float32Member().SetChanged(false);
        t2.Float32Member() = t1.Float32Member();
        Check(t2.Float32Member() == 30);
        Check(t2.Float32Member().IsChanged());
        Check(!t1.Float32Member().IsChanged());

        //Operators
        ++t2.Float32Member();
        Check(t2.Float32Member() == 31);
        t2.Float32Member()++;
        Check(t2.Float32Member() == 32);
        t2.Float32Member() += 10;
        Check(t2.Float32Member() == 42);

        t2.Float32Member() *= 2;
        Check(t2.Float32Member() == 84);
        t2.Float32Member() /= 4;
        Check(t2.Float32Member() == 21);

        //proxied container assign.
        t1.TypeIdMember() = 30;
        t1.TypeIdMember().SetChanged(false);
        t2.TypeIdMember() = t1.TypeIdMember();
        Check(t2.TypeIdMember() == 30);
        Check(t2.TypeIdMember().IsChanged());
        Check(!t1.TypeIdMember().IsChanged());

        //Operators
        ++t2.TypeIdMember();
        Check(t2.TypeIdMember() == 31);
        t2.TypeIdMember()++;
        Check(t2.TypeIdMember() == 32);
        t2.TypeIdMember() += 10;
        Check(t2.TypeIdMember() == 42);

        t2.TypeIdMember() *= 2;
        Check(t2.TypeIdMember() == 84);
        t2.TypeIdMember() /= 4;
        Check(t2.TypeIdMember() == 21);

        //proxied container assign.
        t1.InstanceIdMember() = InstanceId(30);
        t1.InstanceIdMember().SetChanged(false);
        t2.InstanceIdMember() = t1.InstanceIdMember();
        Check(InstanceId(30) == t2.InstanceIdMember());
        Check(t2.InstanceIdMember().IsChanged());
        Check(!t1.InstanceIdMember().IsChanged());

        //Operators for Instance Id fail (as should they!)
        /*
        ++t2.InstanceIdMember();
        t2.InstanceIdMember()++;
        t2.InstanceIdMember() += InstanceId(10);
        t2.InstanceIdMember() *= InstanceId(2);
        t2.InstanceIdMember() /= InstanceId(4);
        */
    }

    //objects
    {
        MemberTypes t;
        t.ObjectMember() = Safir::Dob::Typesystem::Object::Create();
        t.ObjectMember().GetPtr()->GetTypeId();
        t.ObjectMember()->GetTypeId();
    }

    //Copy
    {
        MemberTypes t1,t2;
        t1.Int32Member() = 10;
        t2.Int32Member().Copy(t1.Int32Member());
    }



    if (failures != 0)
    {
        std::wcout << "There were " << failures << " failures!! (out of " << tests << " tests)" << std::endl;
        return -1;
    }
    else
    {
        std::wcout << "All " << tests << " testcases succeeded!" << std::endl;
        return 0;
    }


}

