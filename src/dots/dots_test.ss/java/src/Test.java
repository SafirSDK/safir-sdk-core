// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009-2013, 2024 (http://safirsdkcore.com)
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

import com.saabgroup.dotstest.*;
import com.saabgroup.safir.dob.typesystem.*;
import com.saabgroup.safir.dob.typesystem.Object;
import java.util.Map;
import java.util.Locale;
import java.util.Arrays;

/**
 * Main test class
 */
public class Test {
    private Test() {}
    /**
     * Main program
     * @param args command line
     */
    public static void main(String[] args) {
        Locale.setDefault(new Locale("en", "US"));

        test_Has_Property();
        test_PropertyMappingKind();
        test_Property_GetParameterReference();
        test_GetName();
        test_GetNumberOfMembers();
        test_GetNumberOfParameters();
        test_Create_Routines();
        test_Int32();
        test_Int64();
        test_Float32();
        test_Float64();

        test_Boolean();

        test_Enumeration();
        test_String();
        test_EntityId();
        test_InstanceId();
        test_TypeId();
        test_ChannelId();
        test_HandlerId();
        test_Object();
        test_Binary();
        test_TestClass();
        test_Ampere32();
        test_CubicMeter32();
        test_Hertz32();
        test_Joule32();
        test_Kelvin32();
        test_Kilogram32();
        test_Meter32();
        test_MeterPerSecond32();
        test_MeterPerSecondSquared32();
        test_Newton32();
        test_Pascal32();
        test_Radian32();
        test_RadianPerSecond32();
        test_RadianPerSecondSquared32();
        test_Second32();
        test_SquareMeter32();
        test_Steradian32();
        test_Volt32();
        test_Watt32();
        test_Ampere64();
        test_CubicMeter64();
        test_Hertz64();
        test_Joule64();
        test_Kelvin64();
        test_Kilogram64();
        test_Meter64();
        test_MeterPerSecond64();
        test_MeterPerSecondSquared64();
        test_Newton64();
        test_Pascal64();
        test_Radian64();
        test_RadianPerSecond64();
        test_RadianPerSecondSquared64();
        test_Second64();
        test_SquareMeter64();
        test_Steradian64();
        test_Volt64();
        test_Watt64();

        test_TestException();
        test_LibraryExceptions();
        Test_IsProperty();
        Test_IsEnumeration();
        Test_IsException();

        testSequences();
        testDictionaries();

        Test_DeserializeUnlinkedObject();

        MiscTests misc_tests = new MiscTests();
        misc_tests.test_Containers();
        misc_tests.Test_BlobChangeFlags();
        misc_tests.Test_Misc();
        
        MergeChangesTests merge_tests = new MergeChangesTests();
        merge_tests.Test_MergeChanges();

    }

    private static void Header(String label) {
        System.out.println();
        System.out.println();
        System.out.println();
        System.out.println("=====================================================");
        System.out.println("Testing: " + label);
        System.out.println("=====================================================");
    }

    private static MemberTypes MT1 = new MemberTypes();
    private static MemberTypes MT2 = new MemberTypes();
    private static MemberArrays MA1 = new MemberArrays();
    private static MemberArrays MA2 = new MemberArrays();
    private static EmptyObject EO = new EmptyObject();
    private static AnotherEmptyObject AEO = new AnotherEmptyObject();
    private static MemberItemsArray MIA = new MemberItemsArray();
    private static MemberItems MI = new MemberItems();

    private static void test_Has_Property() {
        Header("Has Property");
        System.out.println("MemberTypes - MemberTypesProperty: " + MemberTypesProperty.hasProperty(MT1));
        System.out.println("MemberTypes - MemberArraysProperty: " + MemberArraysProperty.hasProperty(MT1));
        System.out.println("MemberArrays - MemberTypesProperty: " + MemberTypesProperty.hasProperty(MA1));
        System.out.println("MemberArrays - MemberArraysProperty: " + MemberArraysProperty.hasProperty(MA1));
        System.out.println("MemberItems - MemberTypesProperty: " + MemberTypesProperty.hasProperty(MI));
        System.out.println("MemberItems - MemberArraysProperty: " + MemberArraysProperty.hasProperty(MI));
        System.out.println("MemberItemsArray - MemberTypesProperty: " + MemberTypesProperty.hasProperty(MIA));
        System.out.println("MemberItemsArray - MemberArraysProperty: " + MemberArraysProperty.hasProperty(MIA));
        System.out.println("EmptyObject - MemberTypesProperty: " + MemberTypesProperty.hasProperty(EO));
        System.out.println("EmptyObject - MemberArraysProperty: " + MemberArraysProperty.hasProperty(EO));
        System.out.println("AnotherEmptyObject - MemberTypesProperty: " + MemberTypesProperty.hasProperty(AEO));
        System.out.println("AnotherEmptyObject - MemberArraysProperty: " + MemberArraysProperty.hasProperty(AEO));
    }

    private static String mappingKindStr(PropertyMappingKind k)
    {
        switch (k)
        {
        case MAPPED_TO_NULL: return "MappedToNull";
        case MAPPED_TO_MEMBER: return "MappedToMember";
        case MAPPED_TO_PARAMETER: return "MappedToParameter";
        }
        return "";
    }

    private static void test_PropertyMappingKind()
    {
        Header("Property Mapping Kind");
        System.out.println("EmptyObject: " + mappingKindStr(Properties.getMappingKind(EmptyObject.ClassTypeId, MemberTypesProperty.ClassTypeId, 0)));
        System.out.println("AnotherEmptyObject: " + mappingKindStr(Properties.getMappingKind(AnotherEmptyObject.ClassTypeId, MemberTypesProperty.ClassTypeId, 0)));
        System.out.println("MemberItems: " + mappingKindStr(Properties.getMappingKind(MemberItems.ClassTypeId, MemberTypesProperty.ClassTypeId, 0)));
    }

    private static void test_Property_GetParameterReference()
    {
        Header("Property Get Parameter Reference");

        var p = Properties.getParameterReference(EmptyObject.ClassTypeId, MemberTypesProperty.ClassTypeId, 6, 0);
        System.out.println("EmptyObject: " + Parameters.getString((long)p[0], (int)p[1], (int)p[2]));
        p = Properties.getParameterReference(AnotherEmptyObject.ClassTypeId, MemberTypesProperty.ClassTypeId, 9, 0);
        System.out.println("AnotherEmptyObject: " + Parameters.getTypeId((long)p[0], (int)p[1], (int)p[2]));

        try
        {
            Properties.getParameterReference(MemberItems.ClassTypeId, MemberTypesProperty.ClassTypeId, 0, 0);
        }
        catch (IllegalValueException exc)
        {
            System.out.println("Not mapped to parameter");
        }
    }


    private static void test_GetName() {
        Header("Get Name");
        System.out.println("MemberTypes          - " + Operations.getName(MemberTypes.ClassTypeId));
        System.out.println("MemberArrays         - " + Operations.getName(MemberArrays.ClassTypeId));
        System.out.println("MemberTypesProperty  - " + Operations.getName(MemberTypesProperty.ClassTypeId));
        System.out.println("MemberArraysProperty - " + Operations.getName(MemberArraysProperty.ClassTypeId));
        System.out.println("MemberItems          - " + Operations.getName(MemberItems.ClassTypeId));
        System.out.println("MemberItemsArray     - " + Operations.getName(MemberItemsArray.ClassTypeId));
        System.out.println("EmptyObject          - " + Operations.getName(EmptyObject.ClassTypeId));
    }

    private static void test_GetNumberOfMembers() {
        Header("Get Number Of Members");
        System.out.println("MemberTypes          - " + Members.getNumberOfMembers(MemberTypes.ClassTypeId));
        System.out.println("MemberArrays         - " + Members.getNumberOfMembers(MemberArrays.ClassTypeId));
        System.out.println("MemberTypesProperty  - " + Members.getNumberOfMembers(MemberTypesProperty.ClassTypeId));
        System.out.println("MemberArraysProperty - " + Members.getNumberOfMembers(MemberArraysProperty.ClassTypeId));
        System.out.println("MemberItems          - " + Members.getNumberOfMembers(MemberItems.ClassTypeId));
        System.out.println("MemberItemsArray     - " + Members.getNumberOfMembers(MemberItemsArray.ClassTypeId));
        System.out.println("EmptyObject          - " + Members.getNumberOfMembers(EmptyObject.ClassTypeId));
        System.out.println("ParameterTypes       - " + Members.getNumberOfMembers(ParameterTypes.ClassTypeId));
        System.out.println("ParameterArrays      - " + Members.getNumberOfMembers(ParameterArrays.ClassTypeId));

    }

    private static void test_GetNumberOfParameters() {
        Header("Get Number Of Parameters");
        System.out.println("MemberTypes          - " + Parameters.getNumberOfParameters(MemberTypes.ClassTypeId));
        System.out.println("MemberArrays         - " + Parameters.getNumberOfParameters(MemberArrays.ClassTypeId));
        System.out.println("MemberItems          - " + Parameters.getNumberOfParameters(MemberItems.ClassTypeId));
        System.out.println("MemberItemsArray     - " + Parameters.getNumberOfParameters(MemberItemsArray.ClassTypeId));
        System.out.println("EmptyObject          - " + Parameters.getNumberOfParameters(EmptyObject.ClassTypeId));
        System.out.println("ParameterTypes       - " + Parameters.getNumberOfParameters(ParameterTypes.ClassTypeId));
        System.out.println("ParameterArrays      - " + Parameters.getNumberOfParameters(ParameterArrays.ClassTypeId));
    }

    private static void test_Create_Routines() {
        Header("Create routines (Types)");
        int i = ParameterTypes.getInt32Parameter();
        TestEnum e = ParameterTypes.getEnumerationParameter();
        TestItem c = ParameterTypes.getTestClassParameter();
        System.out.println("Create_ParameterTypes: " + Serialization.toXml(MemberTypes.createParameterTypes(i, e, c)));
        System.out.println("CreateValueTypes     : " + Serialization.toXml(MemberTypes.createValueTypes()));
        System.out.println("Create_ValueArrays   : " + Serialization.toXml(MemberTypes.createValueArrays()));
        System.out.println("CreateValues         : " + Serialization.toXml(TypesItem.createCreateRoutineValues()));
        System.out.println("CreateInheritedValues: " + Serialization.toXml(TypesItemInherited.createCreateRoutineValues2()));

        TypesItem createParameters = TypesItem.createCreateRoutineParameters(
        10,
        20,
        30.1f,
        40.1,
        false,
        TestEnum.MY_SECOND,
        "Hello",
        new EntityId(com.saabgroup.safir.dob.Entity.ClassTypeId, new InstanceId(1)),
        new EntityId(com.saabgroup.safir.dob.Entity.ClassTypeId, new InstanceId(2)),
        com.saabgroup.safir.dob.Entity.ClassTypeId,
        new InstanceId(1),
        new InstanceId(2),
        new ChannelId(1),
        new ChannelId(2),
        new HandlerId(1),
        new HandlerId(2),
        new com.saabgroup.safir.dob.Entity(),
        new byte[]{97,98,99},
        new TestItem(),
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5);

        TypesItemInherited createParametersInherited = TypesItemInherited.createCreateRoutineParameters2(
        "Local Member",
        10,
        20,
        30.1f,
        40.1,
        false,
        TestEnum.MY_SECOND,
        "Hello",
        new EntityId(com.saabgroup.safir.dob.Entity.ClassTypeId, new InstanceId(1)),
        new EntityId(com.saabgroup.safir.dob.Entity.ClassTypeId, new InstanceId(2)),
        com.saabgroup.safir.dob.Entity.ClassTypeId,
        new InstanceId(1),
        new InstanceId(2),
        new ChannelId(1),
        new ChannelId(2),
        new HandlerId(1),
        new HandlerId(2),
        new com.saabgroup.safir.dob.Entity(),
        new byte[]{97,98,99},
        new TestItem(),
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        1.5f,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5,
        2.5);

        System.out.println("CreateParam          : " + Serialization.toXml(createParameters));
        System.out.println("CreateParamInherited : " + Serialization.toXml(createParametersInherited));
    }

    private static String toStringNoUnderscore(MemberType val) {
        return val.toString().replace("_", "");
    }

    private static void test_Int32() {
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Int32");
        System.out.println("MemberId: " + MemberTypes.getInt32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getInt32MemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getInt32ParameterArraySize() == 2 && MemberArrays.getInt32MemberArraySize() == 2
                        && MemberArraysProperty.getInt32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.int32Member().isNull();
        In_Req_Ok = !MT1.int32Member().isChanged();
        MT1.int32Member().setVal(ParameterTypes.getInt32Parameter());
        Null_Ok = Null_Ok && !MT1.int32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.int32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getInt32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getInt32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getInt32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Int32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Int32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Int32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullInt32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedInt32Member(MT2);
        MemberTypesProperty.setInt32Member(MT2, MT1.int32Member().getVal());
        System.out.println("Val: " + MemberTypesProperty.getInt32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInt32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedInt32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullInt32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedInt32Member(MI);
        MemberTypesProperty.setInt32Member(MI, MT2.int32Member().getVal());
        System.out.println("Item Val: " + MemberTypesProperty.getInt32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInt32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedInt32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullInt32Member(MIA);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedInt32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setInt32Member(MIA, MT2.int32Member().getVal());
        System.out.println("Item Array Val: " + MIA.typesItemArray().get(1).getObj().int32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInt32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedInt32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInt32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedInt32Member(EO);
        System.out.println("Property Parameter Val: " + MemberTypesProperty.getInt32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInt32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedInt32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInt32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedInt32Member(AEO);
        System.out.println("Property Parameter Val: " + MemberTypesProperty.getInt32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInt32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedInt32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getInt32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.int32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.int32Member().get(ix).isChanged();
            MA1.int32Member().get(ix).setVal(ParameterArrays.getInt32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.int32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.int32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullInt32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedInt32Member(MA2, ix);
            MA2.int32Member().get(ix).setVal(MA1.int32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInt32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedInt32Member(MA2, ix);

            System.out.println("Val " + ix + ": " + MemberArraysProperty.getInt32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.int32Member().get(ix).setVal(MA1.int32Member().get(ix).getVal());

            MI.arraysItem().setObj(item);

            System.out.println("Array Item Val " + ix + ": " + MemberArraysProperty.getInt32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInt32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedInt32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.println("Array Item Array Val " + ix + ": " + MemberArraysProperty.getInt32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInt32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedInt32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInt32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedInt32Member(EO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + MemberArraysProperty.getInt32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInt32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedInt32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInt32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedInt32Member(AEO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + MemberArraysProperty.getInt32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInt32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedInt32Member(AEO, ix);
        }

        // setNull test
        MT1.int32Member().setNull();
        MemberTypesProperty.setNullInt32Member(MT2);
        MA1.int32Member().get(1).setNull();
        MemberArraysProperty.setNullInt32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.int32Member().isNull() && MemberTypesProperty.isNullInt32Member(MT2)
                && MA1.int32Member().get(1).isNull() && MemberArraysProperty.isNullInt32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    }

    private static void test_Int64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Int64");
        System.out.println("MemberId: " + MemberTypes.getInt64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getInt64MemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getInt64ParameterArraySize() == 2 && MemberArrays.getInt64MemberArraySize() == 2
                        && MemberArraysProperty.getInt64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.int64Member().isNull();
        In_Req_Ok = !MT1.int64Member().isChanged();
        MT1.int64Member().setVal(ParameterTypes.getInt64Parameter());
        Null_Ok = Null_Ok && !MT1.int64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.int64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getInt64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getInt64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getInt64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Int64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Int64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Int64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullInt64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedInt64Member(MT2);
        MemberTypesProperty.setInt64Member(MT2, MT1.int64Member().getVal());
        System.out.println("Val: " + MemberTypesProperty.getInt64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInt64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedInt64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullInt64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedInt64Member(MI);
        MemberTypesProperty.setInt64Member(MI, MT2.int64Member().getVal());
        System.out.println("Item Val: " + MemberTypesProperty.getInt64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInt64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedInt64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullInt64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedInt64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setInt64Member(MIA, MT2.int64Member().getVal());
        System.out.println("Item Array Val: " + MIA.typesItemArray().get(1).getObj().int64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInt64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedInt64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInt64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedInt64Member(EO);
        System.out.println("Property Parameter Val: " + MemberTypesProperty.getInt64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInt64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedInt64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInt64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedInt64Member(AEO);
        System.out.println("Property Parameter Val: " + MemberTypesProperty.getInt64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInt64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedInt64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getInt64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.int64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.int64Member().get(ix).isChanged();
            MA1.int64Member().get(ix).setVal(ParameterArrays.getInt64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.int64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.int64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullInt64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedInt64Member(MA2, ix);
            MA2.int64Member().get(ix).setVal(MA1.int64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInt64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedInt64Member(MA2, ix);

            System.out.println("Val " + ix + ": " + MemberArraysProperty.getInt64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.int64Member().get(ix).setVal(MA1.int64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.println("Array Item Val " + ix + ": " + MemberArraysProperty.getInt64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInt64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedInt64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.println("Array Item Array Val " + ix + ": " + MemberArraysProperty.getInt64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInt64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedInt64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInt64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedInt64Member(EO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + MemberArraysProperty.getInt64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInt64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedInt64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInt64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedInt64Member(AEO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + MemberArraysProperty.getInt64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInt64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedInt64Member(AEO, ix);
        }

        // setNull test
        MT1.int64Member().setNull();
        MemberTypesProperty.setNullInt64Member(MT2);
        MA1.int64Member().get(1).setNull();
        MemberArraysProperty.setNullInt64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.int64Member().isNull() && MemberTypesProperty.isNullInt64Member(MT2)
                && MA1.int64Member().get(1).isNull() && MemberArraysProperty.isNullInt64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // test_Int64

    private static void test_Float32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Float32");
        System.out.println("MemberId: " + MemberTypes.getFloat32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getFloat32MemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getFloat32ParameterArraySize() == 2 && MemberArrays.getFloat32MemberArraySize() == 2
                        && MemberArraysProperty.getFloat32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.float32Member().isNull();
        In_Req_Ok = !MT1.float32Member().isChanged();
        MT1.float32Member().setVal(ParameterTypes.getFloat32Parameter());
        Null_Ok = Null_Ok && !MT1.float32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.float32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getFloat32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getFloat32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getFloat32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Float32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Float32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Float32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullFloat32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedFloat32Member(MT2);
        MemberTypesProperty.setFloat32Member(MT2, MT1.float32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getFloat32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullFloat32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedFloat32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullFloat32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedFloat32Member(MI);
        MemberTypesProperty.setFloat32Member(MI, MT2.float32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getFloat32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullFloat32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedFloat32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullFloat32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedFloat32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setFloat32Member(MIA, MT2.float32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().float32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullFloat32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedFloat32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullFloat32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedFloat32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getFloat32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullFloat32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedFloat32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullFloat32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedFloat32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getFloat32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullFloat32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedFloat32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getFloat32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.float32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.float32Member().get(ix).isChanged();
            MA1.float32Member().get(ix).setVal(ParameterArrays.getFloat32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.float32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.float32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullFloat32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedFloat32Member(MA2, ix);
            MA2.float32Member().get(ix).setVal(MA1.float32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullFloat32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedFloat32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getFloat32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.float32Member().get(ix).setVal(MA1.float32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getFloat32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullFloat32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedFloat32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getFloat32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullFloat32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedFloat32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullFloat32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedFloat32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getFloat32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullFloat32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedFloat32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullFloat32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedFloat32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getFloat32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullFloat32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedFloat32Member(AEO, ix);
        }

        // setNull test
        MT1.float32Member().setNull();
        MemberTypesProperty.setNullFloat32Member(MT2);
        MA1.float32Member().get(1).setNull();
        MemberArraysProperty.setNullFloat32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.float32Member().isNull() && MemberTypesProperty.isNullFloat32Member(MT2)
                && MA1.float32Member().get(1).isNull() && MemberArraysProperty.isNullFloat32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Float32

    private static void test_Float64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Float64");
        System.out.println("MemberId: " + MemberTypes.getFloat64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getFloat64MemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getFloat64ParameterArraySize() == 2 && MemberArrays.getFloat64MemberArraySize() == 2
                        && MemberArraysProperty.getFloat64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.float64Member().isNull();
        In_Req_Ok = !MT1.float64Member().isChanged();
        MT1.float64Member().setVal(ParameterTypes.getFloat64Parameter());
        Null_Ok = Null_Ok && !MT1.float64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.float64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getFloat64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getFloat64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getFloat64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Float64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Float64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Float64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullFloat64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedFloat64Member(MT2);
        MemberTypesProperty.setFloat64Member(MT2, MT1.float64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getFloat64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullFloat64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedFloat64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullFloat64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedFloat64Member(MI);
        MemberTypesProperty.setFloat64Member(MI, MT2.float64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getFloat64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullFloat64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedFloat64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullFloat64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedFloat64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setFloat64Member(MIA, MT2.float64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().float64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullFloat64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedFloat64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullFloat64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedFloat64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getFloat64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullFloat64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedFloat64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullFloat64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedFloat64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getFloat64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullFloat64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedFloat64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getFloat64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.float64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.float64Member().get(ix).isChanged();
            MA1.float64Member().get(ix).setVal(ParameterArrays.getFloat64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.float64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.float64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullFloat64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedFloat64Member(MA2, ix);
            MA2.float64Member().get(ix).setVal(MA1.float64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullFloat64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedFloat64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getFloat64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.float64Member().get(ix).setVal(MA1.float64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getFloat64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullFloat64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedFloat64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getFloat64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullFloat64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedFloat64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullFloat64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedFloat64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getFloat64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullFloat64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedFloat64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullFloat64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedFloat64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getFloat64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullFloat64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedFloat64Member(AEO, ix);
        }

        // setNull test
        MT1.float64Member().setNull();
        MemberTypesProperty.setNullFloat64Member(MT2);
        MA1.float64Member().get(1).setNull();
        MemberArraysProperty.setNullFloat64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.float64Member().isNull() && MemberTypesProperty.isNullFloat64Member(MT2)
                && MA1.float64Member().get(1).isNull() && MemberArraysProperty.isNullFloat64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Float64

    private static void test_Boolean() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Boolean");
        System.out.println("MemberId: " + MemberTypes.getBooleanMemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getBooleanMemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getBooleanParameterArraySize() == 2 && MemberArrays.getBooleanMemberArraySize() == 2
                        && MemberArraysProperty.getBooleanMemberArraySize(MA1) == 2));
        Null_Ok = MT1.booleanMember().isNull();
        In_Req_Ok = !MT1.booleanMember().isChanged();
        MT1.booleanMember().setVal(ParameterTypes.getBooleanParameter());
        Null_Ok = Null_Ok && !MT1.booleanMember().isNull();
        In_Req_Ok = In_Req_Ok && MT1.booleanMember().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getBooleanMemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getBooleanMemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getBooleanMemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "BooleanParameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "BooleanParameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "BooleanParameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullBooleanMember(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedBooleanMember(MT2);
        MemberTypesProperty.setBooleanMember(MT2, MT1.booleanMember().getVal());
        System.out.println("Val: " + MemberTypesProperty.getBooleanMember(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullBooleanMember(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedBooleanMember(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullBooleanMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedBooleanMember(MI);
        MemberTypesProperty.setBooleanMember(MI, MT2.booleanMember().getVal());
        System.out.println("Item Val: " + MemberTypesProperty.getBooleanMember(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullBooleanMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedBooleanMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullBooleanMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedBooleanMember(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setBooleanMember(MIA, MT2.booleanMember().getVal());
        System.out.println("Item Array Val: " + MIA.typesItemArray().get(1).getObj().booleanMember().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullBooleanMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedBooleanMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullBooleanMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedBooleanMember(EO);
        System.out.println("Property Parameter Val: " + MemberTypesProperty.getBooleanMember(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullBooleanMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedBooleanMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullBooleanMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedBooleanMember(AEO);
        System.out.println("Property Parameter Val: " + MemberTypesProperty.getBooleanMember(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullBooleanMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedBooleanMember(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getBooleanParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.booleanMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.booleanMember().get(ix).isChanged();
            MA1.booleanMember().get(ix).setVal(ParameterArrays.getBooleanParameter(ix));
            Null_Ok = Null_Ok && !MA1.booleanMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.booleanMember().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullBooleanMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedBooleanMember(MA2, ix);
            MA2.booleanMember().get(ix).setVal(MA1.booleanMember().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullBooleanMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedBooleanMember(MA2, ix);

            System.out.println("Val " + ix + ": " + MemberArraysProperty.getBooleanMember(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.booleanMember().get(ix).setVal(MA1.booleanMember().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.println("Array Item Val " + ix + ": " + MemberArraysProperty.getBooleanMember(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullBooleanMember(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedBooleanMember(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.println("Array Item Array Val " + ix + ": " + MemberArraysProperty.getBooleanMember(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullBooleanMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedBooleanMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullBooleanMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedBooleanMember(EO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + MemberArraysProperty.getBooleanMember(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullBooleanMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedBooleanMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullBooleanMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedBooleanMember(AEO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + MemberArraysProperty.getBooleanMember(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullBooleanMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedBooleanMember(AEO, ix);
        }

        // setNull test
        MT1.booleanMember().setNull();
        MemberTypesProperty.setNullBooleanMember(MT2);
        MA1.booleanMember().get(1).setNull();
        MemberArraysProperty.setNullBooleanMember(MA2, 1);
        Null_Ok = Null_Ok && MT1.booleanMember().isNull() && MemberTypesProperty.isNullBooleanMember(MT2)
                && MA1.booleanMember().get(1).isNull() && MemberArraysProperty.isNullBooleanMember(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Boolean

    private static String toStringNoUnderscore(TestEnum val) {
        return val.toDouString();
    }

    private static void test_Enumeration() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Enumeration");
        System.out.println("MemberId: " + MemberTypes.getEnumerationMemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getEnumerationMemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getEnumerationParameterArraySize() == 2
                && MemberArrays.getEnumerationMemberArraySize() == 2
                && MemberArraysProperty.getEnumerationMemberArraySize(MA1) == 2));
        Null_Ok = MT1.enumerationMember().isNull();
        In_Req_Ok = !MT1.enumerationMember().isChanged();
        MT1.enumerationMember().setVal(ParameterTypes.getEnumerationParameter());
        Null_Ok = Null_Ok && !MT1.enumerationMember().isNull();
        In_Req_Ok = In_Req_Ok && MT1.enumerationMember().isChanged();
        TestEnum[] enumVals = TestEnum.values();

        System.out.println("First: " + enumVals[0].ordinal());
        System.out.println("Last: " + enumVals[enumVals.length - 1].ordinal());
        System.out.println("Size: " + enumVals.length);
        System.out.println("Test ToString (0): " + TestEnum.MY_FIRST.toDouString());
        System.out.println("Test ToValue (MySecond): " + TestEnum.valueOf("MY_SECOND").ordinal());

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getEnumerationMemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getEnumerationMemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getEnumerationMemberMemberIndex()));
        System.out.println("GetTypeId: "
                + Members.getTypeId(MemberTypes.ClassTypeId, MemberTypes.getEnumerationMemberMemberIndex()));
        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "EnumerationParameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "EnumerationParameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "EnumerationParameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullEnumerationMember(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedEnumerationMember(MT2);
        MemberTypesProperty.setEnumerationMember(MT2, MT1.enumerationMember().getVal());
        System.out.println("Val: " + toStringNoUnderscore(MemberTypesProperty.getEnumerationMember(MT2)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullEnumerationMember(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedEnumerationMember(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullEnumerationMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedEnumerationMember(MI);
        MemberTypesProperty.setEnumerationMember(MI, MT2.enumerationMember().getVal());
        System.out.println("Item Val: " + toStringNoUnderscore(MemberTypesProperty.getEnumerationMember(MI)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullEnumerationMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedEnumerationMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullEnumerationMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedEnumerationMember(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setEnumerationMember(MIA, MT2.enumerationMember().getVal());
        System.out.println("Item Array Val: "
                + toStringNoUnderscore(MIA.typesItemArray().get(1).getObj().enumerationMember().getVal()));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullEnumerationMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedEnumerationMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullEnumerationMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedEnumerationMember(EO);
        System.out.println(
                "Property Parameter Val: " + toStringNoUnderscore(MemberTypesProperty.getEnumerationMember(EO)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullEnumerationMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullEnumerationMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedEnumerationMember(AEO);
        System.out.println(
                "Property Parameter Val: " + toStringNoUnderscore(MemberTypesProperty.getEnumerationMember(AEO)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullEnumerationMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedEnumerationMember(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getEnumerationParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.enumerationMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.enumerationMember().get(ix).isChanged();
            MA1.enumerationMember().get(ix).setVal(ParameterArrays.getEnumerationParameter(ix));
            Null_Ok = Null_Ok && !MA1.enumerationMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.enumerationMember().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullEnumerationMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedEnumerationMember(MA2, ix);
            MA2.enumerationMember().get(ix).setVal(MA1.enumerationMember().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullEnumerationMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedEnumerationMember(MA2, ix);

            System.out.println(
                    "Val " + ix + ": " + toStringNoUnderscore(MemberArraysProperty.getEnumerationMember(MA2, ix)));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.enumerationMember().get(ix).setVal(MA1.enumerationMember().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.println("Array Item Val " + ix + ": "
                    + toStringNoUnderscore(MemberArraysProperty.getEnumerationMember(MI, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullEnumerationMember(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedEnumerationMember(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.println("Array Item Array Val " + ix + ": "
                    + toStringNoUnderscore(MemberArraysProperty.getEnumerationMember(MIA, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullEnumerationMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedEnumerationMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullEnumerationMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedEnumerationMember(EO, ix);
            System.out.println("Parameter Array Val " + ix + ": "
                    + toStringNoUnderscore(MemberArraysProperty.getEnumerationMember(EO, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullEnumerationMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedEnumerationMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullEnumerationMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedEnumerationMember(AEO, ix);
            System.out.println("Parameter Array Val " + ix + ": "
                    + toStringNoUnderscore(MemberArraysProperty.getEnumerationMember(AEO, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullEnumerationMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedEnumerationMember(AEO, ix);
        }

        // setNull test
        MT1.enumerationMember().setNull();
        MemberTypesProperty.setNullEnumerationMember(MT2);
        MA1.enumerationMember().get(1).setNull();
        MemberArraysProperty.setNullEnumerationMember(MA2, 1);
        Null_Ok = Null_Ok && MT1.enumerationMember().isNull() && MemberTypesProperty.isNullEnumerationMember(MT2)
                && MA1.enumerationMember().get(1).isNull() && MemberArraysProperty.isNullEnumerationMember(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Enumeration

    private static String checkString(String str) {
        return checkString(str, 0);
    }

    private static String checkString(String str, int index) {
        boolean correct = false;
        if (index == 0) {
            if (str.equals("Safir\u00AE")) {
                correct = true;
            }
        }
        if (index == 1) {
            if (str.equals("\u00AErifaS")) {
                correct = true;
            }
        }
        if (correct) {
            return "<correct>";
        } else {
            return "<INCORRECT!>";
        }
    }

    private static void test_String() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("String");
        System.out.println("MemberId: " + MemberTypes.getStringMemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getStringMemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getStringParameterArraySize() == 2 && MemberArrays.getStringMemberArraySize() == 2
                        && MemberArraysProperty.getStringMemberArraySize(MA1) == 2));
        System.out.println("MaxStringLength Size Ok (10): " + (MemberTypes.getStringMemberMaxStringLength() == 10
                && MemberArrays.getStringMemberMaxStringLength() == 10));
        Null_Ok = MT1.stringMember().isNull();
        In_Req_Ok = !MT1.stringMember().isChanged();
        MT1.stringMember().setVal(ParameterTypes.getStringParameter());
        Null_Ok = Null_Ok && !MT1.stringMember().isNull();
        In_Req_Ok = In_Req_Ok && MT1.stringMember().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getStringMemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getStringMemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getStringMemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "StringParameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "StringParameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "StringParameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullStringMember(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedStringMember(MT2);
        MemberTypesProperty.setStringMember(MT2, MT1.stringMember().getVal());

        System.out.println("Val: " + checkString(MemberTypesProperty.getStringMember(MT2)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullStringMember(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedStringMember(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullStringMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedStringMember(MI);
        MemberTypesProperty.setStringMember(MI, MT2.stringMember().getVal());
        System.out.println("Item Val: " + checkString(MemberTypesProperty.getStringMember(MI)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullStringMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedStringMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullStringMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedStringMember(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setStringMember(MIA, MT2.stringMember().getVal());
        System.out.println(
                "Item Array Val: " + checkString(MIA.typesItemArray().get(1).getObj().stringMember().getVal()));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullStringMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedStringMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullStringMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedStringMember(EO);
        System.out.println("Property Parameter Val: " + MemberTypesProperty.getStringMember(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullStringMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedStringMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullStringMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedStringMember(AEO);
        System.out.println("Property Parameter Val: " + checkString(MemberTypesProperty.getStringMember(AEO)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullStringMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedStringMember(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getStringParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.stringMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.stringMember().get(ix).isChanged();
            MA1.stringMember().get(ix).setVal(ParameterArrays.getStringParameter(ix));
            Null_Ok = Null_Ok && !MA1.stringMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.stringMember().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullStringMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedStringMember(MA2, ix);
            MA2.stringMember().get(ix).setVal(MA1.stringMember().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullStringMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedStringMember(MA2, ix);

            System.out.println("Val " + ix + ": " + checkString(MemberArraysProperty.getStringMember(MA2, ix), ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.stringMember().get(ix).setVal(MA1.stringMember().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.println(
                    "Array Item Val " + ix + ": " + checkString(MemberArraysProperty.getStringMember(MI, ix), ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullStringMember(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedStringMember(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.println("Array Item Array Val " + ix + ": "
                    + checkString(MemberArraysProperty.getStringMember(MIA, ix), ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullStringMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedStringMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullStringMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedStringMember(EO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + MemberArraysProperty.getStringMember(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullStringMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedStringMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullStringMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedStringMember(AEO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + checkString(MemberArraysProperty.getStringMember(AEO, ix),ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullStringMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedStringMember(AEO, ix);
        }

        // setNull test
        MT1.stringMember().setNull();
        MemberTypesProperty.setNullStringMember(MT2);
        MA1.stringMember().get(1).setNull();
        MemberArraysProperty.setNullStringMember(MA2, 1);
        Null_Ok = Null_Ok && MT1.stringMember().isNull() && MemberTypesProperty.isNullStringMember(MT2)
                && MA1.stringMember().get(1).isNull() && MemberArraysProperty.isNullStringMember(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // String

    private static void test_EntityId() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("EntityId");
        System.out.println("MemberId: " + MemberTypes.getEntityIdMemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getEntityIdMemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getEntityIdParameterArraySize() == 2
                && MemberArrays.getEntityIdMemberArraySize() == 2
                && MemberArraysProperty.getEntityIdMemberArraySize(MA1) == 2));
        Null_Ok = MT1.entityIdMember().isNull();
        In_Req_Ok = !MT1.entityIdMember().isChanged();
        MT1.entityIdMember().setVal(ParameterTypes.getEntityIdParameter());
        Null_Ok = Null_Ok && !MT1.entityIdMember().isNull();
        In_Req_Ok = In_Req_Ok && MT1.entityIdMember().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getEntityIdMemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getEntityIdMemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getEntityIdMemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "EntityIdParameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "EntityIdParameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "EntityIdParameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullEntityIdMember(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedEntityIdMember(MT2);
        MemberTypesProperty.setEntityIdMember(MT2, MT1.entityIdMember().getVal());
        System.out.println("Val: " + MemberTypesProperty.getEntityIdMember(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullEntityIdMember(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedEntityIdMember(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullEntityIdMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedEntityIdMember(MI);
        MemberTypesProperty.setEntityIdMember(MI, MT2.entityIdMember().getVal());
        System.out.println("Item Val: " + MemberTypesProperty.getEntityIdMember(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullEntityIdMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedEntityIdMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullEntityIdMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedEntityIdMember(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setEntityIdMember(MIA, MT2.entityIdMember().getVal());
        System.out.println("Item Array Val:" + MIA.typesItemArray().get(1).getObj().entityIdMember().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullEntityIdMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedEntityIdMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullEntityIdMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedEntityIdMember(EO);
        System.out.println("Property Parameter Val: " + MemberTypesProperty.getEntityIdMember(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullEntityIdMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedEntityIdMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullEntityIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedEntityIdMember(AEO);
        System.out.println("Property Parameter Val: " + MemberTypesProperty.getEntityIdMember(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullEntityIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedEntityIdMember(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getEntityIdParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.entityIdMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.entityIdMember().get(ix).isChanged();
            MA1.entityIdMember().get(ix).setVal(ParameterArrays.getEntityIdParameter(ix));
            Null_Ok = Null_Ok && !MA1.entityIdMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.entityIdMember().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullEntityIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedEntityIdMember(MA2, ix);
            MA2.entityIdMember().get(ix).setVal(MA1.entityIdMember().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullEntityIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedEntityIdMember(MA2, ix);

            System.out.println("Val " + ix + ": " + MemberArraysProperty.getEntityIdMember(MA2, ix));

            ArraysItem item = new ArraysItem();
            item.entityIdMember().get(ix).setVal(MA1.entityIdMember().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.println("Array Item Val " + ix + ": " + MemberArraysProperty.getEntityIdMember(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullEntityIdMember(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedEntityIdMember(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.println("Array Item Array Val " + ix + ": " + MemberArraysProperty.getEntityIdMember(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullEntityIdMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedEntityIdMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullEntityIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedEntityIdMember(EO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + MemberArraysProperty.getEntityIdMember(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullEntityIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedEntityIdMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullEntityIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedEntityIdMember(AEO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + MemberArraysProperty.getEntityIdMember(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullEntityIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedEntityIdMember(AEO, ix);
        }

        // setNull test
        MT1.entityIdMember().setNull();
        MemberTypesProperty.setNullEntityIdMember(MT2);
        MA1.entityIdMember().get(1).setNull();
        MemberArraysProperty.setNullEntityIdMember(MA2, 1);
        Null_Ok = Null_Ok && MT1.entityIdMember().isNull() && MemberTypesProperty.isNullEntityIdMember(MT2)
                && MA1.entityIdMember().get(1).isNull() && MemberArraysProperty.isNullEntityIdMember(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // EntityId

    private static void test_TypeId() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("TypeId");
        System.out.println("MemberId: " + MemberTypes.getTypeIdMemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getTypeIdMemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getTypeIdParameterArraySize() == 2 && MemberArrays.getTypeIdMemberArraySize() == 2
                        && MemberArraysProperty.getTypeIdMemberArraySize(MA1) == 2));
        Null_Ok = MT1.typeIdMember().isNull();
        In_Req_Ok = !MT1.typeIdMember().isChanged();
        MT1.typeIdMember().setVal(ParameterTypes.getTypeIdParameter());
        Null_Ok = Null_Ok && !MT1.typeIdMember().isNull();
        In_Req_Ok = In_Req_Ok && MT1.typeIdMember().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getTypeIdMemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getTypeIdMemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getTypeIdMemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "TypeIdParameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "TypeIdParameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "TypeIdParameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullTypeIdMember(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedTypeIdMember(MT2);
        MemberTypesProperty.setTypeIdMember(MT2, MT1.typeIdMember().getVal());
        System.out.println("Val: " + Operations.getName(MemberTypesProperty.getTypeIdMember(MT2)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullTypeIdMember(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedTypeIdMember(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullTypeIdMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedTypeIdMember(MI);
        MemberTypesProperty.setTypeIdMember(MI, MT2.typeIdMember().getVal());
        System.out.println("Item Val: " + Operations.getName(MemberTypesProperty.getTypeIdMember(MI)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullTypeIdMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedTypeIdMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullTypeIdMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedTypeIdMember(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setTypeIdMember(MIA, MT2.typeIdMember().getVal());
        System.out.println(
                "Item Array Val: " + Operations.getName(MIA.typesItemArray().get(1).getObj().typeIdMember().getVal()));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullTypeIdMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedTypeIdMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullTypeIdMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedTypeIdMember(EO);
        System.out.println("Property Parameter Val: " + Operations.getName(MemberTypesProperty.getTypeIdMember(EO)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullTypeIdMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedTypeIdMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullTypeIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedTypeIdMember(AEO);
        System.out.println("Property Parameter Val: " + Operations.getName(MemberTypesProperty.getTypeIdMember(AEO)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullTypeIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedTypeIdMember(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getTypeIdParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.typeIdMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.typeIdMember().get(ix).isChanged();
            MA1.typeIdMember().get(ix).setVal(ParameterArrays.getTypeIdParameter(ix));
            Null_Ok = Null_Ok && !MA1.typeIdMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.typeIdMember().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullTypeIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedTypeIdMember(MA2, ix);
            MA2.typeIdMember().get(ix).setVal(MA1.typeIdMember().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullTypeIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedTypeIdMember(MA2, ix);

            System.out.println("Val " + ix + ": " + Operations.getName(MemberArraysProperty.getTypeIdMember(MA2, ix)));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.typeIdMember().get(ix).setVal(MA1.typeIdMember().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.println(
                    "Array Item Val " + ix + ": " + Operations.getName(MemberArraysProperty.getTypeIdMember(MI, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullTypeIdMember(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedTypeIdMember(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.println("Array Item Array Val " + ix + ": "
                    + Operations.getName(MemberArraysProperty.getTypeIdMember(MIA, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullTypeIdMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedTypeIdMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullTypeIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedTypeIdMember(EO, ix);
            System.out.println("Parameter Array Val " + ix + ": "
                    + Operations.getName(MemberArraysProperty.getTypeIdMember(EO, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullTypeIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedTypeIdMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullTypeIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedTypeIdMember(AEO, ix);
            System.out.println("Parameter Array Val " + ix + ": "
                    + Operations.getName(MemberArraysProperty.getTypeIdMember(AEO, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullTypeIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedTypeIdMember(AEO, ix);
        }

        // setNull test
        MT1.typeIdMember().setNull();
        MemberTypesProperty.setNullTypeIdMember(MT2);
        MA1.typeIdMember().get(1).setNull();
        MemberArraysProperty.setNullTypeIdMember(MA2, 1);
        Null_Ok = Null_Ok && MT1.typeIdMember().isNull() && MemberTypesProperty.isNullTypeIdMember(MT2)
                && MA1.typeIdMember().get(1).isNull() && MemberArraysProperty.isNullTypeIdMember(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // TypeId

    private static void test_InstanceId() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("InstanceId");
        System.out.println("MemberId: " + MemberTypes.getInstanceIdMemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getInstanceIdMemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getInstanceIdParameterArraySize() == 2
                && MemberArrays.getInstanceIdMemberArraySize() == 2
                && MemberArraysProperty.getInstanceIdMemberArraySize(MA1) == 2));
        Null_Ok = MT1.instanceIdMember().isNull();
        In_Req_Ok = !MT1.instanceIdMember().isChanged();
        MT1.instanceIdMember().setVal(ParameterTypes.getInstanceIdParameter());
        Null_Ok = Null_Ok && !MT1.instanceIdMember().isNull();
        In_Req_Ok = In_Req_Ok && MT1.instanceIdMember().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getInstanceIdMemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getInstanceIdMemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getInstanceIdMemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "InstanceIdParameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "InstanceIdParameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "InstanceIdParameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullInstanceIdMember(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedInstanceIdMember(MT2);
        MemberTypesProperty.setInstanceIdMember(MT2, MT1.instanceIdMember().getVal());
        System.out.println("Val: " + MemberTypesProperty.getInstanceIdMember(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInstanceIdMember(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedInstanceIdMember(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullInstanceIdMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedInstanceIdMember(MI);
        MemberTypesProperty.setInstanceIdMember(MI, MT2.instanceIdMember().getVal());
        System.out.println("Item Val: " + MemberTypesProperty.getInstanceIdMember(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInstanceIdMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedInstanceIdMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullInstanceIdMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedInstanceIdMember(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setInstanceIdMember(MIA, MT2.instanceIdMember().getVal());
        System.out.println("Item Array Val:" + MIA.typesItemArray().get(1).getObj().instanceIdMember().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInstanceIdMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedInstanceIdMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInstanceIdMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedInstanceIdMember(EO);
        System.out.println("Property Parameter Val: " + MemberTypesProperty.getInstanceIdMember(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInstanceIdMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedInstanceIdMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInstanceIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedInstanceIdMember(AEO);
        System.out.println("Property Parameter Val: " + MemberTypesProperty.getInstanceIdMember(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullInstanceIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedInstanceIdMember(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getInstanceIdParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.instanceIdMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.instanceIdMember().get(ix).isChanged();
            MA1.instanceIdMember().get(ix).setVal(ParameterArrays.getInstanceIdParameter(ix));
            Null_Ok = Null_Ok && !MA1.instanceIdMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.instanceIdMember().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullInstanceIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedInstanceIdMember(MA2, ix);
            MA2.instanceIdMember().get(ix).setVal(MA1.instanceIdMember().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInstanceIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedInstanceIdMember(MA2, ix);

            System.out.println("Val " + ix + ": " + MemberArraysProperty.getInstanceIdMember(MA2, ix));

            ArraysItem item = new ArraysItem();
            item.instanceIdMember().get(ix).setVal(MA1.instanceIdMember().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.println("Array Item Val " + ix + ": " + MemberArraysProperty.getInstanceIdMember(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInstanceIdMember(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedInstanceIdMember(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.println("Array Item Array Val " + ix + ": " + MemberArraysProperty.getInstanceIdMember(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInstanceIdMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedInstanceIdMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInstanceIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedInstanceIdMember(EO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + MemberArraysProperty.getInstanceIdMember(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInstanceIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedInstanceIdMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInstanceIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedInstanceIdMember(AEO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + MemberArraysProperty.getInstanceIdMember(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullInstanceIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedInstanceIdMember(AEO, ix);
        }

        // setNull test
        MT1.instanceIdMember().setNull();
        MemberTypesProperty.setNullInstanceIdMember(MT2);
        MA1.instanceIdMember().get(1).setNull();
        MemberArraysProperty.setNullInstanceIdMember(MA2, 1);
        Null_Ok = Null_Ok && MT1.instanceIdMember().isNull() && MemberTypesProperty.isNullInstanceIdMember(MT2)
                && MA1.instanceIdMember().get(1).isNull() && MemberArraysProperty.isNullInstanceIdMember(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // InstanceId

    private static void test_ChannelId() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("ChannelId");
        System.out.println("MemberId: " + MemberTypes.getChannelIdMemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getChannelIdMemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getChannelIdParameterArraySize() == 2
                && MemberArrays.getChannelIdMemberArraySize() == 2
                && MemberArraysProperty.getChannelIdMemberArraySize(MA1) == 2));
        Null_Ok = MT1.channelIdMember().isNull();
        In_Req_Ok = !MT1.channelIdMember().isChanged();
        MT1.channelIdMember().setVal(ParameterTypes.getChannelIdParameter());
        Null_Ok = Null_Ok && !MT1.channelIdMember().isNull();
        In_Req_Ok = In_Req_Ok && MT1.channelIdMember().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getChannelIdMemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getChannelIdMemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getChannelIdMemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "ChannelIdParameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "ChannelIdParameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "ChannelIdParameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullChannelIdMember(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedChannelIdMember(MT2);
        MemberTypesProperty.setChannelIdMember(MT2, MT1.channelIdMember().getVal());
        System.out.println("Val: " + MemberTypesProperty.getChannelIdMember(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullChannelIdMember(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedChannelIdMember(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullChannelIdMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedChannelIdMember(MI);
        MemberTypesProperty.setChannelIdMember(MI, MT2.channelIdMember().getVal());
        System.out.println("Item Val: " + MemberTypesProperty.getChannelIdMember(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullChannelIdMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedChannelIdMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullChannelIdMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedChannelIdMember(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setChannelIdMember(MIA, MT2.channelIdMember().getVal());
        System.out.println("Item Array Val:" + MIA.typesItemArray().get(1).getObj().channelIdMember().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullChannelIdMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedChannelIdMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullChannelIdMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedChannelIdMember(EO);
        System.out.println("Property Parameter Val: " + MemberTypesProperty.getChannelIdMember(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullChannelIdMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedChannelIdMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullChannelIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedChannelIdMember(AEO);
        System.out.println("Property Parameter Val: " + MemberTypesProperty.getChannelIdMember(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullChannelIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedChannelIdMember(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getChannelIdParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.channelIdMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.channelIdMember().get(ix).isChanged();
            MA1.channelIdMember().get(ix).setVal(ParameterArrays.getChannelIdParameter(ix));
            Null_Ok = Null_Ok && !MA1.channelIdMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.channelIdMember().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullChannelIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedChannelIdMember(MA2, ix);
            MA2.channelIdMember().get(ix).setVal(MA1.channelIdMember().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullChannelIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedChannelIdMember(MA2, ix);

            System.out.println("Val " + ix + ": " + MemberArraysProperty.getChannelIdMember(MA2, ix));

            ArraysItem item = new ArraysItem();
            item.channelIdMember().get(ix).setVal(MA1.channelIdMember().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.println("Array Item Val " + ix + ": " + MemberArraysProperty.getChannelIdMember(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullChannelIdMember(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedChannelIdMember(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.println("Array Item Array Val " + ix + ": " + MemberArraysProperty.getChannelIdMember(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullChannelIdMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedChannelIdMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullChannelIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedChannelIdMember(EO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + MemberArraysProperty.getChannelIdMember(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullChannelIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedChannelIdMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullChannelIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedChannelIdMember(AEO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + MemberArraysProperty.getChannelIdMember(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullChannelIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedChannelIdMember(AEO, ix);
        }

        // setNull test
        MT1.channelIdMember().setNull();
        MemberTypesProperty.setNullChannelIdMember(MT2);
        MA1.channelIdMember().get(1).setNull();
        MemberArraysProperty.setNullChannelIdMember(MA2, 1);
        Null_Ok = Null_Ok && MT1.channelIdMember().isNull() && MemberTypesProperty.isNullChannelIdMember(MT2)
                && MA1.channelIdMember().get(1).isNull() && MemberArraysProperty.isNullChannelIdMember(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // ChannelId

    private static void test_HandlerId() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("HandlerId");
        System.out.println("MemberId: " + MemberTypes.getHandlerIdMemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getHandlerIdMemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getHandlerIdParameterArraySize() == 2
                && MemberArrays.getHandlerIdMemberArraySize() == 2
                && MemberArraysProperty.getHandlerIdMemberArraySize(MA1) == 2));
        Null_Ok = MT1.handlerIdMember().isNull();
        In_Req_Ok = !MT1.handlerIdMember().isChanged();
        MT1.handlerIdMember().setVal(ParameterTypes.getHandlerIdParameter());
        Null_Ok = Null_Ok && !MT1.handlerIdMember().isNull();
        In_Req_Ok = In_Req_Ok && MT1.handlerIdMember().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getHandlerIdMemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getHandlerIdMemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getHandlerIdMemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "HandlerIdParameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "HandlerIdParameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "HandlerIdParameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullHandlerIdMember(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedHandlerIdMember(MT2);
        MemberTypesProperty.setHandlerIdMember(MT2, MT1.handlerIdMember().getVal());
        System.out.println("Val: " + MemberTypesProperty.getHandlerIdMember(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHandlerIdMember(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedHandlerIdMember(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullHandlerIdMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedHandlerIdMember(MI);
        MemberTypesProperty.setHandlerIdMember(MI, MT2.handlerIdMember().getVal());
        System.out.println("Item Val: " + MemberTypesProperty.getHandlerIdMember(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHandlerIdMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedHandlerIdMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullHandlerIdMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedHandlerIdMember(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setHandlerIdMember(MIA, MT2.handlerIdMember().getVal());
        System.out.println("Item Array Val:" + MIA.typesItemArray().get(1).getObj().handlerIdMember().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHandlerIdMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedHandlerIdMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHandlerIdMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedHandlerIdMember(EO);
        System.out.println("Property Parameter Val: " + MemberTypesProperty.getHandlerIdMember(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHandlerIdMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedHandlerIdMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHandlerIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedHandlerIdMember(AEO);
        System.out.println("Property Parameter Val: " + MemberTypesProperty.getHandlerIdMember(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHandlerIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedHandlerIdMember(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getHandlerIdParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.handlerIdMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.handlerIdMember().get(ix).isChanged();
            MA1.handlerIdMember().get(ix).setVal(ParameterArrays.getHandlerIdParameter(ix));
            Null_Ok = Null_Ok && !MA1.handlerIdMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.handlerIdMember().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullHandlerIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedHandlerIdMember(MA2, ix);
            MA2.handlerIdMember().get(ix).setVal(MA1.handlerIdMember().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHandlerIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedHandlerIdMember(MA2, ix);

            System.out.println("Val " + ix + ": " + MemberArraysProperty.getHandlerIdMember(MA2, ix));

            ArraysItem item = new ArraysItem();
            item.handlerIdMember().get(ix).setVal(MA1.handlerIdMember().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.println("Array Item Val " + ix + ": " + MemberArraysProperty.getHandlerIdMember(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHandlerIdMember(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedHandlerIdMember(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.println("Array Item Array Val " + ix + ": " + MemberArraysProperty.getHandlerIdMember(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHandlerIdMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedHandlerIdMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHandlerIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedHandlerIdMember(EO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + MemberArraysProperty.getHandlerIdMember(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHandlerIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedHandlerIdMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHandlerIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedHandlerIdMember(AEO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + MemberArraysProperty.getHandlerIdMember(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHandlerIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedHandlerIdMember(AEO, ix);
        }

        // setNull test
        MT1.handlerIdMember().setNull();
        MemberTypesProperty.setNullHandlerIdMember(MT2);
        MA1.handlerIdMember().get(1).setNull();
        MemberArraysProperty.setNullHandlerIdMember(MA2, 1);
        Null_Ok = Null_Ok && MT1.handlerIdMember().isNull() && MemberTypesProperty.isNullHandlerIdMember(MT2)
                && MA1.handlerIdMember().get(1).isNull() && MemberArraysProperty.isNullHandlerIdMember(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // HandlerId

    private static void test_Object() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Object");
        System.out.println("MemberId: " + MemberTypes.getObjectMemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getObjectMemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getObjectParameterArraySize() == 2 && MemberArrays.getObjectMemberArraySize() == 2
                        && MemberArraysProperty.getObjectMemberArraySize(MA1) == 2));
        Null_Ok = MT1.objectMember().isNull();
        In_Req_Ok = !MT1.objectMember().isChanged();
        MT1.objectMember().setObj(ParameterTypes.getObjectParameter());
        Null_Ok = Null_Ok && !MT1.objectMember().isNull();
        In_Req_Ok = In_Req_Ok && MT1.objectMember().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getObjectMemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getObjectMemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getObjectMemberMemberIndex()));
        System.out.println(
                "GetTypeId: " + Members.getTypeId(MemberTypes.ClassTypeId, MemberTypes.getObjectMemberMemberIndex()));
        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "ObjectParameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "ObjectParameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "ObjectParameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullObjectMember(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedObjectMember(MT2);
        MemberTypesProperty.setObjectMember(MT2, MT1.objectMember().getObj());
        System.out.println("Val: " + Serialization.toXml(MemberTypesProperty.getObjectMember(MT2)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullObjectMember(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedObjectMember(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullObjectMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedObjectMember(MI);
        MemberTypesProperty.setObjectMember(MI, MT2.objectMember().getObj());
        System.out.println("Item Val: " + Serialization.toXml(MemberTypesProperty.getObjectMember(MI)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullObjectMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedObjectMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullObjectMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedObjectMember(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setObjectMember(MIA, MT2.objectMember().getObj());
        System.out.println(
                "Item Array Val: " + Serialization.toXml(MIA.typesItemArray().get(1).getObj().objectMember().getObj()));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullObjectMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedObjectMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullObjectMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedObjectMember(EO);
        System.out.println("Property Parameter Val: " + Serialization.toXml(MemberTypesProperty.getObjectMember(EO)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullObjectMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedObjectMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullObjectMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedObjectMember(AEO);
        System.out.println("Property Parameter Val: " + Serialization.toXml(MemberTypesProperty.getObjectMember(AEO)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullObjectMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedObjectMember(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getObjectParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.objectMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.objectMember().get(ix).isChanged();
            MA1.objectMember().get(ix).setObj(ParameterArrays.getObjectParameter(ix));
            Null_Ok = Null_Ok && !MA1.objectMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.objectMember().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullObjectMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedObjectMember(MA2, ix);
            MA2.objectMember().get(ix).setObj(MA1.objectMember().get(ix).getObj());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullObjectMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedObjectMember(MA2, ix);

            System.out.println("Val " + ix + ": " + Serialization.toXml(MemberArraysProperty.getObjectMember(MA2, ix)));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.objectMember().get(ix).setObj(MA1.objectMember().get(ix).getObj());
            MI.arraysItem().setObj(item);
            System.out.println(
                    "Array Item Val " + ix + ": " + Serialization.toXml(MemberArraysProperty.getObjectMember(MI, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullObjectMember(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedObjectMember(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.println("Array Item Array Val " + ix + ": "
                    + Serialization.toXml(MemberArraysProperty.getObjectMember(MIA, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullObjectMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedObjectMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullObjectMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedObjectMember(EO, ix);
            System.out.println("Parameter Array Val " + ix + ": "
                    + Serialization.toXml(MemberArraysProperty.getObjectMember(EO, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullObjectMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedObjectMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullObjectMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedObjectMember(AEO, ix);
            System.out.println("Parameter Array Val " + ix + ": "
                    + Serialization.toXml(MemberArraysProperty.getObjectMember(AEO, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullObjectMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedObjectMember(AEO, ix);
        }

        // setNull test
        MT1.objectMember().setNull();
        MemberTypesProperty.setNullObjectMember(MT2);
        MA1.objectMember().get(1).setNull();
        MemberArraysProperty.setNullObjectMember(MA2, 1);
        Null_Ok = Null_Ok && MT1.objectMember().isNull() && MemberTypesProperty.isNullObjectMember(MT2)
                && MA1.objectMember().get(1).isNull() && MemberArraysProperty.isNullObjectMember(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Object

    private static void test_Binary() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Binary");
        System.out.println("MemberId: " + MemberTypes.getBinaryMemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getBinaryMemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getBinaryParameterArraySize() == 2 && MemberArrays.getBinaryMemberArraySize() == 2
                        && MemberArraysProperty.getBinaryMemberArraySize(MA1) == 2));
        Null_Ok = MT1.binaryMember().isNull();
        In_Req_Ok = !MT1.binaryMember().isChanged();
        MT1.binaryMember().setVal(ParameterTypes.getBinaryParameter());
        Null_Ok = Null_Ok && !MT1.binaryMember().isNull();
        In_Req_Ok = In_Req_Ok && MT1.binaryMember().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getBinaryMemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getBinaryMemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getBinaryMemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "BinaryParameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "BinaryParameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "BinaryParameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullBinaryMember(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedBinaryMember(MT2);
        MemberTypesProperty.setBinaryMember(MT2, MT1.binaryMember().getVal());
        System.out.println("Val: " + com.saabgroup.safir.dob.typesystem.Utilities
                .binaryToBase64(MemberTypesProperty.getBinaryMember(MT2)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullBinaryMember(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedBinaryMember(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullBinaryMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedBinaryMember(MI);
        MemberTypesProperty.setBinaryMember(MI, MT2.binaryMember().getVal());
        System.out.println("Item Val: "
                + com.saabgroup.safir.dob.typesystem.Utilities.binaryToBase64(MemberTypesProperty.getBinaryMember(MI)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullBinaryMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedBinaryMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullBinaryMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedBinaryMember(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setBinaryMember(MIA, MT2.binaryMember().getVal());
        System.out.println("Item Array Val: " + com.saabgroup.safir.dob.typesystem.Utilities
                .binaryToBase64(MIA.typesItemArray().get(1).getObj().binaryMember().getVal()));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullBinaryMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedBinaryMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullBinaryMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedBinaryMember(EO);
        System.out.println("Property Parameter Val: "
                + com.saabgroup.safir.dob.typesystem.Utilities.binaryToBase64(MemberTypesProperty.getBinaryMember(EO)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullBinaryMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedBinaryMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullBinaryMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedBinaryMember(AEO);
        System.out.println("Property Parameter Val: "
                + com.saabgroup.safir.dob.typesystem.Utilities.binaryToBase64(MemberTypesProperty.getBinaryMember(AEO)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullBinaryMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedBinaryMember(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getBinaryParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.binaryMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.binaryMember().get(ix).isChanged();
            MA1.binaryMember().get(ix).setVal(ParameterArrays.getBinaryParameter(ix));
            Null_Ok = Null_Ok && !MA1.binaryMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.binaryMember().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullBinaryMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedBinaryMember(MA2, ix);
            MA2.binaryMember().get(ix).setVal(MA1.binaryMember().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullBinaryMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedBinaryMember(MA2, ix);

            System.out.println("Val " + ix + ": " + com.saabgroup.safir.dob.typesystem.Utilities
                    .binaryToBase64(MemberArraysProperty.getBinaryMember(MA2, ix)));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.binaryMember().get(ix).setVal(MA1.binaryMember().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.println("Array Item Val " + ix + ": " + com.saabgroup.safir.dob.typesystem.Utilities
                    .binaryToBase64(MemberArraysProperty.getBinaryMember(MI, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullBinaryMember(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedBinaryMember(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.println("Array Item Array Val " + ix + ": " + com.saabgroup.safir.dob.typesystem.Utilities
                    .binaryToBase64(MemberArraysProperty.getBinaryMember(MIA, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullBinaryMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedBinaryMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullBinaryMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedBinaryMember(EO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + com.saabgroup.safir.dob.typesystem.Utilities
                    .binaryToBase64(MemberArraysProperty.getBinaryMember(EO, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullBinaryMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedBinaryMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullBinaryMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedBinaryMember(AEO, ix);
            System.out.println("Parameter Array Val " + ix + ": " + com.saabgroup.safir.dob.typesystem.Utilities
                    .binaryToBase64(MemberArraysProperty.getBinaryMember(AEO, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullBinaryMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedBinaryMember(AEO, ix);
        }

        // setNull test
        MT1.binaryMember().setNull();
        MemberTypesProperty.setNullBinaryMember(MT2);
        MA1.binaryMember().get(1).setNull();
        MemberArraysProperty.setNullBinaryMember(MA2, 1);
        Null_Ok = Null_Ok && MT1.binaryMember().isNull() && MemberTypesProperty.isNullBinaryMember(MT2)
                && MA1.binaryMember().get(1).isNull() && MemberArraysProperty.isNullBinaryMember(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Binary

    private static void test_TestClass() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("TestClass");
        System.out.println("MemberId: " + MemberTypes.getTestClassMemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getTestClassMemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getTestClassParameterArraySize() == 2
                && MemberArrays.getTestClassMemberArraySize() == 2
                && MemberArraysProperty.getTestClassMemberArraySize(MA1) == 2));
        Null_Ok = MT1.testClassMember().isNull();
        In_Req_Ok = !MT1.testClassMember().isChanged();
        MT1.testClassMember().setObj(ParameterTypes.getTestClassParameter());
        Null_Ok = Null_Ok && !MT1.testClassMember().isNull();
        In_Req_Ok = In_Req_Ok && MT1.testClassMember().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getTestClassMemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getTestClassMemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getTestClassMemberMemberIndex()));
        System.out.println("GetTypeId: "
                + Members.getTypeId(MemberTypes.ClassTypeId, MemberTypes.getTestClassMemberMemberIndex()));
        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "TestClassParameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "TestClassParameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "TestClassParameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullTestClassMember(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedTestClassMember(MT2);
        MemberTypesProperty.setTestClassMember(MT2, MT1.testClassMember().getObj());
        System.out.println("Val: " + Serialization.toXml(MemberTypesProperty.getTestClassMember(MT2)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullTestClassMember(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedTestClassMember(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullTestClassMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedTestClassMember(MI);
        MemberTypesProperty.setTestClassMember(MI, MT2.testClassMember().getObj());
        System.out.println("Item Val: " + Serialization.toXml(MemberTypesProperty.getTestClassMember(MI)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullTestClassMember(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedTestClassMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullTestClassMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedTestClassMember(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setTestClassMember(MIA, MT2.testClassMember().getObj());
        System.out.println("Item Array Val: "
                + Serialization.toXml(MIA.typesItemArray().get(1).getObj().testClassMember().getObj()));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullTestClassMember(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedTestClassMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullTestClassMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedTestClassMember(EO);
        System.out
                .println("Property Parameter Val: " + Serialization.toXml(MemberTypesProperty.getTestClassMember(EO)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullTestClassMember(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedTestClassMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullTestClassMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedTestClassMember(AEO);
        System.out
                .println("Property Parameter Val: " + Serialization.toXml(MemberTypesProperty.getTestClassMember(AEO)));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullTestClassMember(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedTestClassMember(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getTestClassParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.testClassMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.testClassMember().get(ix).isChanged();
            MA1.testClassMember().get(ix).setObj(ParameterArrays.getTestClassParameter(ix));
            Null_Ok = Null_Ok && !MA1.testClassMember().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.testClassMember().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullTestClassMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedTestClassMember(MA2, ix);
            MA2.testClassMember().get(ix).setObj(MA1.testClassMember().get(ix).getObj());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullTestClassMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedTestClassMember(MA2, ix);

            System.out.println(
                    "Val " + ix + ": " + Serialization.toXml(MemberArraysProperty.getTestClassMember(MA2, ix)));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.testClassMember().get(ix).setObj(MA1.testClassMember().get(ix).getObj());
            MI.arraysItem().setObj(item);
            System.out.println("Array Item Val " + ix + ": "
                    + Serialization.toXml(MemberArraysProperty.getTestClassMember(MI, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullTestClassMember(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedTestClassMember(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.println("Array Item Array Val " + ix + ": "
                    + Serialization.toXml(MemberArraysProperty.getTestClassMember(MIA, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullTestClassMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedTestClassMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullTestClassMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedTestClassMember(EO, ix);
            System.out.println("Parameter Array Val " + ix + ": "
                    + Serialization.toXml(MemberArraysProperty.getTestClassMember(EO, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullTestClassMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedTestClassMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullTestClassMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedTestClassMember(AEO, ix);
            System.out.println("Parameter Array Val " + ix + ": "
                    + Serialization.toXml(MemberArraysProperty.getTestClassMember(AEO, ix)));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullTestClassMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedTestClassMember(AEO, ix);
        }

        // setNull test
        MT1.testClassMember().setNull();
        MemberTypesProperty.setNullTestClassMember(MT2);
        MA1.testClassMember().get(1).setNull();
        MemberArraysProperty.setNullTestClassMember(MA2, 1);
        Null_Ok = Null_Ok && MT1.testClassMember().isNull() && MemberTypesProperty.isNullTestClassMember(MT2)
                && MA1.testClassMember().get(1).isNull() && MemberArraysProperty.isNullTestClassMember(MA2, 1);

        // Make some tests concerning Set/GetChangedHere
        MT1.testClassMember().setObj(ParameterTypes.getTestClassParameter());
        MT1.testClassMember().setChanged(false);
        MT1.testClassMember().getObj().myInt().setVal(3);
        In_Req_Ok = In_Req_Ok && MT1.testClassMember().isChanged();
        In_Req_Ok = In_Req_Ok && !MT1.testClassMember().isChangedHere();
        MT1.testClassMember().setChanged(false);
        MT1.testClassMember().setChangedHere(true);
        In_Req_Ok = In_Req_Ok && MT1.testClassMember().isChangedHere();
        In_Req_Ok = In_Req_Ok && !MT1.testClassMember().getObj().myInt().isChanged();

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // TestClass

    private static void test_Ampere32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Ampere32");
        System.out.println("MemberId: " + MemberTypes.getAmpere32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getAmpere32MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getAmpere32ParameterArraySize() == 2
                && MemberArrays.getAmpere32MemberArraySize() == 2
                && MemberArraysProperty.getAmpere32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.ampere32Member().isNull();
        In_Req_Ok = !MT1.ampere32Member().isChanged();
        MT1.ampere32Member().setVal(ParameterTypes.getAmpere32Parameter());
        Null_Ok = Null_Ok && !MT1.ampere32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.ampere32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getAmpere32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getAmpere32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getAmpere32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Ampere32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Ampere32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Ampere32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullAmpere32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedAmpere32Member(MT2);
        MemberTypesProperty.setAmpere32Member(MT2, MT1.ampere32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getAmpere32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullAmpere32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedAmpere32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullAmpere32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedAmpere32Member(MI);
        MemberTypesProperty.setAmpere32Member(MI, MT2.ampere32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getAmpere32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullAmpere32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedAmpere32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullAmpere32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedAmpere32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setAmpere32Member(MIA, MT2.ampere32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().ampere32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullAmpere32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedAmpere32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullAmpere32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedAmpere32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getAmpere32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullAmpere32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedAmpere32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullAmpere32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedAmpere32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getAmpere32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullAmpere32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedAmpere32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getAmpere32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.ampere32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.ampere32Member().get(ix).isChanged();
            MA1.ampere32Member().get(ix).setVal(ParameterArrays.getAmpere32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.ampere32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.ampere32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullAmpere32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedAmpere32Member(MA2, ix);
            MA2.ampere32Member().get(ix).setVal(MA1.ampere32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullAmpere32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedAmpere32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getAmpere32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.ampere32Member().get(ix).setVal(MA1.ampere32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getAmpere32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullAmpere32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedAmpere32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getAmpere32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullAmpere32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedAmpere32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullAmpere32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedAmpere32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getAmpere32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullAmpere32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedAmpere32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullAmpere32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedAmpere32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getAmpere32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullAmpere32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedAmpere32Member(AEO, ix);
        }

        // setNull test
        MT1.ampere32Member().setNull();
        MemberTypesProperty.setNullAmpere32Member(MT2);
        MA1.ampere32Member().get(1).setNull();
        MemberArraysProperty.setNullAmpere32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.ampere32Member().isNull() && MemberTypesProperty.isNullAmpere32Member(MT2)
                && MA1.ampere32Member().get(1).isNull() && MemberArraysProperty.isNullAmpere32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Ampere32

    private static void test_CubicMeter32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("CubicMeter32");
        System.out.println("MemberId: " + MemberTypes.getCubicMeter32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getCubicMeter32MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getCubicMeter32ParameterArraySize() == 2
                && MemberArrays.getCubicMeter32MemberArraySize() == 2
                && MemberArraysProperty.getCubicMeter32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.cubicMeter32Member().isNull();
        In_Req_Ok = !MT1.cubicMeter32Member().isChanged();
        MT1.cubicMeter32Member().setVal(ParameterTypes.getCubicMeter32Parameter());
        Null_Ok = Null_Ok && !MT1.cubicMeter32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.cubicMeter32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getCubicMeter32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getCubicMeter32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getCubicMeter32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "CubicMeter32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "CubicMeter32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "CubicMeter32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullCubicMeter32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedCubicMeter32Member(MT2);
        MemberTypesProperty.setCubicMeter32Member(MT2, MT1.cubicMeter32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getCubicMeter32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullCubicMeter32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedCubicMeter32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullCubicMeter32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedCubicMeter32Member(MI);
        MemberTypesProperty.setCubicMeter32Member(MI, MT2.cubicMeter32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getCubicMeter32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullCubicMeter32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedCubicMeter32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullCubicMeter32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedCubicMeter32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setCubicMeter32Member(MIA, MT2.cubicMeter32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().cubicMeter32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullCubicMeter32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedCubicMeter32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullCubicMeter32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedCubicMeter32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getCubicMeter32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullCubicMeter32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedCubicMeter32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullCubicMeter32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedCubicMeter32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getCubicMeter32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullCubicMeter32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedCubicMeter32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getCubicMeter32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.cubicMeter32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.cubicMeter32Member().get(ix).isChanged();
            MA1.cubicMeter32Member().get(ix).setVal(ParameterArrays.getCubicMeter32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.cubicMeter32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.cubicMeter32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullCubicMeter32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedCubicMeter32Member(MA2, ix);
            MA2.cubicMeter32Member().get(ix).setVal(MA1.cubicMeter32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullCubicMeter32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedCubicMeter32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getCubicMeter32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.cubicMeter32Member().get(ix).setVal(MA1.cubicMeter32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getCubicMeter32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullCubicMeter32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedCubicMeter32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getCubicMeter32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullCubicMeter32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedCubicMeter32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullCubicMeter32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedCubicMeter32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getCubicMeter32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullCubicMeter32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedCubicMeter32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullCubicMeter32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedCubicMeter32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getCubicMeter32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullCubicMeter32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedCubicMeter32Member(AEO, ix);
        }

        // setNull test
        MT1.cubicMeter32Member().setNull();
        MemberTypesProperty.setNullCubicMeter32Member(MT2);
        MA1.cubicMeter32Member().get(1).setNull();
        MemberArraysProperty.setNullCubicMeter32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.cubicMeter32Member().isNull() && MemberTypesProperty.isNullCubicMeter32Member(MT2)
                && MA1.cubicMeter32Member().get(1).isNull() && MemberArraysProperty.isNullCubicMeter32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // CubicMeter32

    private static void test_Hertz32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Hertz32");
        System.out.println("MemberId: " + MemberTypes.getHertz32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getHertz32MemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getHertz32ParameterArraySize() == 2 && MemberArrays.getHertz32MemberArraySize() == 2
                        && MemberArraysProperty.getHertz32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.hertz32Member().isNull();
        In_Req_Ok = !MT1.hertz32Member().isChanged();
        MT1.hertz32Member().setVal(ParameterTypes.getHertz32Parameter());
        Null_Ok = Null_Ok && !MT1.hertz32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.hertz32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getHertz32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getHertz32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getHertz32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Hertz32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Hertz32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Hertz32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullHertz32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedHertz32Member(MT2);
        MemberTypesProperty.setHertz32Member(MT2, MT1.hertz32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getHertz32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHertz32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedHertz32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullHertz32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedHertz32Member(MI);
        MemberTypesProperty.setHertz32Member(MI, MT2.hertz32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getHertz32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHertz32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedHertz32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullHertz32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedHertz32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setHertz32Member(MIA, MT2.hertz32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().hertz32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHertz32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedHertz32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHertz32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedHertz32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getHertz32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHertz32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedHertz32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHertz32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedHertz32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getHertz32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHertz32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedHertz32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getHertz32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.hertz32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.hertz32Member().get(ix).isChanged();
            MA1.hertz32Member().get(ix).setVal(ParameterArrays.getHertz32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.hertz32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.hertz32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullHertz32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedHertz32Member(MA2, ix);
            MA2.hertz32Member().get(ix).setVal(MA1.hertz32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHertz32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedHertz32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getHertz32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.hertz32Member().get(ix).setVal(MA1.hertz32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getHertz32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHertz32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedHertz32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getHertz32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHertz32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedHertz32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHertz32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedHertz32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getHertz32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHertz32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedHertz32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHertz32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedHertz32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getHertz32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHertz32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedHertz32Member(AEO, ix);
        }

        // setNull test
        MT1.hertz32Member().setNull();
        MemberTypesProperty.setNullHertz32Member(MT2);
        MA1.hertz32Member().get(1).setNull();
        MemberArraysProperty.setNullHertz32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.hertz32Member().isNull() && MemberTypesProperty.isNullHertz32Member(MT2)
                && MA1.hertz32Member().get(1).isNull() && MemberArraysProperty.isNullHertz32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Hertz32

    private static void test_Joule32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Joule32");
        System.out.println("MemberId: " + MemberTypes.getJoule32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getJoule32MemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getJoule32ParameterArraySize() == 2 && MemberArrays.getJoule32MemberArraySize() == 2
                        && MemberArraysProperty.getJoule32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.joule32Member().isNull();
        In_Req_Ok = !MT1.joule32Member().isChanged();
        MT1.joule32Member().setVal(ParameterTypes.getJoule32Parameter());
        Null_Ok = Null_Ok && !MT1.joule32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.joule32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getJoule32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getJoule32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getJoule32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Joule32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Joule32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Joule32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullJoule32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedJoule32Member(MT2);
        MemberTypesProperty.setJoule32Member(MT2, MT1.joule32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getJoule32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullJoule32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedJoule32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullJoule32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedJoule32Member(MI);
        MemberTypesProperty.setJoule32Member(MI, MT2.joule32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getJoule32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullJoule32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedJoule32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullJoule32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedJoule32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setJoule32Member(MIA, MT2.joule32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().joule32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullJoule32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedJoule32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullJoule32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedJoule32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getJoule32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullJoule32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedJoule32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullJoule32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedJoule32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getJoule32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullJoule32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedJoule32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getJoule32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.joule32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.joule32Member().get(ix).isChanged();
            MA1.joule32Member().get(ix).setVal(ParameterArrays.getJoule32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.joule32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.joule32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullJoule32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedJoule32Member(MA2, ix);
            MA2.joule32Member().get(ix).setVal(MA1.joule32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullJoule32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedJoule32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getJoule32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.joule32Member().get(ix).setVal(MA1.joule32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getJoule32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullJoule32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedJoule32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getJoule32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullJoule32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedJoule32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullJoule32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedJoule32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getJoule32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullJoule32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedJoule32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullJoule32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedJoule32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getJoule32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullJoule32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedJoule32Member(AEO, ix);
        }

        // setNull test
        MT1.joule32Member().setNull();
        MemberTypesProperty.setNullJoule32Member(MT2);
        MA1.joule32Member().get(1).setNull();
        MemberArraysProperty.setNullJoule32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.joule32Member().isNull() && MemberTypesProperty.isNullJoule32Member(MT2)
                && MA1.joule32Member().get(1).isNull() && MemberArraysProperty.isNullJoule32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Joule32

    private static void test_Kelvin32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Kelvin32");
        System.out.println("MemberId: " + MemberTypes.getKelvin32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getKelvin32MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getKelvin32ParameterArraySize() == 2
                && MemberArrays.getKelvin32MemberArraySize() == 2
                && MemberArraysProperty.getKelvin32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.kelvin32Member().isNull();
        In_Req_Ok = !MT1.kelvin32Member().isChanged();
        MT1.kelvin32Member().setVal(ParameterTypes.getKelvin32Parameter());
        Null_Ok = Null_Ok && !MT1.kelvin32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.kelvin32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getKelvin32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getKelvin32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getKelvin32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Kelvin32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Kelvin32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Kelvin32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullKelvin32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKelvin32Member(MT2);
        MemberTypesProperty.setKelvin32Member(MT2, MT1.kelvin32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getKelvin32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKelvin32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKelvin32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullKelvin32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKelvin32Member(MI);
        MemberTypesProperty.setKelvin32Member(MI, MT2.kelvin32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getKelvin32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKelvin32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKelvin32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullKelvin32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKelvin32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setKelvin32Member(MIA, MT2.kelvin32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().kelvin32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKelvin32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKelvin32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKelvin32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKelvin32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getKelvin32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKelvin32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKelvin32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKelvin32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKelvin32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getKelvin32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKelvin32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKelvin32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getKelvin32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.kelvin32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.kelvin32Member().get(ix).isChanged();
            MA1.kelvin32Member().get(ix).setVal(ParameterArrays.getKelvin32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.kelvin32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.kelvin32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullKelvin32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKelvin32Member(MA2, ix);
            MA2.kelvin32Member().get(ix).setVal(MA1.kelvin32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKelvin32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedKelvin32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getKelvin32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.kelvin32Member().get(ix).setVal(MA1.kelvin32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getKelvin32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKelvin32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedKelvin32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getKelvin32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKelvin32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedKelvin32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKelvin32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKelvin32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getKelvin32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKelvin32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKelvin32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKelvin32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKelvin32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getKelvin32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKelvin32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKelvin32Member(AEO, ix);
        }

        // setNull test
        MT1.kelvin32Member().setNull();
        MemberTypesProperty.setNullKelvin32Member(MT2);
        MA1.kelvin32Member().get(1).setNull();
        MemberArraysProperty.setNullKelvin32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.kelvin32Member().isNull() && MemberTypesProperty.isNullKelvin32Member(MT2)
                && MA1.kelvin32Member().get(1).isNull() && MemberArraysProperty.isNullKelvin32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Kelvin32

    private static void test_Kilogram32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Kilogram32");
        System.out.println("MemberId: " + MemberTypes.getKilogram32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getKilogram32MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getKilogram32ParameterArraySize() == 2
                && MemberArrays.getKilogram32MemberArraySize() == 2
                && MemberArraysProperty.getKilogram32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.kilogram32Member().isNull();
        In_Req_Ok = !MT1.kilogram32Member().isChanged();
        MT1.kilogram32Member().setVal(ParameterTypes.getKilogram32Parameter());
        Null_Ok = Null_Ok && !MT1.kilogram32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.kilogram32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getKilogram32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getKilogram32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getKilogram32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Kilogram32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Kilogram32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Kilogram32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullKilogram32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKilogram32Member(MT2);
        MemberTypesProperty.setKilogram32Member(MT2, MT1.kilogram32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getKilogram32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKilogram32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKilogram32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullKilogram32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKilogram32Member(MI);
        MemberTypesProperty.setKilogram32Member(MI, MT2.kilogram32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getKilogram32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKilogram32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKilogram32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullKilogram32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKilogram32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setKilogram32Member(MIA, MT2.kilogram32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().kilogram32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKilogram32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKilogram32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKilogram32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKilogram32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getKilogram32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKilogram32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKilogram32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKilogram32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKilogram32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getKilogram32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKilogram32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKilogram32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getKilogram32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.kilogram32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.kilogram32Member().get(ix).isChanged();
            MA1.kilogram32Member().get(ix).setVal(ParameterArrays.getKilogram32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.kilogram32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.kilogram32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullKilogram32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKilogram32Member(MA2, ix);
            MA2.kilogram32Member().get(ix).setVal(MA1.kilogram32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKilogram32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedKilogram32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getKilogram32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.kilogram32Member().get(ix).setVal(MA1.kilogram32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getKilogram32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKilogram32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedKilogram32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getKilogram32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKilogram32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedKilogram32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKilogram32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKilogram32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getKilogram32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKilogram32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKilogram32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKilogram32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKilogram32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getKilogram32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKilogram32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKilogram32Member(AEO, ix);
        }

        // setNull test
        MT1.kilogram32Member().setNull();
        MemberTypesProperty.setNullKilogram32Member(MT2);
        MA1.kilogram32Member().get(1).setNull();
        MemberArraysProperty.setNullKilogram32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.kilogram32Member().isNull() && MemberTypesProperty.isNullKilogram32Member(MT2)
                && MA1.kilogram32Member().get(1).isNull() && MemberArraysProperty.isNullKilogram32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Kilogram32

    private static void test_Meter32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Meter32");
        System.out.println("MemberId: " + MemberTypes.getMeter32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getMeter32MemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getMeter32ParameterArraySize() == 2 && MemberArrays.getMeter32MemberArraySize() == 2
                        && MemberArraysProperty.getMeter32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.meter32Member().isNull();
        In_Req_Ok = !MT1.meter32Member().isChanged();
        MT1.meter32Member().setVal(ParameterTypes.getMeter32Parameter());
        Null_Ok = Null_Ok && !MT1.meter32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.meter32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getMeter32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getMeter32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getMeter32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Meter32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Meter32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Meter32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeter32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeter32Member(MT2);
        MemberTypesProperty.setMeter32Member(MT2, MT1.meter32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getMeter32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeter32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeter32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeter32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeter32Member(MI);
        MemberTypesProperty.setMeter32Member(MI, MT2.meter32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getMeter32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeter32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeter32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeter32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeter32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setMeter32Member(MIA, MT2.meter32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().meter32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeter32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeter32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeter32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeter32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getMeter32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeter32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeter32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeter32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeter32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getMeter32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeter32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeter32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getMeter32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.meter32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.meter32Member().get(ix).isChanged();
            MA1.meter32Member().get(ix).setVal(ParameterArrays.getMeter32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.meter32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.meter32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullMeter32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeter32Member(MA2, ix);
            MA2.meter32Member().get(ix).setVal(MA1.meter32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeter32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeter32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getMeter32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.meter32Member().get(ix).setVal(MA1.meter32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeter32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeter32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeter32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeter32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeter32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeter32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeter32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeter32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getMeter32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeter32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeter32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeter32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeter32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeter32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeter32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeter32Member(AEO, ix);
        }

        // setNull test
        MT1.meter32Member().setNull();
        MemberTypesProperty.setNullMeter32Member(MT2);
        MA1.meter32Member().get(1).setNull();
        MemberArraysProperty.setNullMeter32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.meter32Member().isNull() && MemberTypesProperty.isNullMeter32Member(MT2)
                && MA1.meter32Member().get(1).isNull() && MemberArraysProperty.isNullMeter32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Meter32

    private static void test_MeterPerSecond32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("MeterPerSecond32");
        System.out.println("MemberId: " + MemberTypes.getMeterPerSecond32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getMeterPerSecond32MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getMeterPerSecond32ParameterArraySize() == 2
                && MemberArrays.getMeterPerSecond32MemberArraySize() == 2
                && MemberArraysProperty.getMeterPerSecond32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.meterPerSecond32Member().isNull();
        In_Req_Ok = !MT1.meterPerSecond32Member().isChanged();
        MT1.meterPerSecond32Member().setVal(ParameterTypes.getMeterPerSecond32Parameter());
        Null_Ok = Null_Ok && !MT1.meterPerSecond32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.meterPerSecond32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getMeterPerSecond32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println("GetName: "
                + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getMeterPerSecond32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getMeterPerSecond32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "MeterPerSecond32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "MeterPerSecond32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "MeterPerSecond32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeterPerSecond32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecond32Member(MT2);
        MemberTypesProperty.setMeterPerSecond32Member(MT2, MT1.meterPerSecond32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getMeterPerSecond32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecond32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecond32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeterPerSecond32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecond32Member(MI);
        MemberTypesProperty.setMeterPerSecond32Member(MI, MT2.meterPerSecond32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getMeterPerSecond32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecond32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecond32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeterPerSecond32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecond32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setMeterPerSecond32Member(MIA, MT2.meterPerSecond32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().meterPerSecond32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecond32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecond32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecond32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecond32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n",
                MemberTypesProperty.getMeterPerSecond32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecond32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecond32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecond32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecond32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n",
                MemberTypesProperty.getMeterPerSecond32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecond32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecond32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getMeterPerSecond32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.meterPerSecond32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.meterPerSecond32Member().get(ix).isChanged();
            MA1.meterPerSecond32Member().get(ix).setVal(ParameterArrays.getMeterPerSecond32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.meterPerSecond32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.meterPerSecond32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullMeterPerSecond32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecond32Member(MA2, ix);
            MA2.meterPerSecond32Member().get(ix).setVal(MA1.meterPerSecond32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecond32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeterPerSecond32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getMeterPerSecond32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.meterPerSecond32Member().get(ix).setVal(MA1.meterPerSecond32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeterPerSecond32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecond32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeterPerSecond32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeterPerSecond32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecond32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeterPerSecond32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecond32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecond32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getMeterPerSecond32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecond32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecond32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecond32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecond32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeterPerSecond32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecond32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecond32Member(AEO, ix);
        }

        // setNull test
        MT1.meterPerSecond32Member().setNull();
        MemberTypesProperty.setNullMeterPerSecond32Member(MT2);
        MA1.meterPerSecond32Member().get(1).setNull();
        MemberArraysProperty.setNullMeterPerSecond32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.meterPerSecond32Member().isNull()
                && MemberTypesProperty.isNullMeterPerSecond32Member(MT2) && MA1.meterPerSecond32Member().get(1).isNull()
                && MemberArraysProperty.isNullMeterPerSecond32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // MeterPerSecond32

    private static void test_MeterPerSecondSquared32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("MeterPerSecondSquared32");
        System.out.println("MemberId: " + MemberTypes.getMeterPerSecondSquared32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getMeterPerSecondSquared32MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getMeterPerSecondSquared32ParameterArraySize() == 2
                && MemberArrays.getMeterPerSecondSquared32MemberArraySize() == 2
                && MemberArraysProperty.getMeterPerSecondSquared32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.meterPerSecondSquared32Member().isNull();
        In_Req_Ok = !MT1.meterPerSecondSquared32Member().isChanged();
        MT1.meterPerSecondSquared32Member().setVal(ParameterTypes.getMeterPerSecondSquared32Parameter());
        Null_Ok = Null_Ok && !MT1.meterPerSecondSquared32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.meterPerSecondSquared32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId,
                MemberArrays.getMeterPerSecondSquared32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println("GetName: "
                + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getMeterPerSecondSquared32MemberMemberIndex()));
        System.out.println("GetTypeName: " + Members.getTypeName(MemberTypes.ClassTypeId,
                MemberTypes.getMeterPerSecondSquared32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "MeterPerSecondSquared32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "MeterPerSecondSquared32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "MeterPerSecondSquared32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeterPerSecondSquared32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecondSquared32Member(MT2);
        MemberTypesProperty.setMeterPerSecondSquared32Member(MT2, MT1.meterPerSecondSquared32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getMeterPerSecondSquared32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecondSquared32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecondSquared32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeterPerSecondSquared32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecondSquared32Member(MI);
        MemberTypesProperty.setMeterPerSecondSquared32Member(MI, MT2.meterPerSecondSquared32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getMeterPerSecondSquared32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecondSquared32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecondSquared32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeterPerSecondSquared32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecondSquared32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setMeterPerSecondSquared32Member(MIA, MT2.meterPerSecondSquared32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().meterPerSecondSquared32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecondSquared32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecondSquared32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecondSquared32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecondSquared32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n",
                MemberTypesProperty.getMeterPerSecondSquared32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecondSquared32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecondSquared32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecondSquared32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecondSquared32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n",
                MemberTypesProperty.getMeterPerSecondSquared32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecondSquared32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecondSquared32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getMeterPerSecondSquared32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.meterPerSecondSquared32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.meterPerSecondSquared32Member().get(ix).isChanged();
            MA1.meterPerSecondSquared32Member().get(ix).setVal(ParameterArrays.getMeterPerSecondSquared32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.meterPerSecondSquared32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.meterPerSecondSquared32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullMeterPerSecondSquared32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecondSquared32Member(MA2, ix);
            MA2.meterPerSecondSquared32Member().get(ix).setVal(MA1.meterPerSecondSquared32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecondSquared32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeterPerSecondSquared32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeterPerSecondSquared32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.meterPerSecondSquared32Member().get(ix).setVal(MA1.meterPerSecondSquared32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeterPerSecondSquared32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecondSquared32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeterPerSecondSquared32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeterPerSecondSquared32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecondSquared32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeterPerSecondSquared32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecondSquared32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecondSquared32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getMeterPerSecondSquared32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecondSquared32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecondSquared32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecondSquared32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecondSquared32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeterPerSecondSquared32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecondSquared32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecondSquared32Member(AEO, ix);
        }

        // setNull test
        MT1.meterPerSecondSquared32Member().setNull();
        MemberTypesProperty.setNullMeterPerSecondSquared32Member(MT2);
        MA1.meterPerSecondSquared32Member().get(1).setNull();
        MemberArraysProperty.setNullMeterPerSecondSquared32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.meterPerSecondSquared32Member().isNull()
                && MemberTypesProperty.isNullMeterPerSecondSquared32Member(MT2)
                && MA1.meterPerSecondSquared32Member().get(1).isNull()
                && MemberArraysProperty.isNullMeterPerSecondSquared32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // MeterPerSecondSquared32

    private static void test_Newton32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Newton32");
        System.out.println("MemberId: " + MemberTypes.getNewton32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getNewton32MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getNewton32ParameterArraySize() == 2
                && MemberArrays.getNewton32MemberArraySize() == 2
                && MemberArraysProperty.getNewton32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.newton32Member().isNull();
        In_Req_Ok = !MT1.newton32Member().isChanged();
        MT1.newton32Member().setVal(ParameterTypes.getNewton32Parameter());
        Null_Ok = Null_Ok && !MT1.newton32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.newton32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getNewton32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getNewton32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getNewton32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Newton32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Newton32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Newton32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullNewton32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedNewton32Member(MT2);
        MemberTypesProperty.setNewton32Member(MT2, MT1.newton32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getNewton32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullNewton32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedNewton32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullNewton32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedNewton32Member(MI);
        MemberTypesProperty.setNewton32Member(MI, MT2.newton32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getNewton32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullNewton32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedNewton32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullNewton32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedNewton32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setNewton32Member(MIA, MT2.newton32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().newton32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullNewton32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedNewton32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullNewton32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedNewton32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getNewton32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullNewton32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedNewton32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullNewton32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedNewton32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getNewton32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullNewton32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedNewton32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getNewton32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.newton32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.newton32Member().get(ix).isChanged();
            MA1.newton32Member().get(ix).setVal(ParameterArrays.getNewton32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.newton32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.newton32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullNewton32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedNewton32Member(MA2, ix);
            MA2.newton32Member().get(ix).setVal(MA1.newton32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullNewton32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedNewton32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getNewton32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.newton32Member().get(ix).setVal(MA1.newton32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getNewton32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullNewton32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedNewton32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getNewton32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullNewton32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedNewton32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullNewton32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedNewton32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getNewton32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullNewton32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedNewton32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullNewton32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedNewton32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getNewton32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullNewton32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedNewton32Member(AEO, ix);
        }

        // setNull test
        MT1.newton32Member().setNull();
        MemberTypesProperty.setNullNewton32Member(MT2);
        MA1.newton32Member().get(1).setNull();
        MemberArraysProperty.setNullNewton32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.newton32Member().isNull() && MemberTypesProperty.isNullNewton32Member(MT2)
                && MA1.newton32Member().get(1).isNull() && MemberArraysProperty.isNullNewton32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Newton32

    private static void test_Pascal32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Pascal32");
        System.out.println("MemberId: " + MemberTypes.getPascal32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getPascal32MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getPascal32ParameterArraySize() == 2
                && MemberArrays.getPascal32MemberArraySize() == 2
                && MemberArraysProperty.getPascal32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.pascal32Member().isNull();
        In_Req_Ok = !MT1.pascal32Member().isChanged();
        MT1.pascal32Member().setVal(ParameterTypes.getPascal32Parameter());
        Null_Ok = Null_Ok && !MT1.pascal32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.pascal32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getPascal32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getPascal32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getPascal32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Pascal32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Pascal32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Pascal32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullPascal32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedPascal32Member(MT2);
        MemberTypesProperty.setPascal32Member(MT2, MT1.pascal32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getPascal32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullPascal32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedPascal32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullPascal32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedPascal32Member(MI);
        MemberTypesProperty.setPascal32Member(MI, MT2.pascal32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getPascal32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullPascal32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedPascal32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullPascal32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedPascal32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setPascal32Member(MIA, MT2.pascal32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().pascal32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullPascal32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedPascal32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullPascal32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedPascal32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getPascal32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullPascal32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedPascal32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullPascal32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedPascal32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getPascal32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullPascal32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedPascal32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getPascal32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.pascal32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.pascal32Member().get(ix).isChanged();
            MA1.pascal32Member().get(ix).setVal(ParameterArrays.getPascal32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.pascal32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.pascal32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullPascal32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedPascal32Member(MA2, ix);
            MA2.pascal32Member().get(ix).setVal(MA1.pascal32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullPascal32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedPascal32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getPascal32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.pascal32Member().get(ix).setVal(MA1.pascal32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getPascal32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullPascal32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedPascal32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getPascal32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullPascal32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedPascal32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullPascal32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedPascal32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getPascal32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullPascal32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedPascal32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullPascal32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedPascal32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getPascal32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullPascal32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedPascal32Member(AEO, ix);
        }

        // setNull test
        MT1.pascal32Member().setNull();
        MemberTypesProperty.setNullPascal32Member(MT2);
        MA1.pascal32Member().get(1).setNull();
        MemberArraysProperty.setNullPascal32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.pascal32Member().isNull() && MemberTypesProperty.isNullPascal32Member(MT2)
                && MA1.pascal32Member().get(1).isNull() && MemberArraysProperty.isNullPascal32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Pascal32

    private static void test_Radian32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Radian32");
        System.out.println("MemberId: " + MemberTypes.getRadian32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getRadian32MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getRadian32ParameterArraySize() == 2
                && MemberArrays.getRadian32MemberArraySize() == 2
                && MemberArraysProperty.getRadian32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.radian32Member().isNull();
        In_Req_Ok = !MT1.radian32Member().isChanged();
        MT1.radian32Member().setVal(ParameterTypes.getRadian32Parameter());
        Null_Ok = Null_Ok && !MT1.radian32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.radian32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getRadian32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getRadian32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getRadian32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Radian32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Radian32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Radian32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadian32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadian32Member(MT2);
        MemberTypesProperty.setRadian32Member(MT2, MT1.radian32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getRadian32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadian32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadian32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadian32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadian32Member(MI);
        MemberTypesProperty.setRadian32Member(MI, MT2.radian32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getRadian32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadian32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadian32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadian32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadian32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setRadian32Member(MIA, MT2.radian32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().radian32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadian32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadian32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadian32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadian32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getRadian32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadian32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadian32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadian32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadian32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getRadian32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadian32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadian32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getRadian32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.radian32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.radian32Member().get(ix).isChanged();
            MA1.radian32Member().get(ix).setVal(ParameterArrays.getRadian32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.radian32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.radian32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullRadian32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadian32Member(MA2, ix);
            MA2.radian32Member().get(ix).setVal(MA1.radian32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadian32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadian32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getRadian32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.radian32Member().get(ix).setVal(MA1.radian32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadian32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadian32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadian32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadian32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadian32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadian32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadian32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadian32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getRadian32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadian32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadian32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadian32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadian32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadian32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadian32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadian32Member(AEO, ix);
        }

        // setNull test
        MT1.radian32Member().setNull();
        MemberTypesProperty.setNullRadian32Member(MT2);
        MA1.radian32Member().get(1).setNull();
        MemberArraysProperty.setNullRadian32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.radian32Member().isNull() && MemberTypesProperty.isNullRadian32Member(MT2)
                && MA1.radian32Member().get(1).isNull() && MemberArraysProperty.isNullRadian32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Radian32

    private static void test_RadianPerSecond32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("RadianPerSecond32");
        System.out.println("MemberId: " + MemberTypes.getRadianPerSecond32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getRadianPerSecond32MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getRadianPerSecond32ParameterArraySize() == 2
                && MemberArrays.getRadianPerSecond32MemberArraySize() == 2
                && MemberArraysProperty.getRadianPerSecond32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.radianPerSecond32Member().isNull();
        In_Req_Ok = !MT1.radianPerSecond32Member().isChanged();
        MT1.radianPerSecond32Member().setVal(ParameterTypes.getRadianPerSecond32Parameter());
        Null_Ok = Null_Ok && !MT1.radianPerSecond32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.radianPerSecond32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getRadianPerSecond32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println("GetName: "
                + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getRadianPerSecond32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getRadianPerSecond32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "RadianPerSecond32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "RadianPerSecond32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "RadianPerSecond32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadianPerSecond32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecond32Member(MT2);
        MemberTypesProperty.setRadianPerSecond32Member(MT2, MT1.radianPerSecond32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getRadianPerSecond32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecond32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecond32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadianPerSecond32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecond32Member(MI);
        MemberTypesProperty.setRadianPerSecond32Member(MI, MT2.radianPerSecond32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getRadianPerSecond32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecond32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecond32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadianPerSecond32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecond32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setRadianPerSecond32Member(MIA, MT2.radianPerSecond32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().radianPerSecond32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecond32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecond32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecond32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecond32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n",
                MemberTypesProperty.getRadianPerSecond32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecond32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecond32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecond32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecond32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n",
                MemberTypesProperty.getRadianPerSecond32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecond32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecond32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getRadianPerSecond32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.radianPerSecond32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.radianPerSecond32Member().get(ix).isChanged();
            MA1.radianPerSecond32Member().get(ix).setVal(ParameterArrays.getRadianPerSecond32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.radianPerSecond32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.radianPerSecond32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullRadianPerSecond32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecond32Member(MA2, ix);
            MA2.radianPerSecond32Member().get(ix).setVal(MA1.radianPerSecond32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecond32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadianPerSecond32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadianPerSecond32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.radianPerSecond32Member().get(ix).setVal(MA1.radianPerSecond32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadianPerSecond32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecond32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadianPerSecond32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadianPerSecond32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecond32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadianPerSecond32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecond32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecond32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getRadianPerSecond32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecond32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecond32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecond32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecond32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadianPerSecond32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecond32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecond32Member(AEO, ix);
        }

        // setNull test
        MT1.radianPerSecond32Member().setNull();
        MemberTypesProperty.setNullRadianPerSecond32Member(MT2);
        MA1.radianPerSecond32Member().get(1).setNull();
        MemberArraysProperty.setNullRadianPerSecond32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.radianPerSecond32Member().isNull()
                && MemberTypesProperty.isNullRadianPerSecond32Member(MT2)
                && MA1.radianPerSecond32Member().get(1).isNull()
                && MemberArraysProperty.isNullRadianPerSecond32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // RadianPerSecond32

    private static void test_RadianPerSecondSquared32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("RadianPerSecondSquared32");
        System.out.println("MemberId: " + MemberTypes.getRadianPerSecondSquared32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getRadianPerSecondSquared32MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getRadianPerSecondSquared32ParameterArraySize() == 2
                && MemberArrays.getRadianPerSecondSquared32MemberArraySize() == 2
                && MemberArraysProperty.getRadianPerSecondSquared32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.radianPerSecondSquared32Member().isNull();
        In_Req_Ok = !MT1.radianPerSecondSquared32Member().isChanged();
        MT1.radianPerSecondSquared32Member().setVal(ParameterTypes.getRadianPerSecondSquared32Parameter());
        Null_Ok = Null_Ok && !MT1.radianPerSecondSquared32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.radianPerSecondSquared32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId,
                MemberArrays.getRadianPerSecondSquared32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println("GetName: "
                + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getRadianPerSecondSquared32MemberMemberIndex()));
        System.out.println("GetTypeName: " + Members.getTypeName(MemberTypes.ClassTypeId,
                MemberTypes.getRadianPerSecondSquared32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "RadianPerSecondSquared32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "RadianPerSecondSquared32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "RadianPerSecondSquared32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadianPerSecondSquared32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecondSquared32Member(MT2);
        MemberTypesProperty.setRadianPerSecondSquared32Member(MT2, MT1.radianPerSecondSquared32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getRadianPerSecondSquared32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecondSquared32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecondSquared32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadianPerSecondSquared32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecondSquared32Member(MI);
        MemberTypesProperty.setRadianPerSecondSquared32Member(MI, MT2.radianPerSecondSquared32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getRadianPerSecondSquared32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecondSquared32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecondSquared32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadianPerSecondSquared32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecondSquared32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setRadianPerSecondSquared32Member(MIA, MT2.radianPerSecondSquared32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().radianPerSecondSquared32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecondSquared32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecondSquared32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecondSquared32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecondSquared32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n",
                MemberTypesProperty.getRadianPerSecondSquared32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecondSquared32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecondSquared32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecondSquared32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecondSquared32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n",
                MemberTypesProperty.getRadianPerSecondSquared32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecondSquared32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecondSquared32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getRadianPerSecondSquared32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.radianPerSecondSquared32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.radianPerSecondSquared32Member().get(ix).isChanged();
            MA1.radianPerSecondSquared32Member().get(ix)
                    .setVal(ParameterArrays.getRadianPerSecondSquared32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.radianPerSecondSquared32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.radianPerSecondSquared32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullRadianPerSecondSquared32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecondSquared32Member(MA2, ix);
            MA2.radianPerSecondSquared32Member().get(ix).setVal(MA1.radianPerSecondSquared32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecondSquared32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadianPerSecondSquared32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadianPerSecondSquared32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.radianPerSecondSquared32Member().get(ix).setVal(MA1.radianPerSecondSquared32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadianPerSecondSquared32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecondSquared32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadianPerSecondSquared32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadianPerSecondSquared32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecondSquared32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadianPerSecondSquared32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecondSquared32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecondSquared32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getRadianPerSecondSquared32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecondSquared32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecondSquared32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecondSquared32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecondSquared32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadianPerSecondSquared32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecondSquared32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecondSquared32Member(AEO, ix);
        }

        // setNull test
        MT1.radianPerSecondSquared32Member().setNull();
        MemberTypesProperty.setNullRadianPerSecondSquared32Member(MT2);
        MA1.radianPerSecondSquared32Member().get(1).setNull();
        MemberArraysProperty.setNullRadianPerSecondSquared32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.radianPerSecondSquared32Member().isNull()
                && MemberTypesProperty.isNullRadianPerSecondSquared32Member(MT2)
                && MA1.radianPerSecondSquared32Member().get(1).isNull()
                && MemberArraysProperty.isNullRadianPerSecondSquared32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // RadianPerSecondSquared32

    private static void test_Second32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Second32");
        System.out.println("MemberId: " + MemberTypes.getSecond32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getSecond32MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getSecond32ParameterArraySize() == 2
                && MemberArrays.getSecond32MemberArraySize() == 2
                && MemberArraysProperty.getSecond32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.second32Member().isNull();
        In_Req_Ok = !MT1.second32Member().isChanged();
        MT1.second32Member().setVal(ParameterTypes.getSecond32Parameter());
        Null_Ok = Null_Ok && !MT1.second32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.second32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getSecond32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getSecond32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getSecond32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Second32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Second32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Second32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSecond32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSecond32Member(MT2);
        MemberTypesProperty.setSecond32Member(MT2, MT1.second32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getSecond32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSecond32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSecond32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSecond32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSecond32Member(MI);
        MemberTypesProperty.setSecond32Member(MI, MT2.second32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getSecond32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSecond32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSecond32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSecond32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSecond32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setSecond32Member(MIA, MT2.second32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().second32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSecond32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSecond32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSecond32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSecond32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getSecond32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSecond32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSecond32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSecond32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSecond32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getSecond32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSecond32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSecond32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getSecond32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.second32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.second32Member().get(ix).isChanged();
            MA1.second32Member().get(ix).setVal(ParameterArrays.getSecond32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.second32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.second32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullSecond32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSecond32Member(MA2, ix);
            MA2.second32Member().get(ix).setVal(MA1.second32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSecond32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSecond32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getSecond32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.second32Member().get(ix).setVal(MA1.second32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSecond32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSecond32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSecond32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSecond32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSecond32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSecond32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSecond32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSecond32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getSecond32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSecond32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSecond32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSecond32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSecond32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSecond32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSecond32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSecond32Member(AEO, ix);
        }

        // setNull test
        MT1.second32Member().setNull();
        MemberTypesProperty.setNullSecond32Member(MT2);
        MA1.second32Member().get(1).setNull();
        MemberArraysProperty.setNullSecond32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.second32Member().isNull() && MemberTypesProperty.isNullSecond32Member(MT2)
                && MA1.second32Member().get(1).isNull() && MemberArraysProperty.isNullSecond32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Second32

    private static void test_SquareMeter32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("SquareMeter32");
        System.out.println("MemberId: " + MemberTypes.getSquareMeter32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getSquareMeter32MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getSquareMeter32ParameterArraySize() == 2
                && MemberArrays.getSquareMeter32MemberArraySize() == 2
                && MemberArraysProperty.getSquareMeter32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.squareMeter32Member().isNull();
        In_Req_Ok = !MT1.squareMeter32Member().isChanged();
        MT1.squareMeter32Member().setVal(ParameterTypes.getSquareMeter32Parameter());
        Null_Ok = Null_Ok && !MT1.squareMeter32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.squareMeter32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getSquareMeter32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println("GetName: "
                + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getSquareMeter32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getSquareMeter32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "SquareMeter32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "SquareMeter32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "SquareMeter32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSquareMeter32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSquareMeter32Member(MT2);
        MemberTypesProperty.setSquareMeter32Member(MT2, MT1.squareMeter32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getSquareMeter32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSquareMeter32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSquareMeter32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSquareMeter32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSquareMeter32Member(MI);
        MemberTypesProperty.setSquareMeter32Member(MI, MT2.squareMeter32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getSquareMeter32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSquareMeter32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSquareMeter32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSquareMeter32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSquareMeter32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setSquareMeter32Member(MIA, MT2.squareMeter32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().squareMeter32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSquareMeter32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSquareMeter32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSquareMeter32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSquareMeter32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getSquareMeter32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSquareMeter32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSquareMeter32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSquareMeter32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSquareMeter32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getSquareMeter32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSquareMeter32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSquareMeter32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getSquareMeter32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.squareMeter32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.squareMeter32Member().get(ix).isChanged();
            MA1.squareMeter32Member().get(ix).setVal(ParameterArrays.getSquareMeter32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.squareMeter32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.squareMeter32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullSquareMeter32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSquareMeter32Member(MA2, ix);
            MA2.squareMeter32Member().get(ix).setVal(MA1.squareMeter32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSquareMeter32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSquareMeter32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getSquareMeter32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.squareMeter32Member().get(ix).setVal(MA1.squareMeter32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSquareMeter32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSquareMeter32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSquareMeter32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSquareMeter32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSquareMeter32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSquareMeter32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSquareMeter32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSquareMeter32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getSquareMeter32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSquareMeter32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSquareMeter32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSquareMeter32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSquareMeter32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSquareMeter32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSquareMeter32Member(AEO, ix);
        }

        // setNull test
        MT1.squareMeter32Member().setNull();
        MemberTypesProperty.setNullSquareMeter32Member(MT2);
        MA1.squareMeter32Member().get(1).setNull();
        MemberArraysProperty.setNullSquareMeter32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.squareMeter32Member().isNull() && MemberTypesProperty.isNullSquareMeter32Member(MT2)
                && MA1.squareMeter32Member().get(1).isNull() && MemberArraysProperty.isNullSquareMeter32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // SquareMeter32

    private static void test_Steradian32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Steradian32");
        System.out.println("MemberId: " + MemberTypes.getSteradian32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getSteradian32MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getSteradian32ParameterArraySize() == 2
                && MemberArrays.getSteradian32MemberArraySize() == 2
                && MemberArraysProperty.getSteradian32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.steradian32Member().isNull();
        In_Req_Ok = !MT1.steradian32Member().isChanged();
        MT1.steradian32Member().setVal(ParameterTypes.getSteradian32Parameter());
        Null_Ok = Null_Ok && !MT1.steradian32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.steradian32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getSteradian32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getSteradian32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getSteradian32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Steradian32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Steradian32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Steradian32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSteradian32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSteradian32Member(MT2);
        MemberTypesProperty.setSteradian32Member(MT2, MT1.steradian32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getSteradian32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSteradian32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSteradian32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSteradian32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSteradian32Member(MI);
        MemberTypesProperty.setSteradian32Member(MI, MT2.steradian32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getSteradian32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSteradian32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSteradian32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSteradian32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSteradian32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setSteradian32Member(MIA, MT2.steradian32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().steradian32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSteradian32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSteradian32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSteradian32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSteradian32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getSteradian32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSteradian32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSteradian32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSteradian32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSteradian32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getSteradian32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSteradian32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSteradian32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getSteradian32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.steradian32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.steradian32Member().get(ix).isChanged();
            MA1.steradian32Member().get(ix).setVal(ParameterArrays.getSteradian32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.steradian32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.steradian32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullSteradian32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSteradian32Member(MA2, ix);
            MA2.steradian32Member().get(ix).setVal(MA1.steradian32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSteradian32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSteradian32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getSteradian32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.steradian32Member().get(ix).setVal(MA1.steradian32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSteradian32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSteradian32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSteradian32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSteradian32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSteradian32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSteradian32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSteradian32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSteradian32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getSteradian32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSteradian32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSteradian32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSteradian32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSteradian32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSteradian32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSteradian32Member(EO, ix);
        }

        // setNull test
        MT1.steradian32Member().setNull();
        MemberTypesProperty.setNullSteradian32Member(MT2);
        MA1.steradian32Member().get(1).setNull();
        MemberArraysProperty.setNullSteradian32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.steradian32Member().isNull() && MemberTypesProperty.isNullSteradian32Member(MT2)
                && MA1.steradian32Member().get(1).isNull() && MemberArraysProperty.isNullSteradian32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Steradian32

    private static void test_Volt32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Volt32");
        System.out.println("MemberId: " + MemberTypes.getVolt32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getVolt32MemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getVolt32ParameterArraySize() == 2 && MemberArrays.getVolt32MemberArraySize() == 2
                        && MemberArraysProperty.getVolt32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.volt32Member().isNull();
        In_Req_Ok = !MT1.volt32Member().isChanged();
        MT1.volt32Member().setVal(ParameterTypes.getVolt32Parameter());
        Null_Ok = Null_Ok && !MT1.volt32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.volt32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getVolt32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getVolt32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getVolt32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Volt32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Volt32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Volt32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullVolt32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedVolt32Member(MT2);
        MemberTypesProperty.setVolt32Member(MT2, MT1.volt32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getVolt32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullVolt32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedVolt32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullVolt32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedVolt32Member(MI);
        MemberTypesProperty.setVolt32Member(MI, MT2.volt32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getVolt32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullVolt32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedVolt32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullVolt32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedVolt32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setVolt32Member(MIA, MT2.volt32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().volt32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullVolt32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedVolt32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullVolt32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedVolt32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getVolt32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullVolt32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedVolt32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullVolt32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedVolt32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getVolt32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullVolt32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedVolt32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getVolt32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.volt32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.volt32Member().get(ix).isChanged();
            MA1.volt32Member().get(ix).setVal(ParameterArrays.getVolt32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.volt32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.volt32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullVolt32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedVolt32Member(MA2, ix);
            MA2.volt32Member().get(ix).setVal(MA1.volt32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullVolt32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedVolt32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getVolt32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.volt32Member().get(ix).setVal(MA1.volt32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix, MemberArraysProperty.getVolt32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullVolt32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedVolt32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getVolt32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullVolt32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedVolt32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullVolt32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedVolt32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getVolt32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullVolt32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedVolt32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullVolt32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedVolt32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getVolt32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullVolt32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedVolt32Member(AEO, ix);
        }

        // setNull test
        MT1.volt32Member().setNull();
        MemberTypesProperty.setNullVolt32Member(MT2);
        MA1.volt32Member().get(1).setNull();
        MemberArraysProperty.setNullVolt32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.volt32Member().isNull() && MemberTypesProperty.isNullVolt32Member(MT2)
                && MA1.volt32Member().get(1).isNull() && MemberArraysProperty.isNullVolt32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Volt32

    private static void test_Watt32() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Watt32");
        System.out.println("MemberId: " + MemberTypes.getWatt32MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getWatt32MemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getWatt32ParameterArraySize() == 2 && MemberArrays.getWatt32MemberArraySize() == 2
                        && MemberArraysProperty.getWatt32MemberArraySize(MA1) == 2));
        Null_Ok = MT1.watt32Member().isNull();
        In_Req_Ok = !MT1.watt32Member().isChanged();
        MT1.watt32Member().setVal(ParameterTypes.getWatt32Parameter());
        Null_Ok = Null_Ok && !MT1.watt32Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.watt32Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getWatt32MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getWatt32MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getWatt32MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Watt32Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Watt32Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Watt32Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullWatt32Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedWatt32Member(MT2);
        MemberTypesProperty.setWatt32Member(MT2, MT1.watt32Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getWatt32Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullWatt32Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedWatt32Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullWatt32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedWatt32Member(MI);
        MemberTypesProperty.setWatt32Member(MI, MT2.watt32Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getWatt32Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullWatt32Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedWatt32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullWatt32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedWatt32Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setWatt32Member(MIA, MT2.watt32Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().watt32Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullWatt32Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedWatt32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullWatt32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedWatt32Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getWatt32Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullWatt32Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedWatt32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullWatt32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedWatt32Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getWatt32Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullWatt32Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedWatt32Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getWatt32ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.watt32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.watt32Member().get(ix).isChanged();
            MA1.watt32Member().get(ix).setVal(ParameterArrays.getWatt32Parameter(ix));
            Null_Ok = Null_Ok && !MA1.watt32Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.watt32Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullWatt32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedWatt32Member(MA2, ix);
            MA2.watt32Member().get(ix).setVal(MA1.watt32Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullWatt32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedWatt32Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getWatt32Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.watt32Member().get(ix).setVal(MA1.watt32Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix, MemberArraysProperty.getWatt32Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullWatt32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedWatt32Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getWatt32Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullWatt32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedWatt32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullWatt32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedWatt32Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getWatt32Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullWatt32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedWatt32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullWatt32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedWatt32Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getWatt32Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullWatt32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedWatt32Member(AEO, ix);
        }

        // setNull test
        MT1.watt32Member().setNull();
        MemberTypesProperty.setNullWatt32Member(MT2);
        MA1.watt32Member().get(1).setNull();
        MemberArraysProperty.setNullWatt32Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.watt32Member().isNull() && MemberTypesProperty.isNullWatt32Member(MT2)
                && MA1.watt32Member().get(1).isNull() && MemberArraysProperty.isNullWatt32Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Watt32

    private static void test_Ampere64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Ampere64");
        System.out.println("MemberId: " + MemberTypes.getAmpere64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getAmpere64MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getAmpere64ParameterArraySize() == 2
                && MemberArrays.getAmpere64MemberArraySize() == 2
                && MemberArraysProperty.getAmpere64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.ampere64Member().isNull();
        In_Req_Ok = !MT1.ampere64Member().isChanged();
        MT1.ampere64Member().setVal(ParameterTypes.getAmpere64Parameter());
        Null_Ok = Null_Ok && !MT1.ampere64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.ampere64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getAmpere64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getAmpere64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getAmpere64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Ampere64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Ampere64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Ampere64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullAmpere64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedAmpere64Member(MT2);
        MemberTypesProperty.setAmpere64Member(MT2, MT1.ampere64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getAmpere64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullAmpere64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedAmpere64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullAmpere64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedAmpere64Member(MI);
        MemberTypesProperty.setAmpere64Member(MI, MT2.ampere64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getAmpere64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullAmpere64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedAmpere64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullAmpere64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedAmpere64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setAmpere64Member(MIA, MT2.ampere64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().ampere64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullAmpere64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedAmpere64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullAmpere64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedAmpere64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getAmpere64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullAmpere64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedAmpere64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullAmpere64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedAmpere64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getAmpere64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullAmpere64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedAmpere64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getAmpere64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.ampere64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.ampere64Member().get(ix).isChanged();
            MA1.ampere64Member().get(ix).setVal(ParameterArrays.getAmpere64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.ampere64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.ampere64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullAmpere64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedAmpere64Member(MA2, ix);
            MA2.ampere64Member().get(ix).setVal(MA1.ampere64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullAmpere64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedAmpere64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getAmpere64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.ampere64Member().get(ix).setVal(MA1.ampere64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getAmpere64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullAmpere64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedAmpere64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getAmpere64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullAmpere64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedAmpere64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullAmpere64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedAmpere64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getAmpere64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullAmpere64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedAmpere64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullAmpere64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedAmpere64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getAmpere64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullAmpere64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedAmpere64Member(AEO, ix);
        }

        // setNull test
        MT1.ampere64Member().setNull();
        MemberTypesProperty.setNullAmpere64Member(MT2);
        MA1.ampere64Member().get(1).setNull();
        MemberArraysProperty.setNullAmpere64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.ampere64Member().isNull() && MemberTypesProperty.isNullAmpere64Member(MT2)
                && MA1.ampere64Member().get(1).isNull() && MemberArraysProperty.isNullAmpere64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Ampere64

    private static void test_CubicMeter64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("CubicMeter64");
        System.out.println("MemberId: " + MemberTypes.getCubicMeter64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getCubicMeter64MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getCubicMeter64ParameterArraySize() == 2
                && MemberArrays.getCubicMeter64MemberArraySize() == 2
                && MemberArraysProperty.getCubicMeter64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.cubicMeter64Member().isNull();
        In_Req_Ok = !MT1.cubicMeter64Member().isChanged();
        MT1.cubicMeter64Member().setVal(ParameterTypes.getCubicMeter64Parameter());
        Null_Ok = Null_Ok && !MT1.cubicMeter64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.cubicMeter64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getCubicMeter64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getCubicMeter64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getCubicMeter64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "CubicMeter64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "CubicMeter64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "CubicMeter64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullCubicMeter64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedCubicMeter64Member(MT2);
        MemberTypesProperty.setCubicMeter64Member(MT2, MT1.cubicMeter64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getCubicMeter64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullCubicMeter64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedCubicMeter64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullCubicMeter64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedCubicMeter64Member(MI);
        MemberTypesProperty.setCubicMeter64Member(MI, MT2.cubicMeter64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getCubicMeter64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullCubicMeter64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedCubicMeter64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullCubicMeter64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedCubicMeter64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setCubicMeter64Member(MIA, MT2.cubicMeter64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().cubicMeter64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullCubicMeter64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedCubicMeter64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullCubicMeter64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedCubicMeter64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getCubicMeter64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullCubicMeter64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedCubicMeter64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullCubicMeter64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedCubicMeter64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getCubicMeter64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullCubicMeter64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedCubicMeter64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getCubicMeter64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.cubicMeter64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.cubicMeter64Member().get(ix).isChanged();
            MA1.cubicMeter64Member().get(ix).setVal(ParameterArrays.getCubicMeter64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.cubicMeter64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.cubicMeter64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullCubicMeter64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedCubicMeter64Member(MA2, ix);
            MA2.cubicMeter64Member().get(ix).setVal(MA1.cubicMeter64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullCubicMeter64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedCubicMeter64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getCubicMeter64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.cubicMeter64Member().get(ix).setVal(MA1.cubicMeter64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getCubicMeter64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullCubicMeter64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedCubicMeter64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getCubicMeter64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullCubicMeter64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedCubicMeter64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullCubicMeter64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedCubicMeter64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getCubicMeter64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullCubicMeter64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullCubicMeter64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedCubicMeter64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getCubicMeter64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullCubicMeter64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedCubicMeter64Member(AEO, ix);
        }

        // setNull test
        MT1.cubicMeter64Member().setNull();
        MemberTypesProperty.setNullCubicMeter64Member(MT2);
        MA1.cubicMeter64Member().get(1).setNull();
        MemberArraysProperty.setNullCubicMeter64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.cubicMeter64Member().isNull() && MemberTypesProperty.isNullCubicMeter64Member(MT2)
                && MA1.cubicMeter64Member().get(1).isNull() && MemberArraysProperty.isNullCubicMeter64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // CubicMeter64

    private static void test_Hertz64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Hertz64");
        System.out.println("MemberId: " + MemberTypes.getHertz64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getHertz64MemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getHertz64ParameterArraySize() == 2 && MemberArrays.getHertz64MemberArraySize() == 2
                        && MemberArraysProperty.getHertz64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.hertz64Member().isNull();
        In_Req_Ok = !MT1.hertz64Member().isChanged();
        MT1.hertz64Member().setVal(ParameterTypes.getHertz64Parameter());
        Null_Ok = Null_Ok && !MT1.hertz64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.hertz64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getHertz64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getHertz64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getHertz64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Hertz64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Hertz64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Hertz64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullHertz64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedHertz64Member(MT2);
        MemberTypesProperty.setHertz64Member(MT2, MT1.hertz64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getHertz64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHertz64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedHertz64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullHertz64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedHertz64Member(MI);
        MemberTypesProperty.setHertz64Member(MI, MT2.hertz64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getHertz64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHertz64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedHertz64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullHertz64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedHertz64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setHertz64Member(MIA, MT2.hertz64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().hertz64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHertz64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedHertz64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHertz64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedHertz64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getHertz64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHertz64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedHertz64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHertz64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedHertz64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getHertz64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullHertz64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedHertz64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getHertz64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.hertz64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.hertz64Member().get(ix).isChanged();
            MA1.hertz64Member().get(ix).setVal(ParameterArrays.getHertz64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.hertz64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.hertz64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullHertz64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedHertz64Member(MA2, ix);
            MA2.hertz64Member().get(ix).setVal(MA1.hertz64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHertz64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedHertz64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getHertz64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.hertz64Member().get(ix).setVal(MA1.hertz64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getHertz64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHertz64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedHertz64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getHertz64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHertz64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedHertz64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHertz64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedHertz64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getHertz64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHertz64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedHertz64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHertz64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedHertz64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getHertz64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullHertz64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedHertz64Member(AEO, ix);
        }

        // setNull test
        MT1.hertz64Member().setNull();
        MemberTypesProperty.setNullHertz64Member(MT2);
        MA1.hertz64Member().get(1).setNull();
        MemberArraysProperty.setNullHertz64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.hertz64Member().isNull() && MemberTypesProperty.isNullHertz64Member(MT2)
                && MA1.hertz64Member().get(1).isNull() && MemberArraysProperty.isNullHertz64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Hertz64

    private static void test_Joule64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Joule64");
        System.out.println("MemberId: " + MemberTypes.getJoule64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getJoule64MemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getJoule64ParameterArraySize() == 2 && MemberArrays.getJoule64MemberArraySize() == 2
                        && MemberArraysProperty.getJoule64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.joule64Member().isNull();
        In_Req_Ok = !MT1.joule64Member().isChanged();
        MT1.joule64Member().setVal(ParameterTypes.getJoule64Parameter());
        Null_Ok = Null_Ok && !MT1.joule64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.joule64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getJoule64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getJoule64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getJoule64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Joule64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Joule64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Joule64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullJoule64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedJoule64Member(MT2);
        MemberTypesProperty.setJoule64Member(MT2, MT1.joule64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getJoule64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullJoule64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedJoule64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullJoule64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedJoule64Member(MI);
        MemberTypesProperty.setJoule64Member(MI, MT2.joule64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getJoule64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullJoule64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedJoule64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullJoule64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedJoule64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setJoule64Member(MIA, MT2.joule64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().joule64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullJoule64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedJoule64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullJoule64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedJoule64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getJoule64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullJoule64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedJoule64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullJoule64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedJoule64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getJoule64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullJoule64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedJoule64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getJoule64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.joule64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.joule64Member().get(ix).isChanged();
            MA1.joule64Member().get(ix).setVal(ParameterArrays.getJoule64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.joule64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.joule64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullJoule64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedJoule64Member(MA2, ix);
            MA2.joule64Member().get(ix).setVal(MA1.joule64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullJoule64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedJoule64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getJoule64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.joule64Member().get(ix).setVal(MA1.joule64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getJoule64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullJoule64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedJoule64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getJoule64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullJoule64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedJoule64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullJoule64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedJoule64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getJoule64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullJoule64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedJoule64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullJoule64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedJoule64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getJoule64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullJoule64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedJoule64Member(AEO, ix);
        }

        // setNull test
        MT1.joule64Member().setNull();
        MemberTypesProperty.setNullJoule64Member(MT2);
        MA1.joule64Member().get(1).setNull();
        MemberArraysProperty.setNullJoule64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.joule64Member().isNull() && MemberTypesProperty.isNullJoule64Member(MT2)
                && MA1.joule64Member().get(1).isNull() && MemberArraysProperty.isNullJoule64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Joule64

    private static void test_Kelvin64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Kelvin64");
        System.out.println("MemberId: " + MemberTypes.getKelvin64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getKelvin64MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getKelvin64ParameterArraySize() == 2
                && MemberArrays.getKelvin64MemberArraySize() == 2
                && MemberArraysProperty.getKelvin64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.kelvin64Member().isNull();
        In_Req_Ok = !MT1.kelvin64Member().isChanged();
        MT1.kelvin64Member().setVal(ParameterTypes.getKelvin64Parameter());
        Null_Ok = Null_Ok && !MT1.kelvin64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.kelvin64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getKelvin64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getKelvin64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getKelvin64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Kelvin64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Kelvin64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Kelvin64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullKelvin64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKelvin64Member(MT2);
        MemberTypesProperty.setKelvin64Member(MT2, MT1.kelvin64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getKelvin64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKelvin64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKelvin64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullKelvin64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKelvin64Member(MI);
        MemberTypesProperty.setKelvin64Member(MI, MT2.kelvin64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getKelvin64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKelvin64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKelvin64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullKelvin64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKelvin64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setKelvin64Member(MIA, MT2.kelvin64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().kelvin64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKelvin64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKelvin64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKelvin64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKelvin64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getKelvin64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKelvin64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKelvin64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKelvin64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKelvin64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getKelvin64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKelvin64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKelvin64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getKelvin64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.kelvin64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.kelvin64Member().get(ix).isChanged();
            MA1.kelvin64Member().get(ix).setVal(ParameterArrays.getKelvin64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.kelvin64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.kelvin64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullKelvin64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKelvin64Member(MA2, ix);
            MA2.kelvin64Member().get(ix).setVal(MA1.kelvin64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKelvin64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedKelvin64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getKelvin64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.kelvin64Member().get(ix).setVal(MA1.kelvin64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getKelvin64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKelvin64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedKelvin64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getKelvin64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKelvin64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedKelvin64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKelvin64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKelvin64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getKelvin64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKelvin64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKelvin64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKelvin64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKelvin64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getKelvin64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKelvin64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKelvin64Member(AEO, ix);
        }

        // setNull test
        MT1.kelvin64Member().setNull();
        MemberTypesProperty.setNullKelvin64Member(MT2);
        MA1.kelvin64Member().get(1).setNull();
        MemberArraysProperty.setNullKelvin64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.kelvin64Member().isNull() && MemberTypesProperty.isNullKelvin64Member(MT2)
                && MA1.kelvin64Member().get(1).isNull() && MemberArraysProperty.isNullKelvin64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Kelvin64

    private static void test_Kilogram64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Kilogram64");
        System.out.println("MemberId: " + MemberTypes.getKilogram64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getKilogram64MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getKilogram64ParameterArraySize() == 2
                && MemberArrays.getKilogram64MemberArraySize() == 2
                && MemberArraysProperty.getKilogram64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.kilogram64Member().isNull();
        In_Req_Ok = !MT1.kilogram64Member().isChanged();
        MT1.kilogram64Member().setVal(ParameterTypes.getKilogram64Parameter());
        Null_Ok = Null_Ok && !MT1.kilogram64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.kilogram64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getKilogram64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getKilogram64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getKilogram64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Kilogram64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Kilogram64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Kilogram64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullKilogram64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKilogram64Member(MT2);
        MemberTypesProperty.setKilogram64Member(MT2, MT1.kilogram64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getKilogram64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKilogram64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKilogram64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullKilogram64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKilogram64Member(MI);
        MemberTypesProperty.setKilogram64Member(MI, MT2.kilogram64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getKilogram64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKilogram64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKilogram64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullKilogram64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKilogram64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setKilogram64Member(MIA, MT2.kilogram64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().kilogram64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKilogram64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedKilogram64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKilogram64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKilogram64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getKilogram64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKilogram64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKilogram64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKilogram64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKilogram64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getKilogram64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullKilogram64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedKilogram64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getKilogram64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.kilogram64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.kilogram64Member().get(ix).isChanged();
            MA1.kilogram64Member().get(ix).setVal(ParameterArrays.getKilogram64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.kilogram64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.kilogram64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullKilogram64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKilogram64Member(MA2, ix);
            MA2.kilogram64Member().get(ix).setVal(MA1.kilogram64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKilogram64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedKilogram64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getKilogram64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.kilogram64Member().get(ix).setVal(MA1.kilogram64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getKilogram64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKilogram64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedKilogram64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getKilogram64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKilogram64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedKilogram64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKilogram64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKilogram64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getKilogram64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKilogram64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKilogram64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKilogram64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getKilogram64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullKilogram64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedKilogram64Member(AEO, ix);
        }

        // setNull test
        MT1.kilogram64Member().setNull();
        MemberTypesProperty.setNullKilogram64Member(MT2);
        MA1.kilogram64Member().get(1).setNull();
        MemberArraysProperty.setNullKilogram64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.kilogram64Member().isNull() && MemberTypesProperty.isNullKilogram64Member(MT2)
                && MA1.kilogram64Member().get(1).isNull() && MemberArraysProperty.isNullKilogram64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Kilogram64

    private static void test_Meter64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Meter64");
        System.out.println("MemberId: " + MemberTypes.getMeter64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getMeter64MemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getMeter64ParameterArraySize() == 2 && MemberArrays.getMeter64MemberArraySize() == 2
                        && MemberArraysProperty.getMeter64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.meter64Member().isNull();
        In_Req_Ok = !MT1.meter64Member().isChanged();
        MT1.meter64Member().setVal(ParameterTypes.getMeter64Parameter());
        Null_Ok = Null_Ok && !MT1.meter64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.meter64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getMeter64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getMeter64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getMeter64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Meter64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Meter64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Meter64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeter64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeter64Member(MT2);
        MemberTypesProperty.setMeter64Member(MT2, MT1.meter64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getMeter64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeter64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeter64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeter64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeter64Member(MI);
        MemberTypesProperty.setMeter64Member(MI, MT2.meter64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getMeter64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeter64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeter64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeter64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeter64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setMeter64Member(MIA, MT2.meter64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().meter64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeter64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeter64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeter64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeter64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getMeter64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeter64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeter64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeter64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeter64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getMeter64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeter64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeter64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getMeter64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.meter64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.meter64Member().get(ix).isChanged();
            MA1.meter64Member().get(ix).setVal(ParameterArrays.getMeter64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.meter64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.meter64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullMeter64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeter64Member(MA2, ix);
            MA2.meter64Member().get(ix).setVal(MA1.meter64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeter64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeter64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getMeter64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.meter64Member().get(ix).setVal(MA1.meter64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeter64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeter64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeter64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeter64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeter64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeter64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeter64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeter64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getMeter64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeter64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeter64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeter64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeter64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeter64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeter64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeter64Member(AEO, ix);
        }

        // setNull test
        MT1.meter64Member().setNull();
        MemberTypesProperty.setNullMeter64Member(MT2);
        MA1.meter64Member().get(1).setNull();
        MemberArraysProperty.setNullMeter64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.meter64Member().isNull() && MemberTypesProperty.isNullMeter64Member(MT2)
                && MA1.meter64Member().get(1).isNull() && MemberArraysProperty.isNullMeter64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Meter64

    private static void test_MeterPerSecond64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("MeterPerSecond64");
        System.out.println("MemberId: " + MemberTypes.getMeterPerSecond64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getMeterPerSecond64MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getMeterPerSecond64ParameterArraySize() == 2
                && MemberArrays.getMeterPerSecond64MemberArraySize() == 2
                && MemberArraysProperty.getMeterPerSecond64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.meterPerSecond64Member().isNull();
        In_Req_Ok = !MT1.meterPerSecond64Member().isChanged();
        MT1.meterPerSecond64Member().setVal(ParameterTypes.getMeterPerSecond64Parameter());
        Null_Ok = Null_Ok && !MT1.meterPerSecond64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.meterPerSecond64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getMeterPerSecond64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println("GetName: "
                + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getMeterPerSecond64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getMeterPerSecond64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "MeterPerSecond64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "MeterPerSecond64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "MeterPerSecond64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeterPerSecond64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecond64Member(MT2);
        MemberTypesProperty.setMeterPerSecond64Member(MT2, MT1.meterPerSecond64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getMeterPerSecond64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecond64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecond64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeterPerSecond64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecond64Member(MI);
        MemberTypesProperty.setMeterPerSecond64Member(MI, MT2.meterPerSecond64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getMeterPerSecond64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecond64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecond64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeterPerSecond64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecond64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setMeterPerSecond64Member(MIA, MT2.meterPerSecond64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().meterPerSecond64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecond64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecond64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecond64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecond64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n",
                MemberTypesProperty.getMeterPerSecond64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecond64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecond64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecond64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecond64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n",
                MemberTypesProperty.getMeterPerSecond64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecond64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecond64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getMeterPerSecond64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.meterPerSecond64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.meterPerSecond64Member().get(ix).isChanged();
            MA1.meterPerSecond64Member().get(ix).setVal(ParameterArrays.getMeterPerSecond64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.meterPerSecond64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.meterPerSecond64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullMeterPerSecond64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecond64Member(MA2, ix);
            MA2.meterPerSecond64Member().get(ix).setVal(MA1.meterPerSecond64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecond64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeterPerSecond64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getMeterPerSecond64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.meterPerSecond64Member().get(ix).setVal(MA1.meterPerSecond64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeterPerSecond64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecond64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeterPerSecond64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeterPerSecond64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecond64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeterPerSecond64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecond64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecond64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getMeterPerSecond64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecond64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecond64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecond64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeterPerSecond64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecond64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecond64Member(AEO, ix);
        }

        // setNull test
        MT1.meterPerSecond64Member().setNull();
        MemberTypesProperty.setNullMeterPerSecond64Member(MT2);
        MA1.meterPerSecond64Member().get(1).setNull();
        MemberArraysProperty.setNullMeterPerSecond64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.meterPerSecond64Member().isNull()
                && MemberTypesProperty.isNullMeterPerSecond64Member(MT2) && MA1.meterPerSecond64Member().get(1).isNull()
                && MemberArraysProperty.isNullMeterPerSecond64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // MeterPerSecond64

    private static void test_MeterPerSecondSquared64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("MeterPerSecondSquared64");
        System.out.println("MemberId: " + MemberTypes.getMeterPerSecondSquared64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getMeterPerSecondSquared64MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getMeterPerSecondSquared64ParameterArraySize() == 2
                && MemberArrays.getMeterPerSecondSquared64MemberArraySize() == 2
                && MemberArraysProperty.getMeterPerSecondSquared64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.meterPerSecondSquared64Member().isNull();
        In_Req_Ok = !MT1.meterPerSecondSquared64Member().isChanged();
        MT1.meterPerSecondSquared64Member().setVal(ParameterTypes.getMeterPerSecondSquared64Parameter());
        Null_Ok = Null_Ok && !MT1.meterPerSecondSquared64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.meterPerSecondSquared64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId,
                MemberArrays.getMeterPerSecondSquared64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println("GetName: "
                + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getMeterPerSecondSquared64MemberMemberIndex()));
        System.out.println("GetTypeName: " + Members.getTypeName(MemberTypes.ClassTypeId,
                MemberTypes.getMeterPerSecondSquared64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "MeterPerSecondSquared64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "MeterPerSecondSquared64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "MeterPerSecondSquared64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeterPerSecondSquared64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecondSquared64Member(MT2);
        MemberTypesProperty.setMeterPerSecondSquared64Member(MT2, MT1.meterPerSecondSquared64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getMeterPerSecondSquared64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecondSquared64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecondSquared64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeterPerSecondSquared64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecondSquared64Member(MI);
        MemberTypesProperty.setMeterPerSecondSquared64Member(MI, MT2.meterPerSecondSquared64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getMeterPerSecondSquared64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecondSquared64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecondSquared64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullMeterPerSecondSquared64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecondSquared64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setMeterPerSecondSquared64Member(MIA, MT2.meterPerSecondSquared64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().meterPerSecondSquared64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecondSquared64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedMeterPerSecondSquared64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecondSquared64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecondSquared64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n",
                MemberTypesProperty.getMeterPerSecondSquared64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecondSquared64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecondSquared64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecondSquared64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecondSquared64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n",
                MemberTypesProperty.getMeterPerSecondSquared64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullMeterPerSecondSquared64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedMeterPerSecondSquared64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getMeterPerSecondSquared64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.meterPerSecondSquared64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.meterPerSecondSquared64Member().get(ix).isChanged();
            MA1.meterPerSecondSquared64Member().get(ix).setVal(ParameterArrays.getMeterPerSecondSquared64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.meterPerSecondSquared64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.meterPerSecondSquared64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullMeterPerSecondSquared64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecondSquared64Member(MA2, ix);
            MA2.meterPerSecondSquared64Member().get(ix).setVal(MA1.meterPerSecondSquared64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecondSquared64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeterPerSecondSquared64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeterPerSecondSquared64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.meterPerSecondSquared64Member().get(ix).setVal(MA1.meterPerSecondSquared64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeterPerSecondSquared64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecondSquared64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeterPerSecondSquared64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeterPerSecondSquared64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecondSquared64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedMeterPerSecondSquared64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecondSquared64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecondSquared64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getMeterPerSecondSquared64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecondSquared64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecondSquared64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecondSquared64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecondSquared64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getMeterPerSecondSquared64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullMeterPerSecondSquared64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedMeterPerSecondSquared64Member(AEO, ix);
        }

        // setNull test
        MT1.meterPerSecondSquared64Member().setNull();
        MemberTypesProperty.setNullMeterPerSecondSquared64Member(MT2);
        MA1.meterPerSecondSquared64Member().get(1).setNull();
        MemberArraysProperty.setNullMeterPerSecondSquared64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.meterPerSecondSquared64Member().isNull()
                && MemberTypesProperty.isNullMeterPerSecondSquared64Member(MT2)
                && MA1.meterPerSecondSquared64Member().get(1).isNull()
                && MemberArraysProperty.isNullMeterPerSecondSquared64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // MeterPerSecondSquared64

    private static void test_Newton64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Newton64");
        System.out.println("MemberId: " + MemberTypes.getNewton64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getNewton64MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getNewton64ParameterArraySize() == 2
                && MemberArrays.getNewton64MemberArraySize() == 2
                && MemberArraysProperty.getNewton64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.newton64Member().isNull();
        In_Req_Ok = !MT1.newton64Member().isChanged();
        MT1.newton64Member().setVal(ParameterTypes.getNewton64Parameter());
        Null_Ok = Null_Ok && !MT1.newton64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.newton64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getNewton64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getNewton64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getNewton64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Newton64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Newton64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Newton64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullNewton64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedNewton64Member(MT2);
        MemberTypesProperty.setNewton64Member(MT2, MT1.newton64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getNewton64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullNewton64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedNewton64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullNewton64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedNewton64Member(MI);
        MemberTypesProperty.setNewton64Member(MI, MT2.newton64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getNewton64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullNewton64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedNewton64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullNewton64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedNewton64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setNewton64Member(MIA, MT2.newton64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().newton64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullNewton64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedNewton64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullNewton64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedNewton64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getNewton64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullNewton64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedNewton64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullNewton64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedNewton64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getNewton64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullNewton64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedNewton64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getNewton64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.newton64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.newton64Member().get(ix).isChanged();
            MA1.newton64Member().get(ix).setVal(ParameterArrays.getNewton64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.newton64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.newton64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullNewton64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedNewton64Member(MA2, ix);
            MA2.newton64Member().get(ix).setVal(MA1.newton64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullNewton64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedNewton64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getNewton64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.newton64Member().get(ix).setVal(MA1.newton64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getNewton64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullNewton64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedNewton64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getNewton64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullNewton64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedNewton64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullNewton64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedNewton64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getNewton64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullNewton64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedNewton64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullNewton64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedNewton64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getNewton64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullNewton64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedNewton64Member(AEO, ix);
        }

        // setNull test
        MT1.newton64Member().setNull();
        MemberTypesProperty.setNullNewton64Member(MT2);
        MA1.newton64Member().get(1).setNull();
        MemberArraysProperty.setNullNewton64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.newton64Member().isNull() && MemberTypesProperty.isNullNewton64Member(MT2)
                && MA1.newton64Member().get(1).isNull() && MemberArraysProperty.isNullNewton64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Newton64

    private static void test_Pascal64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Pascal64");
        System.out.println("MemberId: " + MemberTypes.getPascal64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getPascal64MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getPascal64ParameterArraySize() == 2
                && MemberArrays.getPascal64MemberArraySize() == 2
                && MemberArraysProperty.getPascal64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.pascal64Member().isNull();
        In_Req_Ok = !MT1.pascal64Member().isChanged();
        MT1.pascal64Member().setVal(ParameterTypes.getPascal64Parameter());
        Null_Ok = Null_Ok && !MT1.pascal64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.pascal64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getPascal64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getPascal64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getPascal64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Pascal64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Pascal64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Pascal64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullPascal64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedPascal64Member(MT2);
        MemberTypesProperty.setPascal64Member(MT2, MT1.pascal64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getPascal64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullPascal64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedPascal64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullPascal64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedPascal64Member(MI);
        MemberTypesProperty.setPascal64Member(MI, MT2.pascal64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getPascal64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullPascal64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedPascal64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullPascal64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedPascal64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setPascal64Member(MIA, MT2.pascal64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().pascal64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullPascal64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedPascal64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullPascal64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedPascal64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getPascal64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullPascal64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedPascal64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullPascal64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedPascal64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getPascal64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullPascal64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedPascal64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getPascal64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.pascal64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.pascal64Member().get(ix).isChanged();
            MA1.pascal64Member().get(ix).setVal(ParameterArrays.getPascal64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.pascal64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.pascal64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullPascal64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedPascal64Member(MA2, ix);
            MA2.pascal64Member().get(ix).setVal(MA1.pascal64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullPascal64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedPascal64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getPascal64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.pascal64Member().get(ix).setVal(MA1.pascal64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getPascal64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullPascal64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedPascal64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getPascal64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullPascal64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedPascal64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullPascal64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedPascal64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getPascal64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullPascal64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedPascal64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullPascal64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedPascal64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getPascal64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullPascal64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedPascal64Member(AEO, ix);
        }

        // setNull test
        MT1.pascal64Member().setNull();
        MemberTypesProperty.setNullPascal64Member(MT2);
        MA1.pascal64Member().get(1).setNull();
        MemberArraysProperty.setNullPascal64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.pascal64Member().isNull() && MemberTypesProperty.isNullPascal64Member(MT2)
                && MA1.pascal64Member().get(1).isNull() && MemberArraysProperty.isNullPascal64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Pascal64

    private static void test_Radian64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Radian64");
        System.out.println("MemberId: " + MemberTypes.getRadian64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getRadian64MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getRadian64ParameterArraySize() == 2
                && MemberArrays.getRadian64MemberArraySize() == 2
                && MemberArraysProperty.getRadian64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.radian64Member().isNull();
        In_Req_Ok = !MT1.radian64Member().isChanged();
        MT1.radian64Member().setVal(ParameterTypes.getRadian64Parameter());
        Null_Ok = Null_Ok && !MT1.radian64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.radian64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getRadian64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getRadian64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getRadian64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Radian64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Radian64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Radian64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadian64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadian64Member(MT2);
        MemberTypesProperty.setRadian64Member(MT2, MT1.radian64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getRadian64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadian64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadian64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadian64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadian64Member(MI);
        MemberTypesProperty.setRadian64Member(MI, MT2.radian64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getRadian64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadian64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadian64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadian64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadian64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setRadian64Member(MIA, MT2.radian64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().radian64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadian64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadian64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadian64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadian64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getRadian64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadian64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadian64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadian64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadian64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getRadian64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadian64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadian64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getRadian64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.radian64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.radian64Member().get(ix).isChanged();
            MA1.radian64Member().get(ix).setVal(ParameterArrays.getRadian64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.radian64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.radian64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullRadian64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadian64Member(MA2, ix);
            MA2.radian64Member().get(ix).setVal(MA1.radian64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadian64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadian64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getRadian64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.radian64Member().get(ix).setVal(MA1.radian64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadian64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadian64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadian64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadian64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadian64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadian64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadian64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadian64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getRadian64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadian64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadian64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadian64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadian64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadian64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadian64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadian64Member(AEO, ix);
        }

        // setNull test
        MT1.radian64Member().setNull();
        MemberTypesProperty.setNullRadian64Member(MT2);
        MA1.radian64Member().get(1).setNull();
        MemberArraysProperty.setNullRadian64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.radian64Member().isNull() && MemberTypesProperty.isNullRadian64Member(MT2)
                && MA1.radian64Member().get(1).isNull() && MemberArraysProperty.isNullRadian64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Radian64

    private static void test_RadianPerSecond64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("RadianPerSecond64");
        System.out.println("MemberId: " + MemberTypes.getRadianPerSecond64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getRadianPerSecond64MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getRadianPerSecond64ParameterArraySize() == 2
                && MemberArrays.getRadianPerSecond64MemberArraySize() == 2
                && MemberArraysProperty.getRadianPerSecond64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.radianPerSecond64Member().isNull();
        In_Req_Ok = !MT1.radianPerSecond64Member().isChanged();
        MT1.radianPerSecond64Member().setVal(ParameterTypes.getRadianPerSecond64Parameter());
        Null_Ok = Null_Ok && !MT1.radianPerSecond64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.radianPerSecond64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getRadianPerSecond64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println("GetName: "
                + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getRadianPerSecond64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getRadianPerSecond64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "RadianPerSecond64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "RadianPerSecond64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "RadianPerSecond64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadianPerSecond64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecond64Member(MT2);
        MemberTypesProperty.setRadianPerSecond64Member(MT2, MT1.radianPerSecond64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getRadianPerSecond64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecond64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecond64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadianPerSecond64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecond64Member(MI);
        MemberTypesProperty.setRadianPerSecond64Member(MI, MT2.radianPerSecond64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getRadianPerSecond64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecond64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecond64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadianPerSecond64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecond64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setRadianPerSecond64Member(MIA, MT2.radianPerSecond64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().radianPerSecond64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecond64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecond64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecond64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecond64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n",
                MemberTypesProperty.getRadianPerSecond64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecond64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecond64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecond64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecond64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n",
                MemberTypesProperty.getRadianPerSecond64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecond64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecond64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getRadianPerSecond64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.radianPerSecond64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.radianPerSecond64Member().get(ix).isChanged();
            MA1.radianPerSecond64Member().get(ix).setVal(ParameterArrays.getRadianPerSecond64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.radianPerSecond64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.radianPerSecond64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullRadianPerSecond64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecond64Member(MA2, ix);
            MA2.radianPerSecond64Member().get(ix).setVal(MA1.radianPerSecond64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecond64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadianPerSecond64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadianPerSecond64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.radianPerSecond64Member().get(ix).setVal(MA1.radianPerSecond64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadianPerSecond64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecond64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadianPerSecond64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadianPerSecond64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecond64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadianPerSecond64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecond64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecond64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getRadianPerSecond64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecond64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecond64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecond64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecond64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadianPerSecond64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecond64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecond64Member(AEO, ix);
        }

        // setNull test
        MT1.radianPerSecond64Member().setNull();
        MemberTypesProperty.setNullRadianPerSecond64Member(MT2);
        MA1.radianPerSecond64Member().get(1).setNull();
        MemberArraysProperty.setNullRadianPerSecond64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.radianPerSecond64Member().isNull()
                && MemberTypesProperty.isNullRadianPerSecond64Member(MT2)
                && MA1.radianPerSecond64Member().get(1).isNull()
                && MemberArraysProperty.isNullRadianPerSecond64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // RadianPerSecond64

    private static void test_RadianPerSecondSquared64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("RadianPerSecondSquared64");
        System.out.println("MemberId: " + MemberTypes.getRadianPerSecondSquared64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getRadianPerSecondSquared64MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getRadianPerSecondSquared64ParameterArraySize() == 2
                && MemberArrays.getRadianPerSecondSquared64MemberArraySize() == 2
                && MemberArraysProperty.getRadianPerSecondSquared64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.radianPerSecondSquared64Member().isNull();
        In_Req_Ok = !MT1.radianPerSecondSquared64Member().isChanged();
        MT1.radianPerSecondSquared64Member().setVal(ParameterTypes.getRadianPerSecondSquared64Parameter());
        Null_Ok = Null_Ok && !MT1.radianPerSecondSquared64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.radianPerSecondSquared64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId,
                MemberArrays.getRadianPerSecondSquared64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println("GetName: "
                + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getRadianPerSecondSquared64MemberMemberIndex()));
        System.out.println("GetTypeName: " + Members.getTypeName(MemberTypes.ClassTypeId,
                MemberTypes.getRadianPerSecondSquared64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "RadianPerSecondSquared64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "RadianPerSecondSquared64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "RadianPerSecondSquared64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadianPerSecondSquared64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecondSquared64Member(MT2);
        MemberTypesProperty.setRadianPerSecondSquared64Member(MT2, MT1.radianPerSecondSquared64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getRadianPerSecondSquared64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecondSquared64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecondSquared64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadianPerSecondSquared64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecondSquared64Member(MI);
        MemberTypesProperty.setRadianPerSecondSquared64Member(MI, MT2.radianPerSecondSquared64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getRadianPerSecondSquared64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecondSquared64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecondSquared64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullRadianPerSecondSquared64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecondSquared64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setRadianPerSecondSquared64Member(MIA, MT2.radianPerSecondSquared64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().radianPerSecondSquared64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecondSquared64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedRadianPerSecondSquared64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecondSquared64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecondSquared64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n",
                MemberTypesProperty.getRadianPerSecondSquared64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecondSquared64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecondSquared64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecondSquared64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecondSquared64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n",
                MemberTypesProperty.getRadianPerSecondSquared64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullRadianPerSecondSquared64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedRadianPerSecondSquared64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getRadianPerSecondSquared64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.radianPerSecondSquared64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.radianPerSecondSquared64Member().get(ix).isChanged();
            MA1.radianPerSecondSquared64Member().get(ix)
                    .setVal(ParameterArrays.getRadianPerSecondSquared64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.radianPerSecondSquared64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.radianPerSecondSquared64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullRadianPerSecondSquared64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecondSquared64Member(MA2, ix);
            MA2.radianPerSecondSquared64Member().get(ix).setVal(MA1.radianPerSecondSquared64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecondSquared64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadianPerSecondSquared64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadianPerSecondSquared64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.radianPerSecondSquared64Member().get(ix).setVal(MA1.radianPerSecondSquared64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadianPerSecondSquared64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecondSquared64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadianPerSecondSquared64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadianPerSecondSquared64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecondSquared64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedRadianPerSecondSquared64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecondSquared64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecondSquared64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getRadianPerSecondSquared64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecondSquared64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecondSquared64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecondSquared64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecondSquared64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getRadianPerSecondSquared64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullRadianPerSecondSquared64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedRadianPerSecondSquared64Member(AEO, ix);
        }

        // setNull test
        MT1.radianPerSecondSquared64Member().setNull();
        MemberTypesProperty.setNullRadianPerSecondSquared64Member(MT2);
        MA1.radianPerSecondSquared64Member().get(1).setNull();
        MemberArraysProperty.setNullRadianPerSecondSquared64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.radianPerSecondSquared64Member().isNull()
                && MemberTypesProperty.isNullRadianPerSecondSquared64Member(MT2)
                && MA1.radianPerSecondSquared64Member().get(1).isNull()
                && MemberArraysProperty.isNullRadianPerSecondSquared64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // RadianPerSecondSquared64

    private static void test_Second64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Second64");
        System.out.println("MemberId: " + MemberTypes.getSecond64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getSecond64MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getSecond64ParameterArraySize() == 2
                && MemberArrays.getSecond64MemberArraySize() == 2
                && MemberArraysProperty.getSecond64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.second64Member().isNull();
        In_Req_Ok = !MT1.second64Member().isChanged();
        MT1.second64Member().setVal(ParameterTypes.getSecond64Parameter());
        Null_Ok = Null_Ok && !MT1.second64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.second64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getSecond64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getSecond64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getSecond64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Second64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Second64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Second64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSecond64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSecond64Member(MT2);
        MemberTypesProperty.setSecond64Member(MT2, MT1.second64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getSecond64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSecond64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSecond64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSecond64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSecond64Member(MI);
        MemberTypesProperty.setSecond64Member(MI, MT2.second64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getSecond64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSecond64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSecond64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSecond64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSecond64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setSecond64Member(MIA, MT2.second64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().second64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSecond64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSecond64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSecond64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSecond64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getSecond64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSecond64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSecond64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSecond64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSecond64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getSecond64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSecond64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSecond64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getSecond64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.second64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.second64Member().get(ix).isChanged();
            MA1.second64Member().get(ix).setVal(ParameterArrays.getSecond64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.second64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.second64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullSecond64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSecond64Member(MA2, ix);
            MA2.second64Member().get(ix).setVal(MA1.second64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSecond64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSecond64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getSecond64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.second64Member().get(ix).setVal(MA1.second64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSecond64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSecond64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSecond64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSecond64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSecond64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSecond64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSecond64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSecond64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getSecond64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSecond64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSecond64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSecond64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSecond64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSecond64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSecond64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSecond64Member(AEO, ix);
        }

        // setNull test
        MT1.second64Member().setNull();
        MemberTypesProperty.setNullSecond64Member(MT2);
        MA1.second64Member().get(1).setNull();
        MemberArraysProperty.setNullSecond64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.second64Member().isNull() && MemberTypesProperty.isNullSecond64Member(MT2)
                && MA1.second64Member().get(1).isNull() && MemberArraysProperty.isNullSecond64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Second64

    private static void test_SquareMeter64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("SquareMeter64");
        System.out.println("MemberId: " + MemberTypes.getSquareMeter64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getSquareMeter64MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getSquareMeter64ParameterArraySize() == 2
                && MemberArrays.getSquareMeter64MemberArraySize() == 2
                && MemberArraysProperty.getSquareMeter64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.squareMeter64Member().isNull();
        In_Req_Ok = !MT1.squareMeter64Member().isChanged();
        MT1.squareMeter64Member().setVal(ParameterTypes.getSquareMeter64Parameter());
        Null_Ok = Null_Ok && !MT1.squareMeter64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.squareMeter64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getSquareMeter64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println("GetName: "
                + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getSquareMeter64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getSquareMeter64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "SquareMeter64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "SquareMeter64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "SquareMeter64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSquareMeter64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSquareMeter64Member(MT2);
        MemberTypesProperty.setSquareMeter64Member(MT2, MT1.squareMeter64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getSquareMeter64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSquareMeter64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSquareMeter64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSquareMeter64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSquareMeter64Member(MI);
        MemberTypesProperty.setSquareMeter64Member(MI, MT2.squareMeter64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getSquareMeter64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSquareMeter64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSquareMeter64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSquareMeter64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSquareMeter64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setSquareMeter64Member(MIA, MT2.squareMeter64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().squareMeter64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSquareMeter64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSquareMeter64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSquareMeter64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSquareMeter64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getSquareMeter64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSquareMeter64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSquareMeter64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSquareMeter64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSquareMeter64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getSquareMeter64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSquareMeter64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSquareMeter64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getSquareMeter64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.squareMeter64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.squareMeter64Member().get(ix).isChanged();
            MA1.squareMeter64Member().get(ix).setVal(ParameterArrays.getSquareMeter64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.squareMeter64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.squareMeter64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullSquareMeter64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSquareMeter64Member(MA2, ix);
            MA2.squareMeter64Member().get(ix).setVal(MA1.squareMeter64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSquareMeter64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSquareMeter64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getSquareMeter64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.squareMeter64Member().get(ix).setVal(MA1.squareMeter64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSquareMeter64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSquareMeter64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSquareMeter64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSquareMeter64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSquareMeter64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSquareMeter64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSquareMeter64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSquareMeter64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getSquareMeter64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSquareMeter64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSquareMeter64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSquareMeter64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSquareMeter64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSquareMeter64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSquareMeter64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSquareMeter64Member(AEO, ix);
        }

        // setNull test
        MT1.squareMeter64Member().setNull();
        MemberTypesProperty.setNullSquareMeter64Member(MT2);
        MA1.squareMeter64Member().get(1).setNull();
        MemberArraysProperty.setNullSquareMeter64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.squareMeter64Member().isNull() && MemberTypesProperty.isNullSquareMeter64Member(MT2)
                && MA1.squareMeter64Member().get(1).isNull() && MemberArraysProperty.isNullSquareMeter64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // SquareMeter64

    private static void test_Steradian64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Steradian64");
        System.out.println("MemberId: " + MemberTypes.getSteradian64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getSteradian64MemberMemberIndex());
        System.out.println("Array Size Ok: " + (ParameterArrays.getSteradian64ParameterArraySize() == 2
                && MemberArrays.getSteradian64MemberArraySize() == 2
                && MemberArraysProperty.getSteradian64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.steradian64Member().isNull();
        In_Req_Ok = !MT1.steradian64Member().isChanged();
        MT1.steradian64Member().setVal(ParameterTypes.getSteradian64Parameter());
        Null_Ok = Null_Ok && !MT1.steradian64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.steradian64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getSteradian64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getSteradian64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getSteradian64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Steradian64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Steradian64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Steradian64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSteradian64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSteradian64Member(MT2);
        MemberTypesProperty.setSteradian64Member(MT2, MT1.steradian64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getSteradian64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSteradian64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSteradian64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSteradian64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSteradian64Member(MI);
        MemberTypesProperty.setSteradian64Member(MI, MT2.steradian64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getSteradian64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSteradian64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSteradian64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullSteradian64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSteradian64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setSteradian64Member(MIA, MT2.steradian64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().steradian64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSteradian64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedSteradian64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSteradian64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSteradian64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getSteradian64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSteradian64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSteradian64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSteradian64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSteradian64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getSteradian64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullSteradian64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedSteradian64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getSteradian64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.steradian64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.steradian64Member().get(ix).isChanged();
            MA1.steradian64Member().get(ix).setVal(ParameterArrays.getSteradian64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.steradian64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.steradian64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullSteradian64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSteradian64Member(MA2, ix);
            MA2.steradian64Member().get(ix).setVal(MA1.steradian64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSteradian64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSteradian64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getSteradian64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.steradian64Member().get(ix).setVal(MA1.steradian64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSteradian64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSteradian64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSteradian64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSteradian64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSteradian64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedSteradian64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSteradian64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSteradian64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getSteradian64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSteradian64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSteradian64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSteradian64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSteradian64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getSteradian64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullSteradian64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedSteradian64Member(AEO, ix);
        }

        // setNull test
        MT1.steradian64Member().setNull();
        MemberTypesProperty.setNullSteradian64Member(MT2);
        MA1.steradian64Member().get(1).setNull();
        MemberArraysProperty.setNullSteradian64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.steradian64Member().isNull() && MemberTypesProperty.isNullSteradian64Member(MT2)
                && MA1.steradian64Member().get(1).isNull() && MemberArraysProperty.isNullSteradian64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Steradian64

    private static void test_Volt64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Volt64");
        System.out.println("MemberId: " + MemberTypes.getVolt64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getVolt64MemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getVolt64ParameterArraySize() == 2 && MemberArrays.getVolt64MemberArraySize() == 2
                        && MemberArraysProperty.getVolt64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.volt64Member().isNull();
        In_Req_Ok = !MT1.volt64Member().isChanged();
        MT1.volt64Member().setVal(ParameterTypes.getVolt64Parameter());
        Null_Ok = Null_Ok && !MT1.volt64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.volt64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getVolt64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getVolt64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getVolt64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Volt64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Volt64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Volt64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullVolt64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedVolt64Member(MT2);
        MemberTypesProperty.setVolt64Member(MT2, MT1.volt64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getVolt64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullVolt64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedVolt64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullVolt64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedVolt64Member(MI);
        MemberTypesProperty.setVolt64Member(MI, MT2.volt64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getVolt64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullVolt64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedVolt64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullVolt64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedVolt64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setVolt64Member(MIA, MT2.volt64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().volt64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullVolt64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedVolt64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullVolt64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedVolt64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getVolt64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullVolt64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedVolt64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullVolt64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedVolt64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getVolt64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullVolt64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedVolt64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getVolt64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.volt64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.volt64Member().get(ix).isChanged();
            MA1.volt64Member().get(ix).setVal(ParameterArrays.getVolt64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.volt64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.volt64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullVolt64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedVolt64Member(MA2, ix);
            MA2.volt64Member().get(ix).setVal(MA1.volt64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullVolt64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedVolt64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getVolt64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.volt64Member().get(ix).setVal(MA1.volt64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix, MemberArraysProperty.getVolt64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullVolt64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedVolt64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getVolt64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullVolt64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedVolt64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullVolt64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedVolt64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getVolt64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullVolt64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedVolt64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullVolt64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedVolt64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getVolt64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullVolt64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedVolt64Member(AEO, ix);
        }

        // setNull test
        MT1.volt64Member().setNull();
        MemberTypesProperty.setNullVolt64Member(MT2);
        MA1.volt64Member().get(1).setNull();
        MemberArraysProperty.setNullVolt64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.volt64Member().isNull() && MemberTypesProperty.isNullVolt64Member(MT2)
                && MA1.volt64Member().get(1).isNull() && MemberArraysProperty.isNullVolt64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);

    } // Volt64

    private static void test_Watt64() {
        // Locals
        boolean Null_Ok, In_Req_Ok;

        // 1. Member Parameter -> Member Class MT1 -> Member Property MT2 ->
        // Output
        // 2. Member Property MT2 -> Member Item MI -> Output
        // 3. Member Property MT2 -> Member Item Array MIA -> Member Type
        // Property MIA -> Output
        // 4. Member Property EO -> Output
        // 5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member
        // Array Property MA2 -> Output
        // 6. Member Array Property EO -> Output
        Header("Watt64");
        System.out.println("MemberId: " + MemberTypes.getWatt64MemberMemberIndex());
        System.out.println("MemberId (arr): " + MemberArrays.getWatt64MemberMemberIndex());
        System.out.println("Array Size Ok: "
                + (ParameterArrays.getWatt64ParameterArraySize() == 2 && MemberArrays.getWatt64MemberArraySize() == 2
                        && MemberArraysProperty.getWatt64MemberArraySize(MA1) == 2));
        Null_Ok = MT1.watt64Member().isNull();
        In_Req_Ok = !MT1.watt64Member().isChanged();
        MT1.watt64Member().setVal(ParameterTypes.getWatt64Parameter());
        Null_Ok = Null_Ok && !MT1.watt64Member().isNull();
        In_Req_Ok = In_Req_Ok && MT1.watt64Member().isChanged();

        MemberInfo mi = Members.getInfo(MemberArrays.ClassTypeId, MemberArrays.getWatt64MemberMemberIndex());

        System.out.println("----Members---- ");
        System.out.println("GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + mi.getMemberName() + ","
                + mi.getMemberTypeId() + "," + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + ","
                + mi.getArrayLength());
        System.out.println(
                "GetName: " + Members.getName(MemberTypes.ClassTypeId, MemberTypes.getWatt64MemberMemberIndex()));
        System.out.println("GetTypeName: "
                + Members.getTypeName(MemberTypes.ClassTypeId, MemberTypes.getWatt64MemberMemberIndex()));

        System.out.println("----Parameters---- ");
        System.out.println("GetName: " + Parameters.getName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Watt64Parameter")));
        System.out.println("GetType: " + toStringNoUnderscore(Parameters.getType(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Watt64Parameter"))));
        System.out.println("GetTypeName: " + Parameters.getTypeName(ParameterTypes.ClassTypeId,
                Parameters.getIndex(ParameterTypes.ClassTypeId, "Watt64Parameter")));
        System.out.println("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && MemberTypesProperty.isNullWatt64Member(MT2);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedWatt64Member(MT2);
        MemberTypesProperty.setWatt64Member(MT2, MT1.watt64Member().getVal());
        System.out.format(Locale.US, "Val: %.0f%n", MemberTypesProperty.getWatt64Member(MT2));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullWatt64Member(MT2);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedWatt64Member(MT2);

        // MemberItems
        MI.typesItem().setObj(new TypesItem());
        Null_Ok = Null_Ok && MemberTypesProperty.isNullWatt64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedWatt64Member(MI);
        MemberTypesProperty.setWatt64Member(MI, MT2.watt64Member().getVal());
        System.out.format(Locale.US, "Item Val: %.0f%n", MemberTypesProperty.getWatt64Member(MI));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullWatt64Member(MI);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedWatt64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && MemberTypesProperty.isNullWatt64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedWatt64Member(MIA);
        MIA.typesItemArray().get(1).setObj(new TypesItem());
        MemberTypesProperty.setWatt64Member(MIA, MT2.watt64Member().getVal());
        System.out.format(Locale.US, "Item Array Val: %.0f%n",
                MIA.typesItemArray().get(1).getObj().watt64Member().getVal());
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullWatt64Member(MIA);
        In_Req_Ok = In_Req_Ok && MemberTypesProperty.isChangedWatt64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullWatt64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedWatt64Member(EO);
        System.out.format(Locale.US, "Property Parameter Val: %.2f%n", MemberTypesProperty.getWatt64Member(EO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullWatt64Member(EO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedWatt64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullWatt64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedWatt64Member(AEO);
        System.out.format(Locale.US, "Property Parameter Val: %.0f%n", MemberTypesProperty.getWatt64Member(AEO));
        Null_Ok = Null_Ok && !MemberTypesProperty.isNullWatt64Member(AEO);
        In_Req_Ok = In_Req_Ok && !MemberTypesProperty.isChangedWatt64Member(AEO);

        // Array test
        for (int ix = 0; ix < ParameterArrays.getWatt64ParameterArraySize(); ix++) {
            // MemberArray
            Null_Ok = Null_Ok && MA1.watt64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && !MA1.watt64Member().get(ix).isChanged();
            MA1.watt64Member().get(ix).setVal(ParameterArrays.getWatt64Parameter(ix));
            Null_Ok = Null_Ok && !MA1.watt64Member().get(ix).isNull();
            In_Req_Ok = In_Req_Ok && MA1.watt64Member().get(ix).isChanged();

            // MemberArray
            Null_Ok = Null_Ok && MemberArraysProperty.isNullWatt64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedWatt64Member(MA2, ix);
            MA2.watt64Member().get(ix).setVal(MA1.watt64Member().get(ix).getVal());
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullWatt64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedWatt64Member(MA2, ix);

            System.out.format(Locale.US, "Val %d: %.0f%n", ix, MemberArraysProperty.getWatt64Member(MA2, ix));

            // Member Item
            ArraysItem item = new ArraysItem();
            item.watt64Member().get(ix).setVal(MA1.watt64Member().get(ix).getVal());
            MI.arraysItem().setObj(item);
            System.out.format(Locale.US, "Array Item Val %d: %.0f%n", ix, MemberArraysProperty.getWatt64Member(MI, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullWatt64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedWatt64Member(MI, ix);

            // Member Item Array
            MIA.arraysItemArray().get(1).setObj(item);
            System.out.format(Locale.US, "Array Item Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getWatt64Member(MIA, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullWatt64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && MemberArraysProperty.isChangedWatt64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullWatt64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedWatt64Member(EO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.2f%n", ix,
                    MemberArraysProperty.getWatt64Member(EO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullWatt64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedWatt64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullWatt64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedWatt64Member(AEO, ix);
            System.out.format(Locale.US, "Parameter Array Val %d: %.0f%n", ix,
                    MemberArraysProperty.getWatt64Member(AEO, ix));
            Null_Ok = Null_Ok && !MemberArraysProperty.isNullWatt64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !MemberArraysProperty.isChangedWatt64Member(AEO, ix);
        }

        // setNull test
        MT1.watt64Member().setNull();
        MemberTypesProperty.setNullWatt64Member(MT2);
        MA1.watt64Member().get(1).setNull();
        MemberArraysProperty.setNullWatt64Member(MA2, 1);
        Null_Ok = Null_Ok && MT1.watt64Member().isNull() && MemberTypesProperty.isNullWatt64Member(MT2)
                && MA1.watt64Member().get(1).isNull() && MemberArraysProperty.isNullWatt64Member(MA2, 1);

        System.out.println("Is_Null OK: " + Null_Ok);
        System.out.println("Is_Changed OK: " + In_Req_Ok);
    }

    private static void test_TestException() {
        Header("TestException");

        try {
            throw new TestException("Testing a TestException");
        } catch (TestException e) {
            System.out.println("Caught exception: " + Operations.getName(e.getTypeId()));
        }
    }

    static void test_LibraryExceptions() {
        Header("LibraryExceptions");

        LibraryExceptions.getInstance().set(new TestException("For LibraryExceptions"));
        try {
            LibraryExceptions.getInstance().Throw();
        } catch (TestException e) {
            System.out.println("Caught exception: " + Operations.getName(e.getTypeId()));
        }

        LibraryExceptions.getInstance().set(new java.lang.RuntimeException("For LibraryExceptions"));
        try {
            LibraryExceptions.getInstance().Throw();
        } catch (java.lang.RuntimeException e) {
            System.out.println("Caught native exception");
        }
        LibraryExceptions.getInstance();
    }

    private static String ctStr(CollectionType ct) {
        if (ct == CollectionType.ARRAY)
            return "Array";
        else if (ct == CollectionType.DICTIONARY)
            return "Dictionary";
        else if (ct == CollectionType.SEQUENCE)
            return "Sequence";
        else if (ct == CollectionType.SINGLE_VALUE)
            return "Single";
        else
            return "Unknown";
    }

    private static String TestEnumStr(TestEnum e) {
        if (e == TestEnum.MY_FIRST)
            return "MyFirst";
        else if (e == TestEnum.MY_SECOND)
            return "MySecond";
        else if (e == TestEnum.MY_THIRD)
            return "MyThird";
        else
            return "Unknown";
    }

    private static void Test_IsProperty() {
        Header("IsProperty");
        long[] ids = Operations.getAllTypeIds();
        for (int i = 0; i < ids.length; i++) {
            if (Operations.isProperty(ids[i])) {
                // only care about the ones that are ours
                if (Operations.getName(ids[i]).startsWith("DotsTest")) {
                    System.out.println(Operations.getName(ids[i]));
                }
            }
        }
    }

    private static void Test_IsEnumeration() {
        Header("IsEnumeration");
        long[] ids = Operations.getAllTypeIds();
        for (int i = 0; i < ids.length; i++) {
            if (Operations.isEnumeration(ids[i])) {
                // only care about the ones that are ours
                if (Operations.getName(ids[i]).startsWith("DotsTest")) {
                    System.out.println(Operations.getName(ids[i]));
                }
            }
        }
    }

    private static void Test_IsException() {
        Header("IsException");
        long[] ids = Operations.getAllTypeIds();
        for (int i = 0; i < ids.length; i++) {
            if (Operations.isException(ids[i])) {
                // only care about the ones that are ours
                if (Operations.getName(ids[i]).startsWith("DotsTest")) {
                    System.out.println(Operations.getName(ids[i]));
                }
            }
        }
    }

    /*
     * This test attempts to deserialize a piece of xml that represents a class
     * that is part of a dou library whose jar is *not* in the classpath of this
     * test. The idea being that this test will succeed if dots_cpp has loaded
     * all the required jars as specified by typesystem.ini
     */
    private static void Test_DeserializeUnlinkedObject() {
        Header("DeserializeUnlinkedObject");
        String xml = "<?xml version=\"1.0\" encoding=\"utf-8\"?><DotsTest.ExtraObject><Int32Member>-32</Int32Member></DotsTest.ExtraObject>";
        com.saabgroup.safir.dob.typesystem.Object obj = Serialization.toObject(xml);
        System.out.println("Class name: " + Operations.getName(obj.getTypeId()));
    }

    private static int tests = 0;
    private static int failures = 0;

    static class MiscTests {

        private void check(boolean expr) {
            ++tests;
            if (!expr) {
                ++failures;
                System.out.println("Testcase " + tests + " failed!");
            }
        }

        private void check(boolean expr, String description) {
            ++tests;
            if (!expr) {
                ++failures;
                System.out.println("Testcase " + tests + " (" + description + ") failed!");
            }
        }

        public void test_Containers() {

            // int32 container testing
            {
                Int32Container intCont = new Int32Container();
                check(intCont.isNull());
                check(!intCont.isChanged());
                intCont.setVal(10);
                check(intCont.getVal() == 10);
                check(!intCont.isNull());
                check(intCont.isChanged());
                intCont.setNull();
                try {
                    intCont.getVal();
                    check(false);
                } catch (NullException n) {
                    check(true);
                }

                check(intCont.isNull());
                check(intCont.isChanged());
                intCont.setChanged(false);
                check(intCont.isNull());
                check(!intCont.isChanged());
                intCont.setChanged(true);
                check(intCont.isNull());
                check(intCont.isChanged());
            }

            // boolean container testing
            {
                BooleanContainer boolCont = new BooleanContainer();
                check(boolCont.isNull());
                check(!boolCont.isChanged());
                boolCont.setVal(true);
                check(boolCont.getVal());
                check(!boolCont.isNull());
                check(boolCont.isChanged());
                boolCont.setNull();
                try {
                    boolCont.getVal();
                    check(false);
                } catch (NullException n) {
                    check(true);
                }

                check(boolCont.isNull());
                check(boolCont.isChanged());
                boolCont.setChanged(false);
                check(boolCont.isNull());
                check(!boolCont.isChanged());
                boolCont.setChanged(true);
                check(boolCont.isNull());
                check(boolCont.isChanged());
            }

            // string container testing
            {
                StringContainer strCont = new StringContainer();
                check(strCont.isNull());
                check(!strCont.isChanged());
                check(strCont.utf8StringLength() == 0);
                strCont.setVal("foo");
                check(strCont.getVal() == "foo");
                check(strCont.utf8StringLength() == 4);
                check(!strCont.isNull());
                check(strCont.isChanged());
                strCont.setNull();
                try {
                    strCont.getVal();
                    check(false);
                } catch (NullException n) {
                    check(true);
                }

                check(strCont.isNull());
                check(strCont.isChanged());
                strCont.setChanged(false);
                check(strCont.isNull());
                check(!strCont.isChanged());
                strCont.setChanged(true);
                check(strCont.isNull());
                check(strCont.isChanged());
            }

            // Enum container testing
            {
                EnumerationContainerBase<com.saabgroup.dotstest.TestEnum> enumCont = new com.saabgroup.dotstest.TestEnum.Container();
                check(enumCont.isNull());
                check(!enumCont.isChanged());
                enumCont.setVal(TestEnum.MY_SECOND);
                check(enumCont.getVal() == TestEnum.MY_SECOND);
                check(enumCont.getOrdinal() == 1);
                check(!enumCont.isNull());
                check(enumCont.isChanged());
                enumCont.setNull();
                try {
                    enumCont.getVal();
                    check(false);
                } catch (NullException n) {
                    check(true);
                }

                check(enumCont.isNull());
                check(enumCont.isChanged());
                enumCont.setChanged(false);
                check(enumCont.isNull());
                check(!enumCont.isChanged());
                enumCont.setChanged(true);
                check(enumCont.isNull());
                check(enumCont.isChanged());
                enumCont.setOrdinal(2);
                enumCont.setVal(TestEnum.MY_THIRD);
                check(enumCont.getVal() == TestEnum.MY_THIRD);
                check(enumCont.getOrdinal() == 2);
                check(!enumCont.isNull());
                check(enumCont.isChanged());
            }

            // object container testing
            {
                ObjectContainerImpl<com.saabgroup.safir.dob.typesystem.Object> objCont = new ObjectContainerImpl<com.saabgroup.safir.dob.typesystem.Object>();

                check(objCont.isNull());
                check(!objCont.isChanged());
                objCont.setObj(new com.saabgroup.safir.dob.typesystem.Object());
                check(objCont.getObj() != null);
                check(!objCont.isNull());
                check(objCont.isChanged());
                objCont.setNull();
                try {
                    objCont.getObj();
                    check(false);
                } catch (NullException n) {
                    check(true);
                }
                check(objCont.isNull());
                check(objCont.isChanged());
                objCont.setChanged(false);
                check(objCont.isNull());
                check(!objCont.isChanged());
                objCont.setChanged(true);
                check(objCont.isNull());
                check(objCont.isChanged());
            }

            // object container testing
            {
                ObjectContainerImpl<com.saabgroup.dotstest.TestItem> objCont = new ObjectContainerImpl<com.saabgroup.dotstest.TestItem>();

                check(objCont.isNull());
                check(!objCont.isChanged());
                objCont.setObj(new com.saabgroup.dotstest.TestItem());
                check(objCont.getObj() != null);
                check(!objCont.isNull());
                check(objCont.isChanged());
                objCont.setNull();
                try {
                    objCont.getObj();
                    check(false);
                } catch (NullException n) {
                    check(true);
                }
                check(objCont.isNull());
                check(objCont.isChanged());
                objCont.setChanged(false);
                check(objCont.isNull());
                check(!objCont.isChanged());
                objCont.setChanged(true);
                check(objCont.isNull());
                check(objCont.isChanged());
            }

            // test object factory
            {
                com.saabgroup.safir.dob.typesystem.Object object = ObjectFactory.getInstance()
                        .createObject(com.saabgroup.safir.dob.typesystem.Object.ClassTypeId);
                check(object.getTypeId() == com.saabgroup.safir.dob.typesystem.Object.ClassTypeId);
                object = ObjectFactory.getInstance().createObject(MemberTypes.ClassTypeId);
                check(object.getTypeId() == MemberTypes.ClassTypeId);
                object = ObjectFactory.getInstance().createObject(TestItem.ClassTypeId);
                check(object.getTypeId() == TestItem.ClassTypeId);
            }

            {
                MemberTypes t = new com.saabgroup.dotstest.MemberTypes();
                check(t.int32Member().isNull());
                check(!t.int32Member().isChanged());
                t.int32Member().setVal(10);
                check(!t.int32Member().isNull());
                check(t.int32Member().isChanged());
                check(t.int32Member().getVal() == 10);

                check(t.int64Member().isNull());
                check(!t.int64Member().isChanged());
                t.int64Member().setVal(20L);
                check(!t.int64Member().isNull());
                check(t.int64Member().isChanged());
                check(t.int64Member().getVal() == 20);

                check(t.float32Member().isNull());
                check(!t.float32Member().isChanged());
                t.float32Member().setVal(10.01f);
                check(!t.float32Member().isNull());
                check(t.float32Member().isChanged());
                check(t.float32Member().getVal() == 10.01f);

                check(t.ampere32Member().isNull());
                check(!t.ampere32Member().isChanged());
                t.ampere32Member().setVal(10.01f);
                check(!t.ampere32Member().isNull());
                check(t.ampere32Member().isChanged());
                check(t.ampere32Member().getVal() == 10.01f);

                check(t.float64Member().isNull());
                check(!t.float64Member().isChanged());
                t.float64Member().setVal(20.02);
                check(!t.float64Member().isNull());
                check(t.float64Member().isChanged());
                check(t.float64Member().getVal() == 20.02);

                check(t.stringMember().isNull());
                check(!t.stringMember().isChanged());
                t.stringMember().setVal("foo");
                check(!t.stringMember().isNull());
                check(t.stringMember().isChanged());
                check(t.stringMember().getVal().equals("foo"));

                check(t.binaryMember().isNull());
                check(!t.binaryMember().isChanged());
                byte[] bytes = new byte[10];
                for (byte i = 0; i < 10; ++i) {
                    bytes[i] = (byte) ((byte) 100 - i);
                }
                t.binaryMember().setVal(bytes);
                check(!t.binaryMember().isNull());
                check(t.binaryMember().isChanged());
                for (byte i = 0; i < 10; ++i) {
                    check(t.binaryMember().getVal()[i] == (byte) ((byte) 100 - i));
                }

                t.booleanMember().setVal(true);
                check(!t.booleanMember().isNull());
                check(t.booleanMember().isChanged());
                check(t.booleanMember().getVal());

                check(t.typeIdMember().isNull());
                check(!t.typeIdMember().isChanged());
                t.typeIdMember().setVal(MemberTypes.ClassTypeId);
                check(!t.typeIdMember().isNull());
                check(t.typeIdMember().isChanged());
                check(t.typeIdMember().getVal() == MemberTypes.ClassTypeId);

                t.enumerationMember().setVal(com.saabgroup.dotstest.TestEnum.MY_SECOND);
                check(!t.enumerationMember().isNull());
                check(t.enumerationMember().isChanged());
                check(t.enumerationMember().getVal() == com.saabgroup.dotstest.TestEnum.MY_SECOND);

                check(t.instanceIdMember().isNull());
                check(!t.instanceIdMember().isChanged());
                t.instanceIdMember().setVal(new com.saabgroup.safir.dob.typesystem.InstanceId("foo"));
                check(!t.instanceIdMember().isNull());
                check(t.instanceIdMember().isChanged());
                check(t.instanceIdMember().getVal().equals(new com.saabgroup.safir.dob.typesystem.InstanceId("foo")),
                        "instanceId equals");

                check(t.instanceIdMember2().isNull());
                check(!t.instanceIdMember2().isChanged());
                t.instanceIdMember2().setVal(new com.saabgroup.safir.dob.typesystem.InstanceId(6699318081062747564L));
                check(!t.instanceIdMember2().isNull());
                check(t.instanceIdMember2().isChanged());
                check(t.instanceIdMember2().getVal().equals(
                        new com.saabgroup.safir.dob.typesystem.InstanceId(6699318081062747564L)), "instanceId equals");

                check(t.channelIdMember().isNull());
                check(!t.channelIdMember().isChanged());
                t.channelIdMember().setVal(new com.saabgroup.safir.dob.typesystem.ChannelId("foo"));
                check(!t.channelIdMember().isNull());
                check(t.channelIdMember().isChanged());
                check(t.channelIdMember().getVal().equals(new com.saabgroup.safir.dob.typesystem.ChannelId("foo")),
                        "channelId equals");

                check(t.channelIdMember2().isNull());
                check(!t.channelIdMember2().isChanged());
                t.channelIdMember2().setVal(new com.saabgroup.safir.dob.typesystem.ChannelId(6699318081062747564L));
                check(!t.channelIdMember2().isNull());
                check(t.channelIdMember2().isChanged());
                check(t.channelIdMember2().getVal().equals(
                        new com.saabgroup.safir.dob.typesystem.ChannelId(6699318081062747564L)), "channelId equals");

                check(t.handlerIdMember().isNull());
                check(!t.handlerIdMember().isChanged());
                t.handlerIdMember().setVal(new com.saabgroup.safir.dob.typesystem.HandlerId("foo"));
                check(!t.handlerIdMember().isNull());
                check(t.handlerIdMember().isChanged());
                check(t.handlerIdMember().getVal().equals(new com.saabgroup.safir.dob.typesystem.HandlerId("foo")),
                        "handlerId equals");

                check(t.handlerIdMember2().isNull());
                check(!t.handlerIdMember2().isChanged());
                t.handlerIdMember2().setVal(new com.saabgroup.safir.dob.typesystem.HandlerId(6699318081062747564L));
                check(!t.handlerIdMember2().isNull());
                check(t.handlerIdMember2().isChanged());
                check(t.handlerIdMember2().getVal().equals(
                        new com.saabgroup.safir.dob.typesystem.HandlerId(6699318081062747564L)), "handlerId equals");

                check(t.entityIdMember().isNull());
                check(!t.entityIdMember().isChanged());
                t.entityIdMember()
                        .setVal(new EntityId(com.saabgroup.dotstest.TestItem.ClassTypeId, new InstanceId("foo")));
                check(!t.entityIdMember().isNull());
                check(t.entityIdMember().isChanged());
                check(t.entityIdMember().getVal()
                        .equals(new EntityId(com.saabgroup.dotstest.TestItem.ClassTypeId, new InstanceId("foo"))),
                        "entityId equals");

                check(t.entityIdMember2().isNull());
                check(!t.entityIdMember2().isChanged());
                t.entityIdMember2()
                        .setVal(new EntityId(com.saabgroup.dotstest.TestItem.ClassTypeId, new InstanceId(1231234)));
                check(!t.entityIdMember2().isNull());
                check(t.entityIdMember2().isChanged());
                check(t.entityIdMember2().getVal()
                        .equals(new EntityId(com.saabgroup.dotstest.TestItem.ClassTypeId, new InstanceId(1231234))),
                        "entityId equals");

                t.testClassMember().setObj(new TestItem());
                check(!t.testClassMember().isNull());
                check(t.testClassMember().isChanged());
                check(t.testClassMember().getObj().myInt().isNull());
                check(!t.testClassMember().getObj().myInt().isChanged());
                t.testClassMember().getObj().myInt().setVal(11);
                check(!t.testClassMember().getObj().myInt().isNull());
                check(t.testClassMember().getObj().myInt().isChanged());
                t.testClassMember().setChangedHere(false);
                check(!t.testClassMember().isNull());
                check(t.testClassMember().isChanged());
                check(!t.testClassMember().isChangedHere());
                check(!t.testClassMember().getObj().myInt().isNull());
                check(t.testClassMember().getObj().myInt().isChanged());
                check(t.testClassMember().getObj().myInt().getVal() == 11);

                t.objectMember().setObj(t.testClassMember().getObj());

                long[] types = Operations.getAllTypeIds();
                for (int i = 0; i < types.length; i++) {
                    String name = Operations.getName(types[i]);
                    check(types[i] == Operations.getTypeId(name), name);
                }

                try {
                    Operations.getName(0);
                    check(false);
                } catch (IllegalValueException exc) {
                    check(true);
                }

                check(Operations.getEnumerationValueName(7765043459684641017L, 0).equals("MyFirst"));
                try {
                    Operations.getEnumerationValueName(0, 0);
                } catch (IllegalValueException exc) {
                    check(true);
                }
                String xml = Serialization.toXml(t);
                Serialization.toObject(xml);
                byte[] binary = Serialization.toBinary(t);
                t = null;
                MemberTypes t2 = (MemberTypes) Serialization.toObject(binary);

                check(!t2.int32Member().isNull());
                check(t2.int32Member().isChanged());
                check(t2.int32Member().getVal() == 10);

                check(!t2.int64Member().isNull());
                check(t2.int64Member().isChanged());
                check(t2.int64Member().getVal() == 20L);

                check(!t2.typeIdMember().isNull());
                check(t2.typeIdMember().isChanged());
                check(t2.typeIdMember().getVal() == MemberTypes.ClassTypeId);

                check(!t2.float32Member().isNull());
                check(t2.float32Member().isChanged());
                check(t2.float32Member().getVal() == 10.01f);

                check(!t2.ampere32Member().isNull());
                check(t2.ampere32Member().isChanged());
                check(t2.ampere32Member().getVal() == 10.01f);

                check(!t2.float64Member().isNull());
                check(t2.float64Member().isChanged());
                check(t2.float64Member().getVal() == 20.02);

                check(!t2.stringMember().isNull());
                check(t2.stringMember().isChanged());
                check(t2.stringMember().getVal().equals("foo"));

                check(!t2.binaryMember().isNull());
                check(t2.binaryMember().isChanged());
                for (byte i = 0; i < 10; ++i) {
                    check(t2.binaryMember().getVal()[i] == (byte) ((byte) 100 - i));
                }

                check(!t2.booleanMember().isNull());
                check(t2.booleanMember().isChanged());
                check(t2.booleanMember().getVal());

                check(!t2.instanceIdMember().isNull());
                check(t2.instanceIdMember().isChanged());
                check(t2.instanceIdMember().getVal().equals(new com.saabgroup.safir.dob.typesystem.InstanceId("foo")),
                        "instanceId equals");

                check(!t2.instanceIdMember2().isNull());
                check(t2.instanceIdMember2().isChanged());
                check(t2.instanceIdMember2().getVal().equals(
                        new com.saabgroup.safir.dob.typesystem.InstanceId(6699318081062747564L)), "instanceId equals");

                check(!t2.channelIdMember().isNull());
                check(t2.channelIdMember().isChanged());
                check(t2.channelIdMember().getVal().equals(new com.saabgroup.safir.dob.typesystem.ChannelId("foo")),
                        "channelId equals");

                check(!t2.channelIdMember2().isNull());
                check(t2.channelIdMember2().isChanged());
                check(t2.channelIdMember2().getVal().equals(
                        new com.saabgroup.safir.dob.typesystem.ChannelId(6699318081062747564L)), "channelId equals");

                check(!t2.handlerIdMember().isNull());
                check(t2.handlerIdMember().isChanged());
                check(t2.handlerIdMember().getVal().equals(new com.saabgroup.safir.dob.typesystem.HandlerId("foo")),
                        "handlerId equals");

                check(!t2.handlerIdMember2().isNull());
                check(t2.handlerIdMember2().isChanged());
                check(t2.handlerIdMember2().getVal().equals(
                        new com.saabgroup.safir.dob.typesystem.HandlerId(6699318081062747564L)), "handlerId equals");

                check(!t2.entityIdMember().isNull());
                check(t2.entityIdMember().isChanged());
                check(t2.entityIdMember().getVal()
                        .equals(new EntityId(com.saabgroup.dotstest.TestItem.ClassTypeId, new InstanceId("foo"))),
                        "entityId equals");

                check(!t2.entityIdMember2().isNull());
                check(t2.entityIdMember2().isChanged());
                check(t2.entityIdMember2().getVal()
                        .equals(new EntityId(com.saabgroup.dotstest.TestItem.ClassTypeId, new InstanceId(1231234))),
                        "entityId equals");

                t2.enumerationMember().setVal(com.saabgroup.dotstest.TestEnum.MY_SECOND);
                check(!t2.enumerationMember().isNull());
                check(t2.enumerationMember().isChanged());
                check(t2.enumerationMember().getVal() == com.saabgroup.dotstest.TestEnum.MY_SECOND);

                check(!t2.testClassMember().isNull());
                check(t2.testClassMember().isChanged());
                check(!t2.testClassMember().isChangedHere());
                check(!t2.testClassMember().getObj().myInt().isNull());
                check(t2.testClassMember().getObj().myInt().isChanged());
                check(t2.testClassMember().getObj().myInt().getVal() == 11);

                check(!t2.objectMember().isNull());
                check(t2.objectMember().isChanged());
                check(t2.objectMember().isChangedHere());
                check(!((TestItem) t2.objectMember().getObj()).myInt().isNull());
                check(((TestItem) t2.objectMember().getObj()).myInt().isChanged());
                check(((TestItem) t2.objectMember().getObj()).myInt().getVal() == 11);

            }

            // library exceptions
            {
                LibraryExceptions.getInstance().set(new TestException("For LibraryExceptions"));
                try {
                    LibraryExceptions.getInstance().Throw();
                    check(false);
                } catch (TestException e) {
                    check(e.getTypeId() == TestException.ExceptionTypeId);
                }

                LibraryExceptions.getInstance().set(new TestException2("For LibraryExceptions"));
                try {
                    LibraryExceptions.getInstance().Throw();
                    check(false);
                } catch (TestException2 e) {
                    check(e.getTypeId() == TestException2.ExceptionTypeId);
                }

                LibraryExceptions.getInstance().set(new java.lang.RuntimeException("For LibraryExceptions"));
                try {
                    LibraryExceptions.getInstance().Throw();
                    check(false);
                } catch (java.lang.RuntimeException e) {
                    check(true);
                }

            }

            // array types
            {
                MemberArrays t = new com.saabgroup.dotstest.MemberArrays();
                check(t.int32Member().get(1).isNull());
                check(!t.int32Member().get(1).isChanged());
                t.int32Member().get(1).setVal(10);
                check(!t.int32Member().get(1).isNull());
                check(t.int32Member().get(1).isChanged());
                check(t.int32Member().get(1).getVal() == 10);

                check(t.int64Member().get(1).isNull());
                check(!t.int64Member().get(1).isChanged());
                t.int64Member().get(1).setVal(20L);
                check(!t.int64Member().get(1).isNull());
                check(t.int64Member().get(1).isChanged());
                check(t.int64Member().get(1).getVal() == 20);

                check(t.float32Member().get(1).isNull());
                check(!t.float32Member().get(1).isChanged());
                t.float32Member().get(1).setVal(10.01f);
                check(!t.float32Member().get(1).isNull());
                check(t.float32Member().get(1).isChanged());
                check(t.float32Member().get(1).getVal() == 10.01f);

                check(t.ampere32Member().get(1).isNull());
                check(!t.ampere32Member().get(1).isChanged());
                t.ampere32Member().get(1).setVal(10.01f);
                check(!t.ampere32Member().get(1).isNull());
                check(t.ampere32Member().get(1).isChanged());
                check(t.ampere32Member().get(1).getVal() == 10.01f);

                check(t.float64Member().get(1).isNull());
                check(!t.float64Member().get(1).isChanged());
                t.float64Member().get(1).setVal(20.02);
                check(!t.float64Member().get(1).isNull());
                check(t.float64Member().get(1).isChanged());
                check(t.float64Member().get(1).getVal() == 20.02);

                check(t.stringMember().get(1).isNull());
                check(!t.stringMember().get(1).isChanged());
                t.stringMember().get(1).setVal("foo");
                check(!t.stringMember().get(1).isNull());
                check(t.stringMember().get(1).isChanged());
                check(t.stringMember().get(1).getVal().equals("foo"));

                check(t.binaryMember().get(1).isNull());
                check(!t.binaryMember().get(1).isChanged());
                byte[] bytes = new byte[10];
                for (byte i = 0; i < 10; ++i) {
                    bytes[i] = (byte) ((byte) 100 - i);
                }
                t.binaryMember().get(1).setVal(bytes);
                check(!t.binaryMember().get(1).isNull());
                check(t.binaryMember().get(1).isChanged());
                for (byte i = 0; i < 10; ++i) {
                    check(t.binaryMember().get(1).getVal()[i] == (byte) ((byte) 100 - i));
                }

                t.booleanMember().get(1).setVal(true);
                check(!t.booleanMember().get(1).isNull());
                check(t.booleanMember().get(1).isChanged());
                check(t.booleanMember().get(1).getVal());

                check(t.typeIdMember().get(1).isNull());
                check(!t.typeIdMember().get(1).isChanged());
                t.typeIdMember().get(1).setVal(20L);
                check(!t.typeIdMember().get(1).isNull());
                check(t.typeIdMember().get(1).isChanged());
                check(t.typeIdMember().get(1).getVal() == 20);

                t.enumerationMember().get(1).setVal(com.saabgroup.dotstest.TestEnum.MY_SECOND);
                check(!t.enumerationMember().get(1).isNull());
                check(t.enumerationMember().get(1).isChanged());
                check(t.enumerationMember().get(1).getVal() == com.saabgroup.dotstest.TestEnum.MY_SECOND);

                check(t.instanceIdMember().get(1).isNull());
                check(!t.instanceIdMember().get(1).isChanged());
                t.instanceIdMember().get(1).setVal(new com.saabgroup.safir.dob.typesystem.InstanceId("foo"));
                check(!t.instanceIdMember().get(1).isNull());
                check(t.instanceIdMember().get(1).isChanged());
                check(t.instanceIdMember().get(1).getVal()
                        .equals(new com.saabgroup.safir.dob.typesystem.InstanceId("foo")), "instanceId equals");

                check(t.channelIdMember().get(1).isNull());
                check(!t.channelIdMember().get(1).isChanged());
                t.channelIdMember().get(1).setVal(new com.saabgroup.safir.dob.typesystem.ChannelId("foo"));
                check(!t.channelIdMember().get(1).isNull());
                check(t.channelIdMember().get(1).isChanged());
                check(t.channelIdMember().get(1).getVal()
                        .equals(new com.saabgroup.safir.dob.typesystem.ChannelId("foo")), "channelId equals");

                check(t.handlerIdMember().get(1).isNull());
                check(!t.handlerIdMember().get(1).isChanged());
                t.handlerIdMember().get(1).setVal(new com.saabgroup.safir.dob.typesystem.HandlerId("foo"));
                check(!t.handlerIdMember().get(1).isNull());
                check(t.handlerIdMember().get(1).isChanged());
                check(t.handlerIdMember().get(1).getVal()
                        .equals(new com.saabgroup.safir.dob.typesystem.HandlerId("foo")), "handlerId equals");

                check(t.entityIdMember().get(1).isNull());
                check(!t.entityIdMember().get(1).isChanged());
                t.entityIdMember().get(1)
                        .setVal(new EntityId(com.saabgroup.dotstest.TestItem.ClassTypeId, new InstanceId("foo")));
                check(!t.entityIdMember().get(1).isNull());
                check(t.entityIdMember().get(1).isChanged());
                check(t.entityIdMember().get(1).getVal()
                        .equals(new EntityId(com.saabgroup.dotstest.TestItem.ClassTypeId, new InstanceId("foo"))),
                        "entityId equals");

                t.testClassMember().get(1).setObj(new TestItem());
                check(!t.testClassMember().get(1).isNull());
                check(t.testClassMember().get(1).isChanged());
                check(t.testClassMember().get(1).getObj().myInt().isNull());
                check(!t.testClassMember().get(1).getObj().myInt().isChanged());
                t.testClassMember().get(1).getObj().myInt().setVal(11);
                check(!t.testClassMember().get(1).getObj().myInt().isNull());
                check(t.testClassMember().get(1).getObj().myInt().isChanged());
                t.testClassMember().get(1).setChangedHere(false);
                check(!t.testClassMember().get(1).isNull());
                check(t.testClassMember().get(1).isChanged());
                check(!t.testClassMember().get(1).isChangedHere());
                check(!t.testClassMember().get(1).getObj().myInt().isNull());
                check(t.testClassMember().get(1).getObj().myInt().isChanged());
                check(t.testClassMember().get(1).getObj().myInt().getVal() == 11);

                t.objectMember().get(1).setObj(t.testClassMember().get(1).getObj());

                long[] types = Operations.getAllTypeIds();
                for (int i = 0; i < types.length; i++) {
                    String name = Operations.getName(types[i]);
                    check(types[i] == Operations.getTypeId(name), name);
                }

                try {
                    Operations.getName(0);
                    check(false);
                } catch (IllegalValueException exc) {
                    check(true);
                }

                check(Operations.getEnumerationValueName(7765043459684641017L, 0).equals("MyFirst"));
                try {
                    Operations.getEnumerationValueName(0, 0);
                } catch (IllegalValueException exc) {
                    check(true);
                }

                byte[] blob = Serialization.toBinary(t);

                t = null;
                MemberArrays t2 = (MemberArrays) Serialization.toObject(blob);
                check(!t2.int32Member().get(1).isNull());
                check(t2.int32Member().get(1).isChanged());
                check(t2.int32Member().get(1).getVal() == 10);

                check(!t2.int64Member().get(1).isNull());
                check(t2.int64Member().get(1).isChanged());
                check(t2.int64Member().get(1).getVal() == 20L);

                check(!t2.typeIdMember().get(1).isNull());
                check(t2.typeIdMember().get(1).isChanged());
                check(t2.typeIdMember().get(1).getVal() == 20L);

                check(!t2.float32Member().get(1).isNull());
                check(t2.float32Member().get(1).isChanged());
                check(t2.float32Member().get(1).getVal() == 10.01f);

                check(!t2.ampere32Member().get(1).isNull());
                check(t2.ampere32Member().get(1).isChanged());
                check(t2.ampere32Member().get(1).getVal() == 10.01f);

                check(!t2.float64Member().get(1).isNull());
                check(t2.float64Member().get(1).isChanged());
                check(t2.float64Member().get(1).getVal() == 20.02);

                check(!t2.stringMember().get(1).isNull());
                check(t2.stringMember().get(1).isChanged());
                check(t2.stringMember().get(1).getVal().equals("foo"));

                check(!t2.binaryMember().get(1).isNull());
                check(t2.binaryMember().get(1).isChanged());
                for (byte i = 0; i < 10; ++i) {
                    check(t2.binaryMember().get(1).getVal()[i] == (byte) ((byte) 100 - i));
                }

                check(!t2.booleanMember().get(1).isNull());
                check(t2.booleanMember().get(1).isChanged());
                check(t2.booleanMember().get(1).getVal());

                check(!t2.instanceIdMember().get(1).isNull());
                check(t2.instanceIdMember().get(1).isChanged());
                check(t2.instanceIdMember().get(1).getVal()
                        .equals(new com.saabgroup.safir.dob.typesystem.InstanceId("foo")), "instanceId equals");

                check(!t2.channelIdMember().get(1).isNull());
                check(t2.channelIdMember().get(1).isChanged());
                check(t2.channelIdMember().get(1).getVal()
                        .equals(new com.saabgroup.safir.dob.typesystem.ChannelId("foo")), "channelId equals");

                check(!t2.handlerIdMember().get(1).isNull());
                check(t2.handlerIdMember().get(1).isChanged());
                check(t2.handlerIdMember().get(1).getVal()
                        .equals(new com.saabgroup.safir.dob.typesystem.HandlerId("foo")), "handlerId equals");

                check(!t2.entityIdMember().get(1).isNull());
                check(t2.entityIdMember().get(1).isChanged());
                check(t2.entityIdMember().get(1).getVal()
                        .equals(new EntityId(com.saabgroup.dotstest.TestItem.ClassTypeId, new InstanceId("foo"))),
                        "entityId equals");

                t2.enumerationMember().get(1).setVal(com.saabgroup.dotstest.TestEnum.MY_SECOND);
                check(!t2.enumerationMember().get(1).isNull());
                check(t2.enumerationMember().get(1).isChanged());
                check(t2.enumerationMember().get(1).getVal() == com.saabgroup.dotstest.TestEnum.MY_SECOND);

                check(!t2.testClassMember().get(1).isNull());
                check(t2.testClassMember().get(1).isChanged());
                check(!t2.testClassMember().get(1).isChangedHere());
                check(!t2.testClassMember().get(1).getObj().myInt().isNull());
                check(t2.testClassMember().get(1).getObj().myInt().isChanged());
                check(t2.testClassMember().get(1).getObj().myInt().getVal() == 11);

                check(!t2.objectMember().get(1).isNull());
                check(t2.objectMember().get(1).isChanged());
                check(t2.objectMember().get(1).isChangedHere());
                check(!((TestItem) t2.objectMember().get(1).getObj()).myInt().isNull());
                check(((TestItem) t2.objectMember().get(1).getObj()).myInt().isChanged());
                check(((TestItem) t2.objectMember().get(1).getObj()).myInt().getVal() == 11);

            }

            //sequences
            {
                MemberSequences ms= new MemberSequences();

                check(ms.int32Member().isNull());
                check(ms.int32Member().isEmpty());
                check(!ms.int32Member().isChanged());
                ms.int32Member().add(20);
                ms.int32Member().add(30);
                ms.int32Member().add(0, 10);
                check(!ms.int32Member().isNull());
                check(ms.int32Member().isChanged());
                check(!ms.int32Member().isEmpty());
                check(ms.int32Member().size() == 3);
                ms.int32Member().setChanged(false);
                ms.int32Member().setNull();
                check(ms.int32Member().isNull());
                check(ms.int32Member().isEmpty());
                check(ms.int32Member().isChanged());

                //check recursiveness
                check(!ms.testClassMember().isChanged());
                ms.testClassMember().add(new TestItem());
                check(ms.testClassMember().isChanged());
                ms.testClassMember().setChanged(false);
                ms.testClassMember().get(0).myInt().setVal(10);;
                check(ms.testClassMember().get(0).isChanged());
                check(ms.testClassMember().isChanged(), "sequence recursive ischanged");
                ms.testClassMember().get(0).setChanged(false);
                check(!ms.testClassMember().isChanged(), "sequence recursive ischanged 2");
                ms.testClassMember().setChanged(true);
                check(ms.testClassMember().isChanged());
                check(ms.testClassMember().get(0).isChanged(), "sequence recursive ischanged 3");
}

            //dictionaries
            {
                MemberDictionaries md= new MemberDictionaries();

                check(md.int32StringMember().isNull());
                check(md.int32StringMember().isEmpty());
                check(!md.int32StringMember().isChanged());
                md.int32StringMember().putVal(10,ParameterDictionaries.getInt32StringParameter(10));
                md.int32StringMember().putVal(20,ParameterDictionaries.getInt32StringParameter(10));
                check(!md.int32StringMember().isNull());
                check(md.int32StringMember().isChanged());
                check(!md.int32StringMember().isEmpty());
                check(md.int32StringMember().size() == 2);
                md.int32StringMember().setChanged(false);
                md.int32StringMember().setNull();
                check(md.int32StringMember().isNull());
                check(md.int32StringMember().isEmpty());
                check(md.int32StringMember().isChanged());

                //check recursiveness
                check(!md.int32ItemMember().isChanged());
                md.int32ItemMember().put(0, new ObjectContainerImpl<MemberDictionaries>());
                md.int32ItemMember().get(0).setObj(new MemberDictionaries());
                check(md.int32ItemMember().isChanged());
                md.int32ItemMember().setChanged(false);
                md.int32ItemMember().get(0).getObj().int32Int32Member().put(10,new Int32Container());
                md.int32ItemMember().get(0).getObj().int32Int32Member().get(10).setVal(10);
                check(md.int32ItemMember().get(0).isChanged());
                check(md.int32ItemMember().isChanged());
                md.int32ItemMember().get(0).setChanged(false);
                check(!md.int32ItemMember().isChanged());
                md.int32ItemMember().setChanged(true);
                check(md.int32ItemMember().isChanged());
                check(md.int32ItemMember().get(0).isChanged());
            }

            //sequence container Object specialization
            {
                Object.SequenceContainer oc = new Object.SequenceContainer();
                check(!oc.isChanged());
                check(!oc.isChangedHere());
                oc.setChangedHere(true);
                check(oc.isChanged());
                check(oc.isChangedHere());
                oc.setChanged(false);
                oc.add(new Object());
                check(oc.isChanged());
                check(oc.isChangedHere());
                TestItem ti = new TestItem();
                oc.add(ti);
                oc.setChanged(false);
                ti.myInt().setVal(10);
                check(oc.isChanged(), "recursive ischanged");
                check(!oc.isChangedHere(), "recursive ischanged 2");
                check(oc.get(1).isChanged(), "recursive ischanged 3");
            }

            // Some namespacing checks
            {
                {
                    nonamespace.TestItem t = new nonamespace.TestItem();
                    check(t != null);
                    com.saabgroup.safir.dob.typesystem.Object o = ObjectFactory.getInstance()
                            .createObject(nonamespace.TestItem.ClassTypeId);
                    check(o != null);
                    check(NamespaceMappings.getInstance().toJava("NoNamespace.TestItem")
                            .equals("nonamespace.TestItem"));
                }

                {
                    com.somecomp.othercompany.TestItem t = new com.somecomp.othercompany.TestItem();
                    check(t != null);
                    com.saabgroup.safir.dob.typesystem.Object o = ObjectFactory.getInstance()
                            .createObject(com.somecomp.othercompany.TestItem.ClassTypeId);
                    check(o != null);
                    check(NamespaceMappings.getInstance().toJava("OtherCompany.TestItem")
                            .equals("com.somecomp.othercompany.TestItem"));
                }

                {
                    org.yac.othercompany.nested.TestItem t = new org.yac.othercompany.nested.TestItem();
                    check(t != null);
                    com.saabgroup.safir.dob.typesystem.Object o = ObjectFactory.getInstance()
                            .createObject(org.yac.othercompany.nested.TestItem.ClassTypeId);
                    check(o != null);
                    check(NamespaceMappings.getInstance().toJava("OtherCompany.Nested.TestItem")
                            .equals("org.yac.othercompany.nested.TestItem"));
                }

                {
                    com.saabgroup.dotstest.TestItem t = new com.saabgroup.dotstest.TestItem();
                    check(t != null);
                    com.saabgroup.safir.dob.typesystem.Object o = ObjectFactory.getInstance()
                            .createObject(TestItem.ClassTypeId);
                    check(o != null);
                    check(NamespaceMappings.getInstance().toJava("DotsTest.TestItem")
                            .equals("com.saabgroup.dotstest.TestItem"));
                }

            }
        }

        interface Checks {
            void doCheck(Object obj);
        }

        void RunSerializationChecks(Object before,
                                    Checks checks)
        {
            //System.Console.WriteLine("-- BEFORE --");
            checks.doCheck(before);
            byte[] bin = Serialization.toBinary(before);
            before = null;
            Object after = Serialization.toObject(bin);
            //System.Console.WriteLine("-- AFTER --");
            checks.doCheck(after);
        }

        private void Test_BlobChangeFlags_member_types()
        {
            MemberTypes before = new MemberTypes();
            before.int32Member().setVal(10);
            before.stringMember().setVal("asdf");
            before.objectMember().setObj(new Object());

            //change flag only set inside item, not on the member itself
            before.testClassMember().setObj(new TestItem());
            before.testClassMember().getObj().myInt().setVal(1);
            before.testClassMember().setChangedHere(false);

            Checks checks = new Checks () {
                    public void doCheck(Object o) {
                        MemberTypes obj = (MemberTypes)o;
                        check(obj.isChanged());
                        check(obj.int32Member().isChanged(),"A");
                        check(obj.stringMember().isChanged(),"B");
                        check(obj.objectMember().isChanged(),"C");
                        check(!obj.objectMember().getObj().isChanged(),"D");
                        check(obj.testClassMember().isChanged(),"E");
                        check(!obj.testClassMember().isChangedHere(),"F");
                        check(obj.testClassMember().getObj().myInt().isChanged(),"G");
                    }
                };

            RunSerializationChecks(before,checks);
        }

        private void Test_BlobChangeFlags_member_types_2()
        {
            MemberTypes before = new MemberTypes();
            //change flag only set on member, not inside item
            before.testClassMember().setObj(new TestItem());
            before.testClassMember().getObj().myInt().setVal(1);
            before.setChanged(false);
            before.testClassMember().setChangedHere(true);

            Checks checks = new Checks () {
                    public void doCheck(Object o) {
                        MemberTypes obj = (MemberTypes)o;
                        check(obj.isChanged());
                        check(obj.testClassMember().isChanged());
                        check(obj.testClassMember().isChangedHere());
                        check(!obj.testClassMember().getObj().myInt().isChanged());
                    }
                };

            RunSerializationChecks(before,checks);
        }


        private void Test_BlobChangeFlags_member_arrays()
        {
            MemberArrays before = new MemberArrays();
            before.int32Member().get(0).setVal(10);
            before.stringMember().get(0).setVal("asdf");
            before.objectMember().get(1).setObj(new Object());

            //change flag only set inside item, not on the member itself
            before.testClassMember().get(0).setObj(new TestItem());
            before.testClassMember().get(0).getObj().myInt().setVal(1);
            before.testClassMember().get(0).setChangedHere(false);

            //change flag only set on member, not inside item
            before.testClassMember().get(1).setObj(new TestItem());
            before.testClassMember().get(1).getObj().myInt().setVal(1);
            before.testClassMember().get(1).setChanged(false);
            before.testClassMember().get(1).setChangedHere(true);

            Checks checks = new Checks () {
                    public void doCheck(Object o) {
                        MemberArrays obj = (MemberArrays)o;

                    check(obj.isChanged());
                    check(obj.int32Member().isChanged());
                    check(obj.int32Member().get(0).isChanged());
                    check(!obj.int32Member().get(1).isChanged());
                    check(obj.stringMember().isChanged());
                    check(obj.stringMember().get(0).isChanged());
                    check(!obj.stringMember().get(1).isChanged());
                    check(obj.objectMember().isChanged());
                    check(!obj.objectMember().get(0).isChanged());
                    check(obj.objectMember().get(1).isChanged());

                    check(obj.testClassMember().isChanged());
                    check(obj.testClassMember().get(0).isChanged());
                    check(!obj.testClassMember().get(0).isChangedHere());
                    check(obj.testClassMember().get(0).getObj().myInt().isChanged());
                    check(obj.testClassMember().get(1).isChanged());
                    check(obj.testClassMember().get(1).isChangedHere());
                    check(!obj.testClassMember().get(1).getObj().myInt().isChanged());
                    }
                };

            RunSerializationChecks(before,checks);
        }


        private void Test_BlobChangeFlags_member_sequences()
        {
            MemberSequences before = new MemberSequences();
            check(!before.isChanged());

            check(!before.int32Member().isChanged());
            check(!before.stringMember().isChanged());
            check(!before.objectMember().isChanged());
            check(!before.objectMember().isChangedHere());

            before.int32Member().add(10);
            before.stringMember().add("asdf");
            before.objectMember().add(new Object());


            //change flag only set inside item, not on the member itself
            before.testClassMember().add(new TestItem());
            before.testClassMember().get(0).myInt().setVal(1);
            before.testClassMember().setChangedHere(false);

            Checks checks = new Checks () {
                    public void doCheck(Object o) {
                        MemberSequences obj = (MemberSequences)o;

                    check(obj.isChanged());
                    check(obj.int32Member().isChanged());
                    check(obj.stringMember().isChanged());
                    check(obj.objectMember().isChanged());
                    check(obj.objectMember().isChangedHere());
                    check(!obj.objectMember().get(0).isChanged());

                    check(obj.testClassMember().isChanged());
                    check(obj.testClassMember().get(0).isChanged());
                    check(!obj.testClassMember().isChangedHere());
                    check(obj.testClassMember().get(0).myInt().isChanged());
                    }
                };

            RunSerializationChecks(before,checks);
        }


        private void Test_BlobChangeFlags_member_sequences_2()
        {
            MemberSequences before = new MemberSequences();

            //change flag only set on member, not inside item
            before.testClassMember().add(new TestItem());
            before.testClassMember().get(0).myInt().setVal(1);
            before.setChanged(false);
            before.testClassMember().setChangedHere(true);

            Checks checks = new Checks () {
                    public void doCheck(Object o) {
                        MemberSequences obj = (MemberSequences)o;

                    check(obj.isChanged());
                    check(obj.testClassMember().isChanged());
                    check(!obj.testClassMember().get(0).isChanged());
                    check(obj.testClassMember().isChangedHere());
                    check(!obj.testClassMember().get(0).myInt().isChanged());
                    }
                };

            RunSerializationChecks(before,checks);

        }


        private void Test_BlobChangeFlags_member_dictionaries()
        {
            MemberDictionaries before = new MemberDictionaries();
            check(!before.isChanged());

            check(!before.int32Int32Member().isChanged());
            check(!before.int32ObjectMember().isChanged());
            check(!before.int32ObjectMember().isChangedHere());

            // change flag only set inside value, not on the member itself
            before.int32Int32Member().putVal(10,10);
            before.int32Int64Member().putVal(20,20L);
            before.int32Int64Member().setChangedHere(false);

            // change flag only set on dict, not on value
            before.int64Int32Member().putVal(30L,30);
            before.int64Int32Member().setChanged(false);
            before.int64Int32Member().setChangedHere(true);

            //change flags set all over the place
            before.stringStringMember().putVal("asdf","adsf");
            before.int32ObjectMember().putObj(1,new Object());


            //On item dict members there are three change flag levels:
            //A: the dictionary, B: the item container, C: the members inside the item.

            // Only C set
            before.int64ItemMember().putObj(10L,new TestItem());
            before.int64ItemMember().get(10L).getObj().myInt().setVal(1);
            before.int64ItemMember().setChangedHere(false);
            before.int64ItemMember().get(10L).setChangedHere(false);

            // Only B set
            before.typeIdItemMember().putObj(10L,new TestItem());
            before.typeIdItemMember().get(10L).getObj().myInt().setVal(1);
            before.typeIdItemMember().setChanged(false);
            before.typeIdItemMember().get(10L).setChangedHere(true);

            // Only A set
            before.enumItemMember().putObj(TestEnum.MY_FIRST,new TestItem());
            before.enumItemMember().get(TestEnum.MY_FIRST).getObj().myInt().setVal(1);
            before.enumItemMember().setChanged(false);
            before.enumItemMember().setChangedHere(true);

            Checks checks = new Checks () {
                    public void doCheck(Object o) {
                        MemberDictionaries obj = (MemberDictionaries)o;

                    check(obj.isChanged());
                    check(obj.int32Int32Member().isChanged());
                    check(obj.int32Int32Member().isChangedHere());
                    check(obj.int32Int32Member().get(10).isChanged());
                    check(obj.int32Int64Member().isChanged());
                    check(!obj.int32Int64Member().isChangedHere());
                    check(obj.int32Int64Member().get(20).isChanged());
                    check(obj.int64Int32Member().isChanged());
                    check(obj.int64Int32Member().isChangedHere());
                    check(!obj.int64Int32Member().get(30L).isChanged());
                    check(obj.stringStringMember().isChanged());
                    check(obj.int32ObjectMember().isChanged(),"A");
                    check(obj.int32ObjectMember().isChangedHere(),"B");
                    check(!obj.int32ObjectMember().get(1).getObj().isChanged(),"C");

                    check(obj.int64ItemMember().isChanged());
                    check(!obj.int64ItemMember().isChangedHere());
                    check(!obj.int64ItemMember().get(10L).isChangedHere());
                    check(obj.int64ItemMember().get(10L).getObj().myInt().isChanged());

                    check(obj.typeIdItemMember().isChanged());
                    check(!obj.typeIdItemMember().isChangedHere());
                    check(obj.typeIdItemMember().get(10L).isChangedHere());
                    check(!obj.typeIdItemMember().get(10L).getObj().myInt().isChanged());

                    check(obj.enumItemMember().isChanged());
                    check(obj.enumItemMember().isChangedHere());
                    check(!obj.enumItemMember().get(TestEnum.MY_FIRST).isChangedHere());
                    check(!obj.enumItemMember().get(TestEnum.MY_FIRST).getObj().myInt().isChanged());
                    }
                };

            RunSerializationChecks(before,checks);
        }

        public void Test_BlobChangeFlags()
        {
            Test_BlobChangeFlags_member_types();
            Test_BlobChangeFlags_member_types_2();
            Test_BlobChangeFlags_member_arrays();
            Test_BlobChangeFlags_member_sequences();
            Test_BlobChangeFlags_member_sequences_2();
            Test_BlobChangeFlags_member_dictionaries();
        }

        public void Test_Misc()
        {
            Test_EnumerationSequenceReflection();
            Test_ObjectSequenceReflection();
            Test_DictionaryReflection();
            Test_DictionaryReflection_putNull();
            Test_ParameterGetInfo();
            Test_ParameterDictionaryReflection();
            Test_Base64Conversion1();
            Test_Base64Conversion2();
            Test_Base64Conversion3();

        }

        private void Test_EnumerationSequenceReflection()
        {
            var seq = new MemberSequences();
            seq.enumerationMember().add(TestEnum.MY_FIRST);
            seq.enumerationMember().add(TestEnum.MY_SECOND);

            {
                EnumerationSequenceContainerBase b = seq.enumerationMember();
                check(b.size() == 2);
                check(b.getOrdinal(0) == TestEnum.MY_FIRST.ordinal());
                check(b.getOrdinal(1) == TestEnum.MY_SECOND.ordinal());

                b.setOrdinal(0, 1);
                b.addOrdinal(0);
            }

            check(seq.enumerationMember().size() == 3);
            check(seq.enumerationMember().get(0) == TestEnum.MY_SECOND);
            check(seq.enumerationMember().get(1) == TestEnum.MY_SECOND);
            check(seq.enumerationMember().get(2) == TestEnum.MY_FIRST);

            {
                EnumerationSequenceContainerBase b = seq.enumerationMember();
                b.clear();
                check(seq.enumerationMember().size() == 0);
            }

        }

        @SuppressWarnings("unchecked")
        private void Test_ObjectSequenceReflection()
        {
            MemberSequences seq = new MemberSequences();

            seq.testClassMember().add(new TestItem());
            seq.testClassMember().get(0).myInt().setVal(10);
            seq.testClassMember().add(new TestItem());
            seq.testClassMember().get(1).myInt().setVal(20);

            {
                GenericObjectSequenceContainer b = seq.testClassMember();
                check(b.size() == 2);

                check(((Int32Container)((com.saabgroup.safir.dob.typesystem.Object)b.get(0)).
                       getMember(TestItem.getMyIntMemberIndex(),0)).getVal() == 10);
                check(((Int32Container)((com.saabgroup.safir.dob.typesystem.Object)b.get(1)).
                       getMember(TestItem.getMyIntMemberIndex(),0)).getVal() == 20);

                //Try to iterate as well, just for the laughs
                int i = 1;
                for (com.saabgroup.safir.dob.typesystem.Object o :
                         (GenericObjectSequenceContainer<com.saabgroup.safir.dob.typesystem.Object>)b)
                {
                    check(((Int32Container)o.getMember(TestItem.getMyIntMemberIndex(),0)).getVal() == 10*i);
                    ++i;
                }


                ((Int32Container)((com.saabgroup.safir.dob.typesystem.Object)b.get(1)).getMember(TestItem.getMyIntMemberIndex(),0)).setVal(30);
                var item1 = new TestItem();
                var item3 = new TestItem();
                item1.myInt().setVal(400);
                item3.myInt().setVal(500);
                b.add(item3);
                b.set(0, item1);
            }

            check(seq.testClassMember().size() == 3);
            check(seq.testClassMember().get(0).myInt().getVal() == 400);
            check(seq.testClassMember().get(1).myInt().getVal() == 30);
            check(seq.testClassMember().get(2).myInt().getVal() == 500);
        }


        @SuppressWarnings("unchecked")
        private void Test_DictionaryReflection()
        {
            MemberDictionaries dict = new MemberDictionaries();

            dict.int64ItemMember().putObj(1L, new TestItem());
            dict.int64ItemMember().getObj(1L).myInt().setVal(10);
            dict.int64ItemMember().getObj(1L).myString().setVal("one");
            dict.int64ItemMember().putObj(2L, new TestItem());
            dict.int64ItemMember().getObj(2L).myInt().setVal(20);
            dict.int64ItemMember().getObj(2L).myString().setVal("two");

            dict.enumItemMember().putObj(TestEnum.MY_FIRST, new TestItem());
            dict.enumItemMember().getObj(TestEnum.MY_FIRST).myInt().setVal(10);
            dict.enumItemMember().getObj(TestEnum.MY_FIRST).myString().setVal("one");
            dict.enumItemMember().putObj(TestEnum.MY_SECOND, new TestItem());
            dict.enumItemMember().getObj(TestEnum.MY_SECOND).myInt().setVal(20);
            dict.enumItemMember().getObj(TestEnum.MY_SECOND).myString().setVal("two");

            dict.int64StringMember().putVal(1L, "foo");
            dict.int64StringMember().putVal(20L, "bar");
            dict.int64StringMember().putVal(111L, "bart");
            dict.int64StringMember().get(111L).setNull();

            {
                DictionaryContainerBase b = dict.int64ItemMember();

                check(b.size() == 2);
                check((long)b.getKeyAt(0) == 1);
                check((long)b.getKeyAt(1) == 2);
                var container = (ObjectContainerImpl<com.saabgroup.dotstest.TestItem>)b.getValueContainerAt(0);
                check(container != null);
                check(!container.isNull());
                check(container.getObj().myInt().getVal() == 10);
                check(container.getObj().myString().getVal() == "one");
                container = (ObjectContainerImpl<com.saabgroup.dotstest.TestItem>)b.getValueContainerAt(1);
                check(container != null);
                check(!container.isNull());
                check(container.getObj().myInt().getVal() == 20);
                check(container.getObj().myString().getVal() == "two");
            }

            {
                DictionaryContainerBase b = dict.enumItemMember();
                check(b.size() == 2);
                check(((Enum)b.getKeyAt(0)).ordinal() == 0);
                check(((Enum)b.getKeyAt(1)).ordinal() == 1);
                var container = (ObjectContainerImpl<com.saabgroup.dotstest.TestItem>)(b.getValueContainerAt(0));
                check(container != null);
                check(container.isNull() == false);
                check(container.getObj().myInt().getVal() == 10);
                check(container.getObj().myString().getVal() == "one");
                container = (ObjectContainerImpl<com.saabgroup.dotstest.TestItem>)(b.getValueContainerAt(1));
                check(container != null);
                check(container.isNull() == false);
                check(container.getObj().myInt().getVal() == 20);
                check(container.getObj().myString().getVal() == "two");
            }

            {
                DictionaryContainerBase b = dict.int64StringMember();
                check(b.size() == 3);
                check((long)b.getKeyAt(0) == 1);
                check((long)b.getKeyAt(1) == 20);
                check((long)b.getKeyAt(2) == 111);
                var container = (StringContainer)(b.getValueContainerAt(0));
                check(container != null);
                check(container.isNull() == false);
                check(container.getVal() == "foo");
                container = (StringContainer)(b.getValueContainerAt(1));
                check(container != null);
                check(container.isNull() == false);
                check(container.getVal() == "bar");
                container = (StringContainer)(b.getValueContainerAt(2));
                check(container != null);
                check(container.isNull() == true);
            }

        }

        @SuppressWarnings("unchecked")
        void Test_DictionaryReflection_putNull()
        {
            MemberDictionaries dict = new MemberDictionaries();

            // Int64 key
            {
                DictionaryContainerBase base = dict.int64ItemMember();

                var valCont = base.putNull(3);
                base.putNull(4);

                check(base.size()== 2);
                check(valCont.isNull());
                check(valCont.isChanged());
                var objCont = (ObjectContainerImpl)valCont;
                var testItem = new TestItem();
                testItem.myInt().setVal(30);
                testItem.myString().setVal("Three");
                objCont.setObj(testItem);

                check(dict.int64ItemMember().size() == 2);
                check(dict.int64ItemMember().get(3).isNull() == false);
                check(dict.int64ItemMember().get(4).isNull() == true);
                check(dict.int64ItemMember().get(3).getObj().myInt().getVal() == 30);
                check(dict.int64ItemMember().get(3).getObj().myString().getVal() == "Three");
            }




            // Enum key
            {
                DictionaryContainerBase base = dict.enumItemMember();
                var valCont = base.putNull(com.saabgroup.dotstest.TestEnum.MY_SECOND);
                check(base.size() == 1);
                check(valCont.isNull());
                check(valCont.isChanged());
                var objCont = (ObjectContainerBase)valCont;
                var testItem = new TestItem();
                testItem.myInt().setVal(39);
                testItem.myString().setVal("Four");
                objCont.setObjInternal(testItem);

                check(dict.enumItemMember().size() == 1);
                check(dict.enumItemMember().get(com.saabgroup.dotstest.TestEnum.MY_SECOND).isNull() == false);
                check(dict.enumItemMember().get(com.saabgroup.dotstest.TestEnum.MY_SECOND).getObj().myInt().getVal() == 39);
                check(dict.enumItemMember().get(com.saabgroup.dotstest.TestEnum.MY_SECOND).getObj().myString().getVal() == "Four");
            }

            // String key
            {
                DictionaryContainerBase base = dict.stringEnumMember();
                var valCont = base.putNull("MyKey");
                check(base.size() == 1);
                check(valCont.isNull());
                check(valCont.isChanged());
                var castedCont = (EnumerationContainerBase)valCont;
                castedCont.setOrdinal(1);

                check(dict.stringEnumMember().size() == 1);
                check(dict.stringEnumMember().get("MyKey").isNull() == false);
                check(dict.stringEnumMember().get("MyKey").getVal() == com.saabgroup.dotstest.TestEnum.MY_SECOND);
            }

            // InstanceId key
            {
                DictionaryContainerBase base = dict.instanceIdStringMember();

                var valCont = base.putNull("someInstance");
                check(base.size() == 1);
                var castedCont = (StringContainer)valCont;
                castedCont.setVal("Flupp");

                check(dict.instanceIdStringMember().size() == 1);
                check(dict.instanceIdStringMember().get("someInstance").isNull() == false);
                check(dict.instanceIdStringMember().get("someInstance").getVal() == "Flupp");
            }

            // Int32Object dictionary
            {
                DictionaryContainerBase base = dict.int32ObjectMember();

                var valCont1 = base.putNull(1);
                var valCont2 = base.putNull(2);
                base.putNull(3);
                check(base.size() == 3);

                var objCont1 = (ObjectContainerImpl)valCont1;
                var obj1 = new TestItem();
                obj1.myInt().setVal(30);
                obj1.myString().setVal("Three");
                objCont1.setObj(obj1);

                var objCont2 = (ObjectContainerBase)valCont2;
                var obj2 = new TestItem();
                obj2.myInt().setVal(40);
                obj2.myString().setVal("Four");
                var val = new MemberDictionaries();
                val.int64ItemMember().putObj(10L,obj2);
                objCont2.setObjInternal(val);

                check(dict.int32ObjectMember().size() == 3);
                check(dict.int32ObjectMember().get(1).isNull() == false);
                check(dict.int32ObjectMember().get(1).isChanged());
                check(((TestItem)dict.int32ObjectMember().get(1).getObj()).myInt().getVal() == 30);
                check(((TestItem)dict.int32ObjectMember().get(1).getObj()).myString().getVal() == "Three");

                check(dict.int32ObjectMember().get(2).isNull() == false);
                check(dict.int32ObjectMember().get(2).isChanged());
                check(((MemberDictionaries)dict.int32ObjectMember().get(2).getObj()).int64ItemMember().get(10L).getObj().myInt().getVal() == 40);
                check(((MemberDictionaries)dict.int32ObjectMember().get(2).getObj()).int64ItemMember().get(10L).getObj().myString().getVal() == "Four");

                check(dict.int32ObjectMember().get(3).isNull());
                check(dict.int32ObjectMember().get(3).isChanged());
            }

            // Wrong key type throws SoftwareViolationException
            {
                //DictionaryContainerBase base = dict.Int64ItemMember();
                try
                {
                    dict.int64ItemMember().putNull("string");
                    check(false);
                }
                catch(java.lang.ClassCastException e)
                {
                }

                try
                {
                    dict.enumItemMember().putNull(10);
                    check(false);
                }
                catch(java.lang.ClassCastException e)
                {
                }
            }

        }

        private void Test_ParameterGetInfo()
        {
            {
                int index = com.saabgroup.safir.dob.typesystem.Parameters.getIndex(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,
                                                                                    "Int32StringParameter");

                check(com.saabgroup.safir.dob.typesystem.MemberType.STRING_MEMBER_TYPE == com.saabgroup.safir.dob.typesystem.Parameters.getType(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index));
                check(com.saabgroup.safir.dob.typesystem.MemberType.INT32_MEMBER_TYPE == com.saabgroup.safir.dob.typesystem.Parameters.getDictionaryKeyType(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index));
                check(com.saabgroup.safir.dob.typesystem.Parameters.getName(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index).equals("Int32StringParameter"));
                check(com.saabgroup.safir.dob.typesystem.CollectionType.DICTIONARY == com.saabgroup.safir.dob.typesystem.Parameters.getCollectionType(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index));
                check(2 == com.saabgroup.safir.dob.typesystem.Parameters.getCollectionSize(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index));
            }


            {
                int index = com.saabgroup.safir.dob.typesystem.Parameters.getIndex(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,
                                                                                    "StringEnumParameter");

                check(com.saabgroup.safir.dob.typesystem.MemberType.ENUMERATION_MEMBER_TYPE == com.saabgroup.safir.dob.typesystem.Parameters.getType(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index));
                check(com.saabgroup.safir.dob.typesystem.MemberType.STRING_MEMBER_TYPE == com.saabgroup.safir.dob.typesystem.Parameters.getDictionaryKeyType(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index));
                check(com.saabgroup.safir.dob.typesystem.Parameters.getName(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index).equals("StringEnumParameter"));
                check(com.saabgroup.dotstest.TestEnum.EnumerationId == com.saabgroup.safir.dob.typesystem.Parameters.getParameterTypeId(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index));
                check(com.saabgroup.safir.dob.typesystem.CollectionType.DICTIONARY == com.saabgroup.safir.dob.typesystem.Parameters.getCollectionType(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index));
                check(2 == com.saabgroup.safir.dob.typesystem.Parameters.getCollectionSize(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index));
            }

            {
                int index = com.saabgroup.safir.dob.typesystem.Parameters.getIndex(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,
                                                                                    "EnumObjectParameter");

                check(com.saabgroup.safir.dob.typesystem.MemberType.OBJECT_MEMBER_TYPE == com.saabgroup.safir.dob.typesystem.Parameters.getType(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index));
                check(com.saabgroup.safir.dob.typesystem.MemberType.ENUMERATION_MEMBER_TYPE == com.saabgroup.safir.dob.typesystem.Parameters.getDictionaryKeyType(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index));
                check(com.saabgroup.safir.dob.typesystem.Parameters.getName(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index).equals("EnumObjectParameter"));
                check(com.saabgroup.safir.dob.typesystem.Object.ClassTypeId == com.saabgroup.safir.dob.typesystem.Parameters.getParameterTypeId(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index));
                check(com.saabgroup.dotstest.TestEnum.EnumerationId == com.saabgroup.safir.dob.typesystem.Parameters.getDictionaryKeyTypeId(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index));
                check(com.saabgroup.safir.dob.typesystem.CollectionType.DICTIONARY == com.saabgroup.safir.dob.typesystem.Parameters.getCollectionType(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index));
                check(2 == com.saabgroup.safir.dob.typesystem.Parameters.getCollectionSize(com.saabgroup.dotstest.ParameterDictionaries.ClassTypeId,index));
            }

            {
                int index = com.saabgroup.safir.dob.typesystem.Parameters.getIndex(com.saabgroup.dotstest.ParameterTypes.ClassTypeId,
                                                                                    "EnumerationParameter");

                check(com.saabgroup.safir.dob.typesystem.MemberType.ENUMERATION_MEMBER_TYPE == com.saabgroup.safir.dob.typesystem.Parameters.getType(com.saabgroup.dotstest.ParameterTypes.ClassTypeId,index));
                check(com.saabgroup.safir.dob.typesystem.Parameters.getName(com.saabgroup.dotstest.ParameterTypes.ClassTypeId,index).equals("EnumerationParameter"));
                check(com.saabgroup.dotstest.TestEnum.EnumerationId == com.saabgroup.safir.dob.typesystem.Parameters.getParameterTypeId(com.saabgroup.dotstest.ParameterTypes.ClassTypeId,index));
                check(com.saabgroup.safir.dob.typesystem.CollectionType.SINGLE_VALUE == com.saabgroup.safir.dob.typesystem.Parameters.getCollectionType(com.saabgroup.dotstest.ParameterTypes.ClassTypeId,index));
                check(1 == com.saabgroup.safir.dob.typesystem.Parameters.getCollectionSize(com.saabgroup.dotstest.ParameterTypes.ClassTypeId,index));
            }
        }

        private void Test_ParameterDictionaryReflection()
        {
            check(2 == com.saabgroup.dotstest.ParameterDictionaries.getInt32StringParameterDictionarySize());
            check(10 == com.saabgroup.dotstest.ParameterDictionaries.getInt32StringParameterKeyFromIndex(0));
            check(com.saabgroup.dotstest.ParameterDictionaries.getInt32StringParameterValueFromIndex(0).equals("Safir"));
            check(20 == com.saabgroup.dotstest.ParameterDictionaries.getInt32StringParameterKeyFromIndex(1));
            check(com.saabgroup.dotstest.ParameterDictionaries.getInt32StringParameterValueFromIndex(1).equals("rifaS"));

            check(2 == com.saabgroup.dotstest.ParameterDictionaries.getInt32Float64ParameterDictionarySize());
            check(10 == com.saabgroup.dotstest.ParameterDictionaries.getInt32Float64ParameterKeyFromIndex(0));
            check(64.64 == com.saabgroup.dotstest.ParameterDictionaries.getInt32Float64ParameterValueFromIndex(0));
            check(20 == com.saabgroup.dotstest.ParameterDictionaries.getInt32Float64ParameterKeyFromIndex(1));
            check(-64.64 == com.saabgroup.dotstest.ParameterDictionaries.getInt32Float64ParameterValueFromIndex(1));

            check(2 == com.saabgroup.dotstest.ParameterDictionaries.getInt32Ampere64ParameterDictionarySize());
            check(10 == com.saabgroup.dotstest.ParameterDictionaries.getInt32Ampere64ParameterKeyFromIndex(0));
            check(64.64 == com.saabgroup.dotstest.ParameterDictionaries.getInt32Ampere64ParameterValueFromIndex(0));
            check(20 == com.saabgroup.dotstest.ParameterDictionaries.getInt32Ampere64ParameterKeyFromIndex(1));
            check(-64.64 == com.saabgroup.dotstest.ParameterDictionaries.getInt32Ampere64ParameterValueFromIndex(1));

            check(2 == com.saabgroup.dotstest.ParameterDictionaries.getStringEnumParameterDictionarySize());
            check(com.saabgroup.dotstest.ParameterDictionaries.getStringEnumParameterKeyFromIndex(0).equals("Billy"));
            check(com.saabgroup.dotstest.TestEnum.MY_FIRST == com.saabgroup.dotstest.ParameterDictionaries.getStringEnumParameterValueFromIndex(0));
            check(com.saabgroup.dotstest.ParameterDictionaries.getStringEnumParameterKeyFromIndex(1).equals("Svarre"));
            check(com.saabgroup.dotstest.TestEnum.MY_SECOND == com.saabgroup.dotstest.ParameterDictionaries.getStringEnumParameterValueFromIndex(1));

            check(2 == com.saabgroup.dotstest.ParameterDictionaries.getEnumObjectParameterDictionarySize());
            check(com.saabgroup.dotstest.TestEnum.MY_FIRST == com.saabgroup.dotstest.ParameterDictionaries.getEnumObjectParameterKeyFromIndex(0));
            check(null != com.saabgroup.dotstest.ParameterDictionaries.getEnumObjectParameterValueFromIndex(0));
            check(com.saabgroup.safir.dob.typesystem.Object.ClassTypeId == com.saabgroup.dotstest.ParameterDictionaries.getEnumObjectParameterValueFromIndex(0).getTypeId());
            check(com.saabgroup.dotstest.TestEnum.MY_SECOND == com.saabgroup.dotstest.ParameterDictionaries.getEnumObjectParameterKeyFromIndex(1));
            check(null != (com.saabgroup.dotstest.MemberDictionaries)com.saabgroup.dotstest.ParameterDictionaries.getEnumObjectParameterValueFromIndex(1));
        }
        private static final char[] HEX_ARRAY = "0123456789ABCDEF".toCharArray();
        public static String bytesToHex(byte[] bytes) {
            char[] hexChars = new char[bytes.length * 2];
            for (int j = 0; j < bytes.length; j++) {
                int v = bytes[j] & 0xFF;
                hexChars[j * 2] = HEX_ARRAY[v >>> 4];
                hexChars[j * 2 + 1] = HEX_ARRAY[v & 0x0F];
            }
            return new String(hexChars);
        }
        
        private void Test_Base64Conversion1()
        {
            String s = "asdfasdfasdfasdf";
            byte[] bin = s.getBytes();
            String base64 = com.saabgroup.safir.dob.typesystem.Utilities.binaryToBase64(bin);
            check(base64.equals("YXNkZmFzZGZhc2RmYXNkZg=="));
            byte[] back = com.saabgroup.safir.dob.typesystem.Utilities.base64ToBinary(base64);
            check(Arrays.equals(back,bin));
            check(back.length == bin.length);
        }

        private void Test_Base64Conversion2()
        {
            String s = "asdfasdfasdfasdfaa";
            byte[] bin = s.getBytes();
            String base64 = com.saabgroup.safir.dob.typesystem.Utilities.binaryToBase64(bin);
            check(base64.equals("YXNkZmFzZGZhc2RmYXNkZmFh"));
            byte[] back = com.saabgroup.safir.dob.typesystem.Utilities.base64ToBinary(base64);
            check(Arrays.equals(back,bin));
            check(back.length == bin.length);
        }

        private void Test_Base64Conversion3()
        {
            String s = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";
            byte[] bin = s.getBytes();
            String base64 = com.saabgroup.safir.dob.typesystem.Utilities.binaryToBase64(bin);
            check(base64.equals("TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdCwgc2VkIGRvIGVpdXNtb2QgdGVtcG9yIGluY2lkaWR1bnQgdXQgbGFib3JlIGV0IGRvbG9yZSBtYWduYSBhbGlxdWEuIFV0IGVuaW0gYWQgbWluaW0gdmVuaWFtLCBxdWlzIG5vc3RydWQgZXhlcmNpdGF0aW9uIHVsbGFtY28gbGFib3JpcyBuaXNpIHV0IGFsaXF1aXAgZXggZWEgY29tbW9kbyBjb25zZXF1YXQuIER1aXMgYXV0ZSBpcnVyZSBkb2xvciBpbiByZXByZWhlbmRlcml0IGluIHZvbHVwdGF0ZSB2ZWxpdCBlc3NlIGNpbGx1bSBkb2xvcmUgZXUgZnVnaWF0IG51bGxhIHBhcmlhdHVyLiBFeGNlcHRldXIgc2ludCBvY2NhZWNhdCBjdXBpZGF0YXQgbm9uIHByb2lkZW50LCBzdW50IGluIGN1bHBhIHF1aSBvZmZpY2lhIGRlc2VydW50IG1vbGxpdCBhbmltIGlkIGVzdCBsYWJvcnVtLg=="));
            byte[] back = com.saabgroup.safir.dob.typesystem.Utilities.base64ToBinary(base64);
            check(Arrays.equals(back,bin));
            check(back.length == bin.length);
        }

    }

    private static void printSequences(MemberSequences ms) {
        System.out.println("--- Int32Member ---");
        System.out.println("size: " + ms.int32Member().size());
        System.out.println("isChanged: " + ms.int32Member().isChanged());
        System.out.println("val[0]: " + ms.int32Member().get(0));
        System.out.println("val[1]: " + ms.int32Member().get(1));

        System.out.println("--- Int64Member ---");
        System.out.println("size: " + ms.int64Member().size());
        System.out.println("isChanged: " + ms.int64Member().isChanged());
        System.out.println("val[0]: " + ms.int64Member().get(0));
        System.out.println("val[1]: " + ms.int64Member().get(1));

        System.out.println("--- Float32Member ---");
        System.out.println("size: " + ms.float32Member().size());
        System.out.println("isChanged: " + ms.float32Member().isChanged());
        System.out.println("val[0]: " + String.format("%.1f", ms.float32Member().get(0)));
        System.out.println("val[1]: " + String.format("%.1f", ms.float32Member().get(1)));

        System.out.println("--- Float64Member ---");
        System.out.println("size: " + ms.float64Member().size());
        System.out.println("isChanged: " + ms.float64Member().isChanged());
        System.out.println("val[0]: " + String.format("%.1f", ms.float64Member().get(0)));
        System.out.println("val[1]: " + String.format("%.1f", ms.float64Member().get(1)));

        System.out.println("--- BooleanMember ---");
        System.out.println("size: " + ms.booleanMember().size());
        System.out.println("isChanged: " + ms.booleanMember().isChanged());
        System.out.println("val[0]: " + ms.booleanMember().get(0));
        System.out.println("val[1]: " + ms.booleanMember().get(1));

        System.out.println("--- EnumerationMember ---");
        System.out.println("size: " + ms.enumerationMember().size());
        System.out.println("isChanged: " + ms.enumerationMember().isChanged());
        System.out.println("val[0]: " + TestEnumStr(ms.enumerationMember().get(0)));
        System.out.println("val[1]: " + TestEnumStr(ms.enumerationMember().get(1)));

        System.out.println("--- StringMember ---");
        System.out.println("size: " + ms.stringMember().size());
        System.out.println("isChanged: " + ms.stringMember().isChanged());
        System.out.println("val[0]: " + ms.stringMember().get(0));
        System.out.println("val[1]: " + ms.stringMember().get(1));

        System.out.println("--- TypeIdMember ---");
        System.out.println("size: " + ms.typeIdMember().size());
        System.out.println("isChanged: " + ms.typeIdMember().isChanged());
        System.out.println("val[0]: " + Operations.getName(ms.typeIdMember().get(0)));
        System.out.println("val[1]: " + Operations.getName(ms.typeIdMember().get(1)));

        System.out.println("--- HandlerIdMember ---");
        System.out.println("size: " + ms.handlerIdMember().size());
        System.out.println("isChanged: " + ms.handlerIdMember().isChanged());
    }

    private static void testSequences() {
        Header("Sequences");

        MemberSequences ms = new MemberSequences();

        ms.int32Member().add(20);
        ms.int32Member().add(30);
        ms.int32Member().add(0, 10);
        ms.int32Member().remove(2);

        ms.int64Member().add(200L);
        ms.int64Member().add(300L);
        ms.int64Member().add(0, 100L);
        ms.int64Member().remove(2);

        ms.float32Member().add(2.2f);
        ms.float32Member().add(3.3f);
        ms.float32Member().add(0, 1.1f);
        ms.float32Member().remove(2);

        ms.float64Member().add(22.2);
        ms.float64Member().add(33.3);
        ms.float64Member().add(0, 11.1);
        ms.float64Member().remove(2);

        ms.booleanMember().add(false);
        ms.booleanMember().add(false);
        ms.booleanMember().add(0, true);
        ms.booleanMember().remove(2);

        ms.enumerationMember().add(TestEnum.MY_SECOND);
        ms.enumerationMember().add(TestEnum.MY_THIRD);
        ms.enumerationMember().add(0, TestEnum.MY_FIRST);
        ms.enumerationMember().remove(2);

        ms.stringMember().add("Bb");
        ms.stringMember().add("Cc");
        ms.stringMember().add(0, "Aa");
        ms.stringMember().remove(2);

        ms.typeIdMember().add(MemberSequences.ClassTypeId);
        ms.typeIdMember().add(TestEnum.EnumerationId);
        ms.typeIdMember().add(0, MemberDictionaries.ClassTypeId);
        ms.typeIdMember().remove(2);

        printSequences(ms);

        System.out.println("------ To Xml -----");
        String xml = Serialization.toXml(ms);
        System.out.println(xml);

        System.out.println("------ From Xml -----");
        MemberSequences fromXml = (MemberSequences) Serialization.toObject(xml);
        printSequences(fromXml);

        System.out.println("------ To Json -----");
        String json = Serialization.toJson(ms);
        System.out.println(json);

        System.out.println("------ From Json -----");
        MemberSequences fromJson = (MemberSequences) Serialization.toObjectFromJson(json);
        printSequences(fromJson);
    }

    private static void printDictionaryMemberInfo() {
        MemberInfo mi;

        mi = Members.getInfo(MemberDictionaries.ClassTypeId, MemberDictionaries.getInt32StringMemberMemberIndex());

        System.out.println(
                "GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + toStringNoUnderscore(mi.getKeyType())
                        + "," + mi.getMemberName() + "," + mi.getMemberTypeId() + "," + mi.getKeyTypeId() + ","
                        + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + "," + mi.getArrayLength());

        mi = Members.getInfo(MemberDictionaries.ClassTypeId, MemberDictionaries.getInt64BinaryMemberMemberIndex());

        System.out.println(
                "GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + toStringNoUnderscore(mi.getKeyType())
                        + "," + mi.getMemberName() + "," + mi.getMemberTypeId() + "," + mi.getKeyTypeId() + ","
                        + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + "," + mi.getArrayLength());

        mi = Members.getInfo(MemberDictionaries.ClassTypeId, MemberDictionaries.getTypeIdEnumMemberMemberIndex());

        System.out.println(
                "GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + toStringNoUnderscore(mi.getKeyType())
                        + "," + mi.getMemberName() + "," + mi.getMemberTypeId() + "," + mi.getKeyTypeId() + ","
                        + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + "," + mi.getArrayLength());

        mi = Members.getInfo(MemberDictionaries.ClassTypeId, MemberDictionaries.getEnumInstanceIdMemberMemberIndex());

        System.out.println(
                "GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + toStringNoUnderscore(mi.getKeyType())
                        + "," + mi.getMemberName() + "," + mi.getMemberTypeId() + "," + mi.getKeyTypeId() + ","
                        + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + "," + mi.getArrayLength());

        mi = Members.getInfo(MemberDictionaries.ClassTypeId,
                MemberDictionaries.getInstanceIdEntityIdMemberMemberIndex());

        System.out.println(
                "GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + toStringNoUnderscore(mi.getKeyType())
                        + "," + mi.getMemberName() + "," + mi.getMemberTypeId() + "," + mi.getKeyTypeId() + ","
                        + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + "," + mi.getArrayLength());

        mi = Members.getInfo(MemberDictionaries.ClassTypeId,
                MemberDictionaries.getEntityIdHandlerIdMemberMemberIndex());

        System.out.println(
                "GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + toStringNoUnderscore(mi.getKeyType())
                        + "," + mi.getMemberName() + "," + mi.getMemberTypeId() + "," + mi.getKeyTypeId() + ","
                        + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + "," + mi.getArrayLength());

        mi = Members.getInfo(MemberDictionaries.ClassTypeId, MemberDictionaries.getStringItemMemberMemberIndex());

        System.out.println(
                "GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + toStringNoUnderscore(mi.getKeyType())
                        + "," + mi.getMemberName() + "," + mi.getMemberTypeId() + "," + mi.getKeyTypeId() + ","
                        + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + "," + mi.getArrayLength());

        mi = Members.getInfo(MemberDictionaries.ClassTypeId, MemberDictionaries.getStringObjectMemberMemberIndex());

        System.out.println(
                "GetInfo: " + toStringNoUnderscore(mi.getMemberType()) + "," + toStringNoUnderscore(mi.getKeyType())
                        + "," + mi.getMemberName() + "," + mi.getMemberTypeId() + "," + mi.getKeyTypeId() + ","
                        + mi.getStringLength() + "," + ctStr(mi.getCollectionType()) + "," + mi.getArrayLength());

    }

    private static void printDictionaries(MemberDictionaries md) {

        System.out.println("--- Int32StringMember ---");
        System.out.println("size: " + md.int32StringMember().size());
        System.out.println("isChanged: " + md.int32StringMember().isChanged());
        for (Map.Entry<Integer, StringContainer> kv : md.int32StringMember().entrySet()) {
            if (kv.getValue().isNull())
                System.out.println(kv.getKey() + " = NULL, changed: " + kv.getValue().isChanged());
            else
                System.out.println(
                        kv.getKey() + " = " + kv.getValue().getVal() + ", changed: " + kv.getValue().isChanged());
        }

        System.out.println("--- Int64BinaryMember ---");
        System.out.println("size: " + md.int64BinaryMember().size());
        System.out.println("isChanged: " + md.int64BinaryMember().isChanged());
        for (Map.Entry<Long, BinaryContainer> kv : md.int64BinaryMember().entrySet()) {
            if (kv.getValue().isNull())
                System.out.println(kv.getKey() + " = NULL, changed: " + kv.getValue().isChanged());
            else
                System.out.println(kv.getKey() + " = " + (new String(kv.getValue().getVal())) + ", changed: "
                        + kv.getValue().isChanged());
        }

        System.out.println("--- TypeIdEnumMember ---");
        System.out.println("size: " + md.typeIdEnumMember().size());
        System.out.println("isChanged: " + md.typeIdEnumMember().isChanged());
        for (Map.Entry<Long, TestEnum.Container> kv : md.typeIdEnumMember().entrySet()) {
            if (kv.getValue().isNull())
                System.out.println(Operations.getName(kv.getKey()) + " = NULL, changed: " + kv.getValue().isChanged());
            else
                System.out.println(Operations.getName(kv.getKey()) + " = " + TestEnumStr(kv.getValue().getVal())
                        + ", changed: " + kv.getValue().isChanged());
        }

        System.out.println("--- EnumInstanceIdMember ---");
        System.out.println("size: " + md.enumInstanceIdMember().size());
        System.out.println("isChanged: " + md.enumInstanceIdMember().isChanged());
        for (Map.Entry<TestEnum, InstanceIdContainer> kv : md.enumInstanceIdMember().entrySet()) {
            if (kv.getValue().isNull())
                System.out.println(TestEnumStr(kv.getKey()) + " = NULL, changed: " + kv.getValue().isChanged());
            else
                System.out.println(TestEnumStr(kv.getKey()) + " = " + kv.getValue().getVal().toString() + ", changed: "
                        + kv.getValue().isChanged());
        }

        System.out.println("--- InstanceIdEntityIdMember ---");
        System.out.println("size: " + md.instanceIdEntityIdMember().size());
        System.out.println("isChanged: " + md.instanceIdEntityIdMember().isChanged());
        for (Map.Entry<InstanceId, EntityIdContainer> kv : md.instanceIdEntityIdMember().entrySet()) {
            if (kv.getValue().isNull())
                System.out.println(kv.getKey().toString() + " = NULL, changed: " + kv.getValue().isChanged());
            else
                System.out.println(kv.getKey().toString() + " = " + kv.getValue().getVal() + ", changed: "
                        + kv.getValue().isChanged());
        }

        System.out.println("--- EntityIdHandlerIdMember ---");
        System.out.println("size: " + md.entityIdHandlerIdMember().size());
        System.out.println("isChanged: " + md.entityIdHandlerIdMember().isChanged());
        for (Map.Entry<EntityId, HandlerIdContainer> kv : md.entityIdHandlerIdMember().entrySet()) {
            if (kv.getValue().isNull())
                System.out.println(kv.getKey() + " = NULL, changed: " + kv.getValue().isChanged());
            else
                System.out.println(
                        kv.getKey() + " = " + kv.getValue().getVal() + ", changed: " + kv.getValue().isChanged());
        }

        System.out.println("--- StringItemMember ---");
        System.out.println("size: " + md.stringItemMember().size());
        System.out.println("isChanged: " + md.stringItemMember().isChanged());
        for (Map.Entry<String, ObjectContainerImpl<com.saabgroup.dotstest.MemberDictionaries>> kv : md
                .stringItemMember().entrySet()) {
            if (kv.getValue().isNull())
                System.out.println(kv.getKey() + " = NULL, changed: " + kv.getValue().isChanged());
            else
                System.out.println(kv.getKey() + " = " + Serialization.toJson(kv.getValue().getObj()) + ", changed: "
                        + kv.getValue().isChanged());
        }

        System.out.println("--- StringObjectMember ---");
        System.out.println("size: " + md.stringObjectMember().size());
        System.out.println("isChanged: " + md.stringObjectMember().isChanged());
        for (Map.Entry<String, ObjectContainerImpl<Object>> kv : md.stringObjectMember().entrySet()) {
            if (kv.getValue().isNull())
                System.out.println(kv.getKey() + " = NULL, changed: " + kv.getValue().isChanged());
            else
                System.out.println(kv.getKey() + " = " + Serialization.toJson(kv.getValue().getObj()) + ", changed: "
                        + kv.getValue().isChanged());
        }
    }

    private static void testDictionaries() {
        Header("Dictionaries");

        printDictionaryMemberInfo();

        MemberDictionaries md = new MemberDictionaries();

        // int32String
        md.int32StringMember().putVal(10, ParameterDictionaries.getInt32StringParameter(10));
        md.int32StringMember().putVal(20, ParameterDictionaries.getInt32StringParameter(20));
        md.int64BinaryMember().putVal(100L, ParameterDictionaries.getInt32BinaryParameter(10));
        md.int64BinaryMember().putVal(200L, ParameterDictionaries.getInt32BinaryParameter(20));
        md.typeIdEnumMember().putVal(MemberDictionaries.ClassTypeId, ParameterDictionaries.getStringEnumParameter("Billy"));
        md.typeIdEnumMember().putVal(MemberSequences.ClassTypeId, ParameterDictionaries.getStringEnumParameter("Svarre"));


        md.enumInstanceIdMember().putVal(TestEnum.MY_FIRST,
                                         ParameterDictionaries.getEnumInstanceIdParameter(TestEnum.MY_FIRST));
        md.enumInstanceIdMember().putVal(TestEnum.MY_SECOND,
                                         ParameterDictionaries.getEnumInstanceIdParameter(TestEnum.MY_SECOND));
        md.instanceIdEntityIdMember().putVal(new InstanceId("FirstInstance"),
                                             ParameterDictionaries.getHandlerIdEntityIdParameter(new HandlerId("handlerOne")));
        md.instanceIdEntityIdMember().putVal(new InstanceId("SecondInstance"),
                                             ParameterDictionaries.getHandlerIdEntityIdParameter(new HandlerId(2)));

        MemberDictionaries item1 = new MemberDictionaries();


        EntityId key = new EntityId(com.saabgroup.safir.dob.Entity.ClassTypeId, new InstanceId("first"));
        item1.entityIdHandlerIdMember().putVal(key,
                                               ParameterDictionaries.getEntityIdHandlerIdParameter
                                               (new EntityId(com.saabgroup.safir.dob.Entity.ClassTypeId,
                                                             new InstanceId("first"))));
        key = new EntityId(com.saabgroup.safir.dob.Entity.ClassTypeId, new InstanceId(2));
        item1.entityIdHandlerIdMember().putVal(key,
                                               ParameterDictionaries.getEntityIdHandlerIdParameter
                                               (new EntityId(com.saabgroup.safir.dob.Entity.ClassTypeId,
                                                             new InstanceId("second"))));

        md.stringItemMember().putObj("Karl", item1);
        md.stringItemMember().putObj("Philip", null);
        md.stringItemMember().putObj("Gustav", item1);

        md.stringObjectMember().putObj("Dilbert", ParameterDictionaries.getInt32ObjectParameter(10));
        md.stringObjectMember().putObj("Wally", ParameterDictionaries.getInt32ObjectParameter(20));

        printDictionaries(md);

        System.out.println("------ To Xml -----");
        String xml = Serialization.toXml(md);
        System.out.println(xml);

        System.out.println("------ From Xml -----");
        MemberDictionaries fromXml = (MemberDictionaries) Serialization.toObject(xml);
        printDictionaries(fromXml);

        System.out.println("------ To Json -----");
        String json = Serialization.toJson(md);
        System.out.println(json);

        System.out.println("------ From Json -----");
        MemberDictionaries fromJson = (MemberDictionaries) Serialization.toObjectFromJson(json);
        printDictionaries(fromJson);
    }

    interface ThrowingFunction {
        void func();
    }


    public static class MergeChangesTests
    {
        private MergeChangesTests() {}
        private void Check(boolean expr, String desc)
        {
            ++tests;
            if (!expr) {
                ++failures;
                if (desc.length() == 0)
                {
                    System.out.println("Testcase " + tests + " failed!");
                }
                else
                {
                    System.out.println("Testcase " + tests + " (" + desc + ") failed!");
                }
            }
        }

        private void Check(boolean expr)
        {
            Check(expr, "");
        }

        private void CheckEqual(int a, int b)
        {
            if (a != b)
            {
                System.out.println("Failed test: " + a + " != " + b);
            }
            Check(a==b);
        }

        private void CheckEqual(long a, long b)
        {
            if (a != b)
            {
                System.out.println("Failed test: " + a + " != " + b);
            }
            Check(a==b);
        }
        private void CheckEqual(String a, String b)
        {
            if (a != b)
            {
                System.out.println("Failed test: " + a + " != " + b);
            }
            Check(a==b);
        }

        private void CheckEqual(TestEnum a, TestEnum b)
        {
            if (a != b)
            {
                System.out.println("Failed test: " + a + " != " + b);
            }
            Check(a==b);
        }


        private void CheckThrowSV(ThrowingFunction fn)
        {
            boolean caught = false;
            try
            {
                fn.func();
            }
            catch(SoftwareViolationException e)
            {
                caught = true;
            }
            Check(caught);
        }

        private void Simple()
        {
            MemberTypes from = new MemberTypes();
            MemberTypes into = new MemberTypes();

            from.int32Member().setVal(10);
            from.int64Member().setVal(20L);
            from.int64Member().setChanged(false);

            from.stringMember().setVal("asdf");
            from.enumerationMember().setVal(TestEnum.MY_FIRST);

            Utilities.mergeChanges(into,from);

            Check(into.int32Member().isChanged());
            CheckEqual(into.int32Member().getVal(),10);
            Check(!into.int64Member().isChanged());
            Check(into.int64Member().isNull());
            Check(into.stringMember().getVal() == "asdf");
            CheckEqual(into.enumerationMember().getVal(), TestEnum.MY_FIRST);
        }


        private void Arrays()
        {
            MemberArrays from = new MemberArrays();
            MemberArrays into = new MemberArrays();

            from.int32Member().get(0).setVal(10);
            from.int64Member().get(0).setVal(20L);
            from.int64Member().get(0).setChanged(false);

            from.stringMember().get(0).setVal("asdf");
            from.stringMember().get(1).setVal("asdfasdf");
            from.stringMember().get(1).setChanged(false);
            from.enumerationMember().get(0).setVal(TestEnum.MY_FIRST);
            from.enumerationMember().get(1).setVal(TestEnum.MY_SECOND);
            from.enumerationMember().get(1).setChanged(false);

            Utilities.mergeChanges(into,from);

            Check(into.int32Member().get(0).isChanged());
            CheckEqual(into.int32Member().get(0).getVal(),10);
            Check(!into.int64Member().isChanged());
            Check(into.int64Member().get(0).isNull());
            Check(into.stringMember().get(0).getVal() == "asdf");
            Check(into.stringMember().get(1).isNull());
            CheckEqual(into.enumerationMember().get(0).getVal(), TestEnum.MY_FIRST);
            Check(into.enumerationMember().get(1).isNull());
        }


        private void Objects()
        {
            MemberTypes from = new MemberTypes();
            MemberTypes into = new MemberTypes();

            from.testClassMember().setObj(new TestItem());
            from.testClassMember().getObj().myInt().setVal(10);
            from.testClassMember().setChangedHere(false);
            from.objectMember().setObj(new TestItem());
            ((TestItem)from.objectMember().getObj()).myInt().setVal(20);
            from.objectMember().setChangedHere(false);

            Utilities.mergeChanges(into,from);

            CheckEqual(into.testClassMember().getObj().myInt().getVal(),10);
            Check(into.testClassMember().getObj().myInt().isChanged());
            Check(into.testClassMember().isChanged());
            Check(into.testClassMember().isChangedHere());
            CheckEqual(((TestItem)into.objectMember().getObj()).myInt().getVal(),20);
        }

        private void Objects_BothHaveData()
        {
            MemberTypes from = new MemberTypes();
            MemberTypes into = new MemberTypes();

            from.testClassMember().setObj(new TestItem());
            from.objectMember().setObj(new TestItem());
            from.setChanged(false);

            into.testClassMember().setObj(new TestItem());
            into.objectMember().setObj(new TestItem());
            into.setChanged(false);


            from.testClassMember().getObj().myInt().setVal(10);
            from.testClassMember().setChangedHere(false);
            ((TestItem)from.objectMember().getObj()).myInt().setVal(20);
            from.objectMember().setChangedHere(false);

            Utilities.mergeChanges(into,from);

            CheckEqual(into.testClassMember().getObj().myInt().getVal(),10);
            Check(into.testClassMember().getObj().myInt().isChanged());
            Check(into.testClassMember().isChanged());
            Check(!into.testClassMember().isChangedHere());
            CheckEqual(((TestItem)into.objectMember().getObj()).myInt().getVal(),20);
            Check(!into.objectMember().isChangedHere());
        }

        private void Sequences()
        {
            MemberSequences from = new MemberSequences();
            MemberSequences into = new MemberSequences();

            into.int32Member().add(20);
            into.setChanged(false);

            from.int32Member().add(10);
            from.int64Member().add(20L);
            from.int64Member().setChanged(false);

            from.stringMember().add("asdf");

            from.enumerationMember().add(TestEnum.MY_FIRST);
            from.enumerationMember().setChanged(false);

            Utilities.mergeChanges(into,from);

            Check(into.int32Member().isChanged());
            CheckEqual(into.int32Member().get(0),10);
            Check(!into.int64Member().isChanged());
            Check(into.int64Member().size() == 0);
            Check(into.stringMember().get(0) == "asdf");
            Check(into.enumerationMember().size() == 0);
        }

        private void Dictionaries_IntoEmpty()
        {
            MemberDictionaries from = new MemberDictionaries();
            MemberDictionaries into = new MemberDictionaries();

            from.int32Int32Member().putVal(10,10);
            from.int32Int32Member().putVal(20,20);

            from.int32Int64Member().putVal(10,10L);
            from.int32Int64Member().putVal(20,20L);
            from.int32Int64Member().get(20).setChanged(false);

            from.int64Int64Member().putVal(10L,10L);
            from.int64Int64Member().putVal(20L,20L);
            from.int64Int64Member().setChanged(false);

            Utilities.mergeChanges(into,from);

            Check(into.int32Int32Member().isChanged());
            Check(into.int32Int32Member().isChangedHere());
            Check(into.int32Int32Member().get(10).isChanged());
            CheckEqual(into.int32Int32Member().get(10).getVal(),10);
            Check(into.int32Int32Member().get(20).isChanged());
            CheckEqual(into.int32Int32Member().get(20).getVal(),20);

            Check(into.int32Int64Member().isChanged());
            Check(into.int32Int64Member().isChangedHere());
            Check(into.int32Int64Member().get(10).isChanged());
            CheckEqual(into.int32Int64Member().get(10).getVal(),10);
            Check(!into.int32Int64Member().get(20).isChanged());
            CheckEqual(into.int32Int64Member().get(20).getVal(),20);

            Check(!into.int64Int32Member().isChanged());
            CheckEqual(into.int64Int32Member().size(), 0);
        }

        private void Dictionaries_IntoEmpty_Error()
        {
            final MemberDictionaries from = new MemberDictionaries();
            final MemberDictionaries into = new MemberDictionaries();

            from.int64Int32Member().putVal(10L,10);
            from.int64Int32Member().putVal(20L,20);
            from.int64Int32Member().setChangedHere(false);

            CheckThrowSV(new ThrowingFunction () {
                    public void func() {Utilities.mergeChanges(into,from);}});
        }

        private void Dictionaries_IntoNonEmpty_1()
        {
            MemberDictionaries into = new MemberDictionaries();
            into.int32Int32Member().putVal(1,10);
            into.int32Int32Member().putVal(2,20);

            into.int32Int64Member().putVal(1,10L);
            into.int32Int64Member().putVal(2,20L);

            into.int64Int32Member().putVal(1L,10);
            into.int64Int32Member().putVal(2L,20);

            into.int64Int64Member().putVal(1L,10L);
            into.int64Int64Member().putVal(2L,20L);

            into.setChanged(false);

            MemberDictionaries from = new MemberDictionaries();

            from.int32Int32Member().putVal(3,30);

            from.int32Int64Member().putVal(1,100L);
            from.int32Int64Member().putVal(2,20L);
            from.int32Int64Member().get(2).setChanged(false);
            from.int32Int64Member().setChangedHere(false);

            from.int64Int32Member().putVal(1L,100);
            from.int64Int32Member().putVal(2L,555);
            from.int64Int32Member().get(2L).setChanged(false);
            from.int64Int32Member().setChangedHere(false);

            from.int64Int64Member().putVal(1L,100L);
            from.int64Int64Member().setChangedHere(false);

            Utilities.mergeChanges(into,from);

            Check(into.int32Int32Member().isChanged());
            Check(into.int32Int32Member().isChangedHere());
            CheckEqual(into.int32Int32Member().size(), 1);
            Check(into.int32Int32Member().get(3).isChanged());
            CheckEqual(into.int32Int32Member().get(3).getVal(),30);

            Check(into.int32Int64Member().isChanged());
            Check(!into.int32Int64Member().isChangedHere());
            CheckEqual(into.int32Int64Member().size(), 2);
            Check(into.int32Int64Member().get(1).isChanged());
            CheckEqual(into.int32Int64Member().get(1).getVal(),100);
            Check(!into.int32Int64Member().get(2).isChanged());
            CheckEqual(into.int32Int64Member().get(2).getVal(),20);

            Check(into.int64Int32Member().isChanged());
            Check(!into.int64Int32Member().isChangedHere());
            CheckEqual(into.int64Int32Member().size(), 2);
            Check(into.int64Int32Member().get(1L).isChanged());
            CheckEqual(into.int64Int32Member().get(1L).getVal(),100);
            Check(!into.int64Int32Member().get(2L).isChanged());
            CheckEqual(into.int64Int32Member().get(2L).getVal(),20);

            Check(into.int64Int64Member().isChanged());
            Check(!into.int64Int64Member().isChangedHere());
            CheckEqual(into.int64Int64Member().size(), 2);
            Check(into.int64Int64Member().get(1L).isChanged());
            CheckEqual(into.int64Int64Member().get(1L).getVal(),100);
            Check(!into.int64Int64Member().get(2L).isChanged());
            CheckEqual(into.int64Int64Member().get(2L).getVal(),20);

        }


        private void Dictionaries_IntoNonEmpty_2()
        {
            MemberDictionaries into = new MemberDictionaries();
            into.int32Int32Member().putVal(1,10);
            into.int32Int32Member().putVal(2,20);

            into.int32Int64Member().putVal(1,10L);
            into.int32Int64Member().putVal(2,20L);

            into.int64Int32Member().putVal(1L,10);
            into.int64Int32Member().putVal(2L,20);

            into.int64Int64Member().putVal(1L,10L);
            into.int64Int64Member().putVal(2L,20L);

            into.setChanged(false);

            MemberDictionaries from = new MemberDictionaries();

            from.int32Int32Member().putVal(1,10);
            from.int32Int32Member().get(1).setChanged(false);

            from.int32Int64Member().putVal(1,100L);

            from.int64Int32Member().putVal(1L,100);
            from.int64Int32Member().get(1L).setChanged(false);

            from.int64Int64Member().putVal(1L,100L);
            from.int64Int64Member().setChanged(false);

            Utilities.mergeChanges(into,from);

            Check(into.int32Int32Member().isChanged());
            Check(into.int32Int32Member().isChangedHere());
            CheckEqual(into.int32Int32Member().size(), 1);
            Check(!into.int32Int32Member().get(1).isChanged());
            CheckEqual(into.int32Int32Member().get(1).getVal(),10);

            Check(into.int32Int64Member().isChanged());
            Check(into.int32Int64Member().isChangedHere());
            CheckEqual(into.int32Int64Member().size(), 1);
            Check(into.int32Int64Member().get(1).isChanged());
            CheckEqual(into.int32Int64Member().get(1).getVal(),100);

            Check(into.int64Int32Member().isChanged());
            Check(into.int64Int32Member().isChangedHere());
            CheckEqual(into.int64Int32Member().size(), 1);
            Check(!into.int64Int32Member().get(1L).isChanged());
            CheckEqual(into.int64Int32Member().get(1L).getVal(),100);

            Check(!into.int64Int64Member().isChanged());
            CheckEqual(into.int64Int64Member().size(), 2);
            CheckEqual(into.int64Int64Member().get(1L).getVal(),10);
            CheckEqual(into.int64Int64Member().get(2L).getVal(),20);
        }



        private void Dictionaries_IntoNonEmpty_Error()
        {
            final MemberDictionaries into = new MemberDictionaries();
            into.int32Int32Member().putVal(1,10);
            into.int32Int32Member().putVal(2,20);
            into.setChanged(false);

            final MemberDictionaries from = new MemberDictionaries();

            from.int32Int32Member().putVal(3,30);
            from.int32Int32Member().setChangedHere(false);

            CheckThrowSV(new ThrowingFunction () {
                    public void func() {Utilities.mergeChanges(into,from);}});
        }

        private void ObjectDictionaries_IntoEmpty_1()
        {
            MemberDictionaries from = new MemberDictionaries();
            MemberDictionaries into = new MemberDictionaries();

            //empty + x[x1:(xM=10)] => x[x1:(xM=10)]
            from.int64ItemMember().putObj(1L, new TestItem());
            from.int64ItemMember().get(1L).getObj().myInt().setVal(10);

            //empty + x[1:(xM=10)] => x[1:(xM=10)]
            from.typeIdItemMember().putObj(1L, new TestItem());
            from.typeIdItemMember().get(1L).getObj().myInt().setVal(20);
            from.typeIdItemMember().get(1L).setChangedHere(false);

            //empty + x[x1:(M=10)] => x[x1:(M=10)]
            from.enumItemMember().putObj(TestEnum.MY_SECOND, new TestItem());
            from.enumItemMember().get(TestEnum.MY_SECOND).getObj().myInt().setVal(30);
            from.enumItemMember().get(TestEnum.MY_SECOND).setChanged(false);

            Utilities.mergeChanges(into,from);

            Check(into.int64ItemMember().isChangedHere());
            Check(into.int64ItemMember().get(1L).isChangedHere());
            Check(into.int64ItemMember().get(1L).getObj().myInt().isChanged());
            CheckEqual(into.int64ItemMember().get(1L).getObj().myInt().getVal(),10);

            Check(into.typeIdItemMember().isChangedHere());
            Check(!into.typeIdItemMember().get(1L).isChangedHere());
            Check(into.typeIdItemMember().get(1L).getObj().myInt().isChanged());
            CheckEqual(into.typeIdItemMember().get(1L).getObj().myInt().getVal(),20);

            Check(into.enumItemMember().isChangedHere());
            Check(!into.enumItemMember().get(TestEnum.MY_SECOND).isChangedHere());
            Check(!into.enumItemMember().get(TestEnum.MY_SECOND).getObj().myInt().isChanged());
            CheckEqual(into.enumItemMember().get(TestEnum.MY_SECOND).getObj().myInt().getVal(),30);
        }

        private void ObjectDictionaries_IntoEmpty_Error_1()
        {
            final MemberDictionaries from = new MemberDictionaries();
            final MemberDictionaries into = new MemberDictionaries();

            //empty + [x1:(xM=10)] => error
            from.int64ItemMember().putObj(1L, new TestItem());
            from.int64ItemMember().get(1L).getObj().myInt().setVal(10);
            from.int64ItemMember().setChangedHere(false);

            CheckThrowSV(new ThrowingFunction () {
                    public void func() {Utilities.mergeChanges(into,from);}});
        }

        private void ObjectDictionaries_IntoEmpty_Error_2()
        {
            final MemberDictionaries from = new MemberDictionaries();
            final MemberDictionaries into = new MemberDictionaries();

            //empty + [x1:(M=10)] => error
            from.int64ItemMember().putObj(1L, new TestItem());
            from.int64ItemMember().get(1L).getObj().myInt().setVal(10);
            from.int64ItemMember().setChangedHere(false);
            from.int64ItemMember().get(1L).getObj().myInt().setChanged(false);

            CheckThrowSV(new ThrowingFunction () {
                    public void func() {Utilities.mergeChanges(into,from);}});
        }

        private void ObjectDictionaries_IntoEmpty_Error_3()
        {
            final MemberDictionaries from = new MemberDictionaries();
            final MemberDictionaries into = new MemberDictionaries();

            //empty + [1:(xM=10)] => error
            from.int64ItemMember().putObj(1L, new TestItem());
            from.int64ItemMember().get(1L).getObj().myInt().setVal(10);
            from.int64ItemMember().setChangedHere(false);
            from.int64ItemMember().get(1L).setChangedHere(false);

            CheckThrowSV(new ThrowingFunction () {
                    public void func() {Utilities.mergeChanges(into,from);}});
        }

        private void ObjectDictionaries_IntoEmpty_2()
        {
            MemberDictionaries from = new MemberDictionaries();
            MemberDictionaries into = new MemberDictionaries();

            //empty + [1:(M=10)] => empty
            from.int64ItemMember().putObj(1L, new TestItem());
            from.int64ItemMember().get(1L).getObj().myInt().setVal(10);
            from.int64ItemMember().setChanged(false);

            Utilities.mergeChanges(into,from);

            Check(!into.int64ItemMember().isChangedHere());
            CheckEqual(into.int64ItemMember().size(), 0);
        }

        private abstract class NonEmptyObjectDictionariesFixture
        {
            protected MemberDictionaries into = new MemberDictionaries();
            protected MemberDictionaries from = new MemberDictionaries();

            protected NonEmptyObjectDictionariesFixture()
            {
                into.int64ItemMember().putObj(1L, new TestItem());
                into.int64ItemMember().get(1L).getObj().myInt().setVal(10);
                into.int64ItemMember().get(1L).getObj().myString().setVal("one");
                into.int64ItemMember().putObj(2L, new TestItem());
                into.int64ItemMember().get(2L).getObj().myInt().setVal(20);
                into.int64ItemMember().get(2L).getObj().myString().setVal("two");

                into.typeIdItemMember().putObj(1L, new TestItem());
                into.typeIdItemMember().get(1L).getObj().myInt().setVal(10);
                into.typeIdItemMember().get(1L).getObj().myString().setVal("one");
                into.typeIdItemMember().putObj(2L, new TestItem());
                into.typeIdItemMember().get(2L).getObj().myInt().setVal(20);
                into.typeIdItemMember().get(2L).getObj().myString().setVal("two");

                into.enumItemMember().putObj(TestEnum.MY_FIRST, new TestItem());
                into.enumItemMember().get(TestEnum.MY_FIRST).getObj().myInt().setVal(10);
                into.enumItemMember().get(TestEnum.MY_FIRST).getObj().myString().setVal("one");
                into.enumItemMember().putObj(TestEnum.MY_SECOND, new TestItem());
                into.enumItemMember().get(TestEnum.MY_SECOND).getObj().myInt().setVal(20);
                into.enumItemMember().get(TestEnum.MY_SECOND).getObj().myString().setVal("two");

                into.setChanged(false);
            }
        };

        private class ObjectDictionaries_IntoNonEmpty_1 extends NonEmptyObjectDictionariesFixture
        {

            public ObjectDictionaries_IntoNonEmpty_1()
            {
                //[1:(M=10,S=one),2:(M=20,S=two)] + x[x1:(xM=100)] => x[x1:(xM=100)]
                from.int64ItemMember().putObj(1L, new TestItem());
                from.int64ItemMember().get(1L).getObj().myInt().setVal(100);

                //[1:(M=10,S=one),2:(M=20,S=two)] + x[ 1:(xM=200)] => x[ 1:(xM=200)]
                from.typeIdItemMember().putObj(1L, new TestItem());
                from.typeIdItemMember().get(1L).getObj().myInt().setVal(200);
                from.typeIdItemMember().get(1L).setChangedHere(false);

                //[1:(M=10,S=one),2:(M=20,S=two)] + x[ 1:( M=300)] => x[ 1:( M=300)]
                from.enumItemMember().putObj(TestEnum.MY_SECOND, new TestItem());
                from.enumItemMember().get(TestEnum.MY_SECOND).getObj().myInt().setVal(300);
                from.enumItemMember().get(TestEnum.MY_SECOND).setChanged(false);
                Utilities.mergeChanges(into,from);

                CheckEqual(into.int64ItemMember().size(), 1);
                Check(into.int64ItemMember().isChangedHere());
                Check(into.int64ItemMember().get(1L).isChangedHere());
                Check(into.int64ItemMember().get(1L).getObj().myInt().isChanged());
                CheckEqual(into.int64ItemMember().get(1L).getObj().myInt().getVal(),100);
                Check(into.int64ItemMember().get(1L).getObj().myString().isNull());

                CheckEqual(into.typeIdItemMember().size(), 1);
                Check(into.typeIdItemMember().isChangedHere());
                Check(!into.typeIdItemMember().get(1L).isChangedHere());
                Check(into.typeIdItemMember().get(1L).getObj().myInt().isChanged());
                CheckEqual(into.typeIdItemMember().get(1L).getObj().myInt().getVal(),200);
                Check(into.typeIdItemMember().get(1L).getObj().myString().isNull());

                CheckEqual(into.enumItemMember().size(), 1);
                Check(into.enumItemMember().isChangedHere());
                Check(!into.enumItemMember().get(TestEnum.MY_SECOND).isChangedHere());
                Check(!into.enumItemMember().get(TestEnum.MY_SECOND).getObj().myInt().isChanged());
                CheckEqual(into.enumItemMember().get(TestEnum.MY_SECOND).getObj().myInt().getVal(),300);
                Check(into.enumItemMember().get(TestEnum.MY_SECOND).getObj().myString().isNull());

            }
        };

        private class ObjectDictionaries_IntoNonEmpty_2 extends NonEmptyObjectDictionariesFixture
        {

            public ObjectDictionaries_IntoNonEmpty_2()
            {
                //[1:(M=10,S=one),2:(M=20,S=two)] + x[ 3:( M=300)] => x[ 3:( M=300)]
                from.int64ItemMember().putObj(3L, new TestItem());
                from.int64ItemMember().get(3L).getObj().myInt().setVal(300);
                from.int64ItemMember().get(3L).setChanged(false);

                //[1:(M=10,S=one),2:(M=20,S=two)] +  [x2:(xM=200)] =>  [ 1:( M=10, S=one),x2:(xM:200)]
                from.typeIdItemMember().putObj(2L, new TestItem());
                from.typeIdItemMember().get(2L).getObj().myInt().setVal(200);
                from.typeIdItemMember().setChangedHere(false);

                //[1:(M=10,S=one),2:(M=20,S=two)] +  [ 2:(xM=300)] =>  [ 1:( M=10, S=one), 2:(xM:300, S=two)]
                from.enumItemMember().putObj(TestEnum.MY_SECOND, new TestItem());
                from.enumItemMember().get(TestEnum.MY_SECOND).getObj().myInt().setVal(300);
                from.enumItemMember().setChangedHere(false);
                from.enumItemMember().get(TestEnum.MY_SECOND).setChangedHere(false);

                Utilities.mergeChanges(into,from);

                CheckEqual(into.int64ItemMember().size(), 1);
                Check(into.int64ItemMember().isChangedHere());
                Check(!into.int64ItemMember().get(3L).isChangedHere());
                Check(!into.int64ItemMember().get(3L).getObj().myInt().isChanged());
                CheckEqual(into.int64ItemMember().get(3L).getObj().myInt().getVal(),300);
                Check(into.int64ItemMember().get(3L).getObj().myString().isNull());

                CheckEqual(into.typeIdItemMember().size(), 2);
                Check(!into.typeIdItemMember().isChangedHere());
                Check(!into.typeIdItemMember().get(1L).isChanged());
                CheckEqual(into.typeIdItemMember().get(1L).getObj().myInt().getVal(),10);
                Check(into.typeIdItemMember().get(1L).getObj().myString().getVal() == "one");
                Check(into.typeIdItemMember().get(2L).isChangedHere());
                Check(into.typeIdItemMember().get(2L).getObj().myInt().isChanged());
                CheckEqual(into.typeIdItemMember().get(2L).getObj().myInt().getVal(),200);
                Check(!into.typeIdItemMember().get(2L).getObj().myString().isChanged());
                Check(into.typeIdItemMember().get(2L).getObj().myString().isNull());

                CheckEqual(into.enumItemMember().size(), 2);
                Check(!into.enumItemMember().isChangedHere());
                Check(!into.enumItemMember().get(TestEnum.MY_FIRST).isChanged());
                CheckEqual(into.enumItemMember().get(TestEnum.MY_FIRST).getObj().myInt().getVal(),10);
                Check(into.enumItemMember().get(TestEnum.MY_FIRST).getObj().myString().getVal() == "one");
                Check(!into.enumItemMember().get(TestEnum.MY_SECOND).isChangedHere());
                Check(into.enumItemMember().get(TestEnum.MY_SECOND).getObj().myInt().isChanged());
                CheckEqual(into.enumItemMember().get(TestEnum.MY_SECOND).getObj().myInt().getVal(),300);
                Check(into.enumItemMember().get(TestEnum.MY_SECOND).getObj().myString().getVal() == "two");
                Check(!into.enumItemMember().get(TestEnum.MY_SECOND).getObj().myString().isChanged());
            }
        };

        private class ObjectDictionaries_IntoNonEmpty_3 extends NonEmptyObjectDictionariesFixture
        {
            public ObjectDictionaries_IntoNonEmpty_3()
            {
                //[1:(M=10,S=one),2:(M=20,S=two)] +  [x2:( M=200)] =>  [ 1:( M=10, S=one),x2:( M:200)]
                from.int64ItemMember().putObj(2L, new TestItem());
                from.int64ItemMember().get(2L).getObj().myInt().setVal(200);
                from.int64ItemMember().setChangedHere(false);
                from.int64ItemMember().get(2L).getObj().myInt().setChanged(false);

                //[1:(M=10,S=one),2:(M=20,S=two)] +  [ 3:( M=300)] =>  [ 1:( M=10, S=one), 2:( M:20, S=two)]
                from.typeIdItemMember().putObj(3L, new TestItem());
                from.typeIdItemMember().get(3L).getObj().myInt().setVal(300);
                from.typeIdItemMember().setChanged(false);

                //[1:(M=10,S=one),2:(M=20,S=two)] +  [ 2:( M=200)] =>  [ 1:( M=10, S=one), 2:( M:20, S=two)]
                from.enumItemMember().putObj(TestEnum.MY_SECOND, new TestItem());
                from.enumItemMember().get(TestEnum.MY_SECOND).getObj().myInt().setVal(200);
                from.enumItemMember().setChanged(false);

                Utilities.mergeChanges(into,from);
                CheckEqual(into.int64ItemMember().size(), 2);
                Check(!into.int64ItemMember().isChangedHere());
                Check(!into.int64ItemMember().get(1L).isChanged());
                CheckEqual(into.int64ItemMember().get(1L).getObj().myInt().getVal(),10);
                CheckEqual(into.int64ItemMember().get(1L).getObj().myString().getVal(),"one");
                Check(into.int64ItemMember().get(2L).isChangedHere());
                Check(!into.int64ItemMember().get(2L).getObj().myInt().isChanged());
                CheckEqual(into.int64ItemMember().get(2L).getObj().myInt().getVal(),200);
                Check(into.int64ItemMember().get(2L).getObj().myString().isNull());

                CheckEqual(into.typeIdItemMember().size(), 2);
                Check(!into.typeIdItemMember().isChanged());
                CheckEqual(into.typeIdItemMember().get(1L).getObj().myInt().getVal(),10);
                CheckEqual(into.typeIdItemMember().get(1L).getObj().myString().getVal(),"one");
                CheckEqual(into.typeIdItemMember().get(2L).getObj().myInt().getVal(),20);
                CheckEqual(into.typeIdItemMember().get(2L).getObj().myString().getVal(),"two");

                CheckEqual(into.enumItemMember().size(), 2);
                Check(!into.enumItemMember().isChanged());
                CheckEqual(into.enumItemMember().get(TestEnum.MY_FIRST).getObj().myInt().getVal(),10);
                CheckEqual(into.enumItemMember().get(TestEnum.MY_FIRST).getObj().myString().getVal(),"one");
                CheckEqual(into.enumItemMember().get(TestEnum.MY_SECOND).getObj().myInt().getVal(),20);
                CheckEqual(into.enumItemMember().get(TestEnum.MY_SECOND).getObj().myString().getVal(),"two");
            }
        };

        private class ObjectDictionaries_IntoNonEmpty_Error_1 extends NonEmptyObjectDictionariesFixture
        {
            public ObjectDictionaries_IntoNonEmpty_Error_1()
            {
                //[1:(M=10,S=one),2:(M=20,S=two)] +  [x3:( M=300)] =>  error
                from.int64ItemMember().putObj(3L, new TestItem());
                from.int64ItemMember().get(3L).getObj().myInt().setVal(300);
                from.int64ItemMember().setChanged(false);
                from.int64ItemMember().get(3L).setChangedHere(true);

            CheckThrowSV(new ThrowingFunction () {
                    public void func() {Utilities.mergeChanges(into,from);}});
            }
        };

        private class ObjectDictionaries_IntoNonEmpty_Error_2 extends NonEmptyObjectDictionariesFixture
        {
            public ObjectDictionaries_IntoNonEmpty_Error_2()
            {
                //[1:(M=10,S=one),2:(M=20,S=two)] +  [ 3:(xM=300)] =>  error
                from.int64ItemMember().putObj(3L, new TestItem());
                from.int64ItemMember().get(3L).getObj().myInt().setVal(300);
                from.int64ItemMember().setChanged(false);
                from.int64ItemMember().get(3L).getObj().myInt().setChanged(true);

            CheckThrowSV(new ThrowingFunction () {
                    public void func() {Utilities.mergeChanges(into,from);}});
            }
        };

        private class ObjectDictionaries_EmptyIntoNonEmpty extends NonEmptyObjectDictionariesFixture
        {
            public ObjectDictionaries_EmptyIntoNonEmpty()
            {
                //[1:(M=10,S=one),2:(M=20,S=two)] +  x[] =>  x[]
                from.int64ItemMember().setChangedHere(true);

                //[1:(M=10,S=one),2:(M=20,S=two)] +  [] =>  [1:(M=10,S=one),2:(M:20,S=two)]
                //nothing to do...

                Utilities.mergeChanges(into,from);
                CheckEqual(into.int64ItemMember().size(), 0);
                Check(into.int64ItemMember().isChangedHere());

                CheckEqual(into.typeIdItemMember().size(), 2);
                Check(!into.typeIdItemMember().isChanged());
                CheckEqual(into.typeIdItemMember().get(1L).getObj().myInt().getVal(),10);
                CheckEqual(into.typeIdItemMember().get(1L).getObj().myString().getVal(),"one");
                CheckEqual(into.typeIdItemMember().get(2L).getObj().myInt().getVal(),20);
                CheckEqual(into.typeIdItemMember().get(2L).getObj().myString().getVal(),"two");
            }
        };
        private abstract class EmptyObjectSequenceFixture
        {
            protected MemberSequences into = new MemberSequences();
            protected MemberSequences from = new MemberSequences();

            protected EmptyObjectSequenceFixture()
            {
            }
        };

        private class ObjectSequences_1 extends EmptyObjectSequenceFixture
        {
            public ObjectSequences_1()
            {
                //[] + x{(xM=10)} => x{(xM=10)}
                from.testClassMember().add(new TestItem());
                from.testClassMember().get(0).myInt().setVal(10);

                Utilities.mergeChanges(into,from);

                CheckEqual(into.testClassMember().size(),1);
                Check(into.testClassMember().isChangedHere());
                Check(into.testClassMember().get(0).myInt().isChanged());
                CheckEqual(into.testClassMember().get(0).myInt().getVal(),10);
            }
        };


        private class ObjectSequences_2 extends EmptyObjectSequenceFixture
        {
            public ObjectSequences_2()
            {
                //[] + {(xM=10)} => ERROR
                from.testClassMember().add(new TestItem());
                from.testClassMember().get(0).myInt().setVal(10);
                from.testClassMember().setChangedHere(false);

            CheckThrowSV(new ThrowingFunction () {
                    public void func() {Utilities.mergeChanges(into,from);}});
            }
        };

        private class ObjectSequences_3 extends EmptyObjectSequenceFixture
        {
            public ObjectSequences_3()
            {
                //[] + {( M=10)} => []
                from.testClassMember().add(new TestItem());
                from.testClassMember().get(0).myInt().setVal(10);
                from.testClassMember().setChanged(false);

                Utilities.mergeChanges(into,from);
                CheckEqual(into.testClassMember().size(),0);
                Check(!into.testClassMember().isChanged());
            }
        };

        private class ObjectSequences_4 extends EmptyObjectSequenceFixture
        {
            public ObjectSequences_4()
            {
                //[] + x[] => x[]
                from.testClassMember().setChanged(true);

                Utilities.mergeChanges(into,from);
                CheckEqual(into.testClassMember().size(),0);
                Check(into.testClassMember().isChanged());
            }
        };

        private abstract class NonEmptyObjectSequenceFixture
        {
            protected MemberSequences into = new MemberSequences();
            protected MemberSequences from = new MemberSequences();

            protected NonEmptyObjectSequenceFixture()
            {
                into.testClassMember().add(new TestItem());
                into.testClassMember().get(0).myInt().setVal(10);
                into.testClassMember().get(0).myString().setVal("one");
                into.testClassMember().add(new TestItem());
                into.testClassMember().get(1).myInt().setVal(20);
                into.testClassMember().get(1).myString().setVal("two");
                into.setChanged(false);
            }
        };

        private class ObjectSequences_IntoNonEmpty_1 extends NonEmptyObjectSequenceFixture
        {
            public ObjectSequences_IntoNonEmpty_1()
            {
                //{(M=10,S=one),(M=20,S=two)} + x{(M=30)} => x{(M=30)}
                from.testClassMember().add(new TestItem());
                from.testClassMember().get(0).myInt().setVal(30);
                from.testClassMember().get(0).myInt().setChanged(false);

                Utilities.mergeChanges(into,from);

                CheckEqual(into.testClassMember().size(),1);
                Check(into.testClassMember().isChangedHere());
                Check(!into.testClassMember().get(0).myInt().isChanged());
                CheckEqual(into.testClassMember().get(0).myInt().getVal(),30);
            }
        }


        private class ObjectSequences_IntoNonEmpty_2 extends NonEmptyObjectSequenceFixture
        {
            public ObjectSequences_IntoNonEmpty_2()
            {
                //{(M=10,S=one),(M=20,S=two)} + {(M=30)} => {(M=10,S=one),(M=20,S=two)}
                from.testClassMember().add(new TestItem());
                from.testClassMember().get(0).myInt().setVal(30);
                from.testClassMember().setChanged(false);

                Utilities.mergeChanges(into,from);

                CheckEqual(into.testClassMember().size(),2);
                Check(!into.testClassMember().isChanged());
                CheckEqual(into.testClassMember().get(0).myInt().getVal(),10);
                CheckEqual(into.testClassMember().get(0).myString().getVal(),"one");
                CheckEqual(into.testClassMember().get(1).myInt().getVal(),20);
                CheckEqual(into.testClassMember().get(1).myString().getVal(),"two");
            }
        };

        private class ObjectSequences_IntoNonEmpty_3 extends NonEmptyObjectSequenceFixture
        {
            public ObjectSequences_IntoNonEmpty_3()
            {
                //{(M=10,S=one),(M=20,S=two)} + {(xM=30)} => error??
                from.testClassMember().add(new TestItem());
                from.testClassMember().get(0).myInt().setVal(30);
                from.testClassMember().setChangedHere(false);

                CheckThrowSV(new ThrowingFunction () {
                        public void func() {Utilities.mergeChanges(into,from);}});
            }
        };

        private class ObjectSequences_IntoNonEmpty_4 extends NonEmptyObjectSequenceFixture
        {
            public ObjectSequences_IntoNonEmpty_4()
            {
                //{(M=10,S=one),(M=20,S=two)} + {(xM=30),()} => {(xM=30,S=one),(M=20,S=two)}
                from.testClassMember().add(new TestItem());
                from.testClassMember().add(new TestItem());
                from.testClassMember().get(0).myInt().setVal(30);
                from.testClassMember().setChangedHere(false);

                Utilities.mergeChanges(into,from);

                CheckEqual(into.testClassMember().size(),2);
                Check(!into.testClassMember().isChangedHere());
                Check(!into.testClassMember().get(1).isChanged());
                Check(into.testClassMember().get(0).myInt().isChanged());
                Check(!into.testClassMember().get(0).myString().isChanged());
                CheckEqual(into.testClassMember().get(0).myInt().getVal(),30);
                CheckEqual(into.testClassMember().get(0).myString().getVal(),"one");
                CheckEqual(into.testClassMember().get(1).myInt().getVal(),20);
                CheckEqual(into.testClassMember().get(1).myString().getVal(),"two");
            }
        };
        private class ObjectSequences_IntoNonEmpty_5 extends NonEmptyObjectSequenceFixture
        {
            public ObjectSequences_IntoNonEmpty_5()
            {
                //{(M=10,S=one),(M=20,S=two)} + {(xM=30),(S=blahonga)} => {(xM=30,S=one),(M=20,S=two)}
                from.testClassMember().add(new TestItem());
                from.testClassMember().add(new TestItem());
                from.testClassMember().setChangedHere(false);
                from.testClassMember().get(0).myInt().setVal(30);
                from.testClassMember().get(1).myString().setVal("blahonga");
                from.testClassMember().get(1).myString().setChanged(false);

                Utilities.mergeChanges(into,from);

                CheckEqual(into.testClassMember().size(),2);
                Check(!into.testClassMember().isChangedHere());
                Check(!into.testClassMember().get(1).isChanged());
                Check(into.testClassMember().get(0).myInt().isChanged());
                Check(!into.testClassMember().get(0).myString().isChanged());
                CheckEqual(into.testClassMember().get(0).myInt().getVal(),30);
                CheckEqual(into.testClassMember().get(0).myString().getVal(),"one");
                CheckEqual(into.testClassMember().get(1).myInt().getVal(),20);
                CheckEqual(into.testClassMember().get(1).myString().getVal(),"two");
            }
        };

        private void Test_ParserExceptions() {
            String brokenXml = "<?xml version=\"1.0\" encoding=\"utf-8\"?><DotsTest.MemberSequences><Int32WRONGMember><Int32>10</Int32></Int32Member></DotsTest.MemberSequences>";
            String brokenJson = "{\"_DouType\":\"DotsTest.MemberSequences\",\"Int32WRONGMember\":[10,20]}";

            try {
                Serialization.toObject(brokenXml);
                Check(false);
            }
            catch (IllegalValueException exc) {
                Check(exc.getMessage().contains("does not contain a member named"));
            }

            try {
                Serialization.toObjectFromJson(brokenJson);
                Check(false);
            }
            catch (IllegalValueException exc) {
                Check(exc.getMessage().contains("does not contain a member named"));
            }

            try {
                Serialization.toObject("");
                Check(false);
            }
            catch (IllegalValueException exc) {
            }

            try {
                Serialization.toObjectFromJson("");
                Check(false);
            }
            catch (IllegalValueException exc) {
            }
        }



        public void Test_MergeChanges()
        {
            Simple();
            Arrays();
            Objects();
            Objects_BothHaveData();
            Sequences();
            Dictionaries_IntoEmpty();
            Dictionaries_IntoEmpty_Error();
            Dictionaries_IntoNonEmpty_1();
            Dictionaries_IntoNonEmpty_2();
            Dictionaries_IntoNonEmpty_Error();
            ObjectDictionaries_IntoEmpty_1();
            ObjectDictionaries_IntoEmpty_Error_1();
            ObjectDictionaries_IntoEmpty_Error_2();
            ObjectDictionaries_IntoEmpty_Error_3();
            ObjectDictionaries_IntoEmpty_2();
            new ObjectDictionaries_IntoNonEmpty_1();
            new ObjectDictionaries_IntoNonEmpty_2();
            new ObjectDictionaries_IntoNonEmpty_3();
            new ObjectDictionaries_IntoNonEmpty_Error_1();
            new ObjectDictionaries_IntoNonEmpty_Error_2();
            new ObjectDictionaries_EmptyIntoNonEmpty();
            new ObjectSequences_1();
            new ObjectSequences_2();
            new ObjectSequences_3();
            new ObjectSequences_4();
            new ObjectSequences_IntoNonEmpty_1();
            new ObjectSequences_IntoNonEmpty_2();
            new ObjectSequences_IntoNonEmpty_3();
            new ObjectSequences_IntoNonEmpty_4();
            new ObjectSequences_IntoNonEmpty_5();
            Test_ParserExceptions();
        }
    }

}
