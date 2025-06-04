/******************************************************************************
*
* Copyright Saab AB, 2005-2013,2015 (http://safirsdkcore.com)
*
* Created by: Henrik Sundberg / sthesu
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

using System;

/// <summary>
/// Summary description for DotsTestDotnet.
/// </summary>
class DotsTestDotnet
{

    /// <summary>
    /// The main entry point for the application.
    /// </summary>
    [STAThread]
    static void Main(String[] args)
    {
        Console.OutputEncoding = new System.Text.UTF8Encoding(false);
        System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo("en-US");

        Test_Has_Property();
        Test_PropertyMappingKind();
        Test_Property_GetParameterReference();
        Test_GetName();
        Test_GetNumberOfMembers();
        Test_GetNumberOfParameters();
        Test_Create_Routines();
        Test_Int32();
        Test_Int64();
        Test_Float32();
        Test_Float64();
        Test_Boolean();
        Test_Enumeration();
        Test_String();
        Test_EntityId();
        Test_InstanceId();
        Test_TypeId();
        Test_ChannelId();
        Test_HandlerId();
        Test_Object();
        Test_Binary();
        Test_TestClass();
        Test_Ampere32();
        Test_CubicMeter32();
        Test_Hertz32();
        Test_Joule32();
        Test_Kelvin32();
        Test_Kilogram32();
        Test_Meter32();
        Test_MeterPerSecond32();
        Test_MeterPerSecondSquared32();
        Test_Newton32();
        Test_Pascal32();
        Test_Radian32();
        Test_RadianPerSecond32();
        Test_RadianPerSecondSquared32();
        Test_Second32();
        Test_SquareMeter32();
        Test_Steradian32();
        Test_Volt32();
        Test_Watt32();
        Test_Ampere64();
        Test_CubicMeter64();
        Test_Hertz64();
        Test_Joule64();
        Test_Kelvin64();
        Test_Kilogram64();
        Test_Meter64();
        Test_MeterPerSecond64();
        Test_MeterPerSecondSquared64();
        Test_Newton64();
        Test_Pascal64();
        Test_Radian64();
        Test_RadianPerSecond64();
        Test_RadianPerSecondSquared64();
        Test_Second64();
        Test_SquareMeter64();
        Test_Steradian64();
        Test_Volt64();
        Test_Watt64();
        Test_TestException();
        Test_LibraryExceptions();
        Test_IsProperty();
        Test_IsEnumeration();
        Test_IsException();
        Test_GetDouFilePath();
        Test_Sequences();
        Test_Dictionaries();
        Test_DeserializeUnlinkedObject();
        Test_ObjectClone();
        Test_ContainerClone();
        Test_ContainerCopy();
        var misc = new Misc.MiscTests();
        misc.Test_Containers();
        misc.Test_BlobChangeFlags();

        var merge = new Misc.MergeChangesTests();
        merge.Test_MergeChanges();

    }

    private static void Test_IsException()
    {
        Header("IsException");
        long[] idArray = Safir.Dob.Typesystem.Operations.GetAllTypeIds();
        foreach (long id in idArray)
        {
            if (Safir.Dob.Typesystem.Operations.IsException(id))
            {
                //only care about the ones that are ours
                if (Safir.Dob.Typesystem.Operations.GetName(id).StartsWith("DotsTest"))
                {
                    Console.WriteLine(Safir.Dob.Typesystem.Operations.GetName(id));
                }
            }
        }
    }

    private static void Test_IsEnumeration()
    {
        Header("IsEnumeration");
        long[] idArray = Safir.Dob.Typesystem.Operations.GetAllTypeIds();
        foreach (long id in idArray)
        {
            if (Safir.Dob.Typesystem.Operations.IsEnumeration(id))
            {
                //only care about the ones that are ours
                if (Safir.Dob.Typesystem.Operations.GetName(id).StartsWith("DotsTest"))
                {
                    Console.WriteLine(Safir.Dob.Typesystem.Operations.GetName(id));
                }
            }
        }
    }

    private static void Test_IsProperty()
    {
        Header("IsProperty");
        long[] idArray = Safir.Dob.Typesystem.Operations.GetAllTypeIds();
        foreach (long id in idArray)
        {
            if (Safir.Dob.Typesystem.Operations.IsProperty(id))
            {
                //only care about the ones that are ours
                if (Safir.Dob.Typesystem.Operations.GetName(id).StartsWith("DotsTest"))
                {
                    Console.WriteLine(Safir.Dob.Typesystem.Operations.GetName(id));
                }
            }
        }
    }

    private static void Header(String label)
    {
        Console.WriteLine();
        Console.WriteLine();
        Console.WriteLine();
        Console.WriteLine("=====================================================");
        Console.WriteLine("Testing: " + label);
        Console.WriteLine("=====================================================");
    }

    private static DotsTest.MemberTypes MT1 = new DotsTest.MemberTypes();
    private static DotsTest.MemberTypes MT2 = new DotsTest.MemberTypes();
    private static DotsTest.MemberArrays MA1 = new DotsTest.MemberArrays();
    private static DotsTest.MemberArrays MA2 = new DotsTest.MemberArrays();
    private static DotsTest.EmptyObject EO = new DotsTest.EmptyObject();
    private static DotsTest.AnotherEmptyObject AEO = new DotsTest.AnotherEmptyObject();
    private static DotsTest.MemberItemsArray MIA = new DotsTest.MemberItemsArray();
    private static DotsTest.MemberItems MI = new DotsTest.MemberItems();



    private static void Test_Has_Property()
    {
        Header("Has Property");
        Console.WriteLine("MemberTypes - MemberTypesProperty: " + DotsTest.MemberTypesProperty.HasProperty(MT1).ToString().ToLower());
        Console.WriteLine("MemberTypes - MemberArraysProperty: " + DotsTest.MemberArraysProperty.HasProperty(MT1).ToString().ToLower());
        Console.WriteLine("MemberArrays - MemberTypesProperty: " + DotsTest.MemberTypesProperty.HasProperty(MA1).ToString().ToLower());
        Console.WriteLine("MemberArrays - MemberArraysProperty: " + DotsTest.MemberArraysProperty.HasProperty(MA1).ToString().ToLower());
        Console.WriteLine("MemberItems - MemberTypesProperty: " + DotsTest.MemberTypesProperty.HasProperty(MI).ToString().ToLower());
        Console.WriteLine("MemberItems - MemberArraysProperty: " + DotsTest.MemberArraysProperty.HasProperty(MI).ToString().ToLower());
        Console.WriteLine("MemberItemsArray - MemberTypesProperty: " + DotsTest.MemberTypesProperty.HasProperty(MIA).ToString().ToLower());
        Console.WriteLine("MemberItemsArray - MemberArraysProperty: " + DotsTest.MemberArraysProperty.HasProperty(MIA).ToString().ToLower());
        Console.WriteLine("EmptyObject - MemberTypesProperty: " + DotsTest.MemberTypesProperty.HasProperty(EO).ToString().ToLower());
        Console.WriteLine("EmptyObject - MemberArraysProperty: " + DotsTest.MemberArraysProperty.HasProperty(EO).ToString().ToLower());
        Console.WriteLine("AnotherEmptyObject - MemberTypesProperty: " + DotsTest.MemberTypesProperty.HasProperty(AEO).ToString().ToLower());
        Console.WriteLine("AnotherEmptyObject - MemberArraysProperty: " + DotsTest.MemberArraysProperty.HasProperty(AEO).ToString().ToLower());
    }

    private static string MappingKindStr(Safir.Dob.Typesystem.PropertyMappingKind k)
    {
        switch (k)
        {
        case Safir.Dob.Typesystem.PropertyMappingKind.MappedToNull: return "MappedToNull";
        case Safir.Dob.Typesystem.PropertyMappingKind.MappedToMember: return "MappedToMember";
        case Safir.Dob.Typesystem.PropertyMappingKind.MappedToParameter: return "MappedToParameter";
        }
        return "";
    }

    private static void CheckGetInfo(long typeId,
                                     int memberIndex,
                                     Safir.Dob.Typesystem.MemberType  lMT,
                                     Safir.Dob.Typesystem.MemberType  lKT,
                                     string                           lMN,
                                     long                             lMTId,
                                     long                             lKTId,
                                     int                              lSL,
                                     Safir.Dob.Typesystem.CollectionType lCT,
                                     int                              lAL)
    {
        var info = Safir.Dob.Typesystem.Members.GetInfo(typeId, memberIndex);
        if (info.typeId != typeId ||
            info.memberIndex != memberIndex ||
            info.memberType != lMT ||
            info.keyType != lKT ||
            info.memberName != lMN||
            info.memberTypeId != lMTId ||
            info.keyTypeId != lKTId||
            info.stringLength != lSL||
            info.collectionType != lCT||
            info.arrayLength != lAL)
        {
            Console.WriteLine("GetInfo mismatch! Problem in implementation of simplified GetInfo:");
            Console.WriteLine("GetInfo 1: " +lMT + "," + lMN + "," + lMTId+ "," + lSL + "," + CtStr(lCT) + "," + lAL);
            Console.WriteLine("GetInfo 2: "
                     + info.memberType + ","
                     + info.memberName + ","
                     + info.memberTypeId+ ","
                     + info.stringLength + ","
                     + CtStr(info.collectionType) + ","
                              + info.arrayLength);
        }
    }

    private static void Test_PropertyMappingKind()
    {
        Header("Property Mapping Kind");
        Console.WriteLine("EmptyObject: " + MappingKindStr(Safir.Dob.Typesystem.Properties.GetMappingKind(DotsTest.EmptyObject.ClassTypeId, DotsTest.MemberTypesProperty.ClassTypeId, 0)));
        Console.WriteLine("AnotherEmptyObject: "   + MappingKindStr(Safir.Dob.Typesystem.Properties.GetMappingKind(DotsTest.AnotherEmptyObject.ClassTypeId, DotsTest.MemberTypesProperty.ClassTypeId, 0)));
        Console.WriteLine("MemberItems: " + MappingKindStr(Safir.Dob.Typesystem.Properties.GetMappingKind(DotsTest.MemberItems.ClassTypeId, DotsTest.MemberTypesProperty.ClassTypeId, 0)));
    }

    private static void Test_Property_GetParameterReference()
    {
        Header("Property Get Parameter Reference");

        long paramTid;
        int paramIx;
        int paramArrIx;

        Safir.Dob.Typesystem.Properties.GetParameterReference(DotsTest.EmptyObject.ClassTypeId, DotsTest.MemberTypesProperty.ClassTypeId, 6, 0, out paramTid, out paramIx, out paramArrIx);
        Console.WriteLine("EmptyObject: " + Safir.Dob.Typesystem.Parameters.GetString(paramTid, paramIx, paramArrIx));
        Safir.Dob.Typesystem.Properties.GetParameterReference(DotsTest.AnotherEmptyObject.ClassTypeId, DotsTest.MemberTypesProperty.ClassTypeId, 9, 0, out paramTid, out paramIx, out paramArrIx);
        Console.WriteLine("AnotherEmptyObject: " + Safir.Dob.Typesystem.Parameters.GetTypeId(paramTid, paramIx, paramArrIx));

        try
        {
            Safir.Dob.Typesystem.Properties.GetParameterReference(DotsTest.MemberItems.ClassTypeId, DotsTest.MemberTypesProperty.ClassTypeId, 0, 0, out paramTid, out paramIx, out paramArrIx);
        }
        catch (Safir.Dob.Typesystem.IllegalValueException)
        {
            Console.WriteLine("Not mapped to parameter");
        }
    }

    private static void Test_GetName()
    {
        Header("Get Name");
        Console.WriteLine("MemberTypes          - " + Safir.Dob.Typesystem.Operations.GetName(DotsTest.MemberTypes.ClassTypeId));
        Console.WriteLine("MemberArrays         - " + Safir.Dob.Typesystem.Operations.GetName(DotsTest.MemberArrays.ClassTypeId));
        Console.WriteLine("MemberTypesProperty  - " + Safir.Dob.Typesystem.Operations.GetName(DotsTest.MemberTypesProperty.ClassTypeId));
        Console.WriteLine("MemberArraysProperty - " + Safir.Dob.Typesystem.Operations.GetName(DotsTest.MemberArraysProperty.ClassTypeId));
        Console.WriteLine("MemberItems          - " + Safir.Dob.Typesystem.Operations.GetName(DotsTest.MemberItems.ClassTypeId));
        Console.WriteLine("MemberItemsArray     - " + Safir.Dob.Typesystem.Operations.GetName(DotsTest.MemberItemsArray.ClassTypeId));
        Console.WriteLine("EmptyObject          - " + Safir.Dob.Typesystem.Operations.GetName(DotsTest.EmptyObject.ClassTypeId));
    }

    private static void Test_GetNumberOfMembers()
    {
        Header("Get Number Of Members");
        Console.WriteLine("MemberTypes          - " + Safir.Dob.Typesystem.Members.GetNumberOfMembers(DotsTest.MemberTypes.ClassTypeId));
        Console.WriteLine("MemberArrays         - " + Safir.Dob.Typesystem.Members.GetNumberOfMembers(DotsTest.MemberArrays.ClassTypeId));
        Console.WriteLine("MemberTypesProperty  - " + Safir.Dob.Typesystem.Members.GetNumberOfMembers(DotsTest.MemberTypesProperty.ClassTypeId));
        Console.WriteLine("MemberArraysProperty - " + Safir.Dob.Typesystem.Members.GetNumberOfMembers(DotsTest.MemberArraysProperty.ClassTypeId));
        Console.WriteLine("MemberItems          - " + Safir.Dob.Typesystem.Members.GetNumberOfMembers(DotsTest.MemberItems.ClassTypeId));
        Console.WriteLine("MemberItemsArray     - " + Safir.Dob.Typesystem.Members.GetNumberOfMembers(DotsTest.MemberItemsArray.ClassTypeId));
        Console.WriteLine("EmptyObject          - " + Safir.Dob.Typesystem.Members.GetNumberOfMembers(DotsTest.EmptyObject.ClassTypeId));
        Console.WriteLine("ParameterTypes       - " + Safir.Dob.Typesystem.Members.GetNumberOfMembers(DotsTest.ParameterTypes.ClassTypeId));
        Console.WriteLine("ParameterArrays      - " + Safir.Dob.Typesystem.Members.GetNumberOfMembers(DotsTest.ParameterArrays.ClassTypeId));

    }

    private static void Test_GetNumberOfParameters()
    {
        Header("Get Number Of Parameters");
        Console.WriteLine("MemberTypes          - " + Safir.Dob.Typesystem.Parameters.GetNumberOfParameters(DotsTest.MemberTypes.ClassTypeId));
        Console.WriteLine("MemberArrays         - " + Safir.Dob.Typesystem.Parameters.GetNumberOfParameters(DotsTest.MemberArrays.ClassTypeId));
        Console.WriteLine("MemberItems          - " + Safir.Dob.Typesystem.Parameters.GetNumberOfParameters(DotsTest.MemberItems.ClassTypeId));
        Console.WriteLine("MemberItemsArray     - " + Safir.Dob.Typesystem.Parameters.GetNumberOfParameters(DotsTest.MemberItemsArray.ClassTypeId));
        Console.WriteLine("EmptyObject          - " + Safir.Dob.Typesystem.Parameters.GetNumberOfParameters(DotsTest.EmptyObject.ClassTypeId));
        Console.WriteLine("ParameterTypes       - " + Safir.Dob.Typesystem.Parameters.GetNumberOfParameters(DotsTest.ParameterTypes.ClassTypeId));
        Console.WriteLine("ParameterArrays      - " + Safir.Dob.Typesystem.Parameters.GetNumberOfParameters(DotsTest.ParameterArrays.ClassTypeId));
    }

    private static void Test_Create_Routines()
    {
        Header("Create routines (Types)");
        Int32 i = DotsTest.ParameterTypes.Int32Parameter;
        DotsTest.TestEnum.Enumeration e = DotsTest.ParameterTypes.EnumerationParameter;
        DotsTest.TestItem c = DotsTest.ParameterTypes.TestClassParameter;
        Console.WriteLine("Create_ParameterTypes: " +
                          Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberTypes.CreateParameterTypes
                                                                   (i, e, c)));
        Console.WriteLine("CreateValueTypes     : " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberTypes.CreateValueTypes()));
        Console.WriteLine("Create_ValueArrays   : " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberTypes.CreateValueArrays()));
        Console.WriteLine("CreateValues         : " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.TypesItem.CreateCreateRoutineValues()));
        Console.WriteLine("CreateInheritedValues: " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.TypesItemInherited.CreateCreateRoutineValues2()));

        var createParameters = DotsTest.TypesItem.CreateCreateRoutineParameters(
        10,
        20,
        30.1f,
        40.1,
        false,
        DotsTest.TestEnum.Enumeration.MySecond,
        "Hello",
        new Safir.Dob.Typesystem.EntityId(Safir.Dob.Entity.ClassTypeId, new Safir.Dob.Typesystem.InstanceId(1)),
        new Safir.Dob.Typesystem.EntityId(Safir.Dob.Entity.ClassTypeId, new Safir.Dob.Typesystem.InstanceId(2)),
        Safir.Dob.Entity.ClassTypeId,
        new Safir.Dob.Typesystem.InstanceId(1),
        new Safir.Dob.Typesystem.InstanceId(2),
        new Safir.Dob.Typesystem.ChannelId(1),
        new Safir.Dob.Typesystem.ChannelId(2),
        new Safir.Dob.Typesystem.HandlerId(1),
        new Safir.Dob.Typesystem.HandlerId(2),
        new Safir.Dob.Entity(),
        new byte[]{97,98,99},
        new DotsTest.TestItem(),
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

        var createParametersInherited = DotsTest.TypesItemInherited.CreateCreateRoutineParameters2(
        "Local Member",
        10,
        20,
        30.1f,
        40.1,
        false,
        DotsTest.TestEnum.Enumeration.MySecond,
        "Hello",
        new Safir.Dob.Typesystem.EntityId(Safir.Dob.Entity.ClassTypeId, new Safir.Dob.Typesystem.InstanceId(1)),
        new Safir.Dob.Typesystem.EntityId(Safir.Dob.Entity.ClassTypeId, new Safir.Dob.Typesystem.InstanceId(2)),
        Safir.Dob.Entity.ClassTypeId,
        new Safir.Dob.Typesystem.InstanceId(1),
        new Safir.Dob.Typesystem.InstanceId(2),
        new Safir.Dob.Typesystem.ChannelId(1),
        new Safir.Dob.Typesystem.ChannelId(2),
        new Safir.Dob.Typesystem.HandlerId(1),
        new Safir.Dob.Typesystem.HandlerId(2),
        new Safir.Dob.Entity(),
        new byte[]{97,98,99},
        new DotsTest.TestItem(),
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

        Console.WriteLine("CreateParam          : " + Safir.Dob.Typesystem.Serialization.ToXml(createParameters));
        Console.WriteLine("CreateParamInherited : " + Safir.Dob.Typesystem.Serialization.ToXml(createParametersInherited));
    }


    private static void Test_Int32()
    {
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Int32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Int32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Int32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Int32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Int32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Int32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Int32Member.IsNull();
        In_Req_Ok = !MT1.Int32Member.IsChanged();
        MT1.Int32Member.Val = DotsTest.ParameterTypes.Int32Parameter;
        Null_Ok = Null_Ok && !MT1.Int32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Int32Member.IsChanged();

        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Int32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Int32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Int32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Int32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Int32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Int32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Int32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullInt32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedInt32Member(MT2);
        DotsTest.MemberTypesProperty.SetInt32Member(MT2, MT1.Int32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetInt32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInt32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedInt32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullInt32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedInt32Member(MI);
        DotsTest.MemberTypesProperty.SetInt32Member(MI, MT2.Int32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetInt32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInt32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedInt32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullInt32Member(MIA);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedInt32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetInt32Member(MIA, MT2.Int32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Int32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInt32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedInt32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInt32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedInt32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetInt32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInt32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedInt32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInt32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedInt32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetInt32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInt32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedInt32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Int32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Int32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Int32Member[ix].IsChanged();
            MA1.Int32Member[ix].Val = DotsTest.ParameterArrays.Int32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Int32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Int32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullInt32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedInt32Member(MA2, ix);
            MA2.Int32Member[ix].Val = MA1.Int32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInt32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedInt32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetInt32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Int32Member[ix].Val = MA1.Int32Member[ix].Val;

            MI.ArraysItem.Obj = item;

            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetInt32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInt32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedInt32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetInt32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInt32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedInt32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInt32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedInt32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetInt32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInt32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedInt32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInt32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedInt32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetInt32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInt32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedInt32Member(AEO, ix);
        }

        // SetNull test
        MT1.Int32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullInt32Member(MT2);
        MA1.Int32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullInt32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Int32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullInt32Member(MT2) &&
            MA1.Int32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullInt32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());

    }

    private static void Test_Int64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Int64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Int64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Int64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Int64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Int64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Int64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Int64Member.IsNull();
        In_Req_Ok = !MT1.Int64Member.IsChanged();
        MT1.Int64Member.Val = DotsTest.ParameterTypes.Int64Parameter;
        Null_Ok = Null_Ok && !MT1.Int64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Int64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Int64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Int64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Int64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Int64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Int64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Int64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Int64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullInt64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedInt64Member(MT2);
        DotsTest.MemberTypesProperty.SetInt64Member(MT2, MT1.Int64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetInt64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInt64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedInt64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullInt64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedInt64Member(MI);
        DotsTest.MemberTypesProperty.SetInt64Member(MI, MT2.Int64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetInt64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInt64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedInt64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullInt64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedInt64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetInt64Member(MIA, MT2.Int64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Int64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInt64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedInt64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInt64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedInt64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetInt64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInt64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedInt64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInt64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedInt64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetInt64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInt64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedInt64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Int64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Int64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Int64Member[ix].IsChanged();
            MA1.Int64Member[ix].Val = DotsTest.ParameterArrays.Int64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Int64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Int64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullInt64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedInt64Member(MA2, ix);
            MA2.Int64Member[ix].Val = MA1.Int64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInt64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedInt64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetInt64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Int64Member[ix].Val = MA1.Int64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetInt64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInt64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedInt64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetInt64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInt64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedInt64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInt64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedInt64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetInt64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInt64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedInt64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInt64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedInt64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetInt64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInt64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedInt64Member(AEO, ix);
        }

        // SetNull test
        MT1.Int64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullInt64Member(MT2);
        MA1.Int64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullInt64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Int64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullInt64Member(MT2) &&
            MA1.Int64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullInt64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());

    } // Test_Int64

    private static void Test_Float32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Float32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Float32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Float32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Float32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Float32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Float32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Float32Member.IsNull();
        In_Req_Ok = !MT1.Float32Member.IsChanged();
        MT1.Float32Member.Val = DotsTest.ParameterTypes.Float32Parameter;
        Null_Ok = Null_Ok && !MT1.Float32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Float32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Float32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Float32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Float32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Float32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Float32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Float32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Float32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullFloat32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedFloat32Member(MT2);
        DotsTest.MemberTypesProperty.SetFloat32Member(MT2, MT1.Float32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetFloat32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullFloat32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedFloat32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullFloat32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedFloat32Member(MI);
        DotsTest.MemberTypesProperty.SetFloat32Member(MI, MT2.Float32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetFloat32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullFloat32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedFloat32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullFloat32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedFloat32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetFloat32Member(MIA, MT2.Float32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Float32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullFloat32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedFloat32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullFloat32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedFloat32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetFloat32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullFloat32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedFloat32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullFloat32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedFloat32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetFloat32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullFloat32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedFloat32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Float32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Float32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Float32Member[ix].IsChanged();
            MA1.Float32Member[ix].Val = DotsTest.ParameterArrays.Float32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Float32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Float32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullFloat32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedFloat32Member(MA2, ix);
            MA2.Float32Member[ix].Val = MA1.Float32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullFloat32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedFloat32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetFloat32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Float32Member[ix].Val = MA1.Float32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetFloat32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullFloat32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedFloat32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetFloat32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullFloat32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedFloat32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullFloat32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedFloat32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetFloat32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullFloat32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedFloat32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullFloat32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedFloat32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetFloat32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullFloat32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedFloat32Member(AEO, ix);
        }

        // SetNull test
        MT1.Float32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullFloat32Member(MT2);
        MA1.Float32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullFloat32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Float32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullFloat32Member(MT2) &&
            MA1.Float32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullFloat32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Float32

    private static void Test_Float64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Float64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Float64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Float64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Float64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Float64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Float64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Float64Member.IsNull();
        In_Req_Ok = !MT1.Float64Member.IsChanged();
        MT1.Float64Member.Val = DotsTest.ParameterTypes.Float64Parameter;
        Null_Ok = Null_Ok && !MT1.Float64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Float64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Float64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Float64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Float64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Float64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Float64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Float64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Float64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullFloat64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedFloat64Member(MT2);
        DotsTest.MemberTypesProperty.SetFloat64Member(MT2, MT1.Float64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetFloat64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullFloat64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedFloat64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullFloat64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedFloat64Member(MI);
        DotsTest.MemberTypesProperty.SetFloat64Member(MI, MT2.Float64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetFloat64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullFloat64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedFloat64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullFloat64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedFloat64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetFloat64Member(MIA, MT2.Float64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Float64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullFloat64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedFloat64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullFloat64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedFloat64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetFloat64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullFloat64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedFloat64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullFloat64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedFloat64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetFloat64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullFloat64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedFloat64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Float64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Float64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Float64Member[ix].IsChanged();
            MA1.Float64Member[ix].Val = DotsTest.ParameterArrays.Float64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Float64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Float64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullFloat64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedFloat64Member(MA2, ix);
            MA2.Float64Member[ix].Val = MA1.Float64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullFloat64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedFloat64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetFloat64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Float64Member[ix].Val = MA1.Float64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetFloat64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullFloat64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedFloat64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetFloat64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullFloat64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedFloat64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullFloat64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedFloat64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetFloat64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullFloat64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedFloat64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullFloat64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedFloat64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetFloat64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullFloat64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedFloat64Member(AEO, ix);
        }

        // SetNull test
        MT1.Float64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullFloat64Member(MT2);
        MA1.Float64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullFloat64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Float64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullFloat64Member(MT2) &&
            MA1.Float64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullFloat64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Float64

    private static void Test_Boolean()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Boolean");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.BooleanMemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.BooleanMemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.BooleanParameterArraySize == 2 &&
                           DotsTest.MemberArrays.BooleanMemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.BooleanMemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.BooleanMember.IsNull();
        In_Req_Ok = !MT1.BooleanMember.IsChanged();
        MT1.BooleanMember.Val = DotsTest.ParameterTypes.BooleanParameter;
        Null_Ok = Null_Ok && !MT1.BooleanMember.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.BooleanMember.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.BooleanMemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.BooleanMemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.BooleanMemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.BooleanMemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "BooleanParameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "BooleanParameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "BooleanParameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullBooleanMember(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedBooleanMember(MT2);
        DotsTest.MemberTypesProperty.SetBooleanMember(MT2, MT1.BooleanMember.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetBooleanMember(MT2).ToString().ToLower());
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullBooleanMember(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedBooleanMember(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullBooleanMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedBooleanMember(MI);
        DotsTest.MemberTypesProperty.SetBooleanMember(MI, MT2.BooleanMember.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetBooleanMember(MI).ToString().ToLower());
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullBooleanMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedBooleanMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullBooleanMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedBooleanMember(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetBooleanMember(MIA, MT2.BooleanMember.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.BooleanMember.Val.ToString().ToLower());
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullBooleanMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedBooleanMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullBooleanMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedBooleanMember(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetBooleanMember(EO).ToString().ToLower());
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullBooleanMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedBooleanMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullBooleanMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedBooleanMember(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetBooleanMember(AEO).ToString().ToLower());
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullBooleanMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedBooleanMember(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.BooleanParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.BooleanMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.BooleanMember[ix].IsChanged();
            MA1.BooleanMember[ix].Val = DotsTest.ParameterArrays.BooleanParameter(ix);
            Null_Ok = Null_Ok && !MA1.BooleanMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.BooleanMember[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullBooleanMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedBooleanMember(MA2, ix);
            MA2.BooleanMember[ix].Val = MA1.BooleanMember[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullBooleanMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedBooleanMember(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetBooleanMember(MA2, ix).ToString().ToLower());

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.BooleanMember[ix].Val = MA1.BooleanMember[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetBooleanMember(MI, ix).ToString().ToLower());
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullBooleanMember(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedBooleanMember(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetBooleanMember(MIA, ix).ToString().ToLower());
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullBooleanMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedBooleanMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullBooleanMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedBooleanMember(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetBooleanMember(EO, ix).ToString().ToLower());
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullBooleanMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedBooleanMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullBooleanMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedBooleanMember(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetBooleanMember(AEO, ix).ToString().ToLower());
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullBooleanMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedBooleanMember(AEO, ix);
        }

        // SetNull test
        MT1.BooleanMember.SetNull();
        DotsTest.MemberTypesProperty.SetNullBooleanMember(MT2);
        MA1.BooleanMember[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullBooleanMember(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.BooleanMember.IsNull() && DotsTest.MemberTypesProperty.IsNullBooleanMember(MT2) &&
            MA1.BooleanMember[1].IsNull() && DotsTest.MemberArraysProperty.IsNullBooleanMember(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());


    } // Boolean

    private static void Test_Enumeration()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Enumeration");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.EnumerationMemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.EnumerationMemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.EnumerationParameterArraySize == 2 &&
                           DotsTest.MemberArrays.EnumerationMemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.EnumerationMemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.EnumerationMember.IsNull();
        In_Req_Ok = !MT1.EnumerationMember.IsChanged();
        MT1.EnumerationMember.Val = DotsTest.ParameterTypes.EnumerationParameter;
        Null_Ok = Null_Ok && !MT1.EnumerationMember.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.EnumerationMember.IsChanged();
        DotsTest.TestEnum.Enumeration[] enumVals = (DotsTest.TestEnum.Enumeration[])Enum.GetValues(typeof(DotsTest.TestEnum.Enumeration));

        Console.WriteLine("First: " + (int)(enumVals[0]));
        Console.WriteLine("Last: " + (int)(enumVals[enumVals.Length - 1]));
        Console.WriteLine("Size: " + (enumVals.Length));
        Console.WriteLine("Test ToString (0): " + DotsTest.TestEnum.Enumeration.MyFirst.ToString());
        Console.WriteLine("Test ToValue (MySecond): " + (int)Enum.Parse(typeof(DotsTest.TestEnum.Enumeration), "MySecond", false));

        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.EnumerationMemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.EnumerationMemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.EnumerationMemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.EnumerationMemberMemberIndex));
        Console.WriteLine("GetTypeId: " + Safir.Dob.Typesystem.Members.GetTypeId(DotsTest.MemberTypes.ClassTypeId,
                                                                                DotsTest.MemberTypes.EnumerationMemberMemberIndex));
        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "EnumerationParameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "EnumerationParameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "EnumerationParameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullEnumerationMember(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedEnumerationMember(MT2);
        DotsTest.MemberTypesProperty.SetEnumerationMember(MT2, MT1.EnumerationMember.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetEnumerationMember(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullEnumerationMember(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedEnumerationMember(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullEnumerationMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedEnumerationMember(MI);
        DotsTest.MemberTypesProperty.SetEnumerationMember(MI, MT2.EnumerationMember.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetEnumerationMember(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullEnumerationMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedEnumerationMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullEnumerationMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedEnumerationMember(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetEnumerationMember(MIA, MT2.EnumerationMember.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.EnumerationMember.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullEnumerationMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedEnumerationMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullEnumerationMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedEnumerationMember(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetEnumerationMember(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullEnumerationMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedEnumerationMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullEnumerationMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedEnumerationMember(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetEnumerationMember(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullEnumerationMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedEnumerationMember(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.EnumerationParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.EnumerationMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.EnumerationMember[ix].IsChanged();
            MA1.EnumerationMember[ix].Val = DotsTest.ParameterArrays.EnumerationParameter(ix);
            Null_Ok = Null_Ok && !MA1.EnumerationMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.EnumerationMember[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullEnumerationMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedEnumerationMember(MA2, ix);
            MA2.EnumerationMember[ix].Val = MA1.EnumerationMember[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullEnumerationMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedEnumerationMember(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetEnumerationMember(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.EnumerationMember[ix].Val = MA1.EnumerationMember[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetEnumerationMember(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullEnumerationMember(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedEnumerationMember(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetEnumerationMember(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullEnumerationMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedEnumerationMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullEnumerationMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedEnumerationMember(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetEnumerationMember(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullEnumerationMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedEnumerationMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullEnumerationMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedEnumerationMember(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetEnumerationMember(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullEnumerationMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedEnumerationMember(AEO, ix);
        }

        // SetNull test
        MT1.EnumerationMember.SetNull();
        DotsTest.MemberTypesProperty.SetNullEnumerationMember(MT2);
        MA1.EnumerationMember[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullEnumerationMember(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.EnumerationMember.IsNull() && DotsTest.MemberTypesProperty.IsNullEnumerationMember(MT2) &&
            MA1.EnumerationMember[1].IsNull() && DotsTest.MemberArraysProperty.IsNullEnumerationMember(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());


    } // Enumeration


    private static string CheckString(string str) { return CheckString(str,0);}

    private static string CheckString(string str, int index) {
        bool correct = false;
        if (index == 0){
            if (str == "Safir\u00AE") {
                correct = true;
            }
        }
        if (index == 1){
            if (str == "\u00AErifaS") {
                correct = true;
            }
        }
        if (correct) {
            return "<correct>";
        }
        else {
            return "<INCORRECT!>";
        }
    }



    private static void Test_String()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("String");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.StringMemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.StringMemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.StringParameterArraySize == 2 &&
                           DotsTest.MemberArrays.StringMemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.StringMemberArraySize(MA1) == 2).ToString().ToLower());
        Console.WriteLine("MaxStringLength Size Ok (10): " +
                          (DotsTest.MemberTypes.StringMemberMaxStringLength == 10 &&
                           DotsTest.MemberArrays.StringMemberMaxStringLength == 10).ToString().ToLower());
        Null_Ok = MT1.StringMember.IsNull();
        In_Req_Ok = !MT1.StringMember.IsChanged();
        MT1.StringMember.Val = DotsTest.ParameterTypes.StringParameter;
        Null_Ok = Null_Ok && !MT1.StringMember.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.StringMember.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.StringMemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.StringMemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.StringMemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.StringMemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "StringParameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "StringParameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "StringParameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullStringMember(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedStringMember(MT2);
        DotsTest.MemberTypesProperty.SetStringMember(MT2, MT1.StringMember.Val);

        Console.WriteLine("Val: " + CheckString(DotsTest.MemberTypesProperty.GetStringMember(MT2)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullStringMember(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedStringMember(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullStringMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedStringMember(MI);
        DotsTest.MemberTypesProperty.SetStringMember(MI, MT2.StringMember.Val);
        Console.WriteLine("Item Val: " + CheckString(DotsTest.MemberTypesProperty.GetStringMember(MI)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullStringMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedStringMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullStringMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedStringMember(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetStringMember(MIA, MT2.StringMember.Val);
        Console.WriteLine("Item Array Val: " + CheckString(MIA.TypesItemArray[1].Obj.StringMember.Val));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullStringMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedStringMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullStringMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedStringMember(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetStringMember(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullStringMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedStringMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullStringMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedStringMember(AEO);
        Console.WriteLine("Property Parameter Val: " + CheckString(DotsTest.MemberTypesProperty.GetStringMember(AEO)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullStringMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedStringMember(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.StringParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.StringMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.StringMember[ix].IsChanged();
            MA1.StringMember[ix].Val = DotsTest.ParameterArrays.StringParameter(ix);
            Null_Ok = Null_Ok && !MA1.StringMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.StringMember[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullStringMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedStringMember(MA2, ix);
            MA2.StringMember[ix].Val = MA1.StringMember[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullStringMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedStringMember(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + CheckString(DotsTest.MemberArraysProperty.GetStringMember(MA2, ix),ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.StringMember[ix].Val = MA1.StringMember[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + CheckString(DotsTest.MemberArraysProperty.GetStringMember(MI, ix),ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullStringMember(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedStringMember(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + CheckString(DotsTest.MemberArraysProperty.GetStringMember(MIA, ix),ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullStringMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedStringMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullStringMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedStringMember(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetStringMember(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullStringMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedStringMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullStringMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedStringMember(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + CheckString(DotsTest.MemberArraysProperty.GetStringMember(AEO, ix),ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullStringMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedStringMember(AEO, ix);
        }

        // SetNull test
        MT1.StringMember.SetNull();
        DotsTest.MemberTypesProperty.SetNullStringMember(MT2);
        MA1.StringMember[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullStringMember(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.StringMember.IsNull() && DotsTest.MemberTypesProperty.IsNullStringMember(MT2) &&
            MA1.StringMember[1].IsNull() && DotsTest.MemberArraysProperty.IsNullStringMember(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());

    } // String

    private static void Test_EntityId()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("EntityId");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.EntityIdMemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.EntityIdMemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.EntityIdParameterArraySize == 2 &&
                           DotsTest.MemberArrays.EntityIdMemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.EntityIdMemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.EntityIdMember.IsNull();
        In_Req_Ok = !MT1.EntityIdMember.IsChanged();
        MT1.EntityIdMember.Val = DotsTest.ParameterTypes.EntityIdParameter;
        Null_Ok = Null_Ok && !MT1.EntityIdMember.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.EntityIdMember.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.EntityIdMemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.EntityIdMemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.EntityIdMemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.EntityIdMemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "EntityIdParameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "EntityIdParameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "EntityIdParameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullEntityIdMember(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedEntityIdMember(MT2);
        DotsTest.MemberTypesProperty.SetEntityIdMember(MT2, MT1.EntityIdMember.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetEntityIdMember(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullEntityIdMember(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedEntityIdMember(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullEntityIdMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedEntityIdMember(MI);
        DotsTest.MemberTypesProperty.SetEntityIdMember(MI, MT2.EntityIdMember.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetEntityIdMember(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullEntityIdMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedEntityIdMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullEntityIdMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedEntityIdMember(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetEntityIdMember(MIA, MT2.EntityIdMember.Val);
        Console.WriteLine("Item Array Val:" + MIA.TypesItemArray[1].Obj.EntityIdMember.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullEntityIdMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedEntityIdMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullEntityIdMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedEntityIdMember(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetEntityIdMember(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullEntityIdMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedEntityIdMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullEntityIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedEntityIdMember(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetEntityIdMember(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullEntityIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedEntityIdMember(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.EntityIdParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.EntityIdMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.EntityIdMember[ix].IsChanged();
            MA1.EntityIdMember[ix].Val = DotsTest.ParameterArrays.EntityIdParameter(ix);
            Null_Ok = Null_Ok && !MA1.EntityIdMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.EntityIdMember[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullEntityIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedEntityIdMember(MA2, ix);
            MA2.EntityIdMember[ix].Val = MA1.EntityIdMember[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullEntityIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedEntityIdMember(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetEntityIdMember(MA2, ix));

            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.EntityIdMember[ix].Val = MA1.EntityIdMember[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetEntityIdMember(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullEntityIdMember(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedEntityIdMember(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetEntityIdMember(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullEntityIdMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedEntityIdMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullEntityIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedEntityIdMember(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetEntityIdMember(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullEntityIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedEntityIdMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullEntityIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedEntityIdMember(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetEntityIdMember(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullEntityIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedEntityIdMember(AEO, ix);
        }

        // SetNull test
        MT1.EntityIdMember.SetNull();
        DotsTest.MemberTypesProperty.SetNullEntityIdMember(MT2);
        MA1.EntityIdMember[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullEntityIdMember(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.EntityIdMember.IsNull() && DotsTest.MemberTypesProperty.IsNullEntityIdMember(MT2) &&
            MA1.EntityIdMember[1].IsNull() && DotsTest.MemberArraysProperty.IsNullEntityIdMember(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());

    } // EntityId

    private static void Test_TypeId()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("TypeId");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.TypeIdMemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.TypeIdMemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.TypeIdParameterArraySize == 2 &&
                           DotsTest.MemberArrays.TypeIdMemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.TypeIdMemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.TypeIdMember.IsNull();
        In_Req_Ok = !MT1.TypeIdMember.IsChanged();
        MT1.TypeIdMember.Val = DotsTest.ParameterTypes.TypeIdParameter;
        Null_Ok = Null_Ok && !MT1.TypeIdMember.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.TypeIdMember.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.TypeIdMemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.TypeIdMemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.TypeIdMemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.TypeIdMemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "TypeIdParameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "TypeIdParameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "TypeIdParameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullTypeIdMember(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedTypeIdMember(MT2);
        DotsTest.MemberTypesProperty.SetTypeIdMember(MT2, MT1.TypeIdMember.Val);
        Console.WriteLine("Val: " + Safir.Dob.Typesystem.Operations.GetName(
                                                                            DotsTest.MemberTypesProperty.GetTypeIdMember(MT2)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullTypeIdMember(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedTypeIdMember(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullTypeIdMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedTypeIdMember(MI);
        DotsTest.MemberTypesProperty.SetTypeIdMember(MI, MT2.TypeIdMember.Val);
        Console.WriteLine("Item Val: " + Safir.Dob.Typesystem.Operations.GetName(DotsTest.MemberTypesProperty.GetTypeIdMember(MI)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullTypeIdMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedTypeIdMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullTypeIdMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedTypeIdMember(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetTypeIdMember(MIA, MT2.TypeIdMember.Val);
        Console.WriteLine("Item Array Val: " + Safir.Dob.Typesystem.Operations.GetName(MIA.TypesItemArray[1].Obj.TypeIdMember.Val));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullTypeIdMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedTypeIdMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullTypeIdMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedTypeIdMember(EO);
        Console.WriteLine("Property Parameter Val: " + Safir.Dob.Typesystem.Operations.GetName(
                                                                                               DotsTest.MemberTypesProperty.GetTypeIdMember(EO)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullTypeIdMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedTypeIdMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullTypeIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedTypeIdMember(AEO);
        Console.WriteLine("Property Parameter Val: " + Safir.Dob.Typesystem.Operations.GetName(
                                                                                               DotsTest.MemberTypesProperty.GetTypeIdMember(AEO)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullTypeIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedTypeIdMember(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.TypeIdParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.TypeIdMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.TypeIdMember[ix].IsChanged();
            MA1.TypeIdMember[ix].Val = DotsTest.ParameterArrays.TypeIdParameter(ix);
            Null_Ok = Null_Ok && !MA1.TypeIdMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.TypeIdMember[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullTypeIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedTypeIdMember(MA2, ix);
            MA2.TypeIdMember[ix].Val = MA1.TypeIdMember[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullTypeIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedTypeIdMember(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + Safir.Dob.Typesystem.Operations.GetName(
                                                                                           DotsTest.MemberArraysProperty.GetTypeIdMember(MA2, ix)));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.TypeIdMember[ix].Val = MA1.TypeIdMember[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + Safir.Dob.Typesystem.Operations.GetName(DotsTest.MemberArraysProperty.GetTypeIdMember(MI, ix)));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullTypeIdMember(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedTypeIdMember(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + Safir.Dob.Typesystem.Operations.GetName(DotsTest.MemberArraysProperty.GetTypeIdMember(MIA, ix)));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullTypeIdMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedTypeIdMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullTypeIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedTypeIdMember(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + Safir.Dob.Typesystem.Operations.GetName(
                                                                                                           DotsTest.MemberArraysProperty.GetTypeIdMember(EO, ix)));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullTypeIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedTypeIdMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullTypeIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedTypeIdMember(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + Safir.Dob.Typesystem.Operations.GetName(
                                                                                                           DotsTest.MemberArraysProperty.GetTypeIdMember(AEO, ix)));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullTypeIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedTypeIdMember(AEO, ix);
        }

        // SetNull test
        MT1.TypeIdMember.SetNull();
        DotsTest.MemberTypesProperty.SetNullTypeIdMember(MT2);
        MA1.TypeIdMember[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullTypeIdMember(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.TypeIdMember.IsNull() && DotsTest.MemberTypesProperty.IsNullTypeIdMember(MT2) &&
            MA1.TypeIdMember[1].IsNull() && DotsTest.MemberArraysProperty.IsNullTypeIdMember(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());


    } // TypeId

    private static void Test_InstanceId()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("InstanceId");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.InstanceIdMemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.InstanceIdMemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.InstanceIdParameterArraySize == 2 &&
                           DotsTest.MemberArrays.InstanceIdMemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.InstanceIdMemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.InstanceIdMember.IsNull();
        In_Req_Ok = !MT1.InstanceIdMember.IsChanged();
        MT1.InstanceIdMember.Val = DotsTest.ParameterTypes.InstanceIdParameter;
        Null_Ok = Null_Ok && !MT1.InstanceIdMember.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.InstanceIdMember.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.InstanceIdMemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.InstanceIdMemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.InstanceIdMemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.InstanceIdMemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "InstanceIdParameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "InstanceIdParameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "InstanceIdParameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullInstanceIdMember(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedInstanceIdMember(MT2);
        DotsTest.MemberTypesProperty.SetInstanceIdMember(MT2, MT1.InstanceIdMember.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetInstanceIdMember(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInstanceIdMember(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedInstanceIdMember(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullInstanceIdMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedInstanceIdMember(MI);
        DotsTest.MemberTypesProperty.SetInstanceIdMember(MI, MT2.InstanceIdMember.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetInstanceIdMember(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInstanceIdMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedInstanceIdMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullInstanceIdMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedInstanceIdMember(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetInstanceIdMember(MIA, MT2.InstanceIdMember.Val);
        Console.WriteLine("Item Array Val:" + MIA.TypesItemArray[1].Obj.InstanceIdMember.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInstanceIdMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedInstanceIdMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInstanceIdMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedInstanceIdMember(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetInstanceIdMember(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInstanceIdMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedInstanceIdMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInstanceIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedInstanceIdMember(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetInstanceIdMember(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullInstanceIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedInstanceIdMember(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.InstanceIdParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.InstanceIdMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.InstanceIdMember[ix].IsChanged();
            MA1.InstanceIdMember[ix].Val = DotsTest.ParameterArrays.InstanceIdParameter(ix);
            Null_Ok = Null_Ok && !MA1.InstanceIdMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.InstanceIdMember[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullInstanceIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedInstanceIdMember(MA2, ix);
            MA2.InstanceIdMember[ix].Val = MA1.InstanceIdMember[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInstanceIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedInstanceIdMember(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetInstanceIdMember(MA2, ix));

            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.InstanceIdMember[ix].Val = MA1.InstanceIdMember[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetInstanceIdMember(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInstanceIdMember(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedInstanceIdMember(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetInstanceIdMember(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInstanceIdMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedInstanceIdMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInstanceIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedInstanceIdMember(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetInstanceIdMember(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInstanceIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedInstanceIdMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInstanceIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedInstanceIdMember(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetInstanceIdMember(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullInstanceIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedInstanceIdMember(AEO, ix);
        }

        // SetNull test
        MT1.InstanceIdMember.SetNull();
        DotsTest.MemberTypesProperty.SetNullInstanceIdMember(MT2);
        MA1.InstanceIdMember[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullInstanceIdMember(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.InstanceIdMember.IsNull() && DotsTest.MemberTypesProperty.IsNullInstanceIdMember(MT2) &&
            MA1.InstanceIdMember[1].IsNull() && DotsTest.MemberArraysProperty.IsNullInstanceIdMember(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());

    } // InstanceId


    private static void Test_ChannelId()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("ChannelId");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.ChannelIdMemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.ChannelIdMemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.ChannelIdParameterArraySize == 2 &&
                           DotsTest.MemberArrays.ChannelIdMemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.ChannelIdMemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.ChannelIdMember.IsNull();
        In_Req_Ok = !MT1.ChannelIdMember.IsChanged();
        MT1.ChannelIdMember.Val = DotsTest.ParameterTypes.ChannelIdParameter;
        Null_Ok = Null_Ok && !MT1.ChannelIdMember.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.ChannelIdMember.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.ChannelIdMemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.ChannelIdMemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.ChannelIdMemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.ChannelIdMemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "ChannelIdParameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "ChannelIdParameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "ChannelIdParameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullChannelIdMember(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedChannelIdMember(MT2);
        DotsTest.MemberTypesProperty.SetChannelIdMember(MT2, MT1.ChannelIdMember.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetChannelIdMember(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullChannelIdMember(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedChannelIdMember(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullChannelIdMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedChannelIdMember(MI);
        DotsTest.MemberTypesProperty.SetChannelIdMember(MI, MT2.ChannelIdMember.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetChannelIdMember(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullChannelIdMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedChannelIdMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullChannelIdMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedChannelIdMember(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetChannelIdMember(MIA, MT2.ChannelIdMember.Val);
        Console.WriteLine("Item Array Val:" + MIA.TypesItemArray[1].Obj.ChannelIdMember.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullChannelIdMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedChannelIdMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullChannelIdMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedChannelIdMember(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetChannelIdMember(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullChannelIdMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedChannelIdMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullChannelIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedChannelIdMember(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetChannelIdMember(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullChannelIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedChannelIdMember(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.ChannelIdParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.ChannelIdMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.ChannelIdMember[ix].IsChanged();
            MA1.ChannelIdMember[ix].Val = DotsTest.ParameterArrays.ChannelIdParameter(ix);
            Null_Ok = Null_Ok && !MA1.ChannelIdMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.ChannelIdMember[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullChannelIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedChannelIdMember(MA2, ix);
            MA2.ChannelIdMember[ix].Val = MA1.ChannelIdMember[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullChannelIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedChannelIdMember(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetChannelIdMember(MA2, ix));

            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.ChannelIdMember[ix].Val = MA1.ChannelIdMember[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetChannelIdMember(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullChannelIdMember(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedChannelIdMember(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetChannelIdMember(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullChannelIdMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedChannelIdMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullChannelIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedChannelIdMember(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetChannelIdMember(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullChannelIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedChannelIdMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullChannelIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedChannelIdMember(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetChannelIdMember(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullChannelIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedChannelIdMember(AEO, ix);
        }

        // SetNull test
        MT1.ChannelIdMember.SetNull();
        DotsTest.MemberTypesProperty.SetNullChannelIdMember(MT2);
        MA1.ChannelIdMember[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullChannelIdMember(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.ChannelIdMember.IsNull() && DotsTest.MemberTypesProperty.IsNullChannelIdMember(MT2) &&
            MA1.ChannelIdMember[1].IsNull() && DotsTest.MemberArraysProperty.IsNullChannelIdMember(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());

    } // ChannelId



    private static void Test_HandlerId()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("HandlerId");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.HandlerIdMemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.HandlerIdMemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.HandlerIdParameterArraySize == 2 &&
                           DotsTest.MemberArrays.HandlerIdMemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.HandlerIdMemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.HandlerIdMember.IsNull();
        In_Req_Ok = !MT1.HandlerIdMember.IsChanged();
        MT1.HandlerIdMember.Val = DotsTest.ParameterTypes.HandlerIdParameter;
        Null_Ok = Null_Ok && !MT1.HandlerIdMember.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.HandlerIdMember.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.HandlerIdMemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.HandlerIdMemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.HandlerIdMemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.HandlerIdMemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "HandlerIdParameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "HandlerIdParameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "HandlerIdParameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullHandlerIdMember(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedHandlerIdMember(MT2);
        DotsTest.MemberTypesProperty.SetHandlerIdMember(MT2, MT1.HandlerIdMember.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetHandlerIdMember(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHandlerIdMember(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedHandlerIdMember(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullHandlerIdMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedHandlerIdMember(MI);
        DotsTest.MemberTypesProperty.SetHandlerIdMember(MI, MT2.HandlerIdMember.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetHandlerIdMember(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHandlerIdMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedHandlerIdMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullHandlerIdMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedHandlerIdMember(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetHandlerIdMember(MIA, MT2.HandlerIdMember.Val);
        Console.WriteLine("Item Array Val:" + MIA.TypesItemArray[1].Obj.HandlerIdMember.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHandlerIdMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedHandlerIdMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHandlerIdMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedHandlerIdMember(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetHandlerIdMember(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHandlerIdMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedHandlerIdMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHandlerIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedHandlerIdMember(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetHandlerIdMember(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHandlerIdMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedHandlerIdMember(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.HandlerIdParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.HandlerIdMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.HandlerIdMember[ix].IsChanged();
            MA1.HandlerIdMember[ix].Val = DotsTest.ParameterArrays.HandlerIdParameter(ix);
            Null_Ok = Null_Ok && !MA1.HandlerIdMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.HandlerIdMember[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullHandlerIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedHandlerIdMember(MA2, ix);
            MA2.HandlerIdMember[ix].Val = MA1.HandlerIdMember[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHandlerIdMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedHandlerIdMember(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetHandlerIdMember(MA2, ix));

            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.HandlerIdMember[ix].Val = MA1.HandlerIdMember[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetHandlerIdMember(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHandlerIdMember(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedHandlerIdMember(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetHandlerIdMember(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHandlerIdMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedHandlerIdMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHandlerIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedHandlerIdMember(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetHandlerIdMember(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHandlerIdMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedHandlerIdMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHandlerIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedHandlerIdMember(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetHandlerIdMember(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHandlerIdMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedHandlerIdMember(AEO, ix);
        }

        // SetNull test
        MT1.HandlerIdMember.SetNull();
        DotsTest.MemberTypesProperty.SetNullHandlerIdMember(MT2);
        MA1.HandlerIdMember[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullHandlerIdMember(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.HandlerIdMember.IsNull() && DotsTest.MemberTypesProperty.IsNullHandlerIdMember(MT2) &&
            MA1.HandlerIdMember[1].IsNull() && DotsTest.MemberArraysProperty.IsNullHandlerIdMember(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());

    } // HandlerId


    private static void Test_Object()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Object");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.ObjectMemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.ObjectMemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.ObjectParameterArraySize == 2 &&
                           DotsTest.MemberArrays.ObjectMemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.ObjectMemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.ObjectMember.IsNull();
        In_Req_Ok = !MT1.ObjectMember.IsChanged();
        MT1.ObjectMember.Obj = DotsTest.ParameterTypes.ObjectParameter;
        Null_Ok = Null_Ok && !MT1.ObjectMember.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.ObjectMember.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.ObjectMemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.ObjectMemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.ObjectMemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.ObjectMemberMemberIndex));
        Console.WriteLine("GetTypeId: " + Safir.Dob.Typesystem.Members.GetTypeId(DotsTest.MemberTypes.ClassTypeId,
                                                                                DotsTest.MemberTypes.ObjectMemberMemberIndex));
        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "ObjectParameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "ObjectParameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "ObjectParameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullObjectMember(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedObjectMember(MT2);
        DotsTest.MemberTypesProperty.SetObjectMember(MT2, MT1.ObjectMember.Obj);
        Console.WriteLine("Val: " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberTypesProperty.GetObjectMember(MT2)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullObjectMember(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedObjectMember(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullObjectMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedObjectMember(MI);
        DotsTest.MemberTypesProperty.SetObjectMember(MI, MT2.ObjectMember.Obj);
        Console.WriteLine("Item Val: " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberTypesProperty.GetObjectMember(MI)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullObjectMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedObjectMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullObjectMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedObjectMember(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetObjectMember(MIA, MT2.ObjectMember.Obj);
        Console.WriteLine("Item Array Val: " + Safir.Dob.Typesystem.Serialization.ToXml(MIA.TypesItemArray[1].Obj.ObjectMember.Obj));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullObjectMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedObjectMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullObjectMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedObjectMember(EO);
        Console.WriteLine("Property Parameter Val: " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberTypesProperty.GetObjectMember(EO)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullObjectMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedObjectMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullObjectMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedObjectMember(AEO);
        Console.WriteLine("Property Parameter Val: " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberTypesProperty.GetObjectMember(AEO)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullObjectMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedObjectMember(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.ObjectParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.ObjectMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.ObjectMember[ix].IsChanged();
            MA1.ObjectMember[ix].Obj = DotsTest.ParameterArrays.ObjectParameter(ix);
            Null_Ok = Null_Ok && !MA1.ObjectMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.ObjectMember[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullObjectMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedObjectMember(MA2, ix);
            MA2.ObjectMember[ix].Obj = MA1.ObjectMember[ix].Obj;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullObjectMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedObjectMember(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + Safir.Dob.Typesystem.Serialization.ToXml(
                                                                                            DotsTest.MemberArraysProperty.GetObjectMember(MA2, ix)));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.ObjectMember[ix] = MA1.ObjectMember[ix];
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberArraysProperty.GetObjectMember(MI, ix)));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullObjectMember(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedObjectMember(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberArraysProperty.GetObjectMember(MIA, ix)));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullObjectMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedObjectMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullObjectMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedObjectMember(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberArraysProperty.GetObjectMember(EO, ix)));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullObjectMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedObjectMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullObjectMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedObjectMember(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberArraysProperty.GetObjectMember(AEO, ix)));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullObjectMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedObjectMember(AEO, ix);
        }

        // SetNull test
        MT1.ObjectMember.SetNull();
        DotsTest.MemberTypesProperty.SetNullObjectMember(MT2);
        MA1.ObjectMember[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullObjectMember(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.ObjectMember.IsNull() && DotsTest.MemberTypesProperty.IsNullObjectMember(MT2) &&
            MA1.ObjectMember[1].IsNull() && DotsTest.MemberArraysProperty.IsNullObjectMember(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());


    } // Object

    private static void Test_Binary()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Binary");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.BinaryMemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.BinaryMemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.BinaryParameterArraySize == 2 &&
                           DotsTest.MemberArrays.BinaryMemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.BinaryMemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.BinaryMember.IsNull();
        In_Req_Ok = !MT1.BinaryMember.IsChanged();
        MT1.BinaryMember.Val = DotsTest.ParameterTypes.BinaryParameter;
        Null_Ok = Null_Ok && !MT1.BinaryMember.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.BinaryMember.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.BinaryMemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.BinaryMemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.BinaryMemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.BinaryMemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "BinaryParameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "BinaryParameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "BinaryParameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullBinaryMember(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedBinaryMember(MT2);
        DotsTest.MemberTypesProperty.SetBinaryMember(MT2, MT1.BinaryMember.Val);
        Console.WriteLine("Val: " + Convert.ToBase64String(DotsTest.MemberTypesProperty.GetBinaryMember(MT2)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullBinaryMember(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedBinaryMember(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullBinaryMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedBinaryMember(MI);
        DotsTest.MemberTypesProperty.SetBinaryMember(MI, MT2.BinaryMember.Val);
        Console.WriteLine("Item Val: " + Convert.ToBase64String(DotsTest.MemberTypesProperty.GetBinaryMember(MI)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullBinaryMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedBinaryMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullBinaryMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedBinaryMember(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetBinaryMember(MIA, MT2.BinaryMember.Val);
        Console.WriteLine("Item Array Val: " + Convert.ToBase64String(MIA.TypesItemArray[1].Obj.BinaryMember.Val));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullBinaryMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedBinaryMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullBinaryMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedBinaryMember(EO);
        Console.WriteLine("Property Parameter Val: " + Convert.ToBase64String(DotsTest.MemberTypesProperty.GetBinaryMember(EO)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullBinaryMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedBinaryMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullBinaryMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedBinaryMember(AEO);
        Console.WriteLine("Property Parameter Val: " + Convert.ToBase64String(DotsTest.MemberTypesProperty.GetBinaryMember(AEO)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullBinaryMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedBinaryMember(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.BinaryParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.BinaryMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.BinaryMember[ix].IsChanged();
            MA1.BinaryMember[ix].Val = DotsTest.ParameterArrays.BinaryParameter(ix);
            Null_Ok = Null_Ok && !MA1.BinaryMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.BinaryMember[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullBinaryMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedBinaryMember(MA2, ix);
            MA2.BinaryMember[ix].Val = MA1.BinaryMember[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullBinaryMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedBinaryMember(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + Convert.ToBase64String(DotsTest.MemberArraysProperty.GetBinaryMember(MA2, ix)));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.BinaryMember[ix].Val = MA1.BinaryMember[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + Convert.ToBase64String(DotsTest.MemberArraysProperty.GetBinaryMember(MI, ix)));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullBinaryMember(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedBinaryMember(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + Convert.ToBase64String(DotsTest.MemberArraysProperty.GetBinaryMember(MIA, ix)));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullBinaryMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedBinaryMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullBinaryMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedBinaryMember(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + Convert.ToBase64String(DotsTest.MemberArraysProperty.GetBinaryMember(EO, ix)));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullBinaryMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedBinaryMember(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullBinaryMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedBinaryMember(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + Convert.ToBase64String(DotsTest.MemberArraysProperty.GetBinaryMember(AEO, ix)));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullBinaryMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedBinaryMember(AEO, ix);
        }

        // SetNull test
        MT1.BinaryMember.SetNull();
        DotsTest.MemberTypesProperty.SetNullBinaryMember(MT2);
        MA1.BinaryMember[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullBinaryMember(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.BinaryMember.IsNull() && DotsTest.MemberTypesProperty.IsNullBinaryMember(MT2) &&
            MA1.BinaryMember[1].IsNull() && DotsTest.MemberArraysProperty.IsNullBinaryMember(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());

    } // Binary

    private static void Test_TestClass()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("TestClass");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.TestClassMemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.TestClassMemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.TestClassParameterArraySize == 2 &&
                           DotsTest.MemberArrays.TestClassMemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.TestClassMemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.TestClassMember.IsNull();
        In_Req_Ok = !MT1.TestClassMember.IsChanged();
        MT1.TestClassMember.Obj = DotsTest.ParameterTypes.TestClassParameter;
        Null_Ok = Null_Ok && !MT1.TestClassMember.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.TestClassMember.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.TestClassMemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.TestClassMemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.TestClassMemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.TestClassMemberMemberIndex));
        Console.WriteLine("GetTypeId: " + Safir.Dob.Typesystem.Members.GetTypeId(DotsTest.MemberTypes.ClassTypeId,
                                                                                DotsTest.MemberTypes.TestClassMemberMemberIndex));
        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "TestClassParameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "TestClassParameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "TestClassParameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullTestClassMember(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedTestClassMember(MT2);
        DotsTest.MemberTypesProperty.SetTestClassMember(MT2, MT1.TestClassMember.Obj);
        Console.WriteLine("Val: " + Safir.Dob.Typesystem.Serialization.ToXml(
                                                                             DotsTest.MemberTypesProperty.GetTestClassMember(MT2)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullTestClassMember(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedTestClassMember(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullTestClassMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedTestClassMember(MI);
        DotsTest.MemberTypesProperty.SetTestClassMember(MI, MT2.TestClassMember.Obj);
        Console.WriteLine("Item Val: " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberTypesProperty.GetTestClassMember(MI)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullTestClassMember(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedTestClassMember(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullTestClassMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedTestClassMember(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetTestClassMember(MIA, MT2.TestClassMember.Obj);
        Console.WriteLine("Item Array Val: " + Safir.Dob.Typesystem.Serialization.ToXml(MIA.TypesItemArray[1].Obj.TestClassMember.Obj));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullTestClassMember(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedTestClassMember(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullTestClassMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedTestClassMember(EO);
        Console.WriteLine("Property Parameter Val: " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberTypesProperty.GetTestClassMember(EO)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullTestClassMember(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedTestClassMember(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullTestClassMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedTestClassMember(AEO);
        Console.WriteLine("Property Parameter Val: " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberTypesProperty.GetTestClassMember(AEO)));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullTestClassMember(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedTestClassMember(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.TestClassParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.TestClassMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.TestClassMember[ix].IsChanged();
            MA1.TestClassMember[ix].Obj = DotsTest.ParameterArrays.TestClassParameter(ix);
            Null_Ok = Null_Ok && !MA1.TestClassMember[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.TestClassMember[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullTestClassMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedTestClassMember(MA2, ix);
            MA2.TestClassMember[ix].Obj = MA1.TestClassMember[ix].Obj;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullTestClassMember(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedTestClassMember(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + Safir.Dob.Typesystem.Serialization.ToXml(
                                                                                            DotsTest.MemberArraysProperty.GetTestClassMember(MA2, ix)));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.TestClassMember[ix] = MA1.TestClassMember[ix];
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberArraysProperty.GetTestClassMember(MI, ix)));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullTestClassMember(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedTestClassMember(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberArraysProperty.GetTestClassMember(MIA, ix)));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullTestClassMember(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedTestClassMember(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullTestClassMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedTestClassMember(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberArraysProperty.GetTestClassMember(EO, ix)));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullTestClassMember(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedTestClassMember(EO, ix);

            // AnothreEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullTestClassMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedTestClassMember(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + Safir.Dob.Typesystem.Serialization.ToXml(DotsTest.MemberArraysProperty.GetTestClassMember(AEO, ix)));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullTestClassMember(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedTestClassMember(AEO, ix);
        }

        // SetNull test
        MT1.TestClassMember.SetNull();
        DotsTest.MemberTypesProperty.SetNullTestClassMember(MT2);
        MA1.TestClassMember[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullTestClassMember(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.TestClassMember.IsNull() && DotsTest.MemberTypesProperty.IsNullTestClassMember(MT2) &&
            MA1.TestClassMember[1].IsNull() && DotsTest.MemberArraysProperty.IsNullTestClassMember(MA2, 1);

        // Make some tests concerning Set/GetChangedHere
        MT1.TestClassMember.Obj = DotsTest.ParameterTypes.TestClassParameter;
        MT1.TestClassMember.SetChanged(false);
        MT1.TestClassMember.Obj.MyInt.Val = 3;
        In_Req_Ok = In_Req_Ok && MT1.TestClassMember.IsChanged();
        In_Req_Ok = In_Req_Ok && !MT1.TestClassMember.IsChangedHere();
        MT1.TestClassMember.SetChanged(false);
        MT1.TestClassMember.SetChangedHere(true);
        In_Req_Ok = In_Req_Ok && MT1.TestClassMember.IsChangedHere();
        In_Req_Ok = In_Req_Ok && !MT1.TestClassMember.Obj.MyInt.IsChanged();

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // TestClass

    private static void Test_Ampere32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Ampere32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Ampere32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Ampere32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Ampere32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Ampere32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Ampere32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Ampere32Member.IsNull();
        In_Req_Ok = !MT1.Ampere32Member.IsChanged();
        MT1.Ampere32Member.Val = DotsTest.ParameterTypes.Ampere32Parameter;
        Null_Ok = Null_Ok && !MT1.Ampere32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Ampere32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Ampere32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Ampere32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Ampere32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Ampere32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Ampere32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Ampere32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Ampere32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullAmpere32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedAmpere32Member(MT2);
        DotsTest.MemberTypesProperty.SetAmpere32Member(MT2, MT1.Ampere32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetAmpere32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullAmpere32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedAmpere32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullAmpere32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedAmpere32Member(MI);
        DotsTest.MemberTypesProperty.SetAmpere32Member(MI, MT2.Ampere32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetAmpere32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullAmpere32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedAmpere32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullAmpere32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedAmpere32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetAmpere32Member(MIA, MT2.Ampere32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Ampere32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullAmpere32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedAmpere32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullAmpere32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedAmpere32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetAmpere32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullAmpere32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedAmpere32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullAmpere32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedAmpere32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetAmpere32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullAmpere32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedAmpere32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Ampere32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Ampere32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Ampere32Member[ix].IsChanged();
            MA1.Ampere32Member[ix].Val = DotsTest.ParameterArrays.Ampere32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Ampere32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Ampere32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullAmpere32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedAmpere32Member(MA2, ix);
            MA2.Ampere32Member[ix].Val = MA1.Ampere32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullAmpere32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedAmpere32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetAmpere32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Ampere32Member[ix].Val = MA1.Ampere32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetAmpere32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullAmpere32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedAmpere32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetAmpere32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullAmpere32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedAmpere32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullAmpere32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedAmpere32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetAmpere32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullAmpere32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedAmpere32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullAmpere32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedAmpere32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetAmpere32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullAmpere32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedAmpere32Member(AEO, ix);
        }

        // SetNull test
        MT1.Ampere32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullAmpere32Member(MT2);
        MA1.Ampere32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullAmpere32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Ampere32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullAmpere32Member(MT2) &&
            MA1.Ampere32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullAmpere32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Ampere32

    private static void Test_CubicMeter32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("CubicMeter32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.CubicMeter32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.CubicMeter32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.CubicMeter32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.CubicMeter32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.CubicMeter32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.CubicMeter32Member.IsNull();
        In_Req_Ok = !MT1.CubicMeter32Member.IsChanged();
        MT1.CubicMeter32Member.Val = DotsTest.ParameterTypes.CubicMeter32Parameter;
        Null_Ok = Null_Ok && !MT1.CubicMeter32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.CubicMeter32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.CubicMeter32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.CubicMeter32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.CubicMeter32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.CubicMeter32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "CubicMeter32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "CubicMeter32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "CubicMeter32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullCubicMeter32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedCubicMeter32Member(MT2);
        DotsTest.MemberTypesProperty.SetCubicMeter32Member(MT2, MT1.CubicMeter32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetCubicMeter32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullCubicMeter32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedCubicMeter32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullCubicMeter32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedCubicMeter32Member(MI);
        DotsTest.MemberTypesProperty.SetCubicMeter32Member(MI, MT2.CubicMeter32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetCubicMeter32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullCubicMeter32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedCubicMeter32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullCubicMeter32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedCubicMeter32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetCubicMeter32Member(MIA, MT2.CubicMeter32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.CubicMeter32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullCubicMeter32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedCubicMeter32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullCubicMeter32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedCubicMeter32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetCubicMeter32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullCubicMeter32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedCubicMeter32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullCubicMeter32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedCubicMeter32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetCubicMeter32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullCubicMeter32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedCubicMeter32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.CubicMeter32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.CubicMeter32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.CubicMeter32Member[ix].IsChanged();
            MA1.CubicMeter32Member[ix].Val = DotsTest.ParameterArrays.CubicMeter32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.CubicMeter32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.CubicMeter32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullCubicMeter32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedCubicMeter32Member(MA2, ix);
            MA2.CubicMeter32Member[ix].Val = MA1.CubicMeter32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullCubicMeter32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedCubicMeter32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetCubicMeter32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.CubicMeter32Member[ix].Val = MA1.CubicMeter32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetCubicMeter32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullCubicMeter32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedCubicMeter32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetCubicMeter32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullCubicMeter32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedCubicMeter32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullCubicMeter32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedCubicMeter32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetCubicMeter32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullCubicMeter32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedCubicMeter32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullCubicMeter32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedCubicMeter32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetCubicMeter32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullCubicMeter32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedCubicMeter32Member(AEO, ix);
        }

        // SetNull test
        MT1.CubicMeter32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullCubicMeter32Member(MT2);
        MA1.CubicMeter32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullCubicMeter32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.CubicMeter32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullCubicMeter32Member(MT2) &&
            MA1.CubicMeter32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullCubicMeter32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // CubicMeter32

    private static void Test_Hertz32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Hertz32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Hertz32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Hertz32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Hertz32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Hertz32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Hertz32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Hertz32Member.IsNull();
        In_Req_Ok = !MT1.Hertz32Member.IsChanged();
        MT1.Hertz32Member.Val = DotsTest.ParameterTypes.Hertz32Parameter;
        Null_Ok = Null_Ok && !MT1.Hertz32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Hertz32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Hertz32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Hertz32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Hertz32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Hertz32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Hertz32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Hertz32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Hertz32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullHertz32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedHertz32Member(MT2);
        DotsTest.MemberTypesProperty.SetHertz32Member(MT2, MT1.Hertz32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetHertz32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHertz32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedHertz32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullHertz32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedHertz32Member(MI);
        DotsTest.MemberTypesProperty.SetHertz32Member(MI, MT2.Hertz32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetHertz32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHertz32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedHertz32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullHertz32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedHertz32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetHertz32Member(MIA, MT2.Hertz32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Hertz32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHertz32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedHertz32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHertz32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedHertz32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetHertz32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHertz32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedHertz32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHertz32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedHertz32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetHertz32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHertz32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedHertz32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Hertz32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Hertz32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Hertz32Member[ix].IsChanged();
            MA1.Hertz32Member[ix].Val = DotsTest.ParameterArrays.Hertz32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Hertz32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Hertz32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullHertz32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedHertz32Member(MA2, ix);
            MA2.Hertz32Member[ix].Val = MA1.Hertz32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHertz32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedHertz32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetHertz32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Hertz32Member[ix].Val = MA1.Hertz32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetHertz32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHertz32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedHertz32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetHertz32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHertz32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedHertz32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHertz32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedHertz32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetHertz32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHertz32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedHertz32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHertz32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedHertz32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetHertz32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHertz32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedHertz32Member(AEO, ix);
        }

        // SetNull test
        MT1.Hertz32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullHertz32Member(MT2);
        MA1.Hertz32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullHertz32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Hertz32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullHertz32Member(MT2) &&
            MA1.Hertz32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullHertz32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());


    } // Hertz32

    private static void Test_Joule32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Joule32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Joule32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Joule32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Joule32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Joule32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Joule32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Joule32Member.IsNull();
        In_Req_Ok = !MT1.Joule32Member.IsChanged();
        MT1.Joule32Member.Val = DotsTest.ParameterTypes.Joule32Parameter;
        Null_Ok = Null_Ok && !MT1.Joule32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Joule32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Joule32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Joule32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Joule32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Joule32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Joule32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Joule32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Joule32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullJoule32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedJoule32Member(MT2);
        DotsTest.MemberTypesProperty.SetJoule32Member(MT2, MT1.Joule32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetJoule32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullJoule32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedJoule32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullJoule32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedJoule32Member(MI);
        DotsTest.MemberTypesProperty.SetJoule32Member(MI, MT2.Joule32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetJoule32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullJoule32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedJoule32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullJoule32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedJoule32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetJoule32Member(MIA, MT2.Joule32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Joule32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullJoule32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedJoule32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullJoule32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedJoule32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetJoule32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullJoule32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedJoule32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullJoule32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedJoule32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetJoule32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullJoule32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedJoule32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Joule32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Joule32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Joule32Member[ix].IsChanged();
            MA1.Joule32Member[ix].Val = DotsTest.ParameterArrays.Joule32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Joule32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Joule32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullJoule32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedJoule32Member(MA2, ix);
            MA2.Joule32Member[ix].Val = MA1.Joule32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullJoule32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedJoule32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetJoule32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Joule32Member[ix].Val = MA1.Joule32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetJoule32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullJoule32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedJoule32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetJoule32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullJoule32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedJoule32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullJoule32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedJoule32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetJoule32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullJoule32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedJoule32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullJoule32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedJoule32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetJoule32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullJoule32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedJoule32Member(AEO, ix);
        }

        // SetNull test
        MT1.Joule32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullJoule32Member(MT2);
        MA1.Joule32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullJoule32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Joule32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullJoule32Member(MT2) &&
            MA1.Joule32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullJoule32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());


    } // Joule32

    private static void Test_Kelvin32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Kelvin32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Kelvin32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Kelvin32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Kelvin32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Kelvin32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Kelvin32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Kelvin32Member.IsNull();
        In_Req_Ok = !MT1.Kelvin32Member.IsChanged();
        MT1.Kelvin32Member.Val = DotsTest.ParameterTypes.Kelvin32Parameter;
        Null_Ok = Null_Ok && !MT1.Kelvin32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Kelvin32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Kelvin32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Kelvin32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Kelvin32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Kelvin32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Kelvin32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Kelvin32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Kelvin32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullKelvin32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKelvin32Member(MT2);
        DotsTest.MemberTypesProperty.SetKelvin32Member(MT2, MT1.Kelvin32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetKelvin32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKelvin32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKelvin32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullKelvin32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKelvin32Member(MI);
        DotsTest.MemberTypesProperty.SetKelvin32Member(MI, MT2.Kelvin32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetKelvin32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKelvin32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKelvin32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullKelvin32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKelvin32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetKelvin32Member(MIA, MT2.Kelvin32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Kelvin32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKelvin32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKelvin32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKelvin32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKelvin32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetKelvin32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKelvin32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKelvin32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKelvin32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKelvin32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetKelvin32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKelvin32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKelvin32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Kelvin32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Kelvin32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Kelvin32Member[ix].IsChanged();
            MA1.Kelvin32Member[ix].Val = DotsTest.ParameterArrays.Kelvin32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Kelvin32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Kelvin32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullKelvin32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKelvin32Member(MA2, ix);
            MA2.Kelvin32Member[ix].Val = MA1.Kelvin32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKelvin32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedKelvin32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKelvin32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Kelvin32Member[ix].Val = MA1.Kelvin32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKelvin32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKelvin32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedKelvin32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKelvin32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKelvin32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedKelvin32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKelvin32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKelvin32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKelvin32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKelvin32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKelvin32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKelvin32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKelvin32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKelvin32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKelvin32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKelvin32Member(AEO, ix);
        }

        // SetNull test
        MT1.Kelvin32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullKelvin32Member(MT2);
        MA1.Kelvin32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullKelvin32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Kelvin32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullKelvin32Member(MT2) &&
            MA1.Kelvin32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullKelvin32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Kelvin32

    private static void Test_Kilogram32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Kilogram32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Kilogram32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Kilogram32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Kilogram32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Kilogram32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Kilogram32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Kilogram32Member.IsNull();
        In_Req_Ok = !MT1.Kilogram32Member.IsChanged();
        MT1.Kilogram32Member.Val = DotsTest.ParameterTypes.Kilogram32Parameter;
        Null_Ok = Null_Ok && !MT1.Kilogram32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Kilogram32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Kilogram32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Kilogram32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Kilogram32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Kilogram32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Kilogram32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Kilogram32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Kilogram32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullKilogram32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKilogram32Member(MT2);
        DotsTest.MemberTypesProperty.SetKilogram32Member(MT2, MT1.Kilogram32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetKilogram32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKilogram32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKilogram32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullKilogram32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKilogram32Member(MI);
        DotsTest.MemberTypesProperty.SetKilogram32Member(MI, MT2.Kilogram32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetKilogram32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKilogram32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKilogram32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullKilogram32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKilogram32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetKilogram32Member(MIA, MT2.Kilogram32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Kilogram32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKilogram32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKilogram32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKilogram32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKilogram32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetKilogram32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKilogram32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKilogram32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKilogram32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKilogram32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetKilogram32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKilogram32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKilogram32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Kilogram32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Kilogram32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Kilogram32Member[ix].IsChanged();
            MA1.Kilogram32Member[ix].Val = DotsTest.ParameterArrays.Kilogram32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Kilogram32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Kilogram32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullKilogram32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKilogram32Member(MA2, ix);
            MA2.Kilogram32Member[ix].Val = MA1.Kilogram32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKilogram32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedKilogram32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKilogram32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Kilogram32Member[ix].Val = MA1.Kilogram32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKilogram32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKilogram32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedKilogram32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKilogram32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKilogram32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedKilogram32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKilogram32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKilogram32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKilogram32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKilogram32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKilogram32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKilogram32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKilogram32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKilogram32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKilogram32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKilogram32Member(AEO, ix);
        }

        // SetNull test
        MT1.Kilogram32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullKilogram32Member(MT2);
        MA1.Kilogram32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullKilogram32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Kilogram32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullKilogram32Member(MT2) &&
            MA1.Kilogram32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullKilogram32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Kilogram32

    private static void Test_Meter32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Meter32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Meter32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Meter32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Meter32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Meter32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Meter32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Meter32Member.IsNull();
        In_Req_Ok = !MT1.Meter32Member.IsChanged();
        MT1.Meter32Member.Val = DotsTest.ParameterTypes.Meter32Parameter;
        Null_Ok = Null_Ok && !MT1.Meter32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Meter32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Meter32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Meter32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Meter32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Meter32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Meter32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Meter32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Meter32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeter32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeter32Member(MT2);
        DotsTest.MemberTypesProperty.SetMeter32Member(MT2, MT1.Meter32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetMeter32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeter32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeter32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeter32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeter32Member(MI);
        DotsTest.MemberTypesProperty.SetMeter32Member(MI, MT2.Meter32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetMeter32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeter32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeter32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeter32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeter32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetMeter32Member(MIA, MT2.Meter32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Meter32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeter32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeter32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeter32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeter32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetMeter32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeter32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeter32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeter32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeter32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetMeter32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeter32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeter32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Meter32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Meter32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Meter32Member[ix].IsChanged();
            MA1.Meter32Member[ix].Val = DotsTest.ParameterArrays.Meter32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Meter32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Meter32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullMeter32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeter32Member(MA2, ix);
            MA2.Meter32Member[ix].Val = MA1.Meter32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeter32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeter32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeter32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Meter32Member[ix].Val = MA1.Meter32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeter32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeter32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeter32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeter32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeter32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeter32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeter32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeter32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeter32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeter32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeter32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeter32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeter32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeter32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeter32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeter32Member(AEO, ix);
        }

        // SetNull test
        MT1.Meter32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullMeter32Member(MT2);
        MA1.Meter32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullMeter32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Meter32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullMeter32Member(MT2) &&
            MA1.Meter32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullMeter32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Meter32

    private static void Test_MeterPerSecond32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("MeterPerSecond32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.MeterPerSecond32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.MeterPerSecond32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.MeterPerSecond32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.MeterPerSecond32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.MeterPerSecond32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.MeterPerSecond32Member.IsNull();
        In_Req_Ok = !MT1.MeterPerSecond32Member.IsChanged();
        MT1.MeterPerSecond32Member.Val = DotsTest.ParameterTypes.MeterPerSecond32Parameter;
        Null_Ok = Null_Ok && !MT1.MeterPerSecond32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.MeterPerSecond32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.MeterPerSecond32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.MeterPerSecond32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.MeterPerSecond32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.MeterPerSecond32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "MeterPerSecond32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "MeterPerSecond32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "MeterPerSecond32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeterPerSecond32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecond32Member(MT2);
        DotsTest.MemberTypesProperty.SetMeterPerSecond32Member(MT2, MT1.MeterPerSecond32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetMeterPerSecond32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecond32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecond32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeterPerSecond32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecond32Member(MI);
        DotsTest.MemberTypesProperty.SetMeterPerSecond32Member(MI, MT2.MeterPerSecond32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetMeterPerSecond32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecond32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecond32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeterPerSecond32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecond32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetMeterPerSecond32Member(MIA, MT2.MeterPerSecond32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.MeterPerSecond32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecond32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecond32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecond32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecond32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetMeterPerSecond32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecond32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecond32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecond32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecond32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetMeterPerSecond32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecond32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecond32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.MeterPerSecond32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.MeterPerSecond32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.MeterPerSecond32Member[ix].IsChanged();
            MA1.MeterPerSecond32Member[ix].Val = DotsTest.ParameterArrays.MeterPerSecond32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.MeterPerSecond32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.MeterPerSecond32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullMeterPerSecond32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecond32Member(MA2, ix);
            MA2.MeterPerSecond32Member[ix].Val = MA1.MeterPerSecond32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecond32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeterPerSecond32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecond32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.MeterPerSecond32Member[ix].Val = MA1.MeterPerSecond32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecond32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecond32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeterPerSecond32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecond32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecond32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeterPerSecond32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecond32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecond32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecond32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecond32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecond32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecond32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecond32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecond32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecond32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecond32Member(AEO, ix);
        }

        // SetNull test
        MT1.MeterPerSecond32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullMeterPerSecond32Member(MT2);
        MA1.MeterPerSecond32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullMeterPerSecond32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.MeterPerSecond32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullMeterPerSecond32Member(MT2) &&
            MA1.MeterPerSecond32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullMeterPerSecond32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // MeterPerSecond32

    private static void Test_MeterPerSecondSquared32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("MeterPerSecondSquared32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.MeterPerSecondSquared32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.MeterPerSecondSquared32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.MeterPerSecondSquared32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.MeterPerSecondSquared32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.MeterPerSecondSquared32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.MeterPerSecondSquared32Member.IsNull();
        In_Req_Ok = !MT1.MeterPerSecondSquared32Member.IsChanged();
        MT1.MeterPerSecondSquared32Member.Val = DotsTest.ParameterTypes.MeterPerSecondSquared32Parameter;
        Null_Ok = Null_Ok && !MT1.MeterPerSecondSquared32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.MeterPerSecondSquared32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.MeterPerSecondSquared32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.MeterPerSecondSquared32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.MeterPerSecondSquared32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.MeterPerSecondSquared32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "MeterPerSecondSquared32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "MeterPerSecondSquared32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "MeterPerSecondSquared32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared32Member(MT2);
        DotsTest.MemberTypesProperty.SetMeterPerSecondSquared32Member(MT2, MT1.MeterPerSecondSquared32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetMeterPerSecondSquared32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared32Member(MI);
        DotsTest.MemberTypesProperty.SetMeterPerSecondSquared32Member(MI, MT2.MeterPerSecondSquared32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetMeterPerSecondSquared32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetMeterPerSecondSquared32Member(MIA, MT2.MeterPerSecondSquared32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.MeterPerSecondSquared32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetMeterPerSecondSquared32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetMeterPerSecondSquared32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.MeterPerSecondSquared32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.MeterPerSecondSquared32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.MeterPerSecondSquared32Member[ix].IsChanged();
            MA1.MeterPerSecondSquared32Member[ix].Val = DotsTest.ParameterArrays.MeterPerSecondSquared32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.MeterPerSecondSquared32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.MeterPerSecondSquared32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecondSquared32Member(MA2, ix);
            MA2.MeterPerSecondSquared32Member[ix].Val = MA1.MeterPerSecondSquared32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeterPerSecondSquared32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecondSquared32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.MeterPerSecondSquared32Member[ix].Val = MA1.MeterPerSecondSquared32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecondSquared32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeterPerSecondSquared32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecondSquared32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeterPerSecondSquared32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecondSquared32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecondSquared32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecondSquared32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecondSquared32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecondSquared32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecondSquared32Member(AEO, ix);
        }

        // SetNull test
        MT1.MeterPerSecondSquared32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullMeterPerSecondSquared32Member(MT2);
        MA1.MeterPerSecondSquared32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullMeterPerSecondSquared32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.MeterPerSecondSquared32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared32Member(MT2) &&
            MA1.MeterPerSecondSquared32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());


    } // MeterPerSecondSquared32

    private static void Test_Newton32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Newton32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Newton32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Newton32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Newton32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Newton32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Newton32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Newton32Member.IsNull();
        In_Req_Ok = !MT1.Newton32Member.IsChanged();
        MT1.Newton32Member.Val = DotsTest.ParameterTypes.Newton32Parameter;
        Null_Ok = Null_Ok && !MT1.Newton32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Newton32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Newton32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Newton32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Newton32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Newton32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Newton32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Newton32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Newton32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullNewton32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedNewton32Member(MT2);
        DotsTest.MemberTypesProperty.SetNewton32Member(MT2, MT1.Newton32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetNewton32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullNewton32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedNewton32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullNewton32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedNewton32Member(MI);
        DotsTest.MemberTypesProperty.SetNewton32Member(MI, MT2.Newton32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetNewton32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullNewton32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedNewton32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullNewton32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedNewton32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetNewton32Member(MIA, MT2.Newton32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Newton32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullNewton32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedNewton32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullNewton32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedNewton32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetNewton32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullNewton32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedNewton32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullNewton32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedNewton32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetNewton32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullNewton32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedNewton32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Newton32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Newton32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Newton32Member[ix].IsChanged();
            MA1.Newton32Member[ix].Val = DotsTest.ParameterArrays.Newton32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Newton32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Newton32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullNewton32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedNewton32Member(MA2, ix);
            MA2.Newton32Member[ix].Val = MA1.Newton32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullNewton32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedNewton32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetNewton32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Newton32Member[ix].Val = MA1.Newton32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetNewton32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullNewton32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedNewton32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetNewton32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullNewton32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedNewton32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullNewton32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedNewton32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetNewton32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullNewton32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedNewton32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullNewton32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedNewton32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetNewton32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullNewton32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedNewton32Member(AEO, ix);
        }

        // SetNull test
        MT1.Newton32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullNewton32Member(MT2);
        MA1.Newton32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullNewton32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Newton32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullNewton32Member(MT2) &&
            MA1.Newton32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullNewton32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Newton32

    private static void Test_Pascal32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Pascal32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Pascal32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Pascal32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Pascal32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Pascal32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Pascal32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Pascal32Member.IsNull();
        In_Req_Ok = !MT1.Pascal32Member.IsChanged();
        MT1.Pascal32Member.Val = DotsTest.ParameterTypes.Pascal32Parameter;
        Null_Ok = Null_Ok && !MT1.Pascal32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Pascal32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Pascal32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Pascal32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Pascal32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Pascal32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Pascal32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Pascal32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Pascal32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullPascal32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedPascal32Member(MT2);
        DotsTest.MemberTypesProperty.SetPascal32Member(MT2, MT1.Pascal32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetPascal32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullPascal32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedPascal32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullPascal32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedPascal32Member(MI);
        DotsTest.MemberTypesProperty.SetPascal32Member(MI, MT2.Pascal32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetPascal32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullPascal32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedPascal32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullPascal32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedPascal32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetPascal32Member(MIA, MT2.Pascal32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Pascal32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullPascal32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedPascal32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullPascal32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedPascal32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetPascal32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullPascal32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedPascal32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullPascal32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedPascal32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetPascal32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullPascal32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedPascal32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Pascal32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Pascal32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Pascal32Member[ix].IsChanged();
            MA1.Pascal32Member[ix].Val = DotsTest.ParameterArrays.Pascal32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Pascal32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Pascal32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullPascal32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedPascal32Member(MA2, ix);
            MA2.Pascal32Member[ix].Val = MA1.Pascal32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullPascal32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedPascal32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetPascal32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Pascal32Member[ix].Val = MA1.Pascal32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetPascal32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullPascal32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedPascal32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetPascal32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullPascal32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedPascal32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullPascal32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedPascal32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetPascal32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullPascal32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedPascal32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullPascal32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedPascal32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetPascal32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullPascal32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedPascal32Member(AEO, ix);
        }

        // SetNull test
        MT1.Pascal32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullPascal32Member(MT2);
        MA1.Pascal32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullPascal32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Pascal32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullPascal32Member(MT2) &&
            MA1.Pascal32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullPascal32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Pascal32

    private static void Test_Radian32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Radian32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Radian32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Radian32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Radian32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Radian32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Radian32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Radian32Member.IsNull();
        In_Req_Ok = !MT1.Radian32Member.IsChanged();
        MT1.Radian32Member.Val = DotsTest.ParameterTypes.Radian32Parameter;
        Null_Ok = Null_Ok && !MT1.Radian32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Radian32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Radian32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Radian32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Radian32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Radian32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Radian32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Radian32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Radian32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadian32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadian32Member(MT2);
        DotsTest.MemberTypesProperty.SetRadian32Member(MT2, MT1.Radian32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetRadian32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadian32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadian32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadian32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadian32Member(MI);
        DotsTest.MemberTypesProperty.SetRadian32Member(MI, MT2.Radian32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetRadian32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadian32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadian32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadian32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadian32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetRadian32Member(MIA, MT2.Radian32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Radian32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadian32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadian32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadian32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadian32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetRadian32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadian32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadian32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadian32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadian32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetRadian32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadian32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadian32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Radian32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Radian32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Radian32Member[ix].IsChanged();
            MA1.Radian32Member[ix].Val = DotsTest.ParameterArrays.Radian32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Radian32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Radian32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullRadian32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadian32Member(MA2, ix);
            MA2.Radian32Member[ix].Val = MA1.Radian32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadian32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadian32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadian32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Radian32Member[ix].Val = MA1.Radian32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadian32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadian32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadian32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadian32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadian32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadian32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadian32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadian32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadian32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadian32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadian32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadian32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadian32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadian32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadian32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadian32Member(AEO, ix);
        }

        // SetNull test
        MT1.Radian32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullRadian32Member(MT2);
        MA1.Radian32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullRadian32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Radian32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullRadian32Member(MT2) &&
            MA1.Radian32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullRadian32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Radian32

    private static void Test_RadianPerSecond32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("RadianPerSecond32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.RadianPerSecond32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.RadianPerSecond32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.RadianPerSecond32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.RadianPerSecond32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.RadianPerSecond32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.RadianPerSecond32Member.IsNull();
        In_Req_Ok = !MT1.RadianPerSecond32Member.IsChanged();
        MT1.RadianPerSecond32Member.Val = DotsTest.ParameterTypes.RadianPerSecond32Parameter;
        Null_Ok = Null_Ok && !MT1.RadianPerSecond32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.RadianPerSecond32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.RadianPerSecond32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.RadianPerSecond32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.RadianPerSecond32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.RadianPerSecond32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "RadianPerSecond32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "RadianPerSecond32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "RadianPerSecond32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadianPerSecond32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecond32Member(MT2);
        DotsTest.MemberTypesProperty.SetRadianPerSecond32Member(MT2, MT1.RadianPerSecond32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetRadianPerSecond32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecond32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecond32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadianPerSecond32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecond32Member(MI);
        DotsTest.MemberTypesProperty.SetRadianPerSecond32Member(MI, MT2.RadianPerSecond32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetRadianPerSecond32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecond32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecond32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadianPerSecond32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecond32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetRadianPerSecond32Member(MIA, MT2.RadianPerSecond32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.RadianPerSecond32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecond32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecond32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecond32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecond32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetRadianPerSecond32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecond32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecond32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecond32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecond32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetRadianPerSecond32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecond32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecond32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.RadianPerSecond32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.RadianPerSecond32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.RadianPerSecond32Member[ix].IsChanged();
            MA1.RadianPerSecond32Member[ix].Val = DotsTest.ParameterArrays.RadianPerSecond32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.RadianPerSecond32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.RadianPerSecond32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullRadianPerSecond32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecond32Member(MA2, ix);
            MA2.RadianPerSecond32Member[ix].Val = MA1.RadianPerSecond32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecond32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadianPerSecond32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecond32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.RadianPerSecond32Member[ix].Val = MA1.RadianPerSecond32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecond32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecond32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadianPerSecond32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecond32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecond32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadianPerSecond32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecond32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecond32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecond32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecond32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecond32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecond32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecond32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecond32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecond32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecond32Member(AEO, ix);
        }

        // SetNull test
        MT1.RadianPerSecond32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullRadianPerSecond32Member(MT2);
        MA1.RadianPerSecond32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullRadianPerSecond32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.RadianPerSecond32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullRadianPerSecond32Member(MT2) &&
            MA1.RadianPerSecond32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullRadianPerSecond32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());


    } // RadianPerSecond32

    private static void Test_RadianPerSecondSquared32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("RadianPerSecondSquared32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.RadianPerSecondSquared32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.RadianPerSecondSquared32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.RadianPerSecondSquared32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.RadianPerSecondSquared32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.RadianPerSecondSquared32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.RadianPerSecondSquared32Member.IsNull();
        In_Req_Ok = !MT1.RadianPerSecondSquared32Member.IsChanged();
        MT1.RadianPerSecondSquared32Member.Val = DotsTest.ParameterTypes.RadianPerSecondSquared32Parameter;
        Null_Ok = Null_Ok && !MT1.RadianPerSecondSquared32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.RadianPerSecondSquared32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.RadianPerSecondSquared32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.RadianPerSecondSquared32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.RadianPerSecondSquared32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.RadianPerSecondSquared32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "RadianPerSecondSquared32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "RadianPerSecondSquared32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "RadianPerSecondSquared32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared32Member(MT2);
        DotsTest.MemberTypesProperty.SetRadianPerSecondSquared32Member(MT2, MT1.RadianPerSecondSquared32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetRadianPerSecondSquared32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared32Member(MI);
        DotsTest.MemberTypesProperty.SetRadianPerSecondSquared32Member(MI, MT2.RadianPerSecondSquared32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetRadianPerSecondSquared32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetRadianPerSecondSquared32Member(MIA, MT2.RadianPerSecondSquared32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.RadianPerSecondSquared32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetRadianPerSecondSquared32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetRadianPerSecondSquared32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.RadianPerSecondSquared32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.RadianPerSecondSquared32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.RadianPerSecondSquared32Member[ix].IsChanged();
            MA1.RadianPerSecondSquared32Member[ix].Val = DotsTest.ParameterArrays.RadianPerSecondSquared32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.RadianPerSecondSquared32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.RadianPerSecondSquared32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecondSquared32Member(MA2, ix);
            MA2.RadianPerSecondSquared32Member[ix].Val = MA1.RadianPerSecondSquared32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadianPerSecondSquared32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecondSquared32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.RadianPerSecondSquared32Member[ix].Val = MA1.RadianPerSecondSquared32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecondSquared32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadianPerSecondSquared32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecondSquared32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadianPerSecondSquared32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecondSquared32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecondSquared32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecondSquared32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecondSquared32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecondSquared32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecondSquared32Member(AEO, ix);
        }

        // SetNull test
        MT1.RadianPerSecondSquared32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullRadianPerSecondSquared32Member(MT2);
        MA1.RadianPerSecondSquared32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullRadianPerSecondSquared32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.RadianPerSecondSquared32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared32Member(MT2) &&
            MA1.RadianPerSecondSquared32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // RadianPerSecondSquared32

    private static void Test_Second32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Second32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Second32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Second32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Second32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Second32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Second32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Second32Member.IsNull();
        In_Req_Ok = !MT1.Second32Member.IsChanged();
        MT1.Second32Member.Val = DotsTest.ParameterTypes.Second32Parameter;
        Null_Ok = Null_Ok && !MT1.Second32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Second32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Second32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Second32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Second32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Second32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Second32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Second32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Second32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSecond32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSecond32Member(MT2);
        DotsTest.MemberTypesProperty.SetSecond32Member(MT2, MT1.Second32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetSecond32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSecond32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSecond32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSecond32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSecond32Member(MI);
        DotsTest.MemberTypesProperty.SetSecond32Member(MI, MT2.Second32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetSecond32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSecond32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSecond32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSecond32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSecond32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetSecond32Member(MIA, MT2.Second32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Second32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSecond32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSecond32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSecond32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSecond32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetSecond32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSecond32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSecond32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSecond32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSecond32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetSecond32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSecond32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSecond32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Second32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Second32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Second32Member[ix].IsChanged();
            MA1.Second32Member[ix].Val = DotsTest.ParameterArrays.Second32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Second32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Second32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullSecond32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSecond32Member(MA2, ix);
            MA2.Second32Member[ix].Val = MA1.Second32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSecond32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSecond32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSecond32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Second32Member[ix].Val = MA1.Second32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSecond32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSecond32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSecond32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSecond32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSecond32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSecond32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSecond32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSecond32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSecond32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSecond32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSecond32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSecond32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSecond32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSecond32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSecond32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSecond32Member(AEO, ix);
        }

        // SetNull test
        MT1.Second32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullSecond32Member(MT2);
        MA1.Second32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullSecond32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Second32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullSecond32Member(MT2) &&
            MA1.Second32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullSecond32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Second32

    private static void Test_SquareMeter32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("SquareMeter32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.SquareMeter32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.SquareMeter32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.SquareMeter32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.SquareMeter32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.SquareMeter32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.SquareMeter32Member.IsNull();
        In_Req_Ok = !MT1.SquareMeter32Member.IsChanged();
        MT1.SquareMeter32Member.Val = DotsTest.ParameterTypes.SquareMeter32Parameter;
        Null_Ok = Null_Ok && !MT1.SquareMeter32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.SquareMeter32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.SquareMeter32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.SquareMeter32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.SquareMeter32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.SquareMeter32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "SquareMeter32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "SquareMeter32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "SquareMeter32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSquareMeter32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSquareMeter32Member(MT2);
        DotsTest.MemberTypesProperty.SetSquareMeter32Member(MT2, MT1.SquareMeter32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetSquareMeter32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSquareMeter32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSquareMeter32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSquareMeter32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSquareMeter32Member(MI);
        DotsTest.MemberTypesProperty.SetSquareMeter32Member(MI, MT2.SquareMeter32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetSquareMeter32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSquareMeter32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSquareMeter32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSquareMeter32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSquareMeter32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetSquareMeter32Member(MIA, MT2.SquareMeter32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.SquareMeter32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSquareMeter32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSquareMeter32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSquareMeter32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSquareMeter32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetSquareMeter32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSquareMeter32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSquareMeter32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSquareMeter32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSquareMeter32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetSquareMeter32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSquareMeter32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSquareMeter32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.SquareMeter32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.SquareMeter32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.SquareMeter32Member[ix].IsChanged();
            MA1.SquareMeter32Member[ix].Val = DotsTest.ParameterArrays.SquareMeter32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.SquareMeter32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.SquareMeter32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullSquareMeter32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSquareMeter32Member(MA2, ix);
            MA2.SquareMeter32Member[ix].Val = MA1.SquareMeter32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSquareMeter32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSquareMeter32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSquareMeter32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.SquareMeter32Member[ix].Val = MA1.SquareMeter32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSquareMeter32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSquareMeter32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSquareMeter32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSquareMeter32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSquareMeter32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSquareMeter32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSquareMeter32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSquareMeter32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSquareMeter32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSquareMeter32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSquareMeter32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSquareMeter32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSquareMeter32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSquareMeter32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSquareMeter32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSquareMeter32Member(AEO, ix);
        }

        // SetNull test
        MT1.SquareMeter32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullSquareMeter32Member(MT2);
        MA1.SquareMeter32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullSquareMeter32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.SquareMeter32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullSquareMeter32Member(MT2) &&
            MA1.SquareMeter32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullSquareMeter32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // SquareMeter32

    private static void Test_Steradian32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Steradian32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Steradian32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Steradian32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Steradian32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Steradian32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Steradian32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Steradian32Member.IsNull();
        In_Req_Ok = !MT1.Steradian32Member.IsChanged();
        MT1.Steradian32Member.Val = DotsTest.ParameterTypes.Steradian32Parameter;
        Null_Ok = Null_Ok && !MT1.Steradian32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Steradian32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Steradian32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Steradian32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Steradian32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Steradian32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Steradian32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Steradian32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Steradian32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSteradian32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSteradian32Member(MT2);
        DotsTest.MemberTypesProperty.SetSteradian32Member(MT2, MT1.Steradian32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetSteradian32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSteradian32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSteradian32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSteradian32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSteradian32Member(MI);
        DotsTest.MemberTypesProperty.SetSteradian32Member(MI, MT2.Steradian32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetSteradian32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSteradian32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSteradian32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSteradian32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSteradian32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetSteradian32Member(MIA, MT2.Steradian32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Steradian32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSteradian32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSteradian32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSteradian32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSteradian32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetSteradian32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSteradian32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSteradian32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSteradian32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSteradian32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetSteradian32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSteradian32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSteradian32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Steradian32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Steradian32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Steradian32Member[ix].IsChanged();
            MA1.Steradian32Member[ix].Val = DotsTest.ParameterArrays.Steradian32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Steradian32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Steradian32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullSteradian32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSteradian32Member(MA2, ix);
            MA2.Steradian32Member[ix].Val = MA1.Steradian32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSteradian32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSteradian32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSteradian32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Steradian32Member[ix].Val = MA1.Steradian32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSteradian32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSteradian32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSteradian32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSteradian32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSteradian32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSteradian32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSteradian32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSteradian32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSteradian32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSteradian32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSteradian32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSteradian32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSteradian32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSteradian32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSteradian32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSteradian32Member(AEO, ix);
        }

        // SetNull test
        MT1.Steradian32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullSteradian32Member(MT2);
        MA1.Steradian32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullSteradian32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Steradian32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullSteradian32Member(MT2) &&
            MA1.Steradian32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullSteradian32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Steradian32

    private static void Test_Volt32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Volt32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Volt32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Volt32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Volt32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Volt32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Volt32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Volt32Member.IsNull();
        In_Req_Ok = !MT1.Volt32Member.IsChanged();
        MT1.Volt32Member.Val = DotsTest.ParameterTypes.Volt32Parameter;
        Null_Ok = Null_Ok && !MT1.Volt32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Volt32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Volt32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Volt32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Volt32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Volt32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Volt32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Volt32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Volt32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullVolt32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedVolt32Member(MT2);
        DotsTest.MemberTypesProperty.SetVolt32Member(MT2, MT1.Volt32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetVolt32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullVolt32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedVolt32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullVolt32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedVolt32Member(MI);
        DotsTest.MemberTypesProperty.SetVolt32Member(MI, MT2.Volt32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetVolt32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullVolt32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedVolt32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullVolt32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedVolt32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetVolt32Member(MIA, MT2.Volt32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Volt32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullVolt32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedVolt32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullVolt32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedVolt32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetVolt32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullVolt32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedVolt32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullVolt32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedVolt32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetVolt32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullVolt32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedVolt32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Volt32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Volt32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Volt32Member[ix].IsChanged();
            MA1.Volt32Member[ix].Val = DotsTest.ParameterArrays.Volt32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Volt32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Volt32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullVolt32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedVolt32Member(MA2, ix);
            MA2.Volt32Member[ix].Val = MA1.Volt32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullVolt32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedVolt32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetVolt32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Volt32Member[ix].Val = MA1.Volt32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetVolt32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullVolt32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedVolt32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetVolt32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullVolt32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedVolt32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullVolt32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedVolt32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetVolt32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullVolt32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedVolt32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullVolt32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedVolt32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetVolt32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullVolt32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedVolt32Member(AEO, ix);
        }

        // SetNull test
        MT1.Volt32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullVolt32Member(MT2);
        MA1.Volt32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullVolt32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Volt32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullVolt32Member(MT2) &&
            MA1.Volt32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullVolt32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Volt32

    private static void Test_Watt32()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Watt32");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Watt32MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Watt32MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Watt32ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Watt32MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Watt32MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Watt32Member.IsNull();
        In_Req_Ok = !MT1.Watt32Member.IsChanged();
        MT1.Watt32Member.Val = DotsTest.ParameterTypes.Watt32Parameter;
        Null_Ok = Null_Ok && !MT1.Watt32Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Watt32Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Watt32MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Watt32MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Watt32MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Watt32MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Watt32Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Watt32Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Watt32Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullWatt32Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedWatt32Member(MT2);
        DotsTest.MemberTypesProperty.SetWatt32Member(MT2, MT1.Watt32Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetWatt32Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullWatt32Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedWatt32Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullWatt32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedWatt32Member(MI);
        DotsTest.MemberTypesProperty.SetWatt32Member(MI, MT2.Watt32Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetWatt32Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullWatt32Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedWatt32Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullWatt32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedWatt32Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetWatt32Member(MIA, MT2.Watt32Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Watt32Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullWatt32Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedWatt32Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullWatt32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedWatt32Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetWatt32Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullWatt32Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedWatt32Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullWatt32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedWatt32Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetWatt32Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullWatt32Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedWatt32Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Watt32ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Watt32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Watt32Member[ix].IsChanged();
            MA1.Watt32Member[ix].Val = DotsTest.ParameterArrays.Watt32Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Watt32Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Watt32Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullWatt32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedWatt32Member(MA2, ix);
            MA2.Watt32Member[ix].Val = MA1.Watt32Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullWatt32Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedWatt32Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetWatt32Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Watt32Member[ix].Val = MA1.Watt32Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetWatt32Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullWatt32Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedWatt32Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetWatt32Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullWatt32Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedWatt32Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullWatt32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedWatt32Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetWatt32Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullWatt32Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedWatt32Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullWatt32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedWatt32Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetWatt32Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullWatt32Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedWatt32Member(AEO, ix);
        }

        // SetNull test
        MT1.Watt32Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullWatt32Member(MT2);
        MA1.Watt32Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullWatt32Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Watt32Member.IsNull() && DotsTest.MemberTypesProperty.IsNullWatt32Member(MT2) &&
            MA1.Watt32Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullWatt32Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Watt32

    private static void Test_Ampere64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Ampere64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Ampere64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Ampere64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Ampere64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Ampere64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Ampere64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Ampere64Member.IsNull();
        In_Req_Ok = !MT1.Ampere64Member.IsChanged();
        MT1.Ampere64Member.Val = DotsTest.ParameterTypes.Ampere64Parameter;
        Null_Ok = Null_Ok && !MT1.Ampere64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Ampere64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Ampere64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Ampere64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Ampere64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Ampere64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Ampere64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Ampere64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Ampere64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullAmpere64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedAmpere64Member(MT2);
        DotsTest.MemberTypesProperty.SetAmpere64Member(MT2, MT1.Ampere64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetAmpere64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullAmpere64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedAmpere64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullAmpere64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedAmpere64Member(MI);
        DotsTest.MemberTypesProperty.SetAmpere64Member(MI, MT2.Ampere64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetAmpere64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullAmpere64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedAmpere64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullAmpere64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedAmpere64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetAmpere64Member(MIA, MT2.Ampere64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Ampere64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullAmpere64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedAmpere64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullAmpere64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedAmpere64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetAmpere64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullAmpere64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedAmpere64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullAmpere64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedAmpere64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetAmpere64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullAmpere64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedAmpere64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Ampere64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Ampere64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Ampere64Member[ix].IsChanged();
            MA1.Ampere64Member[ix].Val = DotsTest.ParameterArrays.Ampere64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Ampere64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Ampere64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullAmpere64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedAmpere64Member(MA2, ix);
            MA2.Ampere64Member[ix].Val = MA1.Ampere64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullAmpere64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedAmpere64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetAmpere64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Ampere64Member[ix].Val = MA1.Ampere64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetAmpere64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullAmpere64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedAmpere64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetAmpere64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullAmpere64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedAmpere64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullAmpere64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedAmpere64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetAmpere64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullAmpere64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedAmpere64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullAmpere64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedAmpere64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetAmpere64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullAmpere64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedAmpere64Member(AEO, ix);
        }

        // SetNull test
        MT1.Ampere64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullAmpere64Member(MT2);
        MA1.Ampere64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullAmpere64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Ampere64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullAmpere64Member(MT2) &&
            MA1.Ampere64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullAmpere64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Ampere64

    private static void Test_CubicMeter64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("CubicMeter64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.CubicMeter64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.CubicMeter64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.CubicMeter64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.CubicMeter64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.CubicMeter64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.CubicMeter64Member.IsNull();
        In_Req_Ok = !MT1.CubicMeter64Member.IsChanged();
        MT1.CubicMeter64Member.Val = DotsTest.ParameterTypes.CubicMeter64Parameter;
        Null_Ok = Null_Ok && !MT1.CubicMeter64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.CubicMeter64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.CubicMeter64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.CubicMeter64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.CubicMeter64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.CubicMeter64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "CubicMeter64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "CubicMeter64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "CubicMeter64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullCubicMeter64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedCubicMeter64Member(MT2);
        DotsTest.MemberTypesProperty.SetCubicMeter64Member(MT2, MT1.CubicMeter64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetCubicMeter64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullCubicMeter64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedCubicMeter64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullCubicMeter64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedCubicMeter64Member(MI);
        DotsTest.MemberTypesProperty.SetCubicMeter64Member(MI, MT2.CubicMeter64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetCubicMeter64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullCubicMeter64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedCubicMeter64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullCubicMeter64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedCubicMeter64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetCubicMeter64Member(MIA, MT2.CubicMeter64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.CubicMeter64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullCubicMeter64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedCubicMeter64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullCubicMeter64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedCubicMeter64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetCubicMeter64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullCubicMeter64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedCubicMeter64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullCubicMeter64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedCubicMeter64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetCubicMeter64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullCubicMeter64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedCubicMeter64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.CubicMeter64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.CubicMeter64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.CubicMeter64Member[ix].IsChanged();
            MA1.CubicMeter64Member[ix].Val = DotsTest.ParameterArrays.CubicMeter64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.CubicMeter64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.CubicMeter64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullCubicMeter64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedCubicMeter64Member(MA2, ix);
            MA2.CubicMeter64Member[ix].Val = MA1.CubicMeter64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullCubicMeter64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedCubicMeter64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetCubicMeter64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.CubicMeter64Member[ix].Val = MA1.CubicMeter64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetCubicMeter64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullCubicMeter64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedCubicMeter64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetCubicMeter64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullCubicMeter64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedCubicMeter64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullCubicMeter64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedCubicMeter64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetCubicMeter64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullCubicMeter64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedCubicMeter64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullCubicMeter64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedCubicMeter64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetCubicMeter64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullCubicMeter64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedCubicMeter64Member(AEO, ix);
        }

        // SetNull test
        MT1.CubicMeter64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullCubicMeter64Member(MT2);
        MA1.CubicMeter64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullCubicMeter64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.CubicMeter64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullCubicMeter64Member(MT2) &&
            MA1.CubicMeter64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullCubicMeter64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // CubicMeter64

    private static void Test_Hertz64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Hertz64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Hertz64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Hertz64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Hertz64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Hertz64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Hertz64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Hertz64Member.IsNull();
        In_Req_Ok = !MT1.Hertz64Member.IsChanged();
        MT1.Hertz64Member.Val = DotsTest.ParameterTypes.Hertz64Parameter;
        Null_Ok = Null_Ok && !MT1.Hertz64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Hertz64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Hertz64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Hertz64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Hertz64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Hertz64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Hertz64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Hertz64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Hertz64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullHertz64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedHertz64Member(MT2);
        DotsTest.MemberTypesProperty.SetHertz64Member(MT2, MT1.Hertz64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetHertz64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHertz64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedHertz64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullHertz64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedHertz64Member(MI);
        DotsTest.MemberTypesProperty.SetHertz64Member(MI, MT2.Hertz64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetHertz64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHertz64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedHertz64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullHertz64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedHertz64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetHertz64Member(MIA, MT2.Hertz64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Hertz64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHertz64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedHertz64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHertz64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedHertz64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetHertz64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHertz64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedHertz64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHertz64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedHertz64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetHertz64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullHertz64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedHertz64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Hertz64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Hertz64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Hertz64Member[ix].IsChanged();
            MA1.Hertz64Member[ix].Val = DotsTest.ParameterArrays.Hertz64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Hertz64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Hertz64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullHertz64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedHertz64Member(MA2, ix);
            MA2.Hertz64Member[ix].Val = MA1.Hertz64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHertz64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedHertz64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetHertz64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Hertz64Member[ix].Val = MA1.Hertz64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetHertz64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHertz64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedHertz64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetHertz64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHertz64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedHertz64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHertz64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedHertz64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetHertz64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHertz64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedHertz64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHertz64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedHertz64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetHertz64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullHertz64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedHertz64Member(AEO, ix);
        }

        // SetNull test
        MT1.Hertz64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullHertz64Member(MT2);
        MA1.Hertz64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullHertz64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Hertz64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullHertz64Member(MT2) &&
            MA1.Hertz64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullHertz64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());


    }  // Hertz64

    private static void Test_Joule64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Joule64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Joule64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Joule64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Joule64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Joule64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Joule64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Joule64Member.IsNull();
        In_Req_Ok = !MT1.Joule64Member.IsChanged();
        MT1.Joule64Member.Val = DotsTest.ParameterTypes.Joule64Parameter;
        Null_Ok = Null_Ok && !MT1.Joule64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Joule64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Joule64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Joule64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Joule64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Joule64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Joule64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Joule64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Joule64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullJoule64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedJoule64Member(MT2);
        DotsTest.MemberTypesProperty.SetJoule64Member(MT2, MT1.Joule64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetJoule64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullJoule64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedJoule64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullJoule64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedJoule64Member(MI);
        DotsTest.MemberTypesProperty.SetJoule64Member(MI, MT2.Joule64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetJoule64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullJoule64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedJoule64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullJoule64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedJoule64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetJoule64Member(MIA, MT2.Joule64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Joule64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullJoule64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedJoule64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullJoule64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedJoule64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetJoule64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullJoule64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedJoule64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullJoule64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedJoule64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetJoule64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullJoule64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedJoule64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Joule64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Joule64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Joule64Member[ix].IsChanged();
            MA1.Joule64Member[ix].Val = DotsTest.ParameterArrays.Joule64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Joule64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Joule64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullJoule64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedJoule64Member(MA2, ix);
            MA2.Joule64Member[ix].Val = MA1.Joule64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullJoule64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedJoule64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetJoule64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Joule64Member[ix].Val = MA1.Joule64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetJoule64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullJoule64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedJoule64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetJoule64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullJoule64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedJoule64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullJoule64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedJoule64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetJoule64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullJoule64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedJoule64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullJoule64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedJoule64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetJoule64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullJoule64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedJoule64Member(AEO, ix);
        }

        // SetNull test
        MT1.Joule64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullJoule64Member(MT2);
        MA1.Joule64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullJoule64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Joule64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullJoule64Member(MT2) &&
            MA1.Joule64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullJoule64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Joule64

    private static void Test_Kelvin64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Kelvin64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Kelvin64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Kelvin64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Kelvin64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Kelvin64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Kelvin64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Kelvin64Member.IsNull();
        In_Req_Ok = !MT1.Kelvin64Member.IsChanged();
        MT1.Kelvin64Member.Val = DotsTest.ParameterTypes.Kelvin64Parameter;
        Null_Ok = Null_Ok && !MT1.Kelvin64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Kelvin64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Kelvin64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Kelvin64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Kelvin64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Kelvin64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Kelvin64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Kelvin64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Kelvin64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullKelvin64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKelvin64Member(MT2);
        DotsTest.MemberTypesProperty.SetKelvin64Member(MT2, MT1.Kelvin64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetKelvin64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKelvin64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKelvin64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullKelvin64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKelvin64Member(MI);
        DotsTest.MemberTypesProperty.SetKelvin64Member(MI, MT2.Kelvin64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetKelvin64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKelvin64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKelvin64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullKelvin64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKelvin64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetKelvin64Member(MIA, MT2.Kelvin64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Kelvin64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKelvin64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKelvin64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKelvin64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKelvin64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetKelvin64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKelvin64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKelvin64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKelvin64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKelvin64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetKelvin64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKelvin64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKelvin64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Kelvin64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Kelvin64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Kelvin64Member[ix].IsChanged();
            MA1.Kelvin64Member[ix].Val = DotsTest.ParameterArrays.Kelvin64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Kelvin64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Kelvin64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullKelvin64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKelvin64Member(MA2, ix);
            MA2.Kelvin64Member[ix].Val = MA1.Kelvin64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKelvin64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedKelvin64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKelvin64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Kelvin64Member[ix].Val = MA1.Kelvin64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKelvin64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKelvin64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedKelvin64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKelvin64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKelvin64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedKelvin64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKelvin64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKelvin64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKelvin64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKelvin64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKelvin64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKelvin64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKelvin64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKelvin64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKelvin64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKelvin64Member(AEO, ix);
        }

        // SetNull test
        MT1.Kelvin64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullKelvin64Member(MT2);
        MA1.Kelvin64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullKelvin64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Kelvin64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullKelvin64Member(MT2) &&
            MA1.Kelvin64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullKelvin64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Kelvin64

    private static void Test_Kilogram64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Kilogram64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Kilogram64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Kilogram64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Kilogram64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Kilogram64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Kilogram64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Kilogram64Member.IsNull();
        In_Req_Ok = !MT1.Kilogram64Member.IsChanged();
        MT1.Kilogram64Member.Val = DotsTest.ParameterTypes.Kilogram64Parameter;
        Null_Ok = Null_Ok && !MT1.Kilogram64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Kilogram64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Kilogram64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Kilogram64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Kilogram64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Kilogram64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Kilogram64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Kilogram64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Kilogram64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullKilogram64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKilogram64Member(MT2);
        DotsTest.MemberTypesProperty.SetKilogram64Member(MT2, MT1.Kilogram64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetKilogram64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKilogram64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKilogram64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullKilogram64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKilogram64Member(MI);
        DotsTest.MemberTypesProperty.SetKilogram64Member(MI, MT2.Kilogram64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetKilogram64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKilogram64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKilogram64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullKilogram64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKilogram64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetKilogram64Member(MIA, MT2.Kilogram64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Kilogram64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKilogram64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedKilogram64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKilogram64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKilogram64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetKilogram64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKilogram64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKilogram64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKilogram64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKilogram64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetKilogram64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullKilogram64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedKilogram64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Kilogram64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Kilogram64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Kilogram64Member[ix].IsChanged();
            MA1.Kilogram64Member[ix].Val = DotsTest.ParameterArrays.Kilogram64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Kilogram64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Kilogram64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullKilogram64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKilogram64Member(MA2, ix);
            MA2.Kilogram64Member[ix].Val = MA1.Kilogram64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKilogram64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedKilogram64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKilogram64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Kilogram64Member[ix].Val = MA1.Kilogram64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKilogram64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKilogram64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedKilogram64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKilogram64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKilogram64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedKilogram64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKilogram64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKilogram64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKilogram64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKilogram64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKilogram64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKilogram64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKilogram64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetKilogram64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullKilogram64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedKilogram64Member(AEO, ix);
        }

        // SetNull test
        MT1.Kilogram64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullKilogram64Member(MT2);
        MA1.Kilogram64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullKilogram64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Kilogram64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullKilogram64Member(MT2) &&
            MA1.Kilogram64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullKilogram64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Kilogram64

    private static void Test_Meter64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Meter64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Meter64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Meter64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Meter64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Meter64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Meter64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Meter64Member.IsNull();
        In_Req_Ok = !MT1.Meter64Member.IsChanged();
        MT1.Meter64Member.Val = DotsTest.ParameterTypes.Meter64Parameter;
        Null_Ok = Null_Ok && !MT1.Meter64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Meter64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Meter64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Meter64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Meter64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Meter64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Meter64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Meter64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Meter64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeter64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeter64Member(MT2);
        DotsTest.MemberTypesProperty.SetMeter64Member(MT2, MT1.Meter64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetMeter64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeter64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeter64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeter64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeter64Member(MI);
        DotsTest.MemberTypesProperty.SetMeter64Member(MI, MT2.Meter64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetMeter64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeter64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeter64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeter64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeter64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetMeter64Member(MIA, MT2.Meter64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Meter64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeter64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeter64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeter64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeter64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetMeter64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeter64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeter64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeter64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeter64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetMeter64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeter64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeter64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Meter64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Meter64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Meter64Member[ix].IsChanged();
            MA1.Meter64Member[ix].Val = DotsTest.ParameterArrays.Meter64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Meter64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Meter64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullMeter64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeter64Member(MA2, ix);
            MA2.Meter64Member[ix].Val = MA1.Meter64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeter64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeter64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeter64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Meter64Member[ix].Val = MA1.Meter64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeter64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeter64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeter64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeter64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeter64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeter64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeter64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeter64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeter64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeter64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeter64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeter64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeter64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeter64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeter64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeter64Member(AEO, ix);
        }

        // SetNull test
        MT1.Meter64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullMeter64Member(MT2);
        MA1.Meter64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullMeter64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Meter64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullMeter64Member(MT2) &&
            MA1.Meter64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullMeter64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Meter64

    private static void Test_MeterPerSecond64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("MeterPerSecond64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.MeterPerSecond64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.MeterPerSecond64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.MeterPerSecond64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.MeterPerSecond64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.MeterPerSecond64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.MeterPerSecond64Member.IsNull();
        In_Req_Ok = !MT1.MeterPerSecond64Member.IsChanged();
        MT1.MeterPerSecond64Member.Val = DotsTest.ParameterTypes.MeterPerSecond64Parameter;
        Null_Ok = Null_Ok && !MT1.MeterPerSecond64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.MeterPerSecond64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.MeterPerSecond64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.MeterPerSecond64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.MeterPerSecond64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.MeterPerSecond64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "MeterPerSecond64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "MeterPerSecond64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "MeterPerSecond64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeterPerSecond64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecond64Member(MT2);
        DotsTest.MemberTypesProperty.SetMeterPerSecond64Member(MT2, MT1.MeterPerSecond64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetMeterPerSecond64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecond64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecond64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeterPerSecond64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecond64Member(MI);
        DotsTest.MemberTypesProperty.SetMeterPerSecond64Member(MI, MT2.MeterPerSecond64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetMeterPerSecond64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecond64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecond64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeterPerSecond64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecond64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetMeterPerSecond64Member(MIA, MT2.MeterPerSecond64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.MeterPerSecond64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecond64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecond64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecond64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecond64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetMeterPerSecond64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecond64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecond64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecond64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecond64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetMeterPerSecond64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecond64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecond64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.MeterPerSecond64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.MeterPerSecond64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.MeterPerSecond64Member[ix].IsChanged();
            MA1.MeterPerSecond64Member[ix].Val = DotsTest.ParameterArrays.MeterPerSecond64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.MeterPerSecond64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.MeterPerSecond64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullMeterPerSecond64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecond64Member(MA2, ix);
            MA2.MeterPerSecond64Member[ix].Val = MA1.MeterPerSecond64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecond64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeterPerSecond64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecond64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.MeterPerSecond64Member[ix].Val = MA1.MeterPerSecond64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecond64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecond64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeterPerSecond64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecond64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecond64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeterPerSecond64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecond64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecond64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecond64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecond64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecond64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecond64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecond64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecond64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecond64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecond64Member(AEO, ix);
        }

        // SetNull test
        MT1.MeterPerSecond64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullMeterPerSecond64Member(MT2);
        MA1.MeterPerSecond64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullMeterPerSecond64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.MeterPerSecond64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullMeterPerSecond64Member(MT2) &&
            MA1.MeterPerSecond64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullMeterPerSecond64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // MeterPerSecond64

    private static void Test_MeterPerSecondSquared64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("MeterPerSecondSquared64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.MeterPerSecondSquared64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.MeterPerSecondSquared64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.MeterPerSecondSquared64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.MeterPerSecondSquared64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.MeterPerSecondSquared64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.MeterPerSecondSquared64Member.IsNull();
        In_Req_Ok = !MT1.MeterPerSecondSquared64Member.IsChanged();
        MT1.MeterPerSecondSquared64Member.Val = DotsTest.ParameterTypes.MeterPerSecondSquared64Parameter;
        Null_Ok = Null_Ok && !MT1.MeterPerSecondSquared64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.MeterPerSecondSquared64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.MeterPerSecondSquared64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.MeterPerSecondSquared64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.MeterPerSecondSquared64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.MeterPerSecondSquared64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "MeterPerSecondSquared64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "MeterPerSecondSquared64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "MeterPerSecondSquared64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared64Member(MT2);
        DotsTest.MemberTypesProperty.SetMeterPerSecondSquared64Member(MT2, MT1.MeterPerSecondSquared64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetMeterPerSecondSquared64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared64Member(MI);
        DotsTest.MemberTypesProperty.SetMeterPerSecondSquared64Member(MI, MT2.MeterPerSecondSquared64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetMeterPerSecondSquared64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetMeterPerSecondSquared64Member(MIA, MT2.MeterPerSecondSquared64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.MeterPerSecondSquared64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetMeterPerSecondSquared64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetMeterPerSecondSquared64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedMeterPerSecondSquared64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.MeterPerSecondSquared64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.MeterPerSecondSquared64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.MeterPerSecondSquared64Member[ix].IsChanged();
            MA1.MeterPerSecondSquared64Member[ix].Val = DotsTest.ParameterArrays.MeterPerSecondSquared64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.MeterPerSecondSquared64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.MeterPerSecondSquared64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecondSquared64Member(MA2, ix);
            MA2.MeterPerSecondSquared64Member[ix].Val = MA1.MeterPerSecondSquared64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeterPerSecondSquared64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecondSquared64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.MeterPerSecondSquared64Member[ix].Val = MA1.MeterPerSecondSquared64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecondSquared64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeterPerSecondSquared64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecondSquared64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedMeterPerSecondSquared64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecondSquared64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecondSquared64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecondSquared64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecondSquared64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetMeterPerSecondSquared64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedMeterPerSecondSquared64Member(AEO, ix);
        }

        // SetNull test
        MT1.MeterPerSecondSquared64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullMeterPerSecondSquared64Member(MT2);
        MA1.MeterPerSecondSquared64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullMeterPerSecondSquared64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.MeterPerSecondSquared64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullMeterPerSecondSquared64Member(MT2) &&
            MA1.MeterPerSecondSquared64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullMeterPerSecondSquared64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // MeterPerSecondSquared64

    private static void Test_Newton64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Newton64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Newton64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Newton64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Newton64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Newton64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Newton64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Newton64Member.IsNull();
        In_Req_Ok = !MT1.Newton64Member.IsChanged();
        MT1.Newton64Member.Val = DotsTest.ParameterTypes.Newton64Parameter;
        Null_Ok = Null_Ok && !MT1.Newton64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Newton64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Newton64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Newton64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Newton64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Newton64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Newton64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Newton64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Newton64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullNewton64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedNewton64Member(MT2);
        DotsTest.MemberTypesProperty.SetNewton64Member(MT2, MT1.Newton64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetNewton64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullNewton64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedNewton64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullNewton64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedNewton64Member(MI);
        DotsTest.MemberTypesProperty.SetNewton64Member(MI, MT2.Newton64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetNewton64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullNewton64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedNewton64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullNewton64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedNewton64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetNewton64Member(MIA, MT2.Newton64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Newton64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullNewton64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedNewton64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullNewton64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedNewton64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetNewton64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullNewton64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedNewton64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullNewton64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedNewton64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetNewton64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullNewton64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedNewton64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Newton64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Newton64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Newton64Member[ix].IsChanged();
            MA1.Newton64Member[ix].Val = DotsTest.ParameterArrays.Newton64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Newton64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Newton64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullNewton64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedNewton64Member(MA2, ix);
            MA2.Newton64Member[ix].Val = MA1.Newton64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullNewton64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedNewton64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetNewton64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Newton64Member[ix].Val = MA1.Newton64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetNewton64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullNewton64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedNewton64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetNewton64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullNewton64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedNewton64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullNewton64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedNewton64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetNewton64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullNewton64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedNewton64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullNewton64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedNewton64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetNewton64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullNewton64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedNewton64Member(AEO, ix);
        }

        // SetNull test
        MT1.Newton64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullNewton64Member(MT2);
        MA1.Newton64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullNewton64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Newton64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullNewton64Member(MT2) &&
            MA1.Newton64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullNewton64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Newton64

    private static void Test_Pascal64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Pascal64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Pascal64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Pascal64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Pascal64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Pascal64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Pascal64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Pascal64Member.IsNull();
        In_Req_Ok = !MT1.Pascal64Member.IsChanged();
        MT1.Pascal64Member.Val = DotsTest.ParameterTypes.Pascal64Parameter;
        Null_Ok = Null_Ok && !MT1.Pascal64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Pascal64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Pascal64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Pascal64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Pascal64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Pascal64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Pascal64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Pascal64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Pascal64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullPascal64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedPascal64Member(MT2);
        DotsTest.MemberTypesProperty.SetPascal64Member(MT2, MT1.Pascal64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetPascal64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullPascal64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedPascal64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullPascal64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedPascal64Member(MI);
        DotsTest.MemberTypesProperty.SetPascal64Member(MI, MT2.Pascal64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetPascal64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullPascal64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedPascal64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullPascal64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedPascal64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetPascal64Member(MIA, MT2.Pascal64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Pascal64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullPascal64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedPascal64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullPascal64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedPascal64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetPascal64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullPascal64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedPascal64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullPascal64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedPascal64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetPascal64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullPascal64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedPascal64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Pascal64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Pascal64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Pascal64Member[ix].IsChanged();
            MA1.Pascal64Member[ix].Val = DotsTest.ParameterArrays.Pascal64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Pascal64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Pascal64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullPascal64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedPascal64Member(MA2, ix);
            MA2.Pascal64Member[ix].Val = MA1.Pascal64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullPascal64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedPascal64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetPascal64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Pascal64Member[ix].Val = MA1.Pascal64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetPascal64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullPascal64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedPascal64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetPascal64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullPascal64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedPascal64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullPascal64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedPascal64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetPascal64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullPascal64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedPascal64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullPascal64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedPascal64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetPascal64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullPascal64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedPascal64Member(AEO, ix);
        }

        // SetNull test
        MT1.Pascal64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullPascal64Member(MT2);
        MA1.Pascal64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullPascal64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Pascal64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullPascal64Member(MT2) &&
            MA1.Pascal64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullPascal64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());


    } // Pascal64

    private static void Test_Radian64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Radian64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Radian64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Radian64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Radian64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Radian64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Radian64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Radian64Member.IsNull();
        In_Req_Ok = !MT1.Radian64Member.IsChanged();
        MT1.Radian64Member.Val = DotsTest.ParameterTypes.Radian64Parameter;
        Null_Ok = Null_Ok && !MT1.Radian64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Radian64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Radian64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Radian64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Radian64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Radian64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Radian64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Radian64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Radian64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadian64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadian64Member(MT2);
        DotsTest.MemberTypesProperty.SetRadian64Member(MT2, MT1.Radian64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetRadian64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadian64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadian64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadian64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadian64Member(MI);
        DotsTest.MemberTypesProperty.SetRadian64Member(MI, MT2.Radian64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetRadian64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadian64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadian64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadian64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadian64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetRadian64Member(MIA, MT2.Radian64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Radian64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadian64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadian64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadian64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadian64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetRadian64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadian64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadian64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadian64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadian64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetRadian64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadian64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadian64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Radian64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Radian64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Radian64Member[ix].IsChanged();
            MA1.Radian64Member[ix].Val = DotsTest.ParameterArrays.Radian64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Radian64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Radian64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullRadian64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadian64Member(MA2, ix);
            MA2.Radian64Member[ix].Val = MA1.Radian64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadian64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadian64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadian64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Radian64Member[ix].Val = MA1.Radian64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadian64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadian64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadian64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadian64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadian64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadian64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadian64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadian64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadian64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadian64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadian64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadian64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadian64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadian64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadian64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadian64Member(AEO, ix);
        }

        // SetNull test
        MT1.Radian64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullRadian64Member(MT2);
        MA1.Radian64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullRadian64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Radian64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullRadian64Member(MT2) &&
            MA1.Radian64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullRadian64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Radian64

    private static void Test_RadianPerSecond64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("RadianPerSecond64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.RadianPerSecond64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.RadianPerSecond64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.RadianPerSecond64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.RadianPerSecond64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.RadianPerSecond64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.RadianPerSecond64Member.IsNull();
        In_Req_Ok = !MT1.RadianPerSecond64Member.IsChanged();
        MT1.RadianPerSecond64Member.Val = DotsTest.ParameterTypes.RadianPerSecond64Parameter;
        Null_Ok = Null_Ok && !MT1.RadianPerSecond64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.RadianPerSecond64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.RadianPerSecond64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.RadianPerSecond64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.RadianPerSecond64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.RadianPerSecond64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "RadianPerSecond64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "RadianPerSecond64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "RadianPerSecond64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadianPerSecond64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecond64Member(MT2);
        DotsTest.MemberTypesProperty.SetRadianPerSecond64Member(MT2, MT1.RadianPerSecond64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetRadianPerSecond64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecond64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecond64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadianPerSecond64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecond64Member(MI);
        DotsTest.MemberTypesProperty.SetRadianPerSecond64Member(MI, MT2.RadianPerSecond64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetRadianPerSecond64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecond64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecond64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadianPerSecond64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecond64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetRadianPerSecond64Member(MIA, MT2.RadianPerSecond64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.RadianPerSecond64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecond64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecond64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecond64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecond64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetRadianPerSecond64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecond64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecond64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecond64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecond64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetRadianPerSecond64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecond64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecond64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.RadianPerSecond64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.RadianPerSecond64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.RadianPerSecond64Member[ix].IsChanged();
            MA1.RadianPerSecond64Member[ix].Val = DotsTest.ParameterArrays.RadianPerSecond64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.RadianPerSecond64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.RadianPerSecond64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullRadianPerSecond64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecond64Member(MA2, ix);
            MA2.RadianPerSecond64Member[ix].Val = MA1.RadianPerSecond64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecond64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadianPerSecond64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecond64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.RadianPerSecond64Member[ix].Val = MA1.RadianPerSecond64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecond64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecond64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadianPerSecond64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecond64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecond64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadianPerSecond64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecond64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecond64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecond64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecond64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecond64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecond64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecond64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecond64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecond64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecond64Member(AEO, ix);
        }

        // SetNull test
        MT1.RadianPerSecond64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullRadianPerSecond64Member(MT2);
        MA1.RadianPerSecond64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullRadianPerSecond64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.RadianPerSecond64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullRadianPerSecond64Member(MT2) &&
            MA1.RadianPerSecond64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullRadianPerSecond64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // RadianPerSecond64

    private static void Test_RadianPerSecondSquared64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("RadianPerSecondSquared64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.RadianPerSecondSquared64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.RadianPerSecondSquared64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.RadianPerSecondSquared64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.RadianPerSecondSquared64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.RadianPerSecondSquared64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.RadianPerSecondSquared64Member.IsNull();
        In_Req_Ok = !MT1.RadianPerSecondSquared64Member.IsChanged();
        MT1.RadianPerSecondSquared64Member.Val = DotsTest.ParameterTypes.RadianPerSecondSquared64Parameter;
        Null_Ok = Null_Ok && !MT1.RadianPerSecondSquared64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.RadianPerSecondSquared64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.RadianPerSecondSquared64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.RadianPerSecondSquared64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.RadianPerSecondSquared64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.RadianPerSecondSquared64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "RadianPerSecondSquared64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "RadianPerSecondSquared64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "RadianPerSecondSquared64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared64Member(MT2);
        DotsTest.MemberTypesProperty.SetRadianPerSecondSquared64Member(MT2, MT1.RadianPerSecondSquared64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetRadianPerSecondSquared64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared64Member(MI);
        DotsTest.MemberTypesProperty.SetRadianPerSecondSquared64Member(MI, MT2.RadianPerSecondSquared64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetRadianPerSecondSquared64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetRadianPerSecondSquared64Member(MIA, MT2.RadianPerSecondSquared64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.RadianPerSecondSquared64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetRadianPerSecondSquared64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetRadianPerSecondSquared64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedRadianPerSecondSquared64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.RadianPerSecondSquared64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.RadianPerSecondSquared64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.RadianPerSecondSquared64Member[ix].IsChanged();
            MA1.RadianPerSecondSquared64Member[ix].Val = DotsTest.ParameterArrays.RadianPerSecondSquared64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.RadianPerSecondSquared64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.RadianPerSecondSquared64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecondSquared64Member(MA2, ix);
            MA2.RadianPerSecondSquared64Member[ix].Val = MA1.RadianPerSecondSquared64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadianPerSecondSquared64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecondSquared64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.RadianPerSecondSquared64Member[ix].Val = MA1.RadianPerSecondSquared64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecondSquared64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadianPerSecondSquared64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecondSquared64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedRadianPerSecondSquared64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecondSquared64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecondSquared64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecondSquared64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecondSquared64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetRadianPerSecondSquared64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedRadianPerSecondSquared64Member(AEO, ix);
        }

        // SetNull test
        MT1.RadianPerSecondSquared64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullRadianPerSecondSquared64Member(MT2);
        MA1.RadianPerSecondSquared64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullRadianPerSecondSquared64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.RadianPerSecondSquared64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullRadianPerSecondSquared64Member(MT2) &&
            MA1.RadianPerSecondSquared64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullRadianPerSecondSquared64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // RadianPerSecondSquared64

    private static void Test_Second64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Second64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Second64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Second64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Second64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Second64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Second64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Second64Member.IsNull();
        In_Req_Ok = !MT1.Second64Member.IsChanged();
        MT1.Second64Member.Val = DotsTest.ParameterTypes.Second64Parameter;
        Null_Ok = Null_Ok && !MT1.Second64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Second64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Second64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Second64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Second64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Second64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Second64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Second64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Second64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSecond64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSecond64Member(MT2);
        DotsTest.MemberTypesProperty.SetSecond64Member(MT2, MT1.Second64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetSecond64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSecond64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSecond64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSecond64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSecond64Member(MI);
        DotsTest.MemberTypesProperty.SetSecond64Member(MI, MT2.Second64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetSecond64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSecond64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSecond64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSecond64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSecond64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetSecond64Member(MIA, MT2.Second64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Second64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSecond64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSecond64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSecond64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSecond64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetSecond64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSecond64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSecond64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSecond64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSecond64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetSecond64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSecond64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSecond64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Second64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Second64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Second64Member[ix].IsChanged();
            MA1.Second64Member[ix].Val = DotsTest.ParameterArrays.Second64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Second64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Second64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullSecond64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSecond64Member(MA2, ix);
            MA2.Second64Member[ix].Val = MA1.Second64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSecond64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSecond64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSecond64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Second64Member[ix].Val = MA1.Second64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSecond64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSecond64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSecond64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSecond64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSecond64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSecond64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSecond64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSecond64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSecond64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSecond64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSecond64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSecond64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSecond64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSecond64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSecond64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSecond64Member(AEO, ix);
        }

        // SetNull test
        MT1.Second64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullSecond64Member(MT2);
        MA1.Second64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullSecond64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Second64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullSecond64Member(MT2) &&
            MA1.Second64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullSecond64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Second64

    private static void Test_SquareMeter64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("SquareMeter64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.SquareMeter64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.SquareMeter64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.SquareMeter64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.SquareMeter64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.SquareMeter64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.SquareMeter64Member.IsNull();
        In_Req_Ok = !MT1.SquareMeter64Member.IsChanged();
        MT1.SquareMeter64Member.Val = DotsTest.ParameterTypes.SquareMeter64Parameter;
        Null_Ok = Null_Ok && !MT1.SquareMeter64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.SquareMeter64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.SquareMeter64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.SquareMeter64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.SquareMeter64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.SquareMeter64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "SquareMeter64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "SquareMeter64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "SquareMeter64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSquareMeter64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSquareMeter64Member(MT2);
        DotsTest.MemberTypesProperty.SetSquareMeter64Member(MT2, MT1.SquareMeter64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetSquareMeter64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSquareMeter64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSquareMeter64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSquareMeter64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSquareMeter64Member(MI);
        DotsTest.MemberTypesProperty.SetSquareMeter64Member(MI, MT2.SquareMeter64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetSquareMeter64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSquareMeter64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSquareMeter64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSquareMeter64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSquareMeter64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetSquareMeter64Member(MIA, MT2.SquareMeter64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.SquareMeter64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSquareMeter64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSquareMeter64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSquareMeter64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSquareMeter64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetSquareMeter64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSquareMeter64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSquareMeter64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSquareMeter64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSquareMeter64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetSquareMeter64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSquareMeter64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSquareMeter64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.SquareMeter64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.SquareMeter64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.SquareMeter64Member[ix].IsChanged();
            MA1.SquareMeter64Member[ix].Val = DotsTest.ParameterArrays.SquareMeter64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.SquareMeter64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.SquareMeter64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullSquareMeter64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSquareMeter64Member(MA2, ix);
            MA2.SquareMeter64Member[ix].Val = MA1.SquareMeter64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSquareMeter64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSquareMeter64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSquareMeter64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.SquareMeter64Member[ix].Val = MA1.SquareMeter64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSquareMeter64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSquareMeter64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSquareMeter64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSquareMeter64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSquareMeter64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSquareMeter64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSquareMeter64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSquareMeter64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSquareMeter64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSquareMeter64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSquareMeter64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSquareMeter64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSquareMeter64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSquareMeter64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSquareMeter64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSquareMeter64Member(AEO, ix);
        }

        // SetNull test
        MT1.SquareMeter64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullSquareMeter64Member(MT2);
        MA1.SquareMeter64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullSquareMeter64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.SquareMeter64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullSquareMeter64Member(MT2) &&
            MA1.SquareMeter64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullSquareMeter64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // SquareMeter64

    private static void Test_Steradian64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Steradian64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Steradian64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Steradian64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Steradian64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Steradian64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Steradian64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Steradian64Member.IsNull();
        In_Req_Ok = !MT1.Steradian64Member.IsChanged();
        MT1.Steradian64Member.Val = DotsTest.ParameterTypes.Steradian64Parameter;
        Null_Ok = Null_Ok && !MT1.Steradian64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Steradian64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Steradian64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Steradian64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Steradian64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Steradian64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Steradian64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Steradian64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Steradian64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSteradian64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSteradian64Member(MT2);
        DotsTest.MemberTypesProperty.SetSteradian64Member(MT2, MT1.Steradian64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetSteradian64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSteradian64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSteradian64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSteradian64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSteradian64Member(MI);
        DotsTest.MemberTypesProperty.SetSteradian64Member(MI, MT2.Steradian64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetSteradian64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSteradian64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSteradian64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullSteradian64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSteradian64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetSteradian64Member(MIA, MT2.Steradian64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Steradian64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSteradian64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedSteradian64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSteradian64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSteradian64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetSteradian64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSteradian64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSteradian64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSteradian64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSteradian64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetSteradian64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullSteradian64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedSteradian64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Steradian64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Steradian64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Steradian64Member[ix].IsChanged();
            MA1.Steradian64Member[ix].Val = DotsTest.ParameterArrays.Steradian64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Steradian64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Steradian64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullSteradian64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSteradian64Member(MA2, ix);
            MA2.Steradian64Member[ix].Val = MA1.Steradian64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSteradian64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSteradian64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSteradian64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Steradian64Member[ix].Val = MA1.Steradian64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSteradian64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSteradian64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSteradian64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSteradian64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSteradian64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedSteradian64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSteradian64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSteradian64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSteradian64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSteradian64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSteradian64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSteradian64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSteradian64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetSteradian64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullSteradian64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedSteradian64Member(AEO, ix);
        }

        // SetNull test
        MT1.Steradian64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullSteradian64Member(MT2);
        MA1.Steradian64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullSteradian64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Steradian64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullSteradian64Member(MT2) &&
            MA1.Steradian64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullSteradian64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());


    } // Steradian64

    private static void Test_Volt64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Volt64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Volt64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Volt64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Volt64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Volt64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Volt64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Volt64Member.IsNull();
        In_Req_Ok = !MT1.Volt64Member.IsChanged();
        MT1.Volt64Member.Val = DotsTest.ParameterTypes.Volt64Parameter;
        Null_Ok = Null_Ok && !MT1.Volt64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Volt64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Volt64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Volt64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Volt64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Volt64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Volt64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Volt64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Volt64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullVolt64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedVolt64Member(MT2);
        DotsTest.MemberTypesProperty.SetVolt64Member(MT2, MT1.Volt64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetVolt64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullVolt64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedVolt64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullVolt64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedVolt64Member(MI);
        DotsTest.MemberTypesProperty.SetVolt64Member(MI, MT2.Volt64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetVolt64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullVolt64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedVolt64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullVolt64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedVolt64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetVolt64Member(MIA, MT2.Volt64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Volt64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullVolt64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedVolt64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullVolt64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedVolt64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetVolt64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullVolt64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedVolt64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullVolt64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedVolt64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetVolt64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullVolt64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedVolt64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Volt64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Volt64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Volt64Member[ix].IsChanged();
            MA1.Volt64Member[ix].Val = DotsTest.ParameterArrays.Volt64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Volt64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Volt64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullVolt64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedVolt64Member(MA2, ix);
            MA2.Volt64Member[ix].Val = MA1.Volt64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullVolt64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedVolt64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetVolt64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Volt64Member[ix].Val = MA1.Volt64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetVolt64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullVolt64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedVolt64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetVolt64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullVolt64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedVolt64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullVolt64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedVolt64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetVolt64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullVolt64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedVolt64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullVolt64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedVolt64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetVolt64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullVolt64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedVolt64Member(AEO, ix);
        }

        // SetNull test
        MT1.Volt64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullVolt64Member(MT2);
        MA1.Volt64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullVolt64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Volt64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullVolt64Member(MT2) &&
            MA1.Volt64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullVolt64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    } // Volt64

    private static void Test_Watt64()
    {
        // Locals
        bool Null_Ok, In_Req_Ok;

        //  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
        //  2. Member Property MT2 -> Member Item MI -> Output
        //  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
        //  4. Member Property EO -> Output
        //  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
        //  6. Member Array Property EO -> Output
        Header("Watt64");
        Console.WriteLine("MemberId: " + DotsTest.MemberTypes.Watt64MemberMemberIndex);
        Console.WriteLine("MemberId (arr): " + DotsTest.MemberArrays.Watt64MemberMemberIndex);
        Console.WriteLine("Array Size Ok: " +
                          (DotsTest.ParameterArrays.Watt64ParameterArraySize == 2 &&
                           DotsTest.MemberArrays.Watt64MemberArraySize == 2 &&
                           DotsTest.MemberArraysProperty.Watt64MemberArraySize(MA1) == 2).ToString().ToLower());
        Null_Ok = MT1.Watt64Member.IsNull();
        In_Req_Ok = !MT1.Watt64Member.IsChanged();
        MT1.Watt64Member.Val = DotsTest.ParameterTypes.Watt64Parameter;
        Null_Ok = Null_Ok && !MT1.Watt64Member.IsNull();
        In_Req_Ok = In_Req_Ok && MT1.Watt64Member.IsChanged();


        //locals
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Watt64MemberMemberIndex,
                                             out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        CheckGetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Watt64MemberMemberIndex, lMT, lKT, name, lMTId, lKTId, lSL, lCT, lAL );
        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lMTId + "," + lSL + "," + CtStr(lCT) + "," + lAL);
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Members.GetName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Watt64MemberMemberIndex));
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Members.GetTypeName(DotsTest.MemberTypes.ClassTypeId, DotsTest.MemberTypes.Watt64MemberMemberIndex));

        Console.WriteLine("----Parameters---- ");
        Console.WriteLine("GetName: " + Safir.Dob.Typesystem.Parameters.GetName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Watt64Parameter")));
        Console.WriteLine("GetType: " + Safir.Dob.Typesystem.Parameters.GetType(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                              DotsTest.ParameterTypes.ClassTypeId, "Watt64Parameter")).ToString().ToUpper());
        Console.WriteLine("GetTypeName: " + Safir.Dob.Typesystem.Parameters.GetTypeName(DotsTest.ParameterTypes.ClassTypeId, Safir.Dob.Typesystem.Parameters.GetIndex(
                                                                                                                                                                      DotsTest.ParameterTypes.ClassTypeId, "Watt64Parameter")));
        Console.WriteLine("------------------ ");

        // MemberTypes
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullWatt64Member(MT2);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedWatt64Member(MT2);
        DotsTest.MemberTypesProperty.SetWatt64Member(MT2, MT1.Watt64Member.Val);
        Console.WriteLine("Val: " + DotsTest.MemberTypesProperty.GetWatt64Member(MT2));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullWatt64Member(MT2);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedWatt64Member(MT2);

        // MemberItems
        MI.TypesItem.Obj = new DotsTest.TypesItem();
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullWatt64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedWatt64Member(MI);
        DotsTest.MemberTypesProperty.SetWatt64Member(MI, MT2.Watt64Member.Val);
        Console.WriteLine("Item Val: " + DotsTest.MemberTypesProperty.GetWatt64Member(MI));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullWatt64Member(MI);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedWatt64Member(MI);

        // MemberItemsArray
        Null_Ok = Null_Ok && DotsTest.MemberTypesProperty.IsNullWatt64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedWatt64Member(MIA);
        MIA.TypesItemArray[1].Obj = new DotsTest.TypesItem();
        DotsTest.MemberTypesProperty.SetWatt64Member(MIA, MT2.Watt64Member.Val);
        Console.WriteLine("Item Array Val: " + MIA.TypesItemArray[1].Obj.Watt64Member.Val);
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullWatt64Member(MIA);
        In_Req_Ok = In_Req_Ok && DotsTest.MemberTypesProperty.IsChangedWatt64Member(MIA);

        // EmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullWatt64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedWatt64Member(EO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetWatt64Member(EO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullWatt64Member(EO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedWatt64Member(EO);

        // AnotherEmptyObject
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullWatt64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedWatt64Member(AEO);
        Console.WriteLine("Property Parameter Val: " + DotsTest.MemberTypesProperty.GetWatt64Member(AEO));
        Null_Ok = Null_Ok && !DotsTest.MemberTypesProperty.IsNullWatt64Member(AEO);
        In_Req_Ok = In_Req_Ok && !DotsTest.MemberTypesProperty.IsChangedWatt64Member(AEO);

        //Array test
        for (int ix = 0; ix < DotsTest.ParameterArrays.Watt64ParameterArraySize; ix++)
        {
            //MemberArray
            Null_Ok = Null_Ok && MA1.Watt64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && !MA1.Watt64Member[ix].IsChanged();
            MA1.Watt64Member[ix].Val = DotsTest.ParameterArrays.Watt64Parameter(ix);
            Null_Ok = Null_Ok && !MA1.Watt64Member[ix].IsNull();
            In_Req_Ok = In_Req_Ok && MA1.Watt64Member[ix].IsChanged();

            // MemberArray
            Null_Ok = Null_Ok && DotsTest.MemberArraysProperty.IsNullWatt64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedWatt64Member(MA2, ix);
            MA2.Watt64Member[ix].Val = MA1.Watt64Member[ix].Val;
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullWatt64Member(MA2, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedWatt64Member(MA2, ix);

            Console.WriteLine("Val " + ix + ": " + DotsTest.MemberArraysProperty.GetWatt64Member(MA2, ix));

            // Member Item
            DotsTest.ArraysItem item = new DotsTest.ArraysItem();
            item.Watt64Member[ix].Val = MA1.Watt64Member[ix].Val;
            MI.ArraysItem.Obj = item;
            Console.WriteLine("Array Item Val " + ix + ": " + DotsTest.MemberArraysProperty.GetWatt64Member(MI, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullWatt64Member(MI, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedWatt64Member(MI, ix);

            // Member Item Array
            MIA.ArraysItemArray[1].Obj = item;
            Console.WriteLine("Array Item Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetWatt64Member(MIA, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullWatt64Member(MIA, ix);
            In_Req_Ok = In_Req_Ok && DotsTest.MemberArraysProperty.IsChangedWatt64Member(MIA, ix);

            // EmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullWatt64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedWatt64Member(EO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetWatt64Member(EO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullWatt64Member(EO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedWatt64Member(EO, ix);

            // AnotherEmptyObject
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullWatt64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedWatt64Member(AEO, ix);
            Console.WriteLine("Parameter Array Val " + ix + ": " + DotsTest.MemberArraysProperty.GetWatt64Member(AEO, ix));
            Null_Ok = Null_Ok && !DotsTest.MemberArraysProperty.IsNullWatt64Member(AEO, ix);
            In_Req_Ok = In_Req_Ok && !DotsTest.MemberArraysProperty.IsChangedWatt64Member(AEO, ix);
        }

        // SetNull test
        MT1.Watt64Member.SetNull();
        DotsTest.MemberTypesProperty.SetNullWatt64Member(MT2);
        MA1.Watt64Member[1].SetNull();
        DotsTest.MemberArraysProperty.SetNullWatt64Member(MA2, 1);
        Null_Ok = Null_Ok &&
            MT1.Watt64Member.IsNull() && DotsTest.MemberTypesProperty.IsNullWatt64Member(MT2) &&
            MA1.Watt64Member[1].IsNull() && DotsTest.MemberArraysProperty.IsNullWatt64Member(MA2, 1);

        Console.WriteLine("Is_Null OK: " + Null_Ok.ToString().ToLower());
        Console.WriteLine("Is_Changed OK: " + In_Req_Ok.ToString().ToLower());



    }
    private static void Test_TestException()
    {
        Header("TestException");

        try
        {
            throw new DotsTest.TestException("Testing a TestException");
        }
        catch (DotsTest.TestException e)
        {
            Console.WriteLine("Caught exception: " + Safir.Dob.Typesystem.Operations.GetName(e.GetTypeId()));
        }
    }


    private static void Test_LibraryExceptions()
    {
        Header("LibraryExceptions");
        Safir.Dob.Typesystem.LibraryExceptions.Instance.Set(new DotsTest.TestException("For LibraryExceptions"));
        try
        {
            Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
        }
        catch (DotsTest.TestException e)
        {
            Console.WriteLine("Caught exception: " + Safir.Dob.Typesystem.Operations.GetName(e.GetTypeId()));
        }

        Safir.Dob.Typesystem.LibraryExceptions.Instance.Set(new System.Exception("For LibraryExceptions"));
        try
        {
            Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
        }
        catch (System.Exception)
        {
            Console.WriteLine("Caught native exception");
        }
    }

    private static void FileCheck(string path, string expectedName)
    {
        if (!path.EndsWith(expectedName))
        {
            Console.WriteLine("Failed to find {0}", expectedName);
        }

        if (!System.IO.File.Exists(path))
        {
            Console.WriteLine("Dou file does not exist: {0}", path);
        }
    }


    private static void Test_GetDouFilePath()
    {
        FileCheck(Safir.Dob.Typesystem.Internal.InternalOperations.GetDouFilePath
                  (DotsTest.MemberTypes.ClassTypeId),
                  "DotsTest.MemberTypes.dou");

        FileCheck(Safir.Dob.Typesystem.Internal.InternalOperations.GetDouFilePath
                  (DotsTest.TestException.ExceptionTypeId),
                  "DotsTest.TestException.dou");

        FileCheck(Safir.Dob.Typesystem.Internal.InternalOperations.GetDouFilePath
                  (DotsTest.MemberTypesProperty.ClassTypeId),
                  "DotsTest.MemberTypesProperty.dou");
    }


    /* This test attempts to deserialize a piece of xml that represents a class
     * that is part of a dou library whose jar is *not* in the classpath of this test.
     * The idea being that this test will succeed if dots_cpp has loaded all the
     * required jars as specified by typesystem.ini
     */
    private static void Test_DeserializeUnlinkedObject() {
        Header("DeserializeUnlinkedObject");
        string xml = "<?xml version=\"1.0\" encoding=\"utf-8\"?><DotsTest.ExtraObject><Int32Member>-32</Int32Member></DotsTest.ExtraObject>";
        Safir.Dob.Typesystem.Object obj = Safir.Dob.Typesystem.Serialization.ToObject(xml);
        Console.WriteLine("Class name: " + Safir.Dob.Typesystem.Operations.GetName(obj.GetTypeId()));
    }

    private static string CtStr(Safir.Dob.Typesystem.CollectionType ct)
    {
        switch (ct) {
        case Safir.Dob.Typesystem.CollectionType.ArrayCollectionType:
            return "Array";
        case Safir.Dob.Typesystem.CollectionType.DictionaryCollectionType:
            return "Dictionary";
        case Safir.Dob.Typesystem.CollectionType.SequenceCollectionType:
            return "Sequence";
        case Safir.Dob.Typesystem.CollectionType.SingleValueCollectionType:
            return "Single";
        default:
            return "Unknown";
        }
    }

    private static void PrintSequences(DotsTest.MemberSequences ms)
    {
        Console.WriteLine("--- Int32Member ---");
        Console.WriteLine("size: "+ms.Int32Member.Count);
        Console.WriteLine("isChanged: "+ms.Int32Member.IsChanged().ToString().ToLower());
        Console.WriteLine("val[0]: "+ms.Int32Member[0]);
        Console.WriteLine("val[1]: "+ms.Int32Member[1]);

        Console.WriteLine("--- Int64Member ---");
        Console.WriteLine("size: "+ms.Int64Member.Count);
        Console.WriteLine("isChanged: "+ms.Int64Member.IsChanged().ToString().ToLower());
        Console.WriteLine("val[0]: "+ms.Int64Member[0]);
        Console.WriteLine("val[1]: "+ms.Int64Member[1]);

        Console.WriteLine("--- Float32Member ---");
        Console.WriteLine("size: "+ms.Float32Member.Count);
        Console.WriteLine("isChanged: "+ms.Float32Member.IsChanged().ToString().ToLower());
        Console.WriteLine("val[0]: "+ms.Float32Member[0].ToString("0.0"));
        Console.WriteLine("val[1]: "+ms.Float32Member[1].ToString("0.0"));

        Console.WriteLine("--- Float64Member ---");
        Console.WriteLine("size: "+ms.Float64Member.Count);
        Console.WriteLine("isChanged: "+ms.Float64Member.IsChanged().ToString().ToLower());
        Console.WriteLine("val[0]: "+ms.Float64Member[0].ToString("0.0"));
        Console.WriteLine("val[1]: "+ms.Float64Member[1].ToString("0.0"));

        Console.WriteLine("--- BooleanMember ---");
        Console.WriteLine("size: "+ms.BooleanMember.Count);
        Console.WriteLine("isChanged: "+ms.BooleanMember.IsChanged().ToString().ToLower());
        Console.WriteLine("val[0]: "+ms.BooleanMember[0].ToString().ToLower());
        Console.WriteLine("val[1]: "+ms.BooleanMember[1].ToString().ToLower());

        Console.WriteLine("--- EnumerationMember ---");
        Console.WriteLine("size: "+ms.EnumerationMember.Count);
        Console.WriteLine("isChanged: "+ms.EnumerationMember.IsChanged().ToString().ToLower());
        Console.WriteLine("val[0]: "+ms.EnumerationMember[0].ToString());
        Console.WriteLine("val[1]: "+ms.EnumerationMember[1].ToString());

        Console.WriteLine("--- StringMember ---");
        Console.WriteLine("size: "+ms.StringMember.Count);
        Console.WriteLine("isChanged: "+ms.StringMember.IsChanged().ToString().ToLower());
        Console.WriteLine("val[0]: "+ms.StringMember[0]);
        Console.WriteLine("val[1]: "+ms.StringMember[1]);

        Console.WriteLine("--- TypeIdMember ---");
        Console.WriteLine("size: "+ms.TypeIdMember.Count);
        Console.WriteLine("isChanged: "+ms.TypeIdMember.IsChanged().ToString().ToLower());
        Console.WriteLine("val[0]: "+Safir.Dob.Typesystem.Operations.GetName(ms.TypeIdMember[0]));
        Console.WriteLine("val[1]: "+Safir.Dob.Typesystem.Operations.GetName(ms.TypeIdMember[1]));

        Console.WriteLine("--- HandlerIdMember ---");
        Console.WriteLine("size: "+ms.HandlerIdMember.Count);
        Console.WriteLine("isChanged: "+ms.HandlerIdMember.IsChanged().ToString().ToLower());
    }

    private static void Test_Sequences()
    {
        Header("Sequences");

        DotsTest.MemberSequences ms = new DotsTest.MemberSequences ();

        ms.Int32Member.Add(20);
        ms.Int32Member.Add(30);
        ms.Int32Member.Insert(0, 10);
        ms.Int32Member.RemoveAt(2);

        ms.Int64Member.Add(200);
        ms.Int64Member.Add(300);
        ms.Int64Member.Insert(0, 100);
        ms.Int64Member.RemoveAt(2);

        ms.Float32Member.Add(2.2f);
        ms.Float32Member.Add(3.3f);
        ms.Float32Member.Insert(0, 1.1f);
        ms.Float32Member.RemoveAt(2);

        ms.Float64Member.Add(22.2);
        ms.Float64Member.Add(33.3);
        ms.Float64Member.Insert(0, 11.1);
        ms.Float64Member.RemoveAt(2);

        ms.BooleanMember.Add(false);
        ms.BooleanMember.Add(false);
        ms.BooleanMember.Insert(0, true);
        ms.BooleanMember.RemoveAt(2);

        ms.EnumerationMember.Add(DotsTest.TestEnum.Enumeration.MySecond);
        ms.EnumerationMember.Add(DotsTest.TestEnum.Enumeration.MyThird);
        ms.EnumerationMember.Insert(0, DotsTest.TestEnum.Enumeration.MyFirst);
        ms.EnumerationMember.RemoveAt(2);

        ms.StringMember.Add("Bb");
        ms.StringMember.Add("Cc");
        ms.StringMember.Insert(0, "Aa");
        ms.StringMember.RemoveAt(2);

        ms.TypeIdMember.Add(DotsTest.MemberSequences.ClassTypeId);
        ms.TypeIdMember.Add(DotsTest.TestEnum.EnumerationId);
        ms.TypeIdMember.Insert(0, DotsTest.MemberDictionaries.ClassTypeId);
        ms.TypeIdMember.RemoveAt(2);

        PrintSequences(ms);

        Console.WriteLine("------ To Xml -----");
        var xml=Safir.Dob.Typesystem.Serialization.ToXml(ms);
        Console.WriteLine (xml);

        Console.WriteLine("------ From Xml -----");
        DotsTest.MemberSequences fromXml = Safir.Dob.Typesystem.Serialization.ToObject (xml) as DotsTest.MemberSequences;
        PrintSequences(fromXml);


        Console.WriteLine("------ To Json -----");
        var json=Safir.Dob.Typesystem.Serialization.ToJson(ms);
        Console.WriteLine (json);

        Console.WriteLine("------ From Json -----");
        DotsTest.MemberSequences fromJson=Safir.Dob.Typesystem.Serialization.ToObjectFromJson(json) as DotsTest.MemberSequences;
        PrintSequences(fromJson);
    }

    private static void PrintDictionaryMemberInfo()
    {
        //locals
        string name;
        Safir.Dob.Typesystem.MemberType lMT;
        Safir.Dob.Typesystem.MemberType lKT;
        long lMTId;
        long lKTId;
        int lSL;
        Safir.Dob.Typesystem.CollectionType lCT;
        int lAL;

        name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberDictionaries.ClassTypeId,
                                                    DotsTest.MemberDictionaries.Int32StringMemberMemberIndex,
                                                    out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        Console.WriteLine("GetInfo: " +
                          lMT.ToString().ToUpper() + "," + lKT.ToString().ToUpper() + "," +
                          name + "," + lMTId + "," + lKTId + "," +
                          lSL + "," + CtStr(lCT) + "," + lAL);


        name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberDictionaries.ClassTypeId,
                                                    DotsTest.MemberDictionaries.Int64BinaryMemberMemberIndex,
                                                    out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        Console.WriteLine("GetInfo: " +
                          lMT.ToString().ToUpper() + "," + lKT.ToString().ToUpper() + "," +
                          name + "," + lMTId + "," + lKTId + "," +
                          lSL + "," + CtStr(lCT) + "," + lAL);



        name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberDictionaries.ClassTypeId,
                                                    DotsTest.MemberDictionaries.TypeIdEnumMemberMemberIndex,
                                                    out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        Console.WriteLine("GetInfo: " +
                          lMT.ToString().ToUpper() + "," + lKT.ToString().ToUpper() + "," +
                          name + "," + lMTId + "," + lKTId + "," +
                          lSL + "," + CtStr(lCT) + "," + lAL);



        name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberDictionaries.ClassTypeId,
                                                    DotsTest.MemberDictionaries.EnumInstanceIdMemberMemberIndex,
                                                    out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        Console.WriteLine("GetInfo: " +
                          lMT.ToString().ToUpper() + "," + lKT.ToString().ToUpper() + "," +
                          name + "," + lMTId + "," + lKTId + "," +
                          lSL + "," + CtStr(lCT) + "," + lAL);



        name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberDictionaries.ClassTypeId,
                                                    DotsTest.MemberDictionaries.InstanceIdEntityIdMemberMemberIndex,
                                                    out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        Console.WriteLine("GetInfo: " +
                          lMT.ToString().ToUpper() + "," + lKT.ToString().ToUpper() + "," +
                          name + "," + lMTId + "," + lKTId + "," +
                          lSL + "," + CtStr(lCT) + "," + lAL);



        name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberDictionaries.ClassTypeId,
                                                    DotsTest.MemberDictionaries.EntityIdHandlerIdMemberMemberIndex,
                                                    out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        Console.WriteLine("GetInfo: " +
                          lMT.ToString().ToUpper() + "," + lKT.ToString().ToUpper() + "," +
                          name + "," + lMTId + "," + lKTId + "," +
                          lSL + "," + CtStr(lCT) + "," + lAL);



        name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberDictionaries.ClassTypeId,
                                                    DotsTest.MemberDictionaries.StringItemMemberMemberIndex,
                                                    out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        Console.WriteLine("GetInfo: " +
                          lMT.ToString().ToUpper() + "," + lKT.ToString().ToUpper() + "," +
                          name + "," + lMTId + "," + lKTId + "," +
                          lSL + "," + CtStr(lCT) + "," + lAL);



        name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberDictionaries.ClassTypeId,
                                                    DotsTest.MemberDictionaries.StringObjectMemberMemberIndex,
                                                    out lMT, out lKT, out lMTId, out lKTId, out lSL, out lCT, out lAL);

        Console.WriteLine("GetInfo: " +
                          lMT.ToString().ToUpper() + "," + lKT.ToString().ToUpper() + "," +
                          name + "," + lMTId + "," + lKTId + "," +
                          lSL + "," + CtStr(lCT) + "," + lAL);



    }

    private static void PrintDictionaries(DotsTest.MemberDictionaries md)
    {
        Console.WriteLine("--- Int32StringMember ---");
        Console.WriteLine("size: "+md.Int32StringMember.Count);
        Console.WriteLine("isChanged: "+md.Int32StringMember.IsChanged().ToString ().ToLower ());
        foreach (var kv in md.Int32StringMember)
        {
            if (kv.Value.IsNull ())
                Console.WriteLine (kv.Key + " = NULL, changed: " + kv.Value.IsChanged ().ToString ().ToLower ());
            else
                Console.WriteLine (kv.Key + " = "+kv.Value.Val.ToString()+", changed: " + kv.Value.IsChanged ().ToString ().ToLower ());
        }

        Console.WriteLine("--- Int64BinaryMember ---");
        Console.WriteLine("size: "+md.Int64BinaryMember.Count);
        Console.WriteLine("isChanged: "+md.Int64BinaryMember.IsChanged().ToString ().ToLower ());
        foreach (var kv in md.Int64BinaryMember)
        {
            if (kv.Value.IsNull ())
                Console.WriteLine (kv.Key + " = NULL, changed: " + kv.Value.IsChanged ().ToString ().ToLower ());
            else
                Console.WriteLine (kv.Key + " = "+System.Text.Encoding.UTF8.GetString(kv.Value.Val)+", changed: " + kv.Value.IsChanged ().ToString ().ToLower ());
        }

        Console.WriteLine("--- TypeIdEnumMember ---");
        Console.WriteLine("size: "+md.TypeIdEnumMember.Count);
        Console.WriteLine("isChanged: "+md.TypeIdEnumMember.IsChanged().ToString ().ToLower ());
        foreach (var kv in md.TypeIdEnumMember)
        {
            string name = Safir.Dob.Typesystem.Operations.GetName (kv.Key);
            if (kv.Value.IsNull ())
                Console.WriteLine (name + " = NULL, changed: " + kv.Value.IsChanged ().ToString ().ToLower ());
            else
                Console.WriteLine (name + " = "+kv.Value.Val.ToString()+", changed: " + kv.Value.IsChanged ().ToString ().ToLower ());
        }

        Console.WriteLine("--- EnumInstanceIdMember ---");
        Console.WriteLine("size: "+md.EnumInstanceIdMember.Count);
        Console.WriteLine("isChanged: "+md.EnumInstanceIdMember.IsChanged().ToString ().ToLower ());
        foreach (var kv in md.EnumInstanceIdMember)
        {
            if (kv.Value.IsNull ())
                Console.WriteLine (kv.Key.ToString() + " = NULL, changed: " + kv.Value.IsChanged ().ToString ().ToLower ());
            else
                Console.WriteLine (kv.Key.ToString() + " = "+kv.Value.Val.ToString()+", changed: " + kv.Value.IsChanged ().ToString ().ToLower ());
        }

        Console.WriteLine("--- InstanceIdEntityIdMember ---");
        Console.WriteLine("size: "+md.InstanceIdEntityIdMember.Count);
        Console.WriteLine("isChanged: "+md.InstanceIdEntityIdMember.IsChanged().ToString ().ToLower ());
        foreach (var kv in md.InstanceIdEntityIdMember)
        {
            if (kv.Value.IsNull ())
                Console.WriteLine (kv.Key.ToString() + " = NULL, changed: " + kv.Value.IsChanged ().ToString ().ToLower ());
            else
                Console.WriteLine (kv.Key.ToString() + " = "+kv.Value.Val.ToString()+", changed: " + kv.Value.IsChanged ().ToString ().ToLower ());
        }

        Console.WriteLine("--- EntityIdHandlerIdMember ---");
        Console.WriteLine("size: "+md.EntityIdHandlerIdMember.Count);
        Console.WriteLine("isChanged: "+md.EntityIdHandlerIdMember.IsChanged().ToString ().ToLower ());
        foreach (var kv in md.EntityIdHandlerIdMember)
        {
            if (kv.Value.IsNull ())
                Console.WriteLine (kv.Key.ToString() + " = NULL, changed: " + kv.Value.IsChanged ().ToString ().ToLower ());
            else
                Console.WriteLine (kv.Key.ToString() + " = "+kv.Value.Val.ToString()+", changed: " + kv.Value.IsChanged ().ToString ().ToLower ());
        }

        Console.WriteLine("--- StringItemMember ---");
        Console.WriteLine("size: "+md.StringItemMember.Count);
        Console.WriteLine("isChanged: "+md.StringItemMember.IsChanged().ToString ().ToLower ());
        foreach (var kv in md.StringItemMember)
        {
            if (kv.Value.IsNull ())
                Console.WriteLine (kv.Key.ToString() + " = NULL, changed: " + kv.Value.IsChanged ().ToString ().ToLower ());
            else
                Console.WriteLine (kv.Key.ToString() + " = "+Safir.Dob.Typesystem.Serialization.ToJson(kv.Value.Obj)+", changed: " + kv.Value.IsChanged ().ToString ().ToLower ());
        }




        Console.WriteLine("--- StringObjectMember ---");
        Console.WriteLine("size: "+md.StringObjectMember.Count);
        Console.WriteLine("isChanged: "+md.StringObjectMember.IsChanged().ToString().ToLower());
        foreach (var kv in md.StringObjectMember)
        {
            if (kv.Value.IsNull ())
                Console.WriteLine (kv.Key.ToString() + " = NULL, changed: " + kv.Value.IsChanged ().ToString ().ToLower ());
            else
                Console.WriteLine (kv.Key.ToString() + " = "+Safir.Dob.Typesystem.Serialization.ToJson(kv.Value.Obj)+", changed: " + kv.Value.IsChanged ().ToString ().ToLower ());
        }
    }

    private static void Test_Dictionaries()
    {
        Header("Dictionaries");

        PrintDictionaryMemberInfo();

        DotsTest.MemberDictionaries md=new DotsTest.MemberDictionaries();

        md.Int32StringMember.Add (10, DotsTest.ParameterDictionaries.Int32StringParameter (10));
        md.Int32StringMember.Add (20, DotsTest.ParameterDictionaries.Int32StringParameter (20));

        md.Int64BinaryMember.Add (100, DotsTest.ParameterDictionaries.Int32BinaryParameter (10));
        md.Int64BinaryMember.Add (200, DotsTest.ParameterDictionaries.Int32BinaryParameter (20));

        md.TypeIdEnumMember.Add (DotsTest.MemberDictionaries.ClassTypeId, DotsTest.ParameterDictionaries.StringEnumParameter ("Billy"));
        md.TypeIdEnumMember.Add (DotsTest.MemberSequences.ClassTypeId, DotsTest.ParameterDictionaries.StringEnumParameter ("Svarre"));

        md.EnumInstanceIdMember.Add (DotsTest.TestEnum.Enumeration.MyFirst, DotsTest.ParameterDictionaries.EnumInstanceIdParameter (DotsTest.TestEnum.Enumeration.MyFirst));
        md.EnumInstanceIdMember.Add (DotsTest.TestEnum.Enumeration.MySecond, DotsTest.ParameterDictionaries.EnumInstanceIdParameter (DotsTest.TestEnum.Enumeration.MySecond));

        md.InstanceIdEntityIdMember.Add (new Safir.Dob.Typesystem.InstanceId("FirstInstance"), DotsTest.ParameterDictionaries.HandlerIdEntityIdParameter (new Safir.Dob.Typesystem.HandlerId("handlerOne")));
        md.InstanceIdEntityIdMember.Add (new Safir.Dob.Typesystem.InstanceId("SecondInstance"), DotsTest.ParameterDictionaries.HandlerIdEntityIdParameter (new Safir.Dob.Typesystem.HandlerId(2)));

        DotsTest.MemberDictionaries item1=new DotsTest.MemberDictionaries();
        item1.EntityIdHandlerIdMember.Add (new Safir.Dob.Typesystem.EntityId(Safir.Dob.Entity.ClassTypeId, new Safir.Dob.Typesystem.InstanceId("first")),
                                        DotsTest.ParameterDictionaries.EntityIdHandlerIdParameter (new Safir.Dob.Typesystem.EntityId(Safir.Dob.Entity.ClassTypeId, new Safir.Dob.Typesystem.InstanceId("first"))));
        item1.EntityIdHandlerIdMember.Add (new Safir.Dob.Typesystem.EntityId(Safir.Dob.Entity.ClassTypeId, new Safir.Dob.Typesystem.InstanceId(2)),
                                        DotsTest.ParameterDictionaries.EntityIdHandlerIdParameter (new Safir.Dob.Typesystem.EntityId(Safir.Dob.Entity.ClassTypeId, new Safir.Dob.Typesystem.InstanceId("second"))));

        //new Safir.Dob.Typesystem.EntityId(Safir.Dob.Entity.ClassTypeId, new Safir.Dob.Typesystem.InstanceId("first"))
        //---------
        md.StringItemMember.Add ("Karl", item1);
        md.StringItemMember.Add ("Philip").SetNull();
        md.StringItemMember.Add ("Gustav", item1);

        md.StringObjectMember.Add ("Dilbert", DotsTest.ParameterDictionaries.Int32ObjectParameter(10));
        md.StringObjectMember.Add ("Wally", DotsTest.ParameterDictionaries.Int32ObjectParameter(20));

        PrintDictionaries(md);

        Console.WriteLine("------ To Xml -----");
        var xml=Safir.Dob.Typesystem.Serialization.ToXml(md);
        Console.WriteLine (xml);

        Console.WriteLine("------ From Xml -----");

        var fromXml = Safir.Dob.Typesystem.Serialization.ToObject (xml) as DotsTest.MemberDictionaries;
        PrintDictionaries(fromXml);

        Console.WriteLine("------ To Json -----");
        var json=Safir.Dob.Typesystem.Serialization.ToJson(md);
        Console.WriteLine (json);


        Console.WriteLine("------ From Json -----");
        var fromJson = Safir.Dob.Typesystem.Serialization.ToObjectFromJson (json) as DotsTest.MemberDictionaries;
        PrintDictionaries(fromJson);
    }

    private static void Test_ObjectClone()
    {
        {
            DotsTest.MemberTypes mt = new DotsTest.MemberTypes();
            DotsTest.MemberTypes mtc = mt.Clone();
            mt.Int32Member.Val = 10;
            mt.Kelvin64Member.Val = 10;
            if (!mtc.Int32Member.IsNull() || !mtc.Kelvin64Member.IsNull())
            {
                System.Console.WriteLine("Clone Error");
            }

            mtc = mt.Clone();
            if (mtc.Int32Member.Val != 10 || mtc.Kelvin64Member.Val != 10)
            {
                System.Console.WriteLine("Clone Error");
            }
        }

        {
            DotsTest.MemberArrays ma = new DotsTest.MemberArrays();
            DotsTest.MemberArrays mac = ma.Clone();
            ma.Int32Member[1].Val = 10;
            ma.Kelvin64Member[0].Val = 10;
            if (!mac.Int32Member[1].IsNull() || !mac.Kelvin64Member[0].IsNull())
            {
                System.Console.WriteLine("Clone Error");
            }

            mac = ma.Clone();
            if (mac.Int32Member[1].Val != 10 || mac.Kelvin64Member[0].Val != 10)
            {
                System.Console.WriteLine("Clone Error");
            }
        }

        {
            DotsTest.MemberSequences ms = new DotsTest.MemberSequences ();
            DotsTest.MemberSequences msc = ms.Clone();
            ms.Int32Member.Add(20);
            ms.Kelvin64Member.Add(20);
            if (msc.Int32Member.Count != 0 || msc.Kelvin64Member.Count != 0)
            {
                System.Console.WriteLine("Clone Error");
            }

            msc = ms.Clone();
            if (msc.Int32Member.Count != 1 || msc.Kelvin64Member.Count != 1 ||
                msc.Int32Member[0] != 20 || msc.Kelvin64Member[0] != 20)
            {
                System.Console.WriteLine("Clone Error");
            }
        }

        {
            DotsTest.MemberDictionaries md = new DotsTest.MemberDictionaries();
            DotsTest.MemberDictionaries mdc = md.Clone();

            md.Int32StringMember.Add (10, "Rude word");
            if (mdc.Int32StringMember.Count != 0)
            {
                System.Console.WriteLine("Clone Error");
            }

            mdc = md.Clone();
            if (mdc.Int32StringMember.Count != 1 ||
                mdc.Int32StringMember[10].Val != "Rude word")
            {
                System.Console.WriteLine("Clone Error");
            }

        }

    }

    private static void Test_ContainerClone()
    {
        {
            DotsTest.MemberTypes mt = new DotsTest.MemberTypes();
            Safir.Dob.Typesystem.Int32Container intClone = mt.Int32Member.Clone();
            Safir.Dob.Typesystem.Si64.KelvinContainer kelvinClone = mt.Kelvin64Member.Clone();
            mt.Int32Member.Val = 10;
            mt.Kelvin64Member.Val = 10;
            if (!intClone.IsNull() || !kelvinClone.IsNull())
            {
                System.Console.WriteLine("Clone Error");
            }

            intClone = mt.Int32Member.Clone();
            kelvinClone = mt.Kelvin64Member.Clone();
            if (intClone.Val != 10 || kelvinClone.Val != 10)
            {
                System.Console.WriteLine("Clone Error");
            }
        }

        {
            DotsTest.MemberArrays ma = new DotsTest.MemberArrays();
            Safir.Dob.Typesystem.Int32ContainerArray intClone = ma.Int32Member.Clone();
            Safir.Dob.Typesystem.Si64.KelvinContainerArray kelvinClone = ma.Kelvin64Member.Clone();
            ma.Int32Member[1].Val = 10;
            ma.Kelvin64Member[0].Val = 10;
            if (!intClone[1].IsNull() || !kelvinClone[0].IsNull())
            {
                System.Console.WriteLine("Clone Error");
            }

            intClone = ma.Int32Member.Clone();
            kelvinClone = ma.Kelvin64Member.Clone();
            if (intClone[1].Val != 10 || kelvinClone[0].Val != 10)
            {
                System.Console.WriteLine("Clone Error");
            }
        }

        {
            DotsTest.MemberSequences ms = new DotsTest.MemberSequences ();
            Safir.Dob.Typesystem.Int32SequenceContainer intClone = ms.Int32Member.Clone();
            Safir.Dob.Typesystem.Si64.KelvinSequenceContainer kelvinClone = ms.Kelvin64Member.Clone();

            ms.Int32Member.Add(20);
            ms.Kelvin64Member.Add(20);

            if (intClone.Count != 0 || kelvinClone.Count != 0)
            {
                System.Console.WriteLine("Clone Error");
            }

            intClone = ms.Int32Member.Clone();
            kelvinClone = ms.Kelvin64Member.Clone();
            if (intClone.Count != 1 || kelvinClone.Count != 1 ||
                intClone[0] != 20 || kelvinClone[0] != 20)
            {
                System.Console.WriteLine("Clone Error");
            }
        }

        {
            DotsTest.MemberDictionaries md = new DotsTest.MemberDictionaries();
            Safir.Dob.Typesystem.StringDictionaryContainer<Int32> intClone = md.Int32StringMember.Clone();

            md.Int32StringMember.Add (10, "Rude word");
            if (intClone.Count != 0)
            {
                System.Console.WriteLine("Clone Error");
            }

            intClone = md.Int32StringMember.Clone();
            if (intClone.Count != 1 ||
                intClone[10].Val != "Rude word")
            {
                System.Console.WriteLine("Clone Error");
            }

        }
    }

    private static void Test_ContainerCopy()
    {
        {
            DotsTest.MemberTypes mt = new DotsTest.MemberTypes();
            DotsTest.MemberTypes mtc = new DotsTest.MemberTypes();
            mt.Int32Member.Val = 10;
            mt.Kelvin64Member.Val = 10;

            mtc.Int32Member.Copy(mt.Int32Member);
            mtc.Kelvin64Member.Copy(mt.Kelvin64Member);
            if (mtc.Int32Member.Val != 10 || mtc.Kelvin64Member.Val != 10)
            {
                System.Console.WriteLine("Clone Error");
            }

            mt.Int32Member.Val = 20;
            mt.Kelvin64Member.Val = 20;
            if (mtc.Int32Member.Val != 10 || mtc.Kelvin64Member.Val != 10)
            {
                System.Console.WriteLine("Clone Error");
            }
        }

        {
            DotsTest.MemberSequences ms = new DotsTest.MemberSequences ();
            DotsTest.MemberSequences msc = new DotsTest.MemberSequences ();
            ms.Int32Member.Add(20);
            ms.Kelvin64Member.Add(20);

            msc.Int32Member.Copy(ms.Int32Member);
            msc.Kelvin64Member.Copy(ms.Kelvin64Member);
            if (msc.Int32Member.Count != 1 || msc.Kelvin64Member.Count != 1 ||
                msc.Int32Member[0] != 20 || msc.Kelvin64Member[0] != 20)
            {
                System.Console.WriteLine("Clone Error");
            }

            ms.Int32Member.Add(40);
            ms.Kelvin64Member.Add(40);
            if (msc.Int32Member.Count != 1 || msc.Kelvin64Member.Count != 1 ||
                msc.Int32Member[0] != 20 || msc.Kelvin64Member[0] != 20)
            {
                System.Console.WriteLine("Clone Error");
            }
        }

        {
            DotsTest.MemberDictionaries md = new DotsTest.MemberDictionaries();
            DotsTest.MemberDictionaries mdc = new DotsTest.MemberDictionaries();

            md.Int32StringMember.Add (10, "Rude word");

            mdc.Int32StringMember.Copy(md.Int32StringMember);
            if (mdc.Int32StringMember.Count != 1 ||
                mdc.Int32StringMember[10].Val != "Rude word")
            {
                System.Console.WriteLine("Clone Error");
            }

            md.Int32StringMember.Add (20, "Polite word");
            if (mdc.Int32StringMember.Count != 1 ||
                mdc.Int32StringMember[10].Val != "Rude word")
            {
                System.Console.WriteLine("Clone Error");
            }
        }
    }
}

//Put some misc tests in a separate namespace so we can use "using"
namespace Misc
{
    using Safir;
    using Safir.Dob;
    using Safir.Dob.Typesystem;
    using DotsTest;

    public class MiscTests
    {
        private int tests = 0;
        private int failures = 0;
        private void Check(bool expr, string desc = "")
        {
            ++tests;
            if (!expr) {
                ++failures;
                if (desc.Length == 0)
                {
                    Console.WriteLine("Testcase " + tests + " failed!");
                }
                else
                {
                    Console.WriteLine("Testcase " + tests + " (" + desc + ") failed!");
                }
            }
        }

        public void Test_Containers()
        {
            //sequences
            {
                DotsTest.MemberSequences ms= new DotsTest.MemberSequences();

                Check(ms.Int32Member.IsNull());
                Check(ms.Int32Member.Count == 0);
                Check(!ms.Int32Member.IsChanged());
                ms.Int32Member.Add(20);
                ms.Int32Member.Add(30);
                ms.Int32Member.Insert(0, 10);
                Check(!ms.Int32Member.IsNull());
                Check(ms.Int32Member.IsChanged());
                Check(ms.Int32Member.Count == 3);
                ms.Int32Member.SetChanged(false);
                ms.Int32Member.SetNull();
                Check(ms.Int32Member.IsNull());
                Check(ms.Int32Member.Count == 0);
                Check(ms.Int32Member.IsChanged());

                //check recursiveness
                Check(!ms.TestClassMember.IsChanged());
                ms.TestClassMember.Add(new DotsTest.TestItem());
                Check(ms.TestClassMember.IsChanged());
                ms.TestClassMember.SetChanged(false);
                ms.TestClassMember[0].MyInt.Val = 10;;
                Check(ms.TestClassMember[0].IsChanged());
                Check(ms.TestClassMember.IsChanged(), "sequence recursive ischanged");
                ms.TestClassMember[0].SetChanged(false);
                Check(!ms.TestClassMember.IsChanged(), "sequence recursive ischanged 2");
                ms.TestClassMember.SetChanged(true);
                Check(ms.TestClassMember.IsChanged());
                Check(ms.TestClassMember[0].IsChanged(), "sequence recursive ischanged 3");

            }

            //dictionaries
            {
                DotsTest.MemberDictionaries md= new DotsTest.MemberDictionaries();

                Check(md.Int32StringMember.IsNull());
                Check(md.Int32StringMember.Count == 0);
                Check(!md.Int32StringMember.IsChanged());
                md.Int32StringMember.Add(10,"asdf");
                md.Int32StringMember.Add(20,"asdfasdf");
                Check(!md.Int32StringMember.IsNull());
                Check(md.Int32StringMember.IsChanged());
                Check(md.Int32StringMember.Count == 2);
                md.Int32StringMember.SetChanged(false);
                md.Int32StringMember.SetNull();
                Check(md.Int32StringMember.IsNull());
                Check(md.Int32StringMember.Count == 0);
                Check(md.Int32StringMember.IsChanged());

                //check recursiveness
                Check(!md.Int32ItemMember.IsChanged());
                md.Int32ItemMember.Add(0, new DotsTest.MemberDictionaries());
                Check(md.Int32ItemMember.IsChanged());
                md.Int32ItemMember.SetChanged(false);
                md.Int32ItemMember[0].Obj.Int32Int32Member.Add(10,10);
                Check(md.Int32ItemMember[0].IsChanged());
                Check(md.Int32ItemMember.IsChanged());
                md.Int32ItemMember[0].SetChanged(false);
                Check(!md.Int32ItemMember.IsChanged());
                md.Int32ItemMember.SetChanged(true);
                Check(md.Int32ItemMember.IsChanged());
                Check(md.Int32ItemMember[0].IsChanged());

                Test_DictionaryReflection_AddNull();
            }

            //sequence container Object specialization
            {
                var oc = new Safir.Dob.Typesystem.ObjectSequenceContainer();
                Check(!oc.IsChanged());
                Check(!oc.IsChangedHere());
                oc.SetChangedHere(true);
                Check(oc.IsChanged());
                Check(oc.IsChangedHere());
                oc.SetChanged(false);
                oc.Add(new Safir.Dob.Typesystem.Object());
                Check(oc.IsChanged());
                Check(oc.IsChangedHere());
                var ti = new DotsTest.TestItem();
                oc.Add(ti);
                oc.SetChanged(false);
                ti.MyInt.Val = 10;
                Check(oc.IsChanged(), "recursive ischanged");
                Check(!oc.IsChangedHere(), "recursive ischanged 2");
                Check(oc[1].IsChanged(), "recursive ischanged 3");
            }

        }

        delegate void Checks(Safir.Dob.Typesystem.Object obj);

        void RunSerializationChecks(Object before,
                                    Checks checks)
        {
            //System.Console.WriteLine("-- BEFORE --");
            checks(before);
            byte[] bin = Serialization.ToBinary(before);
            before = null;
            Object after = Serialization.ToObject(bin);
            //System.Console.WriteLine("-- AFTER --");
            checks(after);
        }


        private void Test_BlobChangeFlags_member_types()
        {
            MemberTypes before = new MemberTypes();
            before.Int32Member.Val = 10;
            before.StringMember.Val = "asdf";
            before.ObjectMember.Obj = new Object();

            //change flag only set inside item, not on the member itself
            before.TestClassMember.Obj = new TestItem();
            before.TestClassMember.Obj.MyInt.Val = 1;
            before.TestClassMember.SetChangedHere(false);

            Checks checks = (Object o) =>
                {
                    var obj = o as MemberTypes;
                    Check(obj.IsChanged());
                    Check(obj.Int32Member.IsChanged(),"A");
                    Check(obj.StringMember.IsChanged(),"B");
                    Check(obj.ObjectMember.IsChanged(),"C");
                    Check(!obj.ObjectMember.Obj.IsChanged(),"D");
                    Check(obj.TestClassMember.IsChanged(),"E");
                    Check(!obj.TestClassMember.IsChangedHere(),"F");
                    Check(obj.TestClassMember.Obj.MyInt.IsChanged(),"G");
                };

            RunSerializationChecks(before,checks);

        }

        private void Test_BlobChangeFlags_member_types_2()
        {
            MemberTypes before = new MemberTypes();
            //change flag only set on member, not inside item
            before.TestClassMember.Obj = new TestItem();
            before.TestClassMember.Obj.MyInt.Val = 1;
            before.SetChanged(false);
            before.TestClassMember.SetChangedHere(true);

            Checks checks = (Object o) =>
                {
                    var obj = o as MemberTypes;
                    Check(obj.IsChanged());
                    Check(obj.TestClassMember.IsChanged());
                    Check(obj.TestClassMember.IsChangedHere());
                    Check(!obj.TestClassMember.Obj.MyInt.IsChanged());
                };

            RunSerializationChecks(before,checks);
        }


        private void Test_BlobChangeFlags_member_arrays()
        {
            MemberArrays before = new MemberArrays();
            before.Int32Member[0].Val = 10;
            before.StringMember[0].Val = "asdf";
            before.ObjectMember[1].Obj = new Object();

            //change flag only set inside item, not on the member itself
            before.TestClassMember[0].Obj = new TestItem();
            before.TestClassMember[0].Obj.MyInt.Val = 1;
            before.TestClassMember[0].SetChangedHere(false);

            //change flag only set on member, not inside item
            before.TestClassMember[1].Obj = new TestItem();
            before.TestClassMember[1].Obj.MyInt.Val = 1;
            before.TestClassMember[1].SetChanged(false);
            before.TestClassMember[1].SetChangedHere(true);

            Checks checks = (Object o) =>
                {
                    var obj = o as MemberArrays;
                    Check(obj.IsChanged());
                    Check(obj.Int32Member.IsChanged());
                    Check(obj.Int32Member[0].IsChanged());
                    Check(!obj.Int32Member[1].IsChanged());
                    Check(obj.StringMember.IsChanged());
                    Check(obj.StringMember[0].IsChanged());
                    Check(!obj.StringMember[1].IsChanged());
                    Check(obj.ObjectMember.IsChanged());
                    Check(!obj.ObjectMember[0].IsChanged());
                    Check(obj.ObjectMember[1].IsChanged());

                    Check(obj.TestClassMember.IsChanged());
                    Check(obj.TestClassMember[0].IsChanged());
                    Check(!obj.TestClassMember[0].IsChangedHere());
                    Check(obj.TestClassMember[0].Obj.MyInt.IsChanged());
                    Check(obj.TestClassMember[1].IsChanged());
                    Check(obj.TestClassMember[1].IsChangedHere());
                    Check(!obj.TestClassMember[1].Obj.MyInt.IsChanged());
                };

            RunSerializationChecks(before,checks);
        }


        private void Test_BlobChangeFlags_member_sequences()
        {
            MemberSequences before = new MemberSequences();
            Check(!before.IsChanged());

            Check(!before.Int32Member.IsChanged());
            Check(!before.StringMember.IsChanged());
            Check(!before.ObjectMember.IsChanged());
            Check(!before.ObjectMember.IsChangedHere());

            before.Int32Member.Add(10);
            before.StringMember.Add("asdf");
            before.ObjectMember.Add(new Object());


            //change flag only set inside item, not on the member itself
            before.TestClassMember.Add(new TestItem());
            before.TestClassMember[0].MyInt.Val = 1;
            before.TestClassMember.SetChangedHere(false);

            Checks checks = (Object o) =>
                {
                    var obj = o as MemberSequences;
                    Check(obj.IsChanged());
                    Check(obj.Int32Member.IsChanged());
                    Check(obj.StringMember.IsChanged());
                    Check(obj.ObjectMember.IsChanged());
                    Check(obj.ObjectMember.IsChangedHere());
                    Check(!obj.ObjectMember[0].IsChanged());

                    Check(obj.TestClassMember.IsChanged());
                    Check(obj.TestClassMember[0].IsChanged());
                    Check(!obj.TestClassMember.IsChangedHere());
                    Check(obj.TestClassMember[0].MyInt.IsChanged());
                };

            RunSerializationChecks(before,checks);
        }


        private void Test_BlobChangeFlags_member_sequences_2()
        {
            MemberSequences before = new MemberSequences();

            //change flag only set on member, not inside item
            before.TestClassMember.Add(new TestItem());
            before.TestClassMember[0].MyInt.Val = 1;
            before.SetChanged(false);
            before.TestClassMember.SetChangedHere(true);

            Checks checks = (Object o) =>
                {
                    var obj = o as MemberSequences;
                    Check(obj.IsChanged());
                    Check(obj.TestClassMember.IsChanged());
                    Check(!obj.TestClassMember[0].IsChanged());
                    Check(obj.TestClassMember.IsChangedHere());
                    Check(!obj.TestClassMember[0].MyInt.IsChanged());
                };

            RunSerializationChecks(before,checks);

        }


        private void Test_BlobChangeFlags_member_dictionaries()
        {
            MemberDictionaries before = new MemberDictionaries();
            Check(!before.IsChanged());

            Check(!before.Int32Int32Member.IsChanged());
            Check(!before.Int32ObjectMember.IsChanged());
            Check(!before.Int32ObjectMember.IsChangedHere());

            // change flag only set inside value, not on the member itself
            before.Int32Int32Member.Add(10,10);
            before.Int32Int64Member.Add(20,20);
            before.Int32Int64Member.SetChangedHere(false);

            // change flag only set on dict, not on value
            before.Int64Int32Member.Add(30,30);
            before.Int64Int32Member.SetChanged(false);
            before.Int64Int32Member.SetChangedHere(true);

            //change flags set all over the place
            before.StringStringMember.Add("asdf","adsf");
            before.Int32ObjectMember.Add(1,new Object());


            //On item dict members there are three change flag levels:
            //A: the dictionary, B: the item container, C: the members inside the item.

            // Only C set
            before.Int64ItemMember.Add(10,new TestItem());
            before.Int64ItemMember[10].Obj.MyInt.Val = 1;
            before.Int64ItemMember.SetChangedHere(false);
            before.Int64ItemMember[10].SetChangedHere(false);

            // Only B set
            before.TypeIdItemMember.Add(10,new TestItem());
            before.TypeIdItemMember[10].Obj.MyInt.Val = 1;
            before.TypeIdItemMember.SetChanged(false);
            before.TypeIdItemMember[10].SetChangedHere(true);

            // Only A set
            before.EnumItemMember.Add(TestEnum.Enumeration.MyFirst,new TestItem());
            before.EnumItemMember[TestEnum.Enumeration.MyFirst].Obj.MyInt.Val = 1;
            before.EnumItemMember.SetChanged(false);
            before.EnumItemMember.SetChangedHere(true);

            Checks checks = (Object o) =>
                {
                    var obj = o as MemberDictionaries;
                    Check(obj.IsChanged());
                    Check(obj.Int32Int32Member.IsChanged());
                    Check(obj.Int32Int32Member.IsChangedHere());
                    Check(obj.Int32Int32Member[10].IsChanged());
                    Check(obj.Int32Int64Member.IsChanged());
                    Check(!obj.Int32Int64Member.IsChangedHere());
                    Check(obj.Int32Int64Member[20].IsChanged());
                    Check(obj.Int64Int32Member.IsChanged());
                    Check(obj.Int64Int32Member.IsChangedHere());
                    Check(!obj.Int64Int32Member[30].IsChanged());
                    Check(obj.StringStringMember.IsChanged());
                    Check(obj.Int32ObjectMember.IsChanged(),"A");
                    Check(obj.Int32ObjectMember.IsChangedHere(),"B");
                    Check(!obj.Int32ObjectMember[1].Obj.IsChanged(),"C");

                    Check(obj.Int64ItemMember.IsChanged());
                    Check(!obj.Int64ItemMember.IsChangedHere());
                    Check(!obj.Int64ItemMember[10].IsChangedHere());
                    Check(obj.Int64ItemMember[10].Obj.MyInt.IsChanged());

                    Check(obj.TypeIdItemMember.IsChanged());
                    Check(!obj.TypeIdItemMember.IsChangedHere());
                    Check(obj.TypeIdItemMember[10].IsChangedHere());
                    Check(!obj.TypeIdItemMember[10].Obj.MyInt.IsChanged());

                    Check(obj.EnumItemMember.IsChanged());
                    Check(obj.EnumItemMember.IsChangedHere());
                    Check(!obj.EnumItemMember[TestEnum.Enumeration.MyFirst].IsChangedHere());
                    Check(!obj.EnumItemMember[TestEnum.Enumeration.MyFirst].Obj.MyInt.IsChanged());
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
            Test_EnumerationSequenceReflection();
            Test_ObjectSequenceReflection();
            Test_DictionaryReflection();
            Test_ParameterGetInfo();
            Test_ParameterDictionaryReflection();
        }

        public void Test_EnumerationSequenceReflection()
        {
            var seq = new MemberSequences();
            seq.EnumerationMember.Add(TestEnum.Enumeration.MyFirst);
            seq.EnumerationMember.Add(TestEnum.Enumeration.MySecond);


            {
                EnumerationSequenceContainerBase b = seq.EnumerationMember;
                Check(b.Count == 2);
                Check(b.GetOrdinal(0) == (int)TestEnum.Enumeration.MyFirst);
                Check(b.GetOrdinal(1) == (int)TestEnum.Enumeration.MySecond);

                b.SetOrdinal(0, 1);
                b.AddOrdinal(0);
            }

            Check(seq.EnumerationMember.Count == 3);
            Check(seq.EnumerationMember[0] == TestEnum.Enumeration.MySecond);
            Check(seq.EnumerationMember[1] == TestEnum.Enumeration.MySecond);
            Check(seq.EnumerationMember[2] == TestEnum.Enumeration.MyFirst);

            {
                EnumerationSequenceContainerBase b = seq.EnumerationMember;
                b.Clear();
                Check(seq.EnumerationMember.Count == 0);
            }
        }
        
        public void Test_ObjectSequenceReflection()
        {
            var seq = new MemberSequences();
            seq.TestClassMember.Add(new TestItem());
            seq.TestClassMember[0].MyInt.Val = 10;
            seq.TestClassMember.Add(new TestItem());
            seq.TestClassMember[1].MyInt.Val = 20;

            {
                GenericObjectSequenceContainerBase b = seq.TestClassMember;
                Check(b.Count == 2);

                Check(((Int32Container)b[0].GetMember(TestItem.MyIntMemberIndex,0)).Val == 10);
                Check(((Int32Container)b[1].GetMember(TestItem.MyIntMemberIndex,0)).Val == 20);

                //Try to iterate as well, just for the laughs
                int i = 1;
                foreach (var o in b)
                {
                    Check(((Int32Container)o.GetMember(TestItem.MyIntMemberIndex,0)).Val == 10*i);
                    ++i;
                }

                ((Int32Container)b[1].GetMember(TestItem.MyIntMemberIndex,0)).Val = 30;
                var item1 = new TestItem();
                var item3 = new TestItem();
                item1.MyInt.Val = 400;
                item3.MyInt.Val = 500;
                b.Add(item3);
                b[0] = item1;
            }

            Check(seq.TestClassMember.Count == 3);
            Check(seq.TestClassMember[0].MyInt == 400);
            Check(seq.TestClassMember[1].MyInt == 30);
            Check(seq.TestClassMember[2].MyInt == 500);
        }

        public void Test_DictionaryReflection()
        {
            MemberDictionaries dict = new MemberDictionaries();
            dict.Int64ItemMember.Add(1, new TestItem());
            dict.Int64ItemMember[1].Obj.MyInt.Val = 10;
            dict.Int64ItemMember[1].Obj.MyString.Val = "one";
            dict.Int64ItemMember.Add(2, new TestItem());
            dict.Int64ItemMember[2].Obj.MyInt.Val = 20;
            dict.Int64ItemMember[2].Obj.MyString.Val = "two";

            dict.EnumItemMember.Add(TestEnum.Enumeration.MyFirst, new TestItem());
            dict.EnumItemMember[TestEnum.Enumeration.MyFirst].Obj.MyInt.Val = 10;
            dict.EnumItemMember[TestEnum.Enumeration.MyFirst].Obj.MyString.Val = "one";
            dict.EnumItemMember.Add(TestEnum.Enumeration.MySecond, new TestItem());
            dict.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyInt.Val = 20;
            dict.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyString.Val = "two";

            dict.Int64StringMember.Add(1, "foo");
            dict.Int64StringMember.Add(20, "bar");
            dict.Int64StringMember.Add(111, "bart");
            dict.Int64StringMember[111].SetNull();

            {
                DictionaryContainerBase b = dict.Int64ItemMember;
                Check(b.Count == 2);
                Check((long)b.GetKeyAt(0) == 1);
                Check((long)b.GetKeyAt(1) == 2);
                var container = (TestItemContainer)b.GetValueContainerAt(0);
                Check(container != null);
                Check(!container.IsNull());
                Check(container.Obj.MyInt.Val == 10);
                Check(container.Obj.MyString.Val == "one");
                container = (TestItemContainer)b.GetValueContainerAt(1);
                Check(container != null);
                Check(!container.IsNull());
                Check(container.Obj.MyInt.Val == 20);
                Check(container.Obj.MyString.Val == "two");
            }

            {
                DictionaryContainerBase b = dict.EnumItemMember;
                Check(b.Count == 2);
                Check((int)b.GetKeyAt(0) == 0);
                Check((int)b.GetKeyAt(1) == 1);
                var container = (TestItemContainer)(b.GetValueContainerAt(0));
                Check(container != null);
                Check(container.IsNull() == false);
                Check(container.Obj.MyInt.Val == 10);
                Check(container.Obj.MyString.Val == "one");
                container = (TestItemContainer)(b.GetValueContainerAt(1));
                Check(container != null);
                Check(container.IsNull() == false);
                Check(container.Obj.MyInt.Val == 20);
                Check(container.Obj.MyString.Val == "two");
            }

            {
                DictionaryContainerBase b = dict.Int64StringMember;
                Check(b.Count == 3);
                Check((long)b.GetKeyAt(0) == 1);
                Check((long)b.GetKeyAt(1) == 20);
                Check((long)b.GetKeyAt(2) == 111);
                var container = (StringContainer)(b.GetValueContainerAt(0));
                Check(container != null);
                Check(container.IsNull() == false);
                Check(container.Val == "foo");
                container = (StringContainer)(b.GetValueContainerAt(1));
                Check(container != null);
                Check(container.IsNull() == false);
                Check(container.Val == "bar");
                container = (StringContainer)(b.GetValueContainerAt(2));
                Check(container != null);
                Check(container.IsNull() == true);
            }

        }

        public void Test_DictionaryReflection_AddNull()
        {
            MemberDictionaries dict = new MemberDictionaries();

            // Int64 key
            {
                var b = dict.Int64ItemMember;

                var objCont = b.AddNull((long)3) as ObjectContainerBase;
                b.AddNull((long)4);
                Check(b.Count == 2);

                var testItem = new TestItem();
                testItem.MyInt.Val = 30;
                testItem.MyString.Val = "Three";                
                objCont.InternalObj = testItem;

                var container = new ObjectDictionaryContainer<global::System.Int64, DotsTest.TestItemContainer, DotsTest.TestItem>();
                container.Copy(b);
                Check(container.Count == 2);

                var itemCont3 = container[3];
                Check(!itemCont3.IsNull());
                Check(itemCont3.Obj.MyInt.Val == 30);
                Check(itemCont3.Obj.MyString.Val == "Three");
            }

            // Enum key
            {
                var b = dict.EnumItemMember;
                var objCont = b.AddNull(TestEnum.Enumeration.MySecond) as ObjectContainerBase;
                Check(b.Count == 1);

                var testItem = new TestItem();
                testItem.MyInt.Val = 30;
                testItem.MyString.Val = "Three";
                objCont.InternalObj = testItem;

                var container = new ObjectDictionaryContainer<DotsTest.TestEnum.Enumeration, DotsTest.TestItemContainer, DotsTest.TestItem>();
                
                container.Copy(b);
                Check(container.Count == 1);

                var itemCont3 = container[TestEnum.Enumeration.MySecond];
                Check(!itemCont3.IsNull());
                Check(itemCont3.Obj.MyInt.Val == 30);
                Check(itemCont3.Obj.MyString.Val == "Three");
            }

            // String key
            {
                var b = dict.StringItemMember;
                var objCont = b.AddNull("MyKey") as ObjectContainerBase;
                Check(b.Count == 1);

                var testItem = new TestItem();
                testItem.MyInt.Val = 30;
                testItem.MyString.Val = "Three";
                
                var md = new MemberDictionaries();
                md.Int64ItemMember.Add(1, testItem);
                objCont.InternalObj = md;
                
                var container = new Safir.Dob.Typesystem.ObjectDictionaryContainer<string, DotsTest.MemberDictionariesContainer, DotsTest.MemberDictionaries>();

                container.Copy(b);
                Check(container.Count == 1);

                var itemCont = container["MyKey"];
                Check(!itemCont.IsNull());
                Check(itemCont.Obj.Int64ItemMember[1].Obj.MyInt.Val == 30);
                Check(itemCont.Obj.Int64ItemMember[1].Obj.MyString.Val == "Three");
            }

            // InstanceId key
            {
                var b = dict.InstanceIdItemMember;
                var objCont = b.AddNull(new InstanceId("someInstance")) as ObjectContainerBase;
                Check(b.Count == 1);

                var testItem = new TestItem();
                testItem.MyInt.Val = 30;
                testItem.MyString.Val = "Three";

                var md = new MemberDictionaries();
                md.Int64ItemMember.Add(1, testItem);
                objCont.InternalObj = md;
                
                var container = new Safir.Dob.Typesystem.ObjectDictionaryContainer<Safir.Dob.Typesystem.InstanceId, DotsTest.MemberDictionariesContainer, DotsTest.MemberDictionaries>();

                container.Copy(b);
                Check(container.Count == 1);

                var itemCont = container[new InstanceId("someInstance")];
                Check(!itemCont.IsNull());
                Check(itemCont.Obj.Int64ItemMember[1].Obj.MyInt.Val == 30);
                Check(itemCont.Obj.Int64ItemMember[1].Obj.MyString.Val == "Three");
            }

            // EntityId key
            {
                var b = dict.EntityIdItemMember;
                var key = new EntityId(Safir.Dob.Entity.ClassTypeId, new InstanceId(3));
                var objCont = b.AddNull(key) as ObjectContainerBase;
                Check(b.Count == 1);

                var testItem = new TestItem();
                testItem.MyInt.Val = 30;
                testItem.MyString.Val = "Three";

                var md = new MemberDictionaries();
                md.Int64ItemMember.Add(1, testItem);
                objCont.InternalObj = md;
                
                var container = new Safir.Dob.Typesystem.ObjectDictionaryContainer<Safir.Dob.Typesystem.EntityId, DotsTest.MemberDictionariesContainer, DotsTest.MemberDictionaries>();

                container.Copy(b);
                Check(container.Count == 1);

                var itemCont = container[key];
                Check(!itemCont.IsNull());
                Check(itemCont.Obj.Int64ItemMember[1].Obj.MyInt.Val == 30);
                Check(itemCont.Obj.Int64ItemMember[1].Obj.MyString.Val == "Three");
            }

            // Int32Object dictionary
            {
                var b = dict.Int32ObjectMember;

                var objCont1 = b.AddNull(1) as ObjectContainerBase;
                var objCont2 = b.AddNull(2) as ObjectContainerBase;
                b.AddNull(3);
                Check(b.Count == 3);

                var testItem1 = new TestItem();
                testItem1.MyInt.Val = 30;
                testItem1.MyString.Val = "Three";
                objCont1.InternalObj = testItem1;

                var testItem2 = new TestItem();
                testItem2.MyInt.Val = 40;
                testItem2.MyString.Val = "Four";
                var md = new MemberDictionaries();
                md.Int64ItemMember.Add(1, testItem2);
                objCont2.InternalObj = md;

                var container = new Safir.Dob.Typesystem.ObjectDictionaryContainer<global::System.Int32, Safir.Dob.Typesystem.ObjectContainer, Safir.Dob.Typesystem.Object>();
                container.Copy(b);
                Check(container.Count == 3);

                var itemCont1 = container[1];
                Check(!itemCont1.IsNull());
                var obj1 = itemCont1.Obj as TestItem;
                Check(obj1.MyInt.Val == 30);
                Check(obj1.MyString.Val == "Three");

                var itemCont2 = container[2];
                Check(!itemCont2.IsNull());
                var obj2 = itemCont2.Obj as MemberDictionaries;
                Check(obj2.Int64ItemMember[1].Obj.MyInt.Val == 40);
                Check(obj2.Int64ItemMember[1].Obj.MyString.Val == "Four");

                var itemCont3 = container[3];
                Check(itemCont3.IsNull());
            }

            // Wrong key type throws SoftwareViolationException
            {
                try
                {
                    dict.Int64ItemMember.AddNull((int)3);
                    Check(false);
                }
                catch (Safir.Dob.Typesystem.SoftwareViolationException){}

                try
                {
                    dict.EntityIdItemMember.AddNull("Hello");
                    Check(false);
                }
                catch (Safir.Dob.Typesystem.SoftwareViolationException){}

                try
                {
                    dict.StringItemMember.AddNull(new InstanceId("someInstance"));
                    Check(false);
                }
                catch (Safir.Dob.Typesystem.SoftwareViolationException){}
            }

        }

        private void Test_ParameterGetInfo()
        {
            MemberType parameterType;
            MemberType keyType;
            String parameterName;
            System.Int64 parameterTypeId;
            System.Int64 keyTypeId;
            CollectionType collectionType;
            Int32 numberOfValues;

            Safir.Dob.Typesystem.Parameters.GetInfo(DotsTest.ParameterDictionaries.ClassTypeId,
                                                        Safir.Dob.Typesystem.Parameters.GetIndex(DotsTest.ParameterDictionaries.ClassTypeId,
                                                                                                     "Int32StringParameter"),
                                                    out parameterType,
                                                    out keyType,
                                                    out parameterName,
                                                    out parameterTypeId,
                                                    out keyTypeId,
                                                    out collectionType,
                                                    out numberOfValues);
            Check(parameterType == MemberType.StringMemberType);
            Check(keyType == MemberType.Int32MemberType);
            Check(parameterName == "Int32StringParameter");
            Check(collectionType == CollectionType.DictionaryCollectionType);
            Check(numberOfValues == 2);

            Safir.Dob.Typesystem.Parameters.GetInfo(DotsTest.ParameterDictionaries.ClassTypeId,
                                                        Safir.Dob.Typesystem.Parameters.GetIndex(DotsTest.ParameterDictionaries.ClassTypeId,
                                                                                                     "StringEnumParameter"),
                                                    out parameterType,
                                                    out keyType,
                                                    out parameterName,
                                                    out parameterTypeId,
                                                    out keyTypeId,
                                                    out collectionType,
                                                    out numberOfValues);
            Check(parameterType == MemberType.EnumerationMemberType);
            Check(keyType == MemberType.StringMemberType);
            Check(parameterName == "StringEnumParameter");
            Check(collectionType == CollectionType.DictionaryCollectionType);
            Check(parameterTypeId == DotsTest.TestEnum.EnumerationId);
            Check(numberOfValues == 2);

            Safir.Dob.Typesystem.Parameters.GetInfo(DotsTest.ParameterDictionaries.ClassTypeId,
                                                        Safir.Dob.Typesystem.Parameters.GetIndex(DotsTest.ParameterDictionaries.ClassTypeId,
                                                                                                     "EnumObjectParameter"),
                                                    out parameterType,
                                                    out keyType,
                                                    out parameterName,
                                                    out parameterTypeId,
                                                    out keyTypeId,
                                                    out collectionType,
                                                    out numberOfValues);
            Check(parameterType == MemberType.ObjectMemberType);
            Check(keyType == MemberType.EnumerationMemberType);
            Check(parameterName == "EnumObjectParameter");
            Check(collectionType == CollectionType.DictionaryCollectionType);
            Check(parameterTypeId == Safir.Dob.Typesystem.Object.ClassTypeId);
            Check(keyTypeId == DotsTest.TestEnum.EnumerationId);
            Check(numberOfValues == 2);


            Safir.Dob.Typesystem.Parameters.GetInfo(DotsTest.ParameterTypes.ClassTypeId,
                                                        Safir.Dob.Typesystem.Parameters.GetIndex(DotsTest.ParameterTypes.ClassTypeId,
                                                                                                     "EnumerationParameter"),
                                                    out parameterType,
                                                    out keyType,
                                                    out parameterName,
                                                    out parameterTypeId,
                                                    out keyTypeId,
                                                    out collectionType,
                                                    out numberOfValues);
            Check(parameterType == MemberType.EnumerationMemberType);
            Check(parameterName == "EnumerationParameter");
            Check(collectionType == CollectionType.SingleValueCollectionType);
            Check(parameterTypeId == DotsTest.TestEnum.EnumerationId);
            Check(numberOfValues == 1);
        }

        private void Test_ParameterDictionaryReflection()
        {
            Check(2 == DotsTest.ParameterDictionaries.Int32StringParameterDictionarySize());
            Check(10 == DotsTest.ParameterDictionaries.Int32StringParameterKeyFromIndex(0));
            Check("Safir" == DotsTest.ParameterDictionaries.Int32StringParameterValueFromIndex(0));
            Check(20 == DotsTest.ParameterDictionaries.Int32StringParameterKeyFromIndex(1));
            Check("rifaS" == DotsTest.ParameterDictionaries.Int32StringParameterValueFromIndex(1));

            Check(2 == DotsTest.ParameterDictionaries.Int32Float64ParameterDictionarySize());
            Check(10 == DotsTest.ParameterDictionaries.Int32Float64ParameterKeyFromIndex(0));
            Check(64.64 == DotsTest.ParameterDictionaries.Int32Float64ParameterValueFromIndex(0));
            Check(20 == DotsTest.ParameterDictionaries.Int32Float64ParameterKeyFromIndex(1));
            Check(-64.64 == DotsTest.ParameterDictionaries.Int32Float64ParameterValueFromIndex(1));

            Check(2 == DotsTest.ParameterDictionaries.Int32Ampere64ParameterDictionarySize());
            Check(10 == DotsTest.ParameterDictionaries.Int32Ampere64ParameterKeyFromIndex(0));
            Check(64.64 == DotsTest.ParameterDictionaries.Int32Ampere64ParameterValueFromIndex(0));
            Check(20 == DotsTest.ParameterDictionaries.Int32Ampere64ParameterKeyFromIndex(1));
            Check(-64.64 == DotsTest.ParameterDictionaries.Int32Ampere64ParameterValueFromIndex(1));

            Check(2 == DotsTest.ParameterDictionaries.StringEnumParameterDictionarySize());
            Check("Billy" == DotsTest.ParameterDictionaries.StringEnumParameterKeyFromIndex(0));
            Check(TestEnum.Enumeration.MyFirst == DotsTest.ParameterDictionaries.StringEnumParameterValueFromIndex(0));
            Check("Svarre" == DotsTest.ParameterDictionaries.StringEnumParameterKeyFromIndex(1));
            Check(TestEnum.Enumeration.MySecond == DotsTest.ParameterDictionaries.StringEnumParameterValueFromIndex(1));

            Check(2 == DotsTest.ParameterDictionaries.EnumObjectParameterDictionarySize());
            Check(TestEnum.Enumeration.MyFirst == DotsTest.ParameterDictionaries.EnumObjectParameterKeyFromIndex(0));
            Check(null != DotsTest.ParameterDictionaries.EnumObjectParameterValueFromIndex(0));
            Check(Safir.Dob.Typesystem.Object.ClassTypeId == DotsTest.ParameterDictionaries.EnumObjectParameterValueFromIndex(0).GetTypeId());
            Check(TestEnum.Enumeration.MySecond == DotsTest.ParameterDictionaries.EnumObjectParameterKeyFromIndex(1));
            Check(null != (DotsTest.MemberDictionaries)DotsTest.ParameterDictionaries.EnumObjectParameterValueFromIndex(1));
        }
    }

    public class MergeChangesTests
    {
        private static int tests = 0;
        private static int failures = 0;
        private static void Check(bool expr, string desc = "")
        {
            ++tests;
            if (!expr) {
                ++failures;
                if (desc.Length == 0)
                {
                    Console.WriteLine("Testcase " + tests + " failed!");
                }
                else
                {
                    Console.WriteLine("Testcase " + tests + " (" + desc + ") failed!");
                }
            }
        }

        private static void CheckEqual(int a, int b)
        {
            if (a != b)
            {
                Console.WriteLine("Failed test: {0} != {1}", a,b);
            }
            Check(a==b);
        }

        private static void CheckEqual(long a, long b)
        {
            if (a != b)
            {
                Console.WriteLine("Failed test: {0} != {1}", a,b);
            }
            Check(a==b);
        }
        private static void CheckEqual(string a, string b)
        {
            if (a != b)
            {
                Console.WriteLine("Failed test: {0} != {1}", a,b);
            }
            Check(a==b);
        }

        private static void CheckEqual(TestEnum.Enumeration a, TestEnum.Enumeration b)
        {
            if (a != b)
            {
                Console.WriteLine("Failed test: {0} != {1}", a,b);
            }
            Check(a==b);
        }

        delegate void ThrowingFunction();

        private static void CheckThrow<T>(ThrowingFunction fn) where T:System.Exception
        {
            bool caught = false;
            try
            {
                fn();
            }
            catch(T)
            {
                caught = true;
            }
            Check(caught);
        }

        private void Simple()
        {
            var from = new MemberTypes();
            var into = new MemberTypes();

            from.Int32Member.Val = 10;
            from.Int64Member.Val = 20;
            from.Int64Member.SetChanged(false);

            from.StringMember.Val = "asdf";
            from.EnumerationMember.Val = TestEnum.Enumeration.MyFirst;

            Utilities.MergeChanges(into,from);

            Check(into.Int32Member.IsChanged());
            CheckEqual(into.Int32Member.Val,10);
            Check(!into.Int64Member.IsChanged());
            Check(into.Int64Member.IsNull());
            Check(into.StringMember.Val == "asdf");
            CheckEqual(into.EnumerationMember.Val, TestEnum.Enumeration.MyFirst);
        }


        private void Arrays()
        {
            var from = new MemberArrays();
            var into = new MemberArrays();

            from.Int32Member[0].Val = 10;
            from.Int64Member[0].Val = 20;
            from.Int64Member[0].SetChanged(false);

            from.StringMember[0].Val = "asdf";
            from.StringMember[1].Val = "asdfasdf";
            from.StringMember[1].SetChanged(false);
            from.EnumerationMember[0].Val = TestEnum.Enumeration.MyFirst;
            from.EnumerationMember[1].Val = TestEnum.Enumeration.MySecond;
            from.EnumerationMember[1].SetChanged(false);

            Utilities.MergeChanges(into,from);

            Check(into.Int32Member[0].IsChanged());
            CheckEqual(into.Int32Member[0].Val,10);
            Check(!into.Int64Member.IsChanged());
            Check(into.Int64Member[0].IsNull());
            Check(into.StringMember[0].Val == "asdf");
            Check(into.StringMember[1].IsNull());
            CheckEqual(into.EnumerationMember[0].Val, TestEnum.Enumeration.MyFirst);
            Check(into.EnumerationMember[1].IsNull());
        }

        private void Objects()
        {
            var from = new MemberTypes();
            var into = new MemberTypes();

            from.TestClassMember.Obj = new TestItem();
            from.TestClassMember.Obj.MyInt.Val = 10;
            from.TestClassMember.SetChangedHere(false);
            from.ObjectMember.Obj = new TestItem();
            ((TestItem)from.ObjectMember.Obj).MyInt.Val = 20;
            from.ObjectMember.SetChangedHere(false);

            Utilities.MergeChanges(into,from);

            CheckEqual(into.TestClassMember.Obj.MyInt.Val,10);
            Check(into.TestClassMember.Obj.MyInt.IsChanged());
            Check(into.TestClassMember.IsChanged());
            Check(into.TestClassMember.IsChangedHere());
            CheckEqual(((TestItem)into.ObjectMember.Obj).MyInt.Val,20);
        }

        private void Objects_BothHaveData()
        {
            var from = new MemberTypes();

            from.TestClassMember.Obj = new TestItem();
            from.ObjectMember.Obj = new TestItem();
            from.SetChanged(false);

            var into = from.Clone();

            from.TestClassMember.Obj.MyInt.Val = 10;
            from.TestClassMember.SetChangedHere(false);
            ((TestItem)from.ObjectMember.Obj).MyInt.Val = 20;
            from.ObjectMember.SetChangedHere(false);

            Utilities.MergeChanges(into,from);

            CheckEqual(into.TestClassMember.Obj.MyInt.Val,10);
            Check(into.TestClassMember.Obj.MyInt.IsChanged());
            Check(into.TestClassMember.IsChanged());
            Check(!into.TestClassMember.IsChangedHere());
            CheckEqual(((TestItem)into.ObjectMember.Obj).MyInt.Val,20);
            Check(!into.ObjectMember.IsChangedHere());
        }

        private void Sequences()
        {
            var from = new MemberSequences();
            var into = new MemberSequences();

            into.Int32Member.Add(20);
            into.SetChanged(false);

            from.Int32Member.Add(10);
            from.Int64Member.Add(20);
            from.Int64Member.SetChanged(false);

            from.StringMember.Add("asdf");

            from.EnumerationMember.Add(TestEnum.Enumeration.MyFirst);
            from.EnumerationMember.SetChanged(false);

            Utilities.MergeChanges(into,from);

            Check(into.Int32Member.IsChanged());
            CheckEqual(into.Int32Member[0],10);
            Check(!into.Int64Member.IsChanged());
            Check(into.Int64Member.Count == 0);
            Check(into.StringMember[0] == "asdf");
            Check(into.EnumerationMember.Count == 0);
        }

        private void Dictionaries_IntoEmpty()
        {
            var from = new MemberDictionaries();
            var into = new MemberDictionaries();

            from.Int32Int32Member.Add(10,10);
            from.Int32Int32Member.Add(20,20);

            from.Int32Int64Member.Add(10,10);
            from.Int32Int64Member.Add(20,20);
            from.Int32Int64Member[20].SetChanged(false);

            from.Int64Int64Member.Add(10,10);
            from.Int64Int64Member.Add(20,20);
            from.Int64Int64Member.SetChanged(false);

            Utilities.MergeChanges(into,from);

            Check(into.Int32Int32Member.IsChanged());
            Check(into.Int32Int32Member.IsChangedHere());
            Check(into.Int32Int32Member[10].IsChanged());
            CheckEqual(into.Int32Int32Member[10].Val,10);
            Check(into.Int32Int32Member[20].IsChanged());
            CheckEqual(into.Int32Int32Member[20].Val,20);

            Check(into.Int32Int64Member.IsChanged());
            Check(into.Int32Int64Member.IsChangedHere());
            Check(into.Int32Int64Member[10].IsChanged());
            CheckEqual(into.Int32Int64Member[10].Val,10);
            Check(!into.Int32Int64Member[20].IsChanged());
            CheckEqual(into.Int32Int64Member[20].Val,20);

            Check(!into.Int64Int32Member.IsChanged());
            CheckEqual(into.Int64Int32Member.Count, 0);
        }

        private void Dictionaries_IntoEmpty_Error()
        {
            var from = new MemberDictionaries();
            var into = new MemberDictionaries();

            from.Int64Int32Member.Add(10,10);
            from.Int64Int32Member.Add(20,20);
            from.Int64Int32Member.SetChangedHere(false);

            CheckThrow<SoftwareViolationException>(() => Utilities.MergeChanges(into,from));
        }

        private void Dictionaries_IntoNonEmpty_1()
        {
            var into = new MemberDictionaries();
            into.Int32Int32Member.Add(1,10);
            into.Int32Int32Member.Add(2,20);

            into.Int32Int64Member.Add(1,10);
            into.Int32Int64Member.Add(2,20);

            into.Int64Int32Member.Add(1,10);
            into.Int64Int32Member.Add(2,20);

            into.Int64Int64Member.Add(1,10);
            into.Int64Int64Member.Add(2,20);

            into.SetChanged(false);

            var from = new MemberDictionaries();

            from.Int32Int32Member.Add(3,30);

            from.Int32Int64Member.Add(1,100);
            from.Int32Int64Member.Add(2,20);
            from.Int32Int64Member[2].SetChanged(false);
            from.Int32Int64Member.SetChangedHere(false);

            from.Int64Int32Member.Add(1,100);
            from.Int64Int32Member.Add(2,555);
            from.Int64Int32Member[2].SetChanged(false);
            from.Int64Int32Member.SetChangedHere(false);

            from.Int64Int64Member.Add(1,100);
            from.Int64Int64Member.SetChangedHere(false);

            Utilities.MergeChanges(into,from);

            Check(into.Int32Int32Member.IsChanged());
            Check(into.Int32Int32Member.IsChangedHere());
            CheckEqual(into.Int32Int32Member.Count, 1);
            Check(into.Int32Int32Member[3].IsChanged());
            CheckEqual(into.Int32Int32Member[3].Val,30);

            Check(into.Int32Int64Member.IsChanged());
            Check(!into.Int32Int64Member.IsChangedHere());
            CheckEqual(into.Int32Int64Member.Count, 2);
            Check(into.Int32Int64Member[1].IsChanged());
            CheckEqual(into.Int32Int64Member[1].Val,100);
            Check(!into.Int32Int64Member[2].IsChanged());
            CheckEqual(into.Int32Int64Member[2].Val,20);

            Check(into.Int64Int32Member.IsChanged());
            Check(!into.Int64Int32Member.IsChangedHere());
            CheckEqual(into.Int64Int32Member.Count, 2);
            Check(into.Int64Int32Member[1].IsChanged());
            CheckEqual(into.Int64Int32Member[1].Val,100);
            Check(!into.Int64Int32Member[2].IsChanged());
            CheckEqual(into.Int64Int32Member[2].Val,20);

            Check(into.Int64Int64Member.IsChanged());
            Check(!into.Int64Int64Member.IsChangedHere());
            CheckEqual(into.Int64Int64Member.Count, 2);
            Check(into.Int64Int64Member[1].IsChanged());
            CheckEqual(into.Int64Int64Member[1].Val,100);
            Check(!into.Int64Int64Member[2].IsChanged());
            CheckEqual(into.Int64Int64Member[2].Val,20);

        }


        private void Dictionaries_IntoNonEmpty_2()
        {
            var into = new MemberDictionaries();
            into.Int32Int32Member.Add(1,10);
            into.Int32Int32Member.Add(2,20);

            into.Int32Int64Member.Add(1,10);
            into.Int32Int64Member.Add(2,20);

            into.Int64Int32Member.Add(1,10);
            into.Int64Int32Member.Add(2,20);

            into.Int64Int64Member.Add(1,10);
            into.Int64Int64Member.Add(2,20);

            into.SetChanged(false);

            var from = new MemberDictionaries();

            from.Int32Int32Member.Add(1,10);
            from.Int32Int32Member[1].SetChanged(false);

            from.Int32Int64Member.Add(1,100);

            from.Int64Int32Member.Add(1,100);
            from.Int64Int32Member[1].SetChanged(false);

            from.Int64Int64Member.Add(1,100);
            from.Int64Int64Member.SetChanged(false);

            Utilities.MergeChanges(into,from);

            Check(into.Int32Int32Member.IsChanged());
            Check(into.Int32Int32Member.IsChangedHere());
            CheckEqual(into.Int32Int32Member.Count, 1);
            Check(!into.Int32Int32Member[1].IsChanged());
            CheckEqual(into.Int32Int32Member[1].Val,10);

            Check(into.Int32Int64Member.IsChanged());
            Check(into.Int32Int64Member.IsChangedHere());
            CheckEqual(into.Int32Int64Member.Count, 1);
            Check(into.Int32Int64Member[1].IsChanged());
            CheckEqual(into.Int32Int64Member[1].Val,100);

            Check(into.Int64Int32Member.IsChanged());
            Check(into.Int64Int32Member.IsChangedHere());
            CheckEqual(into.Int64Int32Member.Count, 1);
            Check(!into.Int64Int32Member[1].IsChanged());
            CheckEqual(into.Int64Int32Member[1].Val,100);

            Check(!into.Int64Int64Member.IsChanged());
            CheckEqual(into.Int64Int64Member.Count, 2);
            CheckEqual(into.Int64Int64Member[1].Val,10);
            CheckEqual(into.Int64Int64Member[2].Val,20);
        }



        private void Dictionaries_IntoNonEmpty_Error()
        {
            var into = new MemberDictionaries();
            into.Int32Int32Member.Add(1,10);
            into.Int32Int32Member.Add(2,20);
            into.SetChanged(false);

            var from = new MemberDictionaries();

            from.Int32Int32Member.Add(3,30);
            from.Int32Int32Member.SetChangedHere(false);

            CheckThrow<SoftwareViolationException>(() => Utilities.MergeChanges(into,from));
        }

        private void ObjectDictionaries_IntoEmpty_1()
        {
            var from = new MemberDictionaries();
            var into = new MemberDictionaries();

            //empty + x[x1:(xM=10)] => x[x1:(xM=10)]
            from.Int64ItemMember.Add(1, new TestItem());
            from.Int64ItemMember[1].Obj.MyInt.Val = 10;

            //empty + x[1:(xM=10)] => x[1:(xM=10)]
            from.TypeIdItemMember.Add(1, new TestItem());
            from.TypeIdItemMember[1].Obj.MyInt.Val = 20;
            from.TypeIdItemMember[1].SetChangedHere(false);

            //empty + x[x1:(M=10)] => x[x1:(M=10)]
            from.EnumItemMember.Add(TestEnum.Enumeration.MySecond, new TestItem());
            from.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyInt.Val = 30;
            from.EnumItemMember[TestEnum.Enumeration.MySecond].SetChanged(false);

            Utilities.MergeChanges(into,from);

            Check(into.Int64ItemMember.IsChangedHere());
            Check(into.Int64ItemMember[1].IsChangedHere());
            Check(into.Int64ItemMember[1].Obj.MyInt.IsChanged());
            CheckEqual(into.Int64ItemMember[1].Obj.MyInt.Val,10);

            Check(into.TypeIdItemMember.IsChangedHere());
            Check(!into.TypeIdItemMember[1].IsChangedHere());
            Check(into.TypeIdItemMember[1].Obj.MyInt.IsChanged());
            CheckEqual(into.TypeIdItemMember[1].Obj.MyInt.Val,20);

            Check(into.EnumItemMember.IsChangedHere());
            Check(!into.EnumItemMember[TestEnum.Enumeration.MySecond].IsChangedHere());
            Check(!into.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyInt.IsChanged());
            CheckEqual(into.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyInt.Val,30);
        }

        private void ObjectDictionaries_IntoEmpty_Error_1()
        {
            var from = new MemberDictionaries();
            var into = new MemberDictionaries();

            //empty + [x1:(xM=10)] => error
            from.Int64ItemMember.Add(1, new TestItem());
            from.Int64ItemMember[1].Obj.MyInt.Val = 10;
            from.Int64ItemMember.SetChangedHere(false);

            CheckThrow<SoftwareViolationException>(() => Utilities.MergeChanges(into,from));
        }

        private void ObjectDictionaries_IntoEmpty_Error_2()
        {
            var from = new MemberDictionaries();
            var into = new MemberDictionaries();

            //empty + [x1:(M=10)] => error
            from.Int64ItemMember.Add(1, new TestItem());
            from.Int64ItemMember[1].Obj.MyInt.Val = 10;
            from.Int64ItemMember.SetChangedHere(false);
            from.Int64ItemMember[1].Obj.MyInt.SetChanged(false);

            CheckThrow<SoftwareViolationException>(() => Utilities.MergeChanges(into,from));
        }

        private void ObjectDictionaries_IntoEmpty_Error_3()
        {
            var from = new MemberDictionaries();
            var into = new MemberDictionaries();

            //empty + [1:(xM=10)] => error
            from.Int64ItemMember.Add(1, new TestItem());
            from.Int64ItemMember[1].Obj.MyInt.Val = 10;
            from.Int64ItemMember.SetChangedHere(false);
            from.Int64ItemMember[1].SetChangedHere(false);

            CheckThrow<SoftwareViolationException>(() => Utilities.MergeChanges(into,from));
        }

        private void ObjectDictionaries_IntoEmpty_2()
        {
            var from = new MemberDictionaries();
            var into = new MemberDictionaries();

            //empty + [1:(M=10)] => empty
            from.Int64ItemMember.Add(1, new TestItem());
            from.Int64ItemMember[1].Obj.MyInt.Val = 10;
            from.Int64ItemMember.SetChanged(false);

            Utilities.MergeChanges(into,from);

            Check(!into.Int64ItemMember.IsChangedHere());
            CheckEqual(into.Int64ItemMember.Count, 0);
        }

        private abstract class NonEmptyObjectDictionariesFixture
        {
            protected MemberDictionaries into = new MemberDictionaries();
            protected MemberDictionaries from = new MemberDictionaries();

            protected NonEmptyObjectDictionariesFixture()
            {
                into.Int64ItemMember.Add(1, new TestItem());
                into.Int64ItemMember[1].Obj.MyInt.Val = 10;
                into.Int64ItemMember[1].Obj.MyString.Val = "one";
                into.Int64ItemMember.Add(2, new TestItem());
                into.Int64ItemMember[2].Obj.MyInt.Val = 20;
                into.Int64ItemMember[2].Obj.MyString.Val = "two";

                into.TypeIdItemMember.Add(1, new TestItem());
                into.TypeIdItemMember[1].Obj.MyInt.Val = 10;
                into.TypeIdItemMember[1].Obj.MyString.Val = "one";
                into.TypeIdItemMember.Add(2, new TestItem());
                into.TypeIdItemMember[2].Obj.MyInt.Val = 20;
                into.TypeIdItemMember[2].Obj.MyString.Val = "two";

                into.EnumItemMember.Add(TestEnum.Enumeration.MyFirst, new TestItem());
                into.EnumItemMember[TestEnum.Enumeration.MyFirst].Obj.MyInt.Val = 10;
                into.EnumItemMember[TestEnum.Enumeration.MyFirst].Obj.MyString.Val = "one";
                into.EnumItemMember.Add(TestEnum.Enumeration.MySecond, new TestItem());
                into.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyInt.Val = 20;
                into.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyString.Val = "two";

                into.SetChanged(false);
            }
        };

        private class ObjectDictionaries_IntoNonEmpty_1: NonEmptyObjectDictionariesFixture
        {

            public ObjectDictionaries_IntoNonEmpty_1()
            {
                //[1:(M=10,S=one),2:(M=20,S=two)] + x[x1:(xM=100)] => x[x1:(xM=100)]
                from.Int64ItemMember.Add(1, new TestItem());
                from.Int64ItemMember[1].Obj.MyInt.Val = 100;

                //[1:(M=10,S=one),2:(M=20,S=two)] + x[ 1:(xM=200)] => x[ 1:(xM=200)]
                from.TypeIdItemMember.Add(1, new TestItem());
                from.TypeIdItemMember[1].Obj.MyInt.Val = 200;
                from.TypeIdItemMember[1].SetChangedHere(false);

                //[1:(M=10,S=one),2:(M=20,S=two)] + x[ 1:( M=300)] => x[ 1:( M=300)]
                from.EnumItemMember.Add(TestEnum.Enumeration.MySecond, new TestItem());
                from.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyInt.Val = 300;
                from.EnumItemMember[TestEnum.Enumeration.MySecond].SetChanged(false);
                Utilities.MergeChanges(into,from);

                CheckEqual(into.Int64ItemMember.Count, 1);
                Check(into.Int64ItemMember.IsChangedHere());
                Check(into.Int64ItemMember[1].IsChangedHere());
                Check(into.Int64ItemMember[1].Obj.MyInt.IsChanged());
                CheckEqual(into.Int64ItemMember[1].Obj.MyInt.Val,100);
                Check(into.Int64ItemMember[1].Obj.MyString.IsNull());

                CheckEqual(into.TypeIdItemMember.Count, 1);
                Check(into.TypeIdItemMember.IsChangedHere());
                Check(!into.TypeIdItemMember[1].IsChangedHere());
                Check(into.TypeIdItemMember[1].Obj.MyInt.IsChanged());
                CheckEqual(into.TypeIdItemMember[1].Obj.MyInt.Val,200);
                Check(into.TypeIdItemMember[1].Obj.MyString.IsNull());

                CheckEqual(into.EnumItemMember.Count, 1);
                Check(into.EnumItemMember.IsChangedHere());
                Check(!into.EnumItemMember[TestEnum.Enumeration.MySecond].IsChangedHere());
                Check(!into.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyInt.IsChanged());
                CheckEqual(into.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyInt.Val,300);
                Check(into.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyString.IsNull());

            }
        };

        private class ObjectDictionaries_IntoNonEmpty_2: NonEmptyObjectDictionariesFixture
        {

            public ObjectDictionaries_IntoNonEmpty_2()
            {
                //[1:(M=10,S=one),2:(M=20,S=two)] + x[ 3:( M=300)] => x[ 3:( M=300)]
                from.Int64ItemMember.Add(3, new TestItem());
                from.Int64ItemMember[3].Obj.MyInt.Val = 300;
                from.Int64ItemMember[3].SetChanged(false);

                //[1:(M=10,S=one),2:(M=20,S=two)] +  [x2:(xM=200)] =>  [ 1:( M=10, S=one),x2:(xM:200)]
                from.TypeIdItemMember.Add(2, new TestItem());
                from.TypeIdItemMember[2].Obj.MyInt.Val = 200;
                from.TypeIdItemMember.SetChangedHere(false);

                //[1:(M=10,S=one),2:(M=20,S=two)] +  [ 2:(xM=300)] =>  [ 1:( M=10, S=one), 2:(xM:300, S=two)]
                from.EnumItemMember.Add(TestEnum.Enumeration.MySecond, new TestItem());
                from.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyInt.Val = 300;
                from.EnumItemMember.SetChangedHere(false);
                from.EnumItemMember[TestEnum.Enumeration.MySecond].SetChangedHere(false);

                Utilities.MergeChanges(into,from);

                CheckEqual(into.Int64ItemMember.Count, 1);
                Check(into.Int64ItemMember.IsChangedHere());
                Check(!into.Int64ItemMember[3].IsChangedHere());
                Check(!into.Int64ItemMember[3].Obj.MyInt.IsChanged());
                CheckEqual(into.Int64ItemMember[3].Obj.MyInt.Val,300);
                Check(into.Int64ItemMember[3].Obj.MyString.IsNull());

                CheckEqual(into.TypeIdItemMember.Count, 2);
                Check(!into.TypeIdItemMember.IsChangedHere());
                Check(!into.TypeIdItemMember[1].IsChanged());
                CheckEqual(into.TypeIdItemMember[1].Obj.MyInt.Val,10);
                Check(into.TypeIdItemMember[1].Obj.MyString.Val == "one");
                Check(into.TypeIdItemMember[2].IsChangedHere());
                Check(into.TypeIdItemMember[2].Obj.MyInt.IsChanged());
                CheckEqual(into.TypeIdItemMember[2].Obj.MyInt.Val,200);
                Check(!into.TypeIdItemMember[2].Obj.MyString.IsChanged());
                Check(into.TypeIdItemMember[2].Obj.MyString.IsNull());

                CheckEqual(into.EnumItemMember.Count, 2);
                Check(!into.EnumItemMember.IsChangedHere());
                Check(!into.EnumItemMember[TestEnum.Enumeration.MyFirst].IsChanged());
                CheckEqual(into.EnumItemMember[TestEnum.Enumeration.MyFirst].Obj.MyInt.Val,10);
                Check(into.EnumItemMember[TestEnum.Enumeration.MyFirst].Obj.MyString.Val == "one");
                Check(!into.EnumItemMember[TestEnum.Enumeration.MySecond].IsChangedHere());
                Check(into.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyInt.IsChanged());
                CheckEqual(into.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyInt.Val,300);
                Check(into.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyString.Val == "two");
                Check(!into.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyString.IsChanged());
            }
        };

        private class ObjectDictionaries_IntoNonEmpty_3: NonEmptyObjectDictionariesFixture
        {
            public ObjectDictionaries_IntoNonEmpty_3()
            {
                //[1:(M=10,S=one),2:(M=20,S=two)] +  [x2:( M=200)] =>  [ 1:( M=10, S=one),x2:( M:200)]
                from.Int64ItemMember.Add(2, new TestItem());
                from.Int64ItemMember[2].Obj.MyInt.Val = 200;
                from.Int64ItemMember.SetChangedHere(false);
                from.Int64ItemMember[2].Obj.MyInt.SetChanged(false);

                //[1:(M=10,S=one),2:(M=20,S=two)] +  [ 3:( M=300)] =>  [ 1:( M=10, S=one), 2:( M:20, S=two)]
                from.TypeIdItemMember.Add(3, new TestItem());
                from.TypeIdItemMember[3].Obj.MyInt.Val = 300;
                from.TypeIdItemMember.SetChanged(false);

                //[1:(M=10,S=one),2:(M=20,S=two)] +  [ 2:( M=200)] =>  [ 1:( M=10, S=one), 2:( M:20, S=two)]
                from.EnumItemMember.Add(TestEnum.Enumeration.MySecond, new TestItem());
                from.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyInt.Val = 200;
                from.EnumItemMember.SetChanged(false);

                Utilities.MergeChanges(into,from);
                CheckEqual(into.Int64ItemMember.Count, 2);
                Check(!into.Int64ItemMember.IsChangedHere());
                Check(!into.Int64ItemMember[1].IsChanged());
                CheckEqual(into.Int64ItemMember[1].Obj.MyInt.Val,10);
                CheckEqual(into.Int64ItemMember[1].Obj.MyString.Val,"one");
                Check(into.Int64ItemMember[2].IsChangedHere());
                Check(!into.Int64ItemMember[2].Obj.MyInt.IsChanged());
                CheckEqual(into.Int64ItemMember[2].Obj.MyInt.Val,200);
                Check(into.Int64ItemMember[2].Obj.MyString.IsNull());

                CheckEqual(into.TypeIdItemMember.Count, 2);
                Check(!into.TypeIdItemMember.IsChanged());
                CheckEqual(into.TypeIdItemMember[1].Obj.MyInt.Val,10);
                CheckEqual(into.TypeIdItemMember[1].Obj.MyString.Val,"one");
                CheckEqual(into.TypeIdItemMember[2].Obj.MyInt.Val,20);
                CheckEqual(into.TypeIdItemMember[2].Obj.MyString.Val,"two");

                CheckEqual(into.EnumItemMember.Count, 2);
                Check(!into.EnumItemMember.IsChanged());
                CheckEqual(into.EnumItemMember[TestEnum.Enumeration.MyFirst].Obj.MyInt.Val,10);
                CheckEqual(into.EnumItemMember[TestEnum.Enumeration.MyFirst].Obj.MyString.Val,"one");
                CheckEqual(into.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyInt.Val,20);
                CheckEqual(into.EnumItemMember[TestEnum.Enumeration.MySecond].Obj.MyString.Val,"two");
            }
        };

        private class ObjectDictionaries_IntoNonEmpty_Error_1: NonEmptyObjectDictionariesFixture
        {
            public ObjectDictionaries_IntoNonEmpty_Error_1()
            {
                //[1:(M=10,S=one),2:(M=20,S=two)] +  [x3:( M=300)] =>  error
                from.Int64ItemMember.Add(3, new TestItem());
                from.Int64ItemMember[3].Obj.MyInt.Val = 300;
                from.Int64ItemMember.SetChanged(false);
                from.Int64ItemMember[3].SetChangedHere(true);

                CheckThrow<SoftwareViolationException>(() => Utilities.MergeChanges(into,from));
            }
        };

        private class ObjectDictionaries_IntoNonEmpty_Error_2: NonEmptyObjectDictionariesFixture
        {
            public ObjectDictionaries_IntoNonEmpty_Error_2()
            {
                //[1:(M=10,S=one),2:(M=20,S=two)] +  [ 3:(xM=300)] =>  error
                from.Int64ItemMember.Add(3, new TestItem());
                from.Int64ItemMember[3].Obj.MyInt.Val = 300;
                from.Int64ItemMember.SetChanged(false);
                from.Int64ItemMember[3].Obj.MyInt.SetChanged(true);

                CheckThrow<SoftwareViolationException>(() => Utilities.MergeChanges(into,from));
            }
        };

        private class ObjectDictionaries_EmptyIntoNonEmpty: NonEmptyObjectDictionariesFixture
        {
            public ObjectDictionaries_EmptyIntoNonEmpty()
            {
                //[1:(M=10,S=one),2:(M=20,S=two)] +  x[] =>  x[]
                from.Int64ItemMember.SetChangedHere(true);

                //[1:(M=10,S=one),2:(M=20,S=two)] +  [] =>  [1:(M=10,S=one),2:(M:20,S=two)]
                //nothing to do...

                Utilities.MergeChanges(into,from);
                CheckEqual(into.Int64ItemMember.Count, 0);
                Check(into.Int64ItemMember.IsChangedHere());

                CheckEqual(into.TypeIdItemMember.Count, 2);
                Check(!into.TypeIdItemMember.IsChanged());
                CheckEqual(into.TypeIdItemMember[1].Obj.MyInt.Val,10);
                CheckEqual(into.TypeIdItemMember[1].Obj.MyString.Val,"one");
                CheckEqual(into.TypeIdItemMember[2].Obj.MyInt.Val,20);
                CheckEqual(into.TypeIdItemMember[2].Obj.MyString.Val,"two");
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

        private class ObjectSequences_1: EmptyObjectSequenceFixture
        {
            public ObjectSequences_1()
            {
                //[] + x{(xM=10)} => x{(xM=10)}
                from.TestClassMember.Add(new TestItem());
                from.TestClassMember[0].MyInt.Val = 10;

                Utilities.MergeChanges(into,from);

                CheckEqual(into.TestClassMember.Count,1);
                Check(into.TestClassMember.IsChangedHere());
                Check(into.TestClassMember[0].MyInt.IsChanged());
                CheckEqual(into.TestClassMember[0].MyInt.Val,10);
            }
        };


        private class ObjectSequences_2: EmptyObjectSequenceFixture
        {
            public ObjectSequences_2()
            {
                //[] + {(xM=10)} => ERROR
                from.TestClassMember.Add(new TestItem());
                from.TestClassMember[0].MyInt.Val = 10;
                from.TestClassMember.SetChangedHere(false);

                CheckThrow<SoftwareViolationException>(() => Utilities.MergeChanges(into,from));
            }
        };

        private class ObjectSequences_3: EmptyObjectSequenceFixture
        {
            public ObjectSequences_3()
            {
                //[] + {( M=10)} => []
                from.TestClassMember.Add(new TestItem());
                from.TestClassMember[0].MyInt.Val = 10;
                from.TestClassMember.SetChanged(false);

                Utilities.MergeChanges(into,from);
                CheckEqual(into.TestClassMember.Count,0);
                Check(!into.TestClassMember.IsChanged());
            }
        };

        private class ObjectSequences_4: EmptyObjectSequenceFixture
        {
            public ObjectSequences_4()
            {
                //[] + x[] => x[]
                from.TestClassMember.SetChanged(true);

                Utilities.MergeChanges(into,from);
                CheckEqual(into.TestClassMember.Count,0);
                Check(into.TestClassMember.IsChanged());
            }
        };

        private abstract class NonEmptyObjectSequenceFixture
        {
            protected MemberSequences into = new MemberSequences();
            protected MemberSequences from = new MemberSequences();

            protected NonEmptyObjectSequenceFixture()
            {
                into.TestClassMember.Add(new TestItem());
                into.TestClassMember[0].MyInt.Val = 10;
                into.TestClassMember[0].MyString.Val = "one";
                into.TestClassMember.Add(new TestItem());
                into.TestClassMember[1].MyInt.Val = 20;
                into.TestClassMember[1].MyString.Val = "two";
                into.SetChanged(false);
            }
        };

        private class ObjectSequences_IntoNonEmpty_1: NonEmptyObjectSequenceFixture
        {
            public ObjectSequences_IntoNonEmpty_1()
            {
                //{(M=10,S=one),(M=20,S=two)} + x{(M=30)} => x{(M=30)}
                from.TestClassMember.Add(new TestItem());
                from.TestClassMember[0].MyInt.Val = 30;
                from.TestClassMember[0].MyInt.SetChanged(false);

                Utilities.MergeChanges(into,from);

                CheckEqual(into.TestClassMember.Count,1);
                Check(into.TestClassMember.IsChangedHere());
                Check(!into.TestClassMember[0].MyInt.IsChanged());
                CheckEqual(into.TestClassMember[0].MyInt.Val,30);
            }
        }


        private class ObjectSequences_IntoNonEmpty_2: NonEmptyObjectSequenceFixture
        {
            public ObjectSequences_IntoNonEmpty_2()
            {
                //{(M=10,S=one),(M=20,S=two)} + {(M=30)} => {(M=10,S=one),(M=20,S=two)}
                from.TestClassMember.Add(new TestItem());
                from.TestClassMember[0].MyInt.Val = 30;
                from.TestClassMember.SetChanged(false);

                Utilities.MergeChanges(into,from);

                CheckEqual(into.TestClassMember.Count,2);
                Check(!into.TestClassMember.IsChanged());
                CheckEqual(into.TestClassMember[0].MyInt.Val,10);
                CheckEqual(into.TestClassMember[0].MyString.Val,"one");
                CheckEqual(into.TestClassMember[1].MyInt.Val,20);
                CheckEqual(into.TestClassMember[1].MyString.Val,"two");
            }
        };

        private class ObjectSequences_IntoNonEmpty_3: NonEmptyObjectSequenceFixture
        {
            public ObjectSequences_IntoNonEmpty_3()
            {
                //{(M=10,S=one),(M=20,S=two)} + {(xM=30)} => error??
                from.TestClassMember.Add(new TestItem());
                from.TestClassMember[0].MyInt.Val = 30;
                from.TestClassMember.SetChangedHere(false);

                CheckThrow<SoftwareViolationException>(() => Utilities.MergeChanges(into,from));
            }
        };

        private class ObjectSequences_IntoNonEmpty_4: NonEmptyObjectSequenceFixture
        {
            public ObjectSequences_IntoNonEmpty_4()
            {
                //{(M=10,S=one),(M=20,S=two)} + {(xM=30),()} => {(xM=30,S=one),(M=20,S=two)}
                from.TestClassMember.Add(new TestItem());
                from.TestClassMember.Add(new TestItem());
                from.TestClassMember[0].MyInt.Val = 30;
                from.TestClassMember.SetChangedHere(false);

                Utilities.MergeChanges(into,from);

                CheckEqual(into.TestClassMember.Count,2);
                Check(!into.TestClassMember.IsChangedHere());
                Check(!into.TestClassMember[1].IsChanged());
                Check(into.TestClassMember[0].MyInt.IsChanged());
                Check(!into.TestClassMember[0].MyString.IsChanged());
                CheckEqual(into.TestClassMember[0].MyInt.Val,30);
                CheckEqual(into.TestClassMember[0].MyString.Val,"one");
                CheckEqual(into.TestClassMember[1].MyInt.Val,20);
                CheckEqual(into.TestClassMember[1].MyString.Val,"two");
            }
        };
        private class ObjectSequences_IntoNonEmpty_5: NonEmptyObjectSequenceFixture
        {
            public ObjectSequences_IntoNonEmpty_5()
            {
                //{(M=10,S=one),(M=20,S=two)} + {(xM=30),(S=blahonga)} => {(xM=30,S=one),(M=20,S=two)}
                from.TestClassMember.Add(new TestItem());
                from.TestClassMember.Add(new TestItem());
                from.TestClassMember.SetChangedHere(false);
                from.TestClassMember[0].MyInt.Val = 30;
                from.TestClassMember[1].MyString.Val = "blahonga";
                from.TestClassMember[1].MyString.SetChanged(false);

                Utilities.MergeChanges(into,from);

                CheckEqual(into.TestClassMember.Count,2);
                Check(!into.TestClassMember.IsChangedHere());
                Check(!into.TestClassMember[1].IsChanged());
                Check(into.TestClassMember[0].MyInt.IsChanged());
                Check(!into.TestClassMember[0].MyString.IsChanged());
                CheckEqual(into.TestClassMember[0].MyInt.Val,30);
                CheckEqual(into.TestClassMember[0].MyString.Val,"one");
                CheckEqual(into.TestClassMember[1].MyInt.Val,20);
                CheckEqual(into.TestClassMember[1].MyString.Val,"two");
            }
        };


        private void Test_ParserExceptions() {
            string brokenXml = "<?xml version=\"1.0\" encoding=\"utf-8\"?><DotsTest.MemberSequences><Int32WRONGMember><Int32>10</Int32></Int32Member></DotsTest.MemberSequences>";
            string brokenJson = "{\"_DouType\":\"DotsTest.MemberSequences\",\"Int32WRONGMember\":[10,20]}";

            try {
                Serialization.ToObject(brokenXml);
                Check(false);
            }
            catch (IllegalValueException exc) {
                Check(exc.Message.Contains("does not contain a member named"));
            }

            try {
                Serialization.ToObjectFromJson(brokenJson);
                Check(false);
            }
            catch (IllegalValueException exc) {
                Check(exc.Message.Contains("does not contain a member named"));
            }

            CheckThrow<IllegalValueException>(() => Serialization.ToObject(""));
            CheckThrow<IllegalValueException>(() => Serialization.ToObjectFromJson(""));
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

    private static string Mapping(Safir.Dob.Typesystem.CollectionType ct)
    {
        switch (ct) {
        case Safir.Dob.Typesystem.CollectionType.ArrayCollectionType:
            return "Array";
        case Safir.Dob.Typesystem.CollectionType.DictionaryCollectionType:
            return "Dictionary";
        case Safir.Dob.Typesystem.CollectionType.SequenceCollectionType:
            return "Sequence";
        case Safir.Dob.Typesystem.CollectionType.SingleValueCollectionType:
            return "Single";
        default:
            return "Unknown";
        }
    }
    }

}
