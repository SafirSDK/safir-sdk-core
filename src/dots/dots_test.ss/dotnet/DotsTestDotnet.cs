/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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
        try
        {
            Console.OutputEncoding = new System.Text.UTF8Encoding(false);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo("en-US");

            Test_Has_Property();
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
            TestSequences();
            TestDictionaries();
        }
        finally
        {
            Console.OutputEncoding = System.Text.Encoding.Default;
        }
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;       

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Int32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Int64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Float32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Float64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.BooleanMemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.EnumerationMemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.StringMemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.EntityIdMemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.TypeIdMemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.InstanceIdMemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.ChannelIdMemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.HandlerIdMemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.ObjectMemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.BinaryMemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.TestClassMemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Ampere32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.CubicMeter32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Hertz32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Joule32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Kelvin32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Kilogram32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Meter32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.MeterPerSecond32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.MeterPerSecondSquared32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Newton32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Pascal32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Radian32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.RadianPerSecond32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.RadianPerSecondSquared32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Second32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.SquareMeter32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Steradian32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Volt32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Watt32MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Ampere64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.CubicMeter64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Hertz64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Joule64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Kelvin64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Kilogram64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Meter64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.MeterPerSecond64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.MeterPerSecondSquared64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Newton64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Pascal64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Radian64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.RadianPerSecond64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.RadianPerSecondSquared64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Second64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.SquareMeter64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Steradian64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Volt64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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
        long lCT;
        int lTS;
        Safir.Dob.Typesystem.CollectionType liA;
        int lAL;

        string name = Safir.Dob.Typesystem.Members.GetInfo(DotsTest.MemberArrays.ClassTypeId, DotsTest.MemberArrays.Watt64MemberMemberIndex,
                                             out lMT, out lCT, out lTS, out liA, out lAL);

        Console.WriteLine("----Members---- ");
        Console.WriteLine("GetInfo: " + lMT.ToString().ToUpper() + "," + name + "," + lCT + "," + lTS + "," + CtStr(liA) + "," + lAL);
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

    private static void TestSequences()
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

        Console.WriteLine("------ Clone -----");
        DotsTest.MemberSequences clone=ms as DotsTest.MemberSequences;
        PrintSequences(clone);
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

    private static void TestDictionaries()
    {
        Header("Dictionaries");

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

        Console.WriteLine("------ Clone -----");
        DotsTest.MemberDictionaries clone = md as DotsTest.MemberDictionaries;
        PrintDictionaries(clone);
    }
}
