-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2009-2013 (http://safir.sourceforge.net)
--
--  Created by: Anders Widén / stawi
--
-------------------------------------------------------------------------------
--
--  This file is part of Safir SDK Core.
--
--  Safir SDK Core is free software: you can redistribute it and/or modify
--  it under the terms of version 3 of the GNU General Public License as
--  published by the Free Software Foundation.
--
--  Safir SDK Core is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
--
-------------------------------------------------------------------------------
with Interfaces.C.Strings;
with Safir.Dob.Typesystem.Internal_Defs;
pragma Warnings ("H");  -- turn off warnings for hiding variable

package Safir.Dob.Typesystem.Kernel is
   pragma Preelaborate (Safir.Dob.Typesystem.Kernel);

   type Underlying_Entity_Id_Type is record
      Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Instance  : Safir.Dob.Typesystem.Int_64;
   end record;

   for Underlying_Entity_Id_Type use record
      Type_Id   at 0 range 0 .. 63;
      Instance at 8 range 0 .. 63;
   end record;
   for Underlying_Entity_Id_Type'Size use 16*8;

   type String_Deleter_Cb_Type is access procedure (Ptr : in out C.Strings.chars_ptr);
   pragma Convention (C, String_Deleter_Cb_Type);

   type Blob_Deleter_Cb_Type is access procedure (Ptr : in out Safir.Dob.Typesystem.Blob_T);
   pragma Convention (C, Blob_Deleter_Cb_Type);

   type Type_Id_Arr is array (Safir.Dob.Typesystem.Index range <>) of aliased Safir.Dob.Typesystem.Type_Id;
   package Type_Id_Ptrs is
     new C.Pointers (Index              => Safir.Dob.Typesystem.Index,
                     Element            => Safir.Dob.Typesystem.Type_Id,
                     Element_Array      => Type_Id_Arr,
                     Default_Terminator => 0);

--      //********************************************************
--      //* Base operations on blobs
--      //*
--      //* Warning: Be careful when using these methods. Wrong
--      //*          usage will cause memory leaks.
--      //********************************************************
--      // Function:    DotsC_CreateBlob
--      // Parameters:  typeId -    id of class
--      //              blob - the blob to be created (NULL if typeId was not valid)
--      // Comments:    Initializes blob to default size and members according to the given type id.
--      //              Any already crated blob must be deleted before this function is called.
--      //              No delete on 'blob' is done here.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_CreateBlob(const DotsC_TypeId typeId,
--                                                               char * & blob);
--
--      // Function:    DotsC_DeleteBlob
--      // Parameters:  blob - the blob to be deleted
--      // Returns:     -
--      // Comments:    Deletes a blob. Blobs created within dots_kernel must be deleted there too.
--      //              This is the only method that performs a 'delete blob'. All other methods that
--      //              allocates relies on that this metod is used for deletion.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_DeleteBlob(char * & blob);
--
--      // Function:    DotsC_CreateCopyOfBlob
--      // Parameters:  to - the copy to be created
--      //              from - the original blob
--      // Returns:     -
--      // Comments:    This method will create an exact copy of a blob. The blob 'to' shall not already
--      //              have been created since it will cause memory leaks. This method does not delete
--      //              the blob 'to' before it is allocated.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_CreateCopyOfBlob(char * & to,
--                                                                     const char * const from);
--
   function Get_Type_Id (Blob   : in Safir.Dob.Typesystem.Blob_T)
                         return Safir.Dob.Typesystem.Type_Id;
   pragma Import (C, Get_Type_Id, "DotsC_GetTypeId");

   function Get_Size (Blob : in Safir.Dob.Typesystem.Blob_T)
                     return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Get_Size, "DotsC_GetSize");

--
--      // Function:    DotsC_IsAnythingChanged
--      // Parameters:  blob - the blob
--      // Returns:     true if any member has changed, else false.
--      // Comments:    Returns true if any member in the blob has been changed
--      //              since last call to DotsC_ResetChanged.
--      DOTS_KERNEL_API bool CALLING_CONVENTION DotsC_IsAnythingChanged(const char * const blob);
--
--      // Function:    DotsC_ResetChanged
--      // Parameters:  blob - the blob
--      // Returns:     -
--      // Comments:    Reset changed flags for all members in the blob.
--      //               Note that this function is not recursive
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_ResetChanged(char * const blob);
--
--      /** Recursively set all change flags.
--       *
--       * Recursively set changed flags for all members in the blob.
--       *
--       * @param blob [in,out] - The blob to modify.
--       * @param changed [in] - The value to set all change flags to
--       */
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetChanged(char * const blob, const bool changed);
--
--      /** Set the change flag on one member (non-recursively).
--       *
--       * @param blob [in,out] - The blob to modify.
--       * @param member [in] - id of the member.
--       * @param index [in] - array index of member. Shall be 0 if member is not an array.
--       * @param changed [in] - The value to set change flag to.
--       */
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetChangedHere(char * const blob,
--                                                                   const DotsC_MemberIndex member,
--                                                                   const DotsC_ArrayIndex index,
--                                                                   const bool changed);
--
--      // Function:    DotsC_SetChangedMembers
--      // Parameters:  val -   the blob containing changes.
--      //              blob -  blob to be changed
--      // Returns:     -
--      // Comments:    Apply changes in val on the blob. Note that val and blob must be of exactly the same type.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetChangedMembers(const char * const val,
--                                                                      char * & blob);
--
--      // Function:    DotsC_SetChangedSinceLastRead
--      // Parameters:  current -   the current version of the object.
--      //              lastRead -  last read version of the object.
--      // Returns:     -
--      // Comments:    Set changed flags for all members in current that have been changed since last read object.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetChangedSinceLastRead(const char * const lastRead,
--                                                                            char * const current);
--
   ------------------------------
   -- Type information operations
   ------------------------------
   function Number_Of_Type_Ids return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Number_Of_Type_Ids, "DotsC_NumberOfTypeIds");

   function Number_Of_Classes return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Number_Of_Classes, "DotsC_NumberOfClasses");

   function Number_Of_Properties return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Number_Of_Properties, "DotsC_NumberOfProperties");

   function Number_Of_Enumerations return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Number_Of_Enumerations, "DotsC_NumberOfEnumerations");

--      // Function:    DotsC_NumberOfExceptions
--      // Parameters:  -
--      // Returns:     number of existing exception types
--      // Comments:    Get the number of exception types defined in the system.
--      DOTS_KERNEL_API DotsC_Int32 CALLING_CONVENTION DotsC_NumberOfExceptions();
--

   procedure Get_All_Type_Ids
     (Buf : in Type_Id_Ptrs.Pointer;
      Buf_Size : in Safir.Dob.Typesystem.Int_32;
      Size     : out Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Get_All_Type_Ids, "DotsC_GetAllTypeIds");

   function Type_Exists
     (Type_Id : in  Safir.Dob.Typesystem.Type_Id) return C.char;
   pragma Import (C, Type_Exists, "DotsC_TypeExists");

   function Is_Class
     (Type_Id : in  Safir.Dob.Typesystem.Type_Id) return C.char;
   pragma Import (C, Is_Class, "DotsC_IsClass");

   function Is_Property
     (Type_Id : in  Safir.Dob.Typesystem.Type_Id) return C.char;
   pragma Import (C, Is_Property, "DotsC_IsProperty");

   function Is_Enumeration
     (Type_Id : in  Safir.Dob.Typesystem.Type_Id) return C.char;
   pragma Import (C, Is_Enumeration, "DotsC_IsEnumeration");

   function Is_Exception
     (Type_Id : in  Safir.Dob.Typesystem.Type_Id) return C.char;
   pragma Import (C, Is_Exception, "DotsC_IsException");

--      // Function:    DotsC_IsException
--      // Parameters:  typeId - id of exception type
--      // Returns:     true if the type exists as an exception type
--      // Comments:    Check if type id belongs to a existing enumeration type
--      DOTS_KERNEL_API bool CALLING_CONVENTION DotsC_IsException(const DotsC_TypeId typeId);
--
   function Type_Id_From_Name
     (Type_Name : in C.char_array) return Safir.Dob.Typesystem.Type_Id;
   pragma Import (C, Type_Id_From_Name, "DotsC_TypeIdFromName");

   function Get_Type_Name
     (TypeId : in Safir.Dob.Typesystem.Type_Id) return C.Strings.chars_ptr;
   pragma Import (C, Get_Type_Name, "DotsC_GetTypeName");

   function Get_Number_Of_Enumeration_Values
     (Enumeration_Id : in Safir.Dob.Typesystem.Type_Id) return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Get_Number_Of_Enumeration_Values, "DotsC_GetNumberOfEnumerationValues");

   function Get_Enumeration_Value_Name
     (EnumId  : in Safir.Dob.Typesystem.Type_Id;
      EnumVal : in Safir.Dob.Typesystem.Int_32) return C.Strings.chars_ptr;
   pragma Import (C, Get_Enumeration_Value_Name, "DotsC_GetEnumerationValueName");

   function Enumeration_Value_From_Name
     (EnumId        : in Safir.Dob.Typesystem.Type_Id;
      EnumValueName : in C.char_array) return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Enumeration_Value_From_Name, "DotsC_EnumerationValueFromName");

--
--      //***********************************************************
--      //* Functions for retrieving member info about object types
--      //***********************************************************

      function Get_Number_Of_Members
        (Class_Id : in Safir.Dob.Typesystem.Type_Id) return Safir.Dob.Typesystem.Int_32;
      pragma Import (C, Get_Number_Of_Members, "DotsC_GetNumberOfMembers");

      function Get_Member_Id
        (typeId     : in Safir.Dob.Typesystem.Type_Id;
         memberName : in C.char_array) return Safir.Dob.Typesystem.Int_32;
      pragma Import (C, Get_Member_Id, "DotsC_GetMemberId");

      function Get_Member_Name
        (Type_Id : in Safir.Dob.Typesystem.Type_Id;
         Member : in Safir.Dob.Typesystem.Member_Index)
         return C.Strings.chars_ptr;
      pragma Import (C, Get_Member_Name, "DotsC_GetMemberName");

   function Get_Complex_Member_Type_Id
     (Type_Id : in Safir.Dob.Typesystem.Type_Id;
      Member  : in Safir.Dob.Typesystem.Member_Index)
         return Safir.Dob.Typesystem.Type_Id;
   pragma Import (C, Get_Complex_Member_Type_Id, "DotsC_GetComplexMemberTypeId");

   procedure Get_Member_Info
     (Type_Id       : in Safir.Dob.Typesystem.Type_Id;
      Member        : in Safir.Dob.Typesystem.Member_Index;
      Member_Type   : out Safir.Dob.Typesystem.Member_Type;
      Member_Name   : in out C.Strings.chars_ptr;
      Complex_Type  : out Safir.Dob.Typesystem.Type_Id;
      String_Length : out Safir.Dob.Typesystem.Int_32;
      Is_Array      : out C.char;
      Array_Length  : out Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Get_Member_Info, "DotsC_GetMemberInfo");

   function Get_Member_Array_Size
     (Type_Id : in Safir.Dob.Typesystem.Type_Id;
      Member  : in Safir.Dob.Typesystem.Member_Index) return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Get_Member_Array_Size, "DotsC_GetMemberArraySize");

   function Get_String_Member_Max_Length
     (TypeId : in Safir.Dob.Typesystem.Type_Id;
      Member : in Safir.Dob.Typesystem.Member_Index) return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Get_String_Member_Max_Length, "DotsC_GetStringMemberMaxLength");

   function Get_Member_Array_Size_Property
     (ClassId    : in Safir.Dob.Typesystem.Type_Id;
      PropertyId : in Safir.Dob.Typesystem.Type_Id;
      Member     : in Safir.Dob.Typesystem.Member_Index) return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Get_Member_Array_Size_Property, "DotsC_GetMemberArraySizeProperty");

   function Get_String_Member_Max_Length_Property
     (ClassId    : in Safir.Dob.Typesystem.Type_Id;
      PropertyId : in Safir.Dob.Typesystem.Type_Id;
      Member     : in Safir.Dob.Typesystem.Member_Index) return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Get_String_Member_Max_Length_Property, "DotsC_GetStringMemberMaxLengthProperty");

   function Get_Member_Type_Name
     (Type_Id : in Safir.Dob.Typesystem.Type_Id;
      Member  : in Safir.Dob.Typesystem.Member_Index)
         return C.Strings.chars_ptr;
   pragma Import (C, Get_Member_Type_Name, "DotsC_GetMemberTypeName");

   -----------------------------------------------------------------------
   -- Functions retrieving definitions of parameter values in object types
   -----------------------------------------------------------------------
   function Get_Number_Of_Parameters
     (Type_Id : in Safir.Dob.Typesystem.Type_Id) return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Get_Number_Of_Parameters, "DotsC_GetNumberOfParameters");

   function Get_Parameter_Id
     (Type_Id : in Safir.Dob.Typesystem.Type_Id;
      Name    : in C.char_array) return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Get_Parameter_Id, "DotsC_GetParameterId");

   function Get_Parameter_Name
     (Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Parameter  : in Safir.Dob.Typesystem.Parameter_Index)
         return C.Strings.chars_ptr;
   pragma Import (C, Get_Parameter_Name, "DotsC_GetParameterName");

   function Get_Parameter_Type
     (Type_Id   : in Safir.Dob.Typesystem.Type_Id;
      Parameter : in Safir.Dob.Typesystem.Parameter_Index)
      return Safir.Dob.Typesystem.Member_Type;
   pragma Import (C, Get_Parameter_Type, "DotsC_GetParameterType");

   function Get_Parameter_Type_Name
     (Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Parameter  : in Safir.Dob.Typesystem.Parameter_Index)
         return C.Strings.chars_ptr;
   pragma Import (C, Get_Parameter_Type_Name, "DotsC_GetParameterTypeName");

   function Get_Parameter_Array_Size
     (Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Parameter  : in Safir.Dob.Typesystem.Parameter_Index) return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Get_Parameter_Array_Size, "DotsC_GetParameterArraySize");

   ---------------------
   -- Type compatibility
   ---------------------
   function Is_Of_Type
     (Type_Id   : in Safir.Dob.Typesystem.Type_Id;
      Of_Type_Id : in Safir.Dob.Typesystem.Type_Id) return C.char;
   pragma Import (C, Is_Of_Type, "DotsC_IsOfType");

   procedure Get_Complete_Type
     (Type_Id         : in Safir.Dob.Typesystem.Type_Id;
      Buf             : in Type_Id_Ptrs.Pointer;
      Buf_Size        : in Safir.Dob.Typesystem.Int_32;
      Result_Size     : out Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Get_Complete_Type, "DotsC_GetCompleteType");
--
--      // Function:    DotsC_GetParentType
--      // Parameters:  type        -   the type for which the parent type is requested
--      // Returns:     type id for the parent
--      // Comments:    Returns the typeId  for the parent class to 'type'. If type==object,
--      //              then the object typeId is returned again.
--      DOTS_KERNEL_API DotsC_TypeId CALLING_CONVENTION DotsC_GetParentType(const DotsC_TypeId type);
--
   function Get_Parent_Type
     (Type_Id   : in Safir.Dob.Typesystem.Type_Id)
      return Safir.Dob.Typesystem.Type_Id;
   pragma Import (C, Get_Parent_Type, "DotsC_GetParentType");

   procedure Has_Property
     (ClassTypeId    : in Safir.Dob.Typesystem.Type_Id;
      PropertyTypeId : in Safir.Dob.Typesystem.Type_Id;
      HasProperty    : out C.char;
      IsInherited    : out C.char);
   pragma Import (C, Has_Property, "DotsC_HasProperty");


   ----------------
   -- Serialization
   ----------------

   procedure Better_Blob_To_Xml
     (Xml_Dest    : in Safir.Dob.Typesystem.Char_Star;
      Blob_Source : in      Safir.Dob.Typesystem.Blob_T;
      Buf_Size    : in      Safir.Dob.Typesystem.Int_32;
      Result_Size : out Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Better_Blob_To_Xml, "DotsC_BetterBlobToXml");

   procedure Xml_To_Blob (Blob_Dest   : out Safir.Dob.Typesystem.Blob_T;
                          Deleter     : out Blob_Deleter_Cb_Type;
                          Xml_Source  : in C.char_array);
   pragma Import (C, Xml_To_Blob, "DotsC_XmlToBlob");

   procedure Blob_To_Json
     (Json_Dest   : in Safir.Dob.Typesystem.Char_Star;
      Blob_Source : in      Safir.Dob.Typesystem.Blob_T;
      Buf_Size    : in      Safir.Dob.Typesystem.Int_32;
      Result_Size : out Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Blob_To_Json, "DotsC_BlobToJson");

   procedure Json_To_Blob (Blob_Dest   : out Safir.Dob.Typesystem.Blob_T;
                           Deleter     : out Blob_Deleter_Cb_Type;
                           Json_Source : in C.char_array);
   pragma Import (C, Json_To_Blob, "DotsC_JsonToBlob");

   function Calculate_Base_64_Buffer_Size
     (Binary_Source_Size : Safir.Dob.Typesystem.Int_32)
         return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Calculate_Base_64_Buffer_Size, "DotsC_CalculateBase64BufferSize");

   procedure Binary_To_Base_64
     (Base_64_Dest   : in     Safir.Dob.Typesystem.Char_Star;
      Dest_Size      : in     Safir.Dob.Typesystem.Int_32;
      Binary_Source  : in     Safir.Dob.Typesystem.Char_Star;
      Source_Size    : in     Safir.Dob.Typesystem.Int_32;
      Result_Size    : in out Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Binary_To_Base_64, "DotsC_BinaryToBase64");

   function Calculate_Binary_Buffer_Size
        (Base_64_Source_Size : Safir.Dob.Typesystem.Int_32)
        return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Calculate_Binary_Buffer_Size, "DotsC_CalculateBinaryBufferSize");

   procedure Base_64_To_Binary
     (Binary_Dest    : in Safir.Dob.Typesystem.Char_Star;
      Dest_Size      : in     Safir.Dob.Typesystem.Int_32;
      Base_64_Source : in     C.char_array;
      Source_Size    : in     Safir.Dob.Typesystem.Int_32;
      ResultSize     : in out Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Base_64_To_Binary, "DotsC_Base64ToBinary");
--
--      //************************************************************************************
--      //* Functions for retrieval of parameters
--      //************************************************************************************

   procedure Get_Boolean_Parameter
     (Type_Id      : in Safir.Dob.Typesystem.Type_Id;
      Parameter    : in Safir.Dob.Typesystem.Parameter_Index;
      Index        : in Safir.Dob.Typesystem.Array_Index;
      Val          : out C.char);
   pragma Import (C, Get_Boolean_Parameter, "DotsC_GetBooleanParameter");

   procedure Get_Enumeration_Parameter
     (Type_Id      : in Safir.Dob.Typesystem.Type_Id;
      Parameter    : in Safir.Dob.Typesystem.Parameter_Index;
      Index        : in Safir.Dob.Typesystem.Array_Index;
      Val          : out Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Get_Enumeration_Parameter, "DotsC_GetEnumerationParameter");

   procedure Get_Int_32_Parameter
     (Type_Id      : in Safir.Dob.Typesystem.Type_Id;
      Parameter    : in Safir.Dob.Typesystem.Parameter_Index;
      Index        : in Safir.Dob.Typesystem.Array_Index;
      Val          : out Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Get_Int_32_Parameter, "DotsC_GetInt32Parameter");

   procedure Get_Int_64_Parameter
     (Type_Id      : in Safir.Dob.Typesystem.Type_Id;
      Parameter    : in Safir.Dob.Typesystem.Parameter_Index;
      Index        : in Safir.Dob.Typesystem.Array_Index;
      Val          : out Safir.Dob.Typesystem.Int_64);
   pragma Import (C, Get_Int_64_Parameter, "DotsC_GetInt64Parameter");

   procedure Get_Float_32_Parameter
     (Type_Id      : in Safir.Dob.Typesystem.Type_Id;
      Parameter    : in Safir.Dob.Typesystem.Parameter_Index;
      Index        : in Safir.Dob.Typesystem.Array_Index;
      Val          : out Safir.Dob.Typesystem.Float_32);
   pragma Import (C, Get_Float_32_Parameter, "DotsC_GetFloat32Parameter");

   procedure Get_Float_64_Parameter
     (Type_Id      : in Safir.Dob.Typesystem.Type_Id;
      Parameter    : in Safir.Dob.Typesystem.Parameter_Index;
      Index        : in Safir.Dob.Typesystem.Array_Index;
      Val          : out Safir.Dob.Typesystem.Float_64);
   pragma Import (C, Get_Float_64_Parameter, "DotsC_GetFloat64Parameter");

   procedure Get_String_Parameter
     (Type_Id      : in Safir.Dob.Typesystem.Type_Id;
      Parameter    : in Safir.Dob.Typesystem.Parameter_Index;
      Index        : in Safir.Dob.Typesystem.Array_Index;
      Val          : out C.Strings.chars_ptr);
   pragma Import (C, Get_String_Parameter, "DotsC_GetStringParameter");

   procedure Get_Type_Id_Parameter
     (Type_Id      : in Safir.Dob.Typesystem.Type_Id;
      Parameter    : in Safir.Dob.Typesystem.Parameter_Index;
      Index        : in Safir.Dob.Typesystem.Array_Index;
      Val          : out Safir.Dob.Typesystem.Type_Id);
   pragma Import (C, Get_Type_Id_Parameter, "DotsC_GetTypeIdParameter");

   procedure Get_Hashed_Id_Parameter
     (Type_Id     : in Safir.Dob.Typesystem.Type_Id;
      Parameter   : in Safir.Dob.Typesystem.Parameter_Index;
      Index       : in Safir.Dob.Typesystem.Array_Index;
      Hash_Val    : out Safir.Dob.Typesystem.Int_64;
      Str_Val     : out C.Strings.chars_ptr);
   pragma Import (C, Get_Hashed_Id_Parameter, "DotsC_GetHashedIdParameter");

   procedure Get_Entity_Id_Parameter
     (Type_Id      : in Safir.Dob.Typesystem.Type_Id;
      Parameter    : in Safir.Dob.Typesystem.Parameter_Index;
      Index        : in Safir.Dob.Typesystem.Array_Index;
      Entity_Id    : out Underlying_Entity_Id_Type;
      Str_Val      : out C.Strings.chars_ptr);
   pragma Import (C, Get_Entity_Id_Parameter, "DotsC_GetEntityIdParameter");

   procedure Get_Object_Parameter
     (Type_Id      : in Safir.Dob.Typesystem.Type_Id;
      Parameter    : in Safir.Dob.Typesystem.Parameter_Index;
      Index        : in Safir.Dob.Typesystem.Array_Index;
      Val          : out Safir.Dob.Typesystem.Blob_T);
   pragma Import (C, Get_Object_Parameter, "DotsC_GetObjectParameter");

--
--      // Function:    DotsC_GetBinaryParameter
--      // Parameters:  typeId      -   id of class
--      //              parameter   -   id of parameter
--      //              index       -   array index. If parameter is not an array index shall be 0.
--      //              val         -   retrived value, out parameter
--      //              size        -   number of bytes in val.
--      // Returns:     -
--      // Comments:    Gets a parameter object value.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetBinaryParameter(const DotsC_TypeId typeId,
--                                                                       const DotsC_ParameterIndex parameter,
--                                                                       const DotsC_ArrayIndex index,
--                                                                       const char * & val,
--                                                                       DotsC_Int32 & size);
   procedure Get_Binary_Parameter
     (Type_Id      : in Safir.Dob.Typesystem.Type_Id;
      Parameter    : in Safir.Dob.Typesystem.Parameter_Index;
      Index        : in Safir.Dob.Typesystem.Array_Index;
      Val          : out Char_Ptrs.Pointer;
      Size         : out Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Get_Binary_Parameter, "DotsC_GetBinaryParameter");

--
--      //************************************************************************************
--      //* Functions for retrieving member values
--      //************************************************************************************
--      // Function:    DotsC_IsNullMember
--      // Parameters:  blob    -   blob containing the object
--      //              member  -   id of member
--      //              index   -   array index. If member is not an array index shall be 0.
--      // Returns:     boolean value
--      // Comments:    Get the if a member is null or a value.
--      DOTS_KERNEL_API bool CALLING_CONVENTION DotsC_IsNullMember(const char * const blob,
--                                                                 const DotsC_MemberIndex member,
--                                                                 const DotsC_ArrayIndex index);
--
--      // Function:    DotsC_IsChangedMember
--      // Parameters:  blob    -   blob containing the object
--      //              member  -   id of member
--      //              index   -   array index. If member is not an array index shall be 0.
--      // Returns:     boolean value
--      // Comments:    Get the if a member is changed
--      DOTS_KERNEL_API bool CALLING_CONVENTION DotsC_IsChangedMember(const char * const blob,
--                                                                    const DotsC_MemberIndex member,
--                                                                    const DotsC_ArrayIndex index);
--

   procedure Get_Boolean_Member
     (Blob        : in Safir.Dob.Typesystem.Blob_T;
      Member      : in Safir.Dob.Typesystem.Member_Index;
      Idx         : in Safir.Dob.Typesystem.Int_32;
      Value       : out C.char;
      Is_Null     : out C.char;
      Is_Changed  : out C.char);
   pragma Import (C, Get_Boolean_Member, "DotsC_GetBooleanMember");

--
--      // Function:    DotsC_GetEnumerationMember
--      // Parameters:  blob    -   blob containing the object
--      //              member  -   id of member
--      //              index   -   array index. If member is not an array index shall be 0.
--      //              val     -   the retrieved value.
--      //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
--      //              isChanged - indicates if the value has been changed.
--      // Returns:     -
--      // Comments:    Get a enumeration member from a blob.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetEnumerationMember(const char * const blob,
--                                                                         const DotsC_MemberIndex member,
--                                                                         const DotsC_ArrayIndex index,
--                                                                         DotsC_EnumerationValue & val,
--                                                                         bool & isNull,
--                                                                         bool & isChanged);
--

   procedure Get_Int_32_Member
     (Blob        : in Safir.Dob.Typesystem.Blob_T;
      Member      : in Safir.Dob.Typesystem.Member_Index;
      Idx         : in Safir.Dob.Typesystem.Int_32;
      Value       : out Safir.Dob.Typesystem.Int_32;
      Is_Null     : out C.char;
      Is_Changed  : out C.char);
   pragma Import (C, Get_Int_32_Member, "DotsC_GetInt32Member");

   procedure Get_Int_64_Member
     (Blob        : in Safir.Dob.Typesystem.Blob_T;
      Member      : in Safir.Dob.Typesystem.Member_Index;
      Idx         : in Safir.Dob.Typesystem.Int_32;
      Value       : out Safir.Dob.Typesystem.Int_64;
      Is_Null     : out C.char;
      Is_Changed  : out C.char);
   pragma Import (C, Get_Int_64_Member, "DotsC_GetInt64Member");

   procedure Get_Float_32_Member
     (Blob        : in Safir.Dob.Typesystem.Blob_T;
      Member      : in Safir.Dob.Typesystem.Member_Index;
      Idx         : in Safir.Dob.Typesystem.Int_32;
      Value       : out Safir.Dob.Typesystem.Float_32;
      Is_Null     : out C.char;
      Is_Changed  : out C.char);
   pragma Import (C, Get_Float_32_Member, "DotsC_GetFloat32Member");

   procedure Get_Float_64_Member
     (Blob        : in Safir.Dob.Typesystem.Blob_T;
      Member      : in Safir.Dob.Typesystem.Member_Index;
      Idx         : in Safir.Dob.Typesystem.Int_32;
      Value       : out Safir.Dob.Typesystem.Float_64;
      Is_Null     : out C.char;
      Is_Changed  : out C.char);
   pragma Import (C, Get_Float_64_Member, "DotsC_GetFloat64Member");

   procedure Get_String_Member
     (Blob                 : in Safir.Dob.Typesystem.Blob_T;
      Member               : in Safir.Dob.Typesystem.Member_Index;
      Index                : in Safir.Dob.Typesystem.Array_Index;
      Str_Val              : out C.Strings.chars_ptr;
      Is_Null              : out C.char;
      Is_Changed           : out C.char);
   pragma Import (C, Get_String_Member, "DotsC_GetStringMember");

--      // Function:    DotsC_GetTypeIdMember
--      // Parameters:  blob    -   blob containing the object
--      //              member  -   id of member
--      //              index   -   array index. If member is not an array index shall be 0.
--      //              val     -   the retrieved value.
--      //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
--      //              isChanged - indicates if the value has been changed.
--      // Returns:     -
--      // Comments:    Get a type id member from a blob.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetTypeIdMember(const char * const blob,
--                                                                    const DotsC_MemberIndex member,
--                                                                    const DotsC_ArrayIndex index,
--                                                                    DotsC_TypeId & val,
--                                                                    bool & isNull,
--                                                                    bool & isChanged);
--

   procedure Get_Hashed_Id_Member
     (Blob                 : in Safir.Dob.Typesystem.Blob_T;
      Member               : in Safir.Dob.Typesystem.Member_Index;
      Index                : in Safir.Dob.Typesystem.Array_Index;
      Hash_Val             : out Safir.Dob.Typesystem.Int_64;
      Str_Val              : out C.Strings.chars_ptr;
      Is_Null              : out C.char;
      Is_Changed           : out C.char);
   pragma Import (C, Get_Hashed_Id_Member, "DotsC_GetHashedIdMember");

   procedure Get_Entity_Id_Member
     (Blob                 : in Safir.Dob.Typesystem.Blob_T;
      Member               : in Safir.Dob.Typesystem.Member_Index;
      Index                : in Safir.Dob.Typesystem.Array_Index;
      Entity_Id            : out Safir.Dob.Typesystem.Internal_Defs.DotsC_Entity_Id;
      Str_Val              : out C.Strings.chars_ptr;
      Is_Null              : out C.char;
      Is_Changed           : out C.char);
   pragma Import (C, Get_Entity_Id_Member, "DotsC_GetEntityIdMember");

   procedure Get_Object_Member
     (Blob                 : in Safir.Dob.Typesystem.Blob_T;
      Member               : in Safir.Dob.Typesystem.Member_Index;
      Index                : in Safir.Dob.Typesystem.Array_Index;
      Val                  : out Safir.Dob.Typesystem.Blob_T;
      Is_Null              : out C.char;
      Is_Changed           : out C.char);
   pragma Import (C, Get_Object_Member, "DotsC_GetObjectMember");

--
--
--      //Same as above, but val is non-const.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetWriteableObjectMember(char * const blob,
--                                                                             const DotsC_MemberIndex member,
--                                                                             const DotsC_ArrayIndex index,
--                                                                             char * & val,
--                                                                             bool & isNull,
--                                                                             bool & isChanged);
--

   procedure Get_Binary_Member
     (Blob                 : in Safir.Dob.Typesystem.Blob_T;
      Member               : in Safir.Dob.Typesystem.Member_Index;
      Index                : in Safir.Dob.Typesystem.Array_Index;
      Val                  : out Safir.Dob.Typesystem.Char_Star;
      Size                 : out Safir.Dob.Typesystem.Int_32;
      Is_Null              : out C.char;
      Is_Changed           : out C.char);
   pragma Import (C, Get_Binary_Member, "DotsC_GetBinaryMember");

   procedure Set_Null_Member
     (Blob   : in  Safir.Dob.Typesystem.Blob_T;
      Member : in  Safir.Dob.Typesystem.Member_Index;
      Idx    : in  Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Set_Null_Member, "DotsC_SetNullMember");
--
--      // Function:    DotsC_SetBooleanMember
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      // Returns:     -
--      // Comments:    Sets the value for a boolean member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetBooleanMember(const bool val,
--                                                                     char * & blob,
--                                                                     const DotsC_MemberIndex member,
--                                                                     const DotsC_ArrayIndex index);
--
--      // Function:    DotsC_SetEnumerationMember
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      // Returns:     -
--      // Comments:    Sets the value for a enumeration member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetEnumerationMember(const DotsC_EnumerationValue val,
--                                                                         char * & blob,
--                                                                         const DotsC_MemberIndex member,
--                                                                         const DotsC_ArrayIndex index);
--
--      // Function:    DotsC_SetInt32Member
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      // Returns:     -
--      // Comments:    Sets the value for a 32-bits integer member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetInt32Member(const DotsC_Int32 val,
--                                                                   char * & blob,
--                                                                   const DotsC_MemberIndex member,
--                                                                   const DotsC_ArrayIndex index);
--
--      // Function:    DotsC_SetInt64Member
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      // Returns:     -
--      // Comments:    Sets the value for a 64-bits integer member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetInt64Member(const DotsC_Int64 val,
--                                                                   char * & blob,
--                                                                   const DotsC_MemberIndex member,
--                                                                   const DotsC_ArrayIndex index);
--
--      // Function:    DotsC_SetFloat32Member
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      // Comments:    Sets the value for a 32-bits float member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetFloat32Member(const DotsC_Float32 val,
--                                                                     char * & blob,
--                                                                     const DotsC_MemberIndex member,
--                                                                     const DotsC_ArrayIndex index);
--
--      // Function:    DotsC_SetFloat64Member
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      // Returns:     -
--      // Comments:    Sets the value for a 64-bits float member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetFloat64Member(const DotsC_Float64 val,
--                                                                     char * & blob,
--                                                                     const DotsC_MemberIndex member,
--                                                                     const DotsC_ArrayIndex index);
--
--      // Function:    DotsC_SetStringMember
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      // Returns:     -
--      // Comments:    Sets the value for a string member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetStringMember(const char * const val,
--                                                                    char * & blob,
--                                                                    const DotsC_MemberIndex member,
--                                                                    const DotsC_ArrayIndex index);
--
--      // Function:    DotsC_SetTypeIdMember
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      // Returns:     -
--      // Comments:    Sets the value for a type id member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetTypeIdMember(const DotsC_TypeId val,
--                                                                    char * & blob,
--                                                                    const DotsC_MemberIndex member,
--                                                                    const DotsC_ArrayIndex index);
--
--
--      // Function:    DotsC_SetHashedIdMember
--      // Parameters:  hashVal     -   the value to be set.
--      //              strVal      - the string value to set, can be NULL if need be.
--      //              blob    -   blob containing the member.
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      // Returns:     -
--      // Comments:    Sets the value for a hashed id member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetHashedIdMember(const DotsC_Int64 hashVal,
--                                                                      const char * const strVal,
--                                                                      char * & blob,
--                                                                      const DotsC_MemberIndex member,
--                                                                      const DotsC_ArrayIndex index);
--
--      // Function:    DotsC_SetEntityIdMember
--      // Parameters:  val     -   the value to be set.
--      //              instanceIdStr - the string value to set, can be NULL if need be.
--      //              blob    -   blob containing the member.
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      // Returns:     -
--      // Comments:    Sets the value for a object id member.
--      //              NOTE! The parameter val is sent by reference because of Ada will not work
--      //                    with a structure by value.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetEntityIdMember(const DotsC_EntityId& val,
--                                                                      const char * const instanceIdStr,
--                                                                      char * & blob,
--                                                                      const DotsC_MemberIndex member,
--                                                                      const DotsC_ArrayIndex index);
--
--      // Function:    DotsC_SetObjectMember
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      // Returns:     -
--      // Comments:    Sets the value for a object member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetObjectMember(const char * const val,
--                                                                    char * & blob,
--                                                                    const DotsC_MemberIndex member,
--                                                                    const DotsC_ArrayIndex index);
--
--      // Function:    DotsC_SetBinaryMember
--      // Parameters:  val     -   the value to be set
--      //              numberOfBytes    -   number of bytes in val to be written into the blob
--      //              blob    -   blob containing the member.
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      // Returns:     -
--      // Comments:    Sets the value for a object member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetBinaryMember(const char * const val,
--                                                                    const DotsC_Int32 numberOfBytes,
--                                                                    char * & blob,
--                                                                    const DotsC_MemberIndex member,
--                                                                    const DotsC_ArrayIndex index);
--
--      //************************************************************************************
--      //* Functions for retrieving property member values
--      //************************************************************************************
--      // Function:    DotsC_IsNullProperty
--      // Parameters:  blob    -   blob containing the object
--      //              member  -   id of member
--      //              index   -   array index. If member is not an array index shall be 0.
--      // Returns:     boolean value
--      // Comments:    Get the if a property is null or a value.
--      DOTS_KERNEL_API bool CALLING_CONVENTION DotsC_IsNullProperty(const char * const blob,
--                                                                   const DotsC_TypeId property,
--                                                                   const DotsC_MemberIndex member,
--                                                                   const DotsC_ArrayIndex index);
--
--      // Function:    DotsC_IsChangedProperty
--      // Parameters:  blob    -   blob containing the object
--      //              member  -   id of member
--      //              index   -   array index. If member is not an array index shall be 0.
--      // Returns:     boolean value
--      // Comments:    Get the if a property is changed
--      DOTS_KERNEL_API bool CALLING_CONVENTION DotsC_IsChangedProperty(const char * const blob,
--                                                                      const DotsC_TypeId property,
--                                                                      const DotsC_MemberIndex member,
--                                                                      const DotsC_ArrayIndex index);
--
--      // Function:    DotsC_GetBooleanProperty
--      // Parameters:  blob    -   blob containing the object
--      //              property -  id of the property
--      //              member  -   id of property member
--      //              index   -   array index.
--      //              val     -   the retrieved value.
--      //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
--      //              isChanged - indicates if the value has been changed.
--      //              errorCode - NoError or UnableToDereferenceProperty.
--      // Returns:     -
--      // Comments:    Get a boolean member defined in a property supporeted by the object. The value is retrived from the blob.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetBooleanProperty(const char * const blob,
--                                                                       const DotsC_TypeId property,
--                                                                       const DotsC_MemberIndex member,
--                                                                       const DotsC_ArrayIndex index,
--                                                                       bool & val,
--                                                                       bool & isNull,
--                                                                       bool & isChanged,
--                                                                       DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_GetEnumerationProperty
--      // Parameters:  blob    -   blob containing the object
--      //              property -  id of the property
--      //              member  -   id of property member
--      //              index   -   array index.
--      //              val     -   the retrieved value.
--      //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
--      //              isChanged - indicates if the value has been changed.
--      //              errorCode - NoError or UnableToDereferenceProperty.
--      // Returns:     -
--      // Comments:    Get a enumeration member defined in a property supporeted by the object. The value is retrived from the blob.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetEnumerationProperty(const char * const blob,
--                                                                           const DotsC_TypeId property,
--                                                                           const DotsC_MemberIndex member,
--                                                                           const DotsC_ArrayIndex index,
--                                                                           DotsC_EnumerationValue & val,
--                                                                           bool & isNull,
--                                                                           bool & isChanged,
--                                                                           DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_GetInt32Property
--      // Parameters:  blob    -   blob containing the object
--      //              property -  id of the property
--      //              member  -   id of property member
--      //              index   -   array index.
--      //              val     -   the retrieved value.
--      //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
--      //              isChanged - indicates if the value has been changed.
--      //              errorCode - NoError or UnableToDereferenceProperty.
--      // Returns:     -
--      // Comments:    Get a int32 member defined in a property supporeted by the object. The value is retrived from the blob.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetInt32Property(const char * const blob,
--                                                                     const DotsC_TypeId property,
--                                                                     const DotsC_MemberIndex member,
--                                                                     const DotsC_ArrayIndex index,
--                                                                     DotsC_Int32 & val,
--                                                                     bool & isNull,
--                                                                     bool & isChanged,
--                                                                     DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_GetInt64Property
--      // Parameters:  blob    -   blob containing the object
--      //              property -  id of the property
--      //              member  -   id of property member
--      //              index   -   array index.
--      //              val     -   the retrieved value.
--      //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
--      //              isChanged - indicates if the value has been changed.
--      //              errorCode - NoError or UnableToDereferenceProperty.
--      // Returns:     -
--      // Comments:    Get a int64 member defined in a property supporeted by the object. The value is retrived from the blob.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetInt64Property(const char * const blob,
--                                                                     const DotsC_TypeId property,
--                                                                     const DotsC_MemberIndex member,
--                                                                     const DotsC_ArrayIndex index,
--                                                                     DotsC_Int64 & val,
--                                                                     bool & isNull,
--                                                                     bool & isChanged,
--                                                                     DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_GetFloat32Property
--      // Parameters:  blob    -   blob containing the object
--      //              property -  id of the property
--      //              member  -   id of property member
--      //              index   -   array index.
--      //              val     -   the retrieved value.
--      //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
--      //              isChanged - indicates if the value has been changed.
--      //              errorCode - NoError or UnableToDereferenceProperty.
--      // Returns:     -
--      // Comments:    Get a float member defined in a property supporeted by the object. The value is retrived from the blob.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetFloat32Property(const char * const blob,
--                                                                       const DotsC_TypeId property,
--                                                                       const DotsC_MemberIndex member,
--                                                                       const DotsC_ArrayIndex index,
--                                                                       DotsC_Float32 & val,
--                                                                       bool & isNull,
--                                                                       bool & isChanged,
--                                                                       DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_GetFloat64Property
--      // Parameters:  blob    -   blob containing the object
--      //              property -  id of the property
--      //              member  -   id of property member
--      //              index   -   array index.
--      //              val     -   the retrieved value.
--      //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
--      //              isChanged - indicates if the value has been changed.
--      //              errorCode - NoError or UnableToDereferenceProperty.
--      // Returns:     -
--      // Comments:    Get a 64 bit float member defined in a property supporeted by the object. The value is retrived from the blob.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetFloat64Property(const char * const blob,
--                                                                       const DotsC_TypeId property,
--                                                                       const DotsC_MemberIndex member,
--                                                                       const DotsC_ArrayIndex index,
--                                                                       DotsC_Float64 & val,
--                                                                       bool & isNull,
--                                                                       bool & isChanged,
--                                                                       DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_GetStringProperty
--      // Parameters:  blob    -   blob containing the object
--      //              property -  id of the property
--      //              member  -   id of property member
--      //              index   -   array index.
--      //              val     -   the retrieved value.
--      //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
--      //              isChanged - indicates if the value has been changed.
--      //              errorCode - NoError or UnableToDereferenceProperty.
--      // Returns:     -
--      // Comments:    Get a string member defined in a property supporeted by the object. The value is retrived from the blob.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetStringProperty(const char * const blob,
--                                                                      const DotsC_TypeId property,
--                                                                      const DotsC_MemberIndex member,
--                                                                      const DotsC_ArrayIndex index,
--                                                                      const char * & val,
--                                                                      bool & isNull,
--                                                                      bool & isChanged,
--                                                                      DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_GetTypeIdProperty
--      // Parameters:  blob    -   blob containing the object
--      //              property -  id of the property
--      //              member  -   id of property member
--      //              index   -   array index.
--      //              val     -   the retrieved value.
--      //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
--      //              isChanged - indicates if the value has been changed.
--      //              errorCode - NoError or UnableToDereferenceProperty.
--      // Returns:     -
--      // Comments:    Get a type id member defined in a property supporeted by the object. The value is retrived from the blob.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetTypeIdProperty(const char * const blob,
--                                                                      const DotsC_TypeId property,
--                                                                      const DotsC_MemberIndex member,
--                                                                      const DotsC_ArrayIndex index,
--                                                                      DotsC_TypeId & val,
--                                                                      bool & isNull,
--                                                                      bool & isChanged,
--                                                                      DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_GetHashedIdProperty
--      // Parameters:  blob    -   blob containing the object
--      //              property -  id of the property
--      //              member  -   id of property member
--      //              index   -   array index.
--      //              hashVal -   the retrieved value.
--      //              strVal  -   the string representation, if there is one otherwise NULL.
--      //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
--      //              isChanged - indicates if the value has been changed.
--      //              errorCode - NoError or UnableToDereferenceProperty.
--      // Returns:     -
--      // Comments:    Get a hashed id member defined in a property supporeted by the object. The value is retrived from the blob.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetHashedIdProperty(const char * const blob,
--                                                                        const DotsC_TypeId property,
--                                                                        const DotsC_MemberIndex member,
--                                                                        const DotsC_ArrayIndex index,
--                                                                        DotsC_Int64 & hashVal,
--                                                                        const char * & strVal,
--                                                                        bool & isNull,
--                                                                        bool & isChanged,
--                                                                        DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_GetEntityIdProperty
--      // Parameters:  blob    -   blob containing the object
--      //              property -  id of the property
--      //              member  -   id of property member
--      //              index   -   array index.
--      //              val     -   the retrieved value.
--      //              instanceIdStr - the string representation, if there is one otherwise NULL.
--      //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
--      //              isChanged - indicates if the value has been changed.
--      //              errorCode - NoError or UnableToDereferenceProperty.
--      // Returns:     -
--      // Comments:    Get a object id member defined in a property supporeted by the object. The value is retrived from the blob.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetEntityIdProperty(const char * const blob,
--                                                                        const DotsC_TypeId property,
--                                                                        const DotsC_MemberIndex member,
--                                                                        const DotsC_ArrayIndex index,
--                                                                        DotsC_EntityId & val,
--                                                                        const char * & instanceIdStr,
--                                                                        bool & isNull,
--                                                                        bool & isChanged,
--                                                                        DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_GetObjectProperty
--      // Parameters:  blob    -   blob containing the object
--      //              property -  id of the property
--      //              member  -   id of property member
--      //              index   -   array index.
--      //              val     -   the retrieved value.
--      //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
--      //              isChanged - indicates if the value has been changed.
--      //              errorCode - NoError or UnableToDereferenceProperty.
--      // Returns:     -
--      // Comments:    Get a object member defined in a property supporeted by the object. The value is retrived from the blob.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetObjectProperty(const char * const blob,
--                                                                      const DotsC_TypeId property,
--                                                                      const DotsC_MemberIndex member,
--                                                                      const DotsC_ArrayIndex index,
--                                                                      const char * & val,
--                                                                      bool & isNull,
--                                                                      bool & isChanged,
--                                                                      DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_GetBinaryProperty
--      // Parameters:  blob    -   blob containing the object
--      //              property -  id of the property
--      //              member  -   id of property member
--      //              index   -   array index.
--      //              val     -   the retrieved value.
--      //              size     -  number of bytes in val.
--      //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
--      //              isChanged - indicates if the value has been changed.
--      //              errorCode - NoError or UnableToDereferenceProperty.
--      // Returns:     -
--      // Comments:    Get a object member defined in a property supporeted by the object. The value is retrived from the blob.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetBinaryProperty(const char * const blob,
--                                                                      const DotsC_TypeId property,
--                                                                      const DotsC_MemberIndex member,
--                                                                      const DotsC_ArrayIndex index,
--                                                                      const char * & val,
--                                                                      DotsC_Int32 & size,
--                                                                      bool & isNull,
--                                                                      bool & isChanged,
--                                                                      DotsC_ErrorCode & errorCode);
--
--      //************************************************************************************
--      //* Functions for setting property member values
--      //************************************************************************************
--      // Function:    DotsC_SetNullProperty
--      // Parameters:  blob        -   blob containing the member.
--      //              property    -   id of the property
--      //              member      -   id of the member
--      //              index       -   array index of member. Shall be 0 if member is not an array.
--      //              errorCode   -   NoError, UnableToDereferenceProperty or ReadOnlyProperty.
--      // Returns:     -
--      // Comments:    Sets the property member to null.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetNullProperty(char * & blob,
--                                                                    const DotsC_TypeId property,
--                                                                    const DotsC_MemberIndex member,
--                                                                    const DotsC_ArrayIndex index,
--                                                                    DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_SetBooleanProperty
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              property -  id of the property
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      //              errorCode   -   NoError, UnableToDereferenceProperty or ReadOnlyProperty.
--      // Returns:     -
--      // Comments:    Sets the value for a boolean property member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetBooleanProperty(const bool val,
--                                                                       char * & blob,
--                                                                       const DotsC_TypeId property,
--                                                                       const DotsC_MemberIndex member,
--                                                                       const DotsC_ArrayIndex index,
--                                                                       DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_SetEnumerationProperty
--      // Parameters:  val     -   the value to be set
--      //              enumId  -   id of enumeration type
--      //              blob    -   blob containing the member.
--      //              property -  id of the property
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      //              isMismatch - true if there is mismatch between caller and dots.
--      //              errorCode   -   NoError, UnableToDereferenceProperty or ReadOnlyProperty.
--      // Returns:     -
--      // Comments:    Sets the value for a enumeration property member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetEnumerationProperty(const DotsC_EnumerationValue val,
--                                                                           char * & blob,
--                                                                           const DotsC_TypeId property,
--                                                                           const DotsC_MemberIndex member,
--                                                                           const DotsC_ArrayIndex index,
--                                                                           DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_SetInt32Property
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              property -  id of the property
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      //              errorCode   -   NoError, UnableToDereferenceProperty or ReadOnlyProperty.
--      // Returns:     -
--      // Comments:    Sets the value for a 32-bits integer property member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetInt32Property(const DotsC_Int32 val,
--                                                                     char * & blob,
--                                                                     const DotsC_TypeId property,
--                                                                     const DotsC_MemberIndex member,
--                                                                     const DotsC_ArrayIndex index,
--                                                                     DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_SetInt64Property
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              property -  id of the property
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      //              errorCode   -   NoError, UnableToDereferenceProperty or ReadOnlyProperty.
--      // Returns:     -
--      // Comments:    Sets the value for a 64-bits integer property member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetInt64Property(const DotsC_Int64 val,
--                                                                     char * & blob,
--                                                                     const DotsC_TypeId property,
--                                                                     const DotsC_MemberIndex member,
--                                                                     const DotsC_ArrayIndex index,
--                                                                     DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_SetFloat32Property
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              property -  id of the property
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      //              errorCode   -   NoError, UnableToDereferenceProperty or ReadOnlyProperty.
--      // Comments:    Sets the value for a 32-bits float property member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetFloat32Property(const DotsC_Float32 val,
--                                                                       char * & blob,
--                                                                       const DotsC_TypeId property,
--                                                                       const DotsC_MemberIndex member,
--                                                                       const DotsC_ArrayIndex index,
--                                                                       DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_SetFloat64Property
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              property -  id of the property
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      //              errorCode   -   NoError, UnableToDereferenceProperty or ReadOnlyProperty.
--      // Returns:     -
--      // Comments:    Sets the value for a 64-bits float property member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetFloat64Property(const DotsC_Float64 val,
--                                                                       char * & blob,
--                                                                       const DotsC_TypeId property,
--                                                                       const DotsC_MemberIndex member,
--                                                                       const DotsC_ArrayIndex index,
--                                                                       DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_SetStringProperty
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              property -  id of the property
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      //              errorCode   -   NoError, UnableToDereferenceProperty or ReadOnlyProperty.
--      // Returns:     -
--      // Comments:    Sets the value for a string property member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetStringProperty(const char * const val,
--                                                                      char * & blob,
--                                                                      const DotsC_TypeId property,
--                                                                      const DotsC_MemberIndex member,
--                                                                      const DotsC_ArrayIndex index,
--                                                                      DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_SetTypeIdProperty
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              property -  id of the property
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      //              errorCode   -   NoError, UnableToDereferenceProperty or ReadOnlyProperty.
--      // Returns:     -
--      // Comments:    Sets the value for a type id property member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetTypeIdProperty(const DotsC_TypeId val,
--                                                                      char * & blob,
--                                                                      const DotsC_TypeId property,
--                                                                      const DotsC_MemberIndex member,
--                                                                      const DotsC_ArrayIndex index,
--                                                                      DotsC_ErrorCode & errorCode);
--
--
--      // Function:    DotsC_SetEntityIdProperty
--      // Parameters:  val     -   the value to be set
--      //              instanceIdStr - the string rep if there is one, otherwise NULL
--      //              blob    -   blob containing the member.
--      //              property -  id of the property
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      //              errorCode   -   NoError, UnableToDereferenceProperty or ReadOnlyProperty.
--      // Returns:     -
--      // Comments:    Sets the value for a object id property member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetHashedIdProperty(const DotsC_Int64 hashVal,
--                                                                        const char * const strVal,
--                                                                        char * & blob,
--                                                                        const DotsC_TypeId property,
--                                                                        const DotsC_MemberIndex member,
--                                                                        const DotsC_ArrayIndex index,
--                                                                        DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_SetEntityIdProperty
--      // Parameters:  val     -   the value to be set
--      //              instanceIdStr - the string rep if there is one, otherwise NULL
--      //              blob    -   blob containing the member.
--      //              property -  id of the property
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      //              errorCode   -   NoError, UnableToDereferenceProperty or ReadOnlyProperty.
--      // Returns:     -
--      // Comments:    Sets the value for a object id property member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetEntityIdProperty(const DotsC_EntityId& val,
--                                                                        const char * const instanceIdStr,
--                                                                        char * & blob,
--                                                                        const DotsC_TypeId property,
--                                                                        const DotsC_MemberIndex member,
--                                                                        const DotsC_ArrayIndex index,
--                                                                        DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_SetObjectProperty
--      // Parameters:  val     -   the value to be set
--      //              blob    -   blob containing the member.
--      //              property -  id of the property
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      //              errorCode   -   NoError, UnableToDereferenceProperty or ReadOnlyProperty.
--      // Returns:     -
--      // Comments:    Sets the value for a object property member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetObjectProperty(const char * const val,
--                                                                      char * & blob,
--                                                                      const DotsC_TypeId property,
--                                                                      const DotsC_MemberIndex member,
--                                                                      const DotsC_ArrayIndex index,
--                                                                      DotsC_ErrorCode & errorCode);
--
--      // Function:    DotsC_SetBinaryProperty
--      // Parameters:  val     -   the value to be set
--      //              size    -   number of bytes to be written to the blob
--      //              blob    -   blob containing the member.
--      //              property -  id of the property
--      //              member  -   id of the member
--      //              index   -   array index of member. Shall be 0 if member is not an array.
--      //              errorCode   -   NoError, UnableToDereferenceProperty or ReadOnlyProperty.
--      // Returns:     -
--      // Comments:    Sets the value for a string property member.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_SetBinaryProperty(const char * const val,
--                                                                      const DotsC_Int32 size,
--                                                                      char * & blob,
--                                                                      const DotsC_TypeId property,
--                                                                      const DotsC_MemberIndex member,
--                                                                      const DotsC_ArrayIndex index,
--                                                                      DotsC_ErrorCode & errorCode);
--
--      //*********************************
--      //* For debug
--      //*********************************
--      // Function:    DotsC_DumpClassDescriptions
--      // Parameters:  -
--      // Returns:     -
--      // Comments:    Prints all type information to standard output.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_DumpClassDescriptions();
--
--      // Function:    DotsC_DumpMemoryBlockInfo
--      // Parameters:  -
--      // Returns:     -
--      // Comments:    Prints shared memory pool status to standard output.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_DumpMemoryBlockInfo();
--
--      //*********************************
--      //* For "real classes"
--      //*********************************
--
   function Get_Initial_Size
     (Type_Id : in  Safir.Dob.Typesystem.Type_Id) return Safir.Dob.Typesystem.Int_32;
   pragma Import (C, Get_Initial_Size, "DotsC_GetInitialSize");

   procedure Format_Blob (Blob                : in Safir.Dob.Typesystem.Blob_T;
                          Blob_Size           : in Safir.Dob.Typesystem.Int_32;
                          Type_Id             : in Safir.Dob.Typesystem.Type_Id;
                          Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T);
   pragma Import (C, Format_Blob, "DotsC_FormatBlob");

   procedure Create_Object_Member
     (Blob                : in Safir.Dob.Typesystem.Blob_T;
      Blob_Size           : in Safir.Dob.Typesystem.Int_32;
      Type_Id             : in  Safir.Dob.Typesystem.Type_Id;
      Member              : in Safir.Dob.Typesystem.Member_Index;
      Idx                 : in Safir.Dob.Typesystem.Int_32;
      Is_Changed          : in C.char;
      Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T);
   pragma Import (C, Create_Object_Member, "DotsC_CreateObjectMember");

   procedure Create_String_Member
     (Blob                : in Safir.Dob.Typesystem.Blob_T;
      String_Length       : in Safir.Dob.Typesystem.Int_32;
      Member              : in Safir.Dob.Typesystem.Member_Index;
      Idx                 : in Safir.Dob.Typesystem.Int_32;
      Is_Changed          : in C.char;
      Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T);
   pragma Import (C, Create_String_Member, "DotsC_CreateStringMember");

   procedure Create_Binary_Member
     (Blob                : in Safir.Dob.Typesystem.Blob_T;
      Binary_Size         : in Safir.Dob.Typesystem.Int_32;
      Member              : in Safir.Dob.Typesystem.Member_Index;
      Idx                 : in Safir.Dob.Typesystem.Int_32;
      Is_Changed          : in C.char;
      Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T);
   pragma Import (C, Create_Binary_Member, "DotsC_CreateBinaryMember");

   procedure Set_Boolean_Member_In_Preallocated
     (Value       : in C.char;
      Is_Null     : in C.char;
      Is_Changed  : in C.char;
      Blob        : in Safir.Dob.Typesystem.Blob_T;
      Member      : in Safir.Dob.Typesystem.Member_Index;
      Idx       : in Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Set_Boolean_Member_In_Preallocated, "DotsC_SetBooleanMemberInPreallocated");

   procedure Set_Int_32_Member_In_Preallocated
     (Value       : in Safir.Dob.Typesystem.Int_32;
      Is_Null     : in C.char;
      Is_Changed  : in C.char;
      Blob        : in Safir.Dob.Typesystem.Blob_T;
      Member      : in Safir.Dob.Typesystem.Member_Index;
      Idx         : in Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Set_Int_32_Member_In_Preallocated, "DotsC_SetInt32MemberInPreallocated");

   procedure Set_Int_64_Member_In_Preallocated
     (Value       : in Safir.Dob.Typesystem.Int_64;
      Is_Null     : in C.char;
      Is_Changed  : in C.char;
      Blob        : in Safir.Dob.Typesystem.Blob_T;
      Member      : in Safir.Dob.Typesystem.Member_Index;
      Idx         : in Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Set_Int_64_Member_In_Preallocated, "DotsC_SetInt64MemberInPreallocated");

   procedure Set_Float_32_Member_In_Preallocated
     (Value       : in Safir.Dob.Typesystem.Float_32;
      Is_Null     : in C.char;
      Is_Changed  : in C.char;
      Blob        : in Safir.Dob.Typesystem.Blob_T;
      Member      : in Safir.Dob.Typesystem.Member_Index;
      Idx         : in Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Set_Float_32_Member_In_Preallocated, "DotsC_SetFloat32MemberInPreallocated");

   procedure Set_Float_64_Member_In_Preallocated
     (Value       : in Safir.Dob.Typesystem.Float_64;
      Is_Null     : in C.char;
      Is_Changed  : in C.char;
      Blob        : in Safir.Dob.Typesystem.Blob_T;
      Member      : in Safir.Dob.Typesystem.Member_Index;
      Idx         : in Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Set_Float_64_Member_In_Preallocated, "DotsC_SetFloat64MemberInPreallocated");

   procedure Set_Hashed_Id_Member_In_Preallocated
     (Value               : in Safir.Dob.Typesystem.Int_64;
      Str_Val             : in C.Strings.chars_ptr;
      String_Length       : in Safir.Dob.Typesystem.Int_32;
      Is_Null             : in C.char;
      Is_Changed          : in C.char;
      Blob                : in Safir.Dob.Typesystem.Blob_T;
      Member              : in Safir.Dob.Typesystem.Member_Index;
      Idx                 : in Safir.Dob.Typesystem.Int_32;
      Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T);
   pragma Import (C, Set_Hashed_Id_Member_In_Preallocated, "DotsC_SetHashedIdMemberInPreallocated");

   procedure Set_Entity_Id_Member_In_Preallocated
     (Entity_Id           : in out Underlying_Entity_Id_Type;
      Instance_Id_Str     : in C.Strings.chars_ptr;
      String_Length       : in Safir.Dob.Typesystem.Int_32;
      Is_Null             : in C.char;
      Is_Changed          : in C.char;
      Blob                : in Safir.Dob.Typesystem.Blob_T;
      Member              : in Safir.Dob.Typesystem.Member_Index;
      Idx                 : in Safir.Dob.Typesystem.Int_32;
      Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T);
   pragma Import (C, Set_Entity_Id_Member_In_Preallocated, "DotsC_SetEntityIdMemberInPreallocated");

   procedure Get_Property_Mapping_Kind
     (Type_Id              : in Safir.Dob.Typesystem.Type_Id;
      Property_Id          : in Safir.Dob.Typesystem.Type_Id;
      Member               : in Safir.Dob.Typesystem.Member_Index;
      Mapping_Kind         : out Safir.Dob.Typesystem.Internal_Defs.DotsC_Property_Mapping_Kind;
      Error_Code           : out Safir.Dob.Typesystem.Internal_Defs.DotsC_Error_Code);
   pragma Import (C, Get_Property_Mapping_Kind, "DotsC_GetPropertyMappingKind");

   procedure Get_Class_Member_Reference
     (Type_Id              : in Safir.Dob.Typesystem.Type_Id;
      Property_Id          : in Safir.Dob.Typesystem.Type_Id;
      Member               : in Safir.Dob.Typesystem.Member_Index;
      Class_Member_Ref     : out Int_Ptrs.Pointer;
      Ref_Size             : out Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Get_Class_Member_Reference, "DotsC_GetClassMemberReference");

   procedure Get_Enumeration_Checksum
     (Type_Id              : in Safir.Dob.Typesystem.Type_Id;
      Checksum             : out Safir.Dob.Typesystem.Type_Id);
   pragma Import (C, Get_Enumeration_Checksum, "DotsC_GetEnumerationChecksum");

   ------------------------------------------------------
   -- Functions for retrieval of parameters in properties
   ------------------------------------------------------

      procedure Get_Boolean_Property_Parameter
        (Type_Id              : in Safir.Dob.Typesystem.Type_Id;
         Property_Id          : in Safir.Dob.Typesystem.Type_Id;
         Member               : in Safir.Dob.Typesystem.Member_Index;
         Index                : in Safir.Dob.Typesystem.Array_Index;
         Val                  : out C.char);
      pragma Import (C, Get_Boolean_Property_Parameter, "DotsC_GetBooleanPropertyParameter");

      procedure Get_Enumeration_Property_Parameter
        (Type_Id              : in Safir.Dob.Typesystem.Type_Id;
         Property_Id          : in Safir.Dob.Typesystem.Type_Id;
         Member               : in Safir.Dob.Typesystem.Member_Index;
         Index                : in Safir.Dob.Typesystem.Array_Index;
         Val                  : out Safir.Dob.Typesystem.Int_32);
      pragma Import (C, Get_Enumeration_Property_Parameter, "DotsC_GetEnumerationPropertyParameter");

      procedure Get_Int_32_Property_Parameter
        (Type_Id              : in Safir.Dob.Typesystem.Type_Id;
         Property_Id          : in Safir.Dob.Typesystem.Type_Id;
         Member               : in Safir.Dob.Typesystem.Member_Index;
         Index                : in Safir.Dob.Typesystem.Array_Index;
         Val                  : out Safir.Dob.Typesystem.Int_32);
      pragma Import (C, Get_Int_32_Property_Parameter, "DotsC_GetInt32PropertyParameter");

      procedure Get_Int_64_Property_Parameter
        (Type_Id              : in Safir.Dob.Typesystem.Type_Id;
         Property_Id          : in Safir.Dob.Typesystem.Type_Id;
         Member               : in Safir.Dob.Typesystem.Member_Index;
         Index                : in Safir.Dob.Typesystem.Array_Index;
         Val                  : out Safir.Dob.Typesystem.Int_64);
      pragma Import (C, Get_Int_64_Property_Parameter, "DotsC_GetInt64PropertyParameter");

      procedure Get_Float_32_Property_Parameter
        (Type_Id              : in Safir.Dob.Typesystem.Type_Id;
         Property_Id          : in Safir.Dob.Typesystem.Type_Id;
         Member               : in Safir.Dob.Typesystem.Member_Index;
         Index                : in Safir.Dob.Typesystem.Array_Index;
         Val                  : out Safir.Dob.Typesystem.Float_32);
      pragma Import (C, Get_Float_32_Property_Parameter, "DotsC_GetFloat32PropertyParameter");

      procedure Get_Float_64_Property_Parameter
        (Type_Id              : in Safir.Dob.Typesystem.Type_Id;
         Property_Id          : in Safir.Dob.Typesystem.Type_Id;
         Member               : in Safir.Dob.Typesystem.Member_Index;
         Index                : in Safir.Dob.Typesystem.Array_Index;
         Val                  : out Safir.Dob.Typesystem.Float_64);
      pragma Import (C, Get_Float_64_Property_Parameter, "DotsC_GetFloat64PropertyParameter");

      procedure Get_String_Property_Parameter
        (Type_Id              : in Safir.Dob.Typesystem.Type_Id;
         Property_Id          : in Safir.Dob.Typesystem.Type_Id;
         Member               : in Safir.Dob.Typesystem.Member_Index;
         Index                : in Safir.Dob.Typesystem.Array_Index;
         Val                  : out C.Strings.chars_ptr);
      pragma Import (C, Get_String_Property_Parameter, "DotsC_GetStringPropertyParameter");

--      // Function:    DotsC_GetTypeIdPropertyParameter
--      // Parameters:  typeId      -   id of class
--      //              propertyId  -   TypeId of the property
--      //              member      -   member index
--      //              index       -   array index. If parameter is not an array index shall be 0.
--      //              val         -   retrived value, out parameter
--      // Returns:     -
--      // Comments:    Gets a parameter type id value.
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_GetTypeIdPropertyParameter(const DotsC_TypeId typeId,
--                                                                               const DotsC_TypeId propertyId,
--                                                                               const DotsC_MemberIndex member,
--                                                                               const DotsC_ArrayIndex index,
--                                                                               DotsC_TypeId & val);
--
--
--
      procedure Get_Hashed_Id_Property_Parameter
        (Type_Id              : in Safir.Dob.Typesystem.Type_Id;
         Property_Id          : in Safir.Dob.Typesystem.Type_Id;
         Member               : in Safir.Dob.Typesystem.Member_Index;
         Index                : in Safir.Dob.Typesystem.Array_Index;
         Hash_Val             : out Safir.Dob.Typesystem.Int_64;
         Str_Val              : out C.Strings.chars_ptr);
      pragma Import (C, Get_Hashed_Id_Property_Parameter, "DotsC_GetHashedIdPropertyParameter");

      procedure Get_Entity_Id_Property_Parameter
        (Type_Id              : in Safir.Dob.Typesystem.Type_Id;
         Property_Id          : in Safir.Dob.Typesystem.Type_Id;
         Member               : in Safir.Dob.Typesystem.Member_Index;
         Index                : in Safir.Dob.Typesystem.Array_Index;
         Entity_Id_Val        : out Safir.Dob.Typesystem.Internal_Defs.DotsC_Entity_Id;
         Str_Val              : out C.Strings.chars_ptr);
      pragma Import (C, Get_Entity_Id_Property_Parameter, "DotsC_GetEntityIdPropertyParameter");

      procedure Get_Object_Property_Parameter
        (Type_Id              : in Safir.Dob.Typesystem.Type_Id;
         Property_Id          : in Safir.Dob.Typesystem.Type_Id;
         Member               : in Safir.Dob.Typesystem.Member_Index;
         Index                : in Safir.Dob.Typesystem.Array_Index;
         Blob                 : out Safir.Dob.Typesystem.Blob_T);
      pragma Import (C, Get_Object_Property_Parameter, "DotsC_GetObjectPropertyParameter");

      procedure Get_Binary_Property_Parameter
        (Type_Id              : in Safir.Dob.Typesystem.Type_Id;
         Property_Id          : in Safir.Dob.Typesystem.Type_Id;
         Member               : in Safir.Dob.Typesystem.Member_Index;
         Index                : in Safir.Dob.Typesystem.Array_Index;
         Val                  : out Char_Star;
         Size                 : out Safir.Dob.Typesystem.Int_32);
      pragma Import (C, Get_Binary_Property_Parameter, "DotsC_GetBinaryPropertyParameter");

      procedure Set_Exception (ExceptionId : in Safir.Dob.Typesystem.Type_Id;
                               Description : in C.char_array);
      pragma Import (C, Set_Exception, "DotsC_SetException");
--
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_AppendExceptionDescription(const char * const moreDescription);
--
   procedure Get_And_Clear_Exception (ExceptionId : out Safir.Dob.Typesystem.Type_Id;
                                      Description : out C.Strings.chars_ptr;
                                      Deleter     : out String_Deleter_Cb_Type;
                                      WasSet      : out C.char);
   pragma Import (C, Get_And_Clear_Exception, "DotsC_GetAndClearException");

--
--      DOTS_KERNEL_API void CALLING_CONVENTION DotsC_PeekAtException(DotsC_TypeId & exceptionId);
--
--

end Safir.Dob.Typesystem.Kernel;
