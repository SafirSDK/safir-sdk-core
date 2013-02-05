-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2009 (http://www.safirsdk.com)
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

with Ada.Exceptions;
with Interfaces.C.Strings;
with Safir.Dob.Typesystem.Kernel;
with Safir.Dob.Typesystem.Utilities; use Safir.Dob.Typesystem.Utilities;

package body Safir.Dob.Typesystem.Members is

   package C renames Interfaces.C;
   use type C.Strings.chars_ptr;

   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String := "")
                    renames  Ada.Exceptions.Raise_Exception;

   ---------------------------
   -- GetNumberOfMembers --
   ---------------------------

   function Get_Number_Of_Members
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Safir.Dob.Typesystem.Int_32 is

      Result    : Safir.Dob.Typesystem.Int_32;
   begin
      Result := Safir.Dob.Typesystem.Kernel.Get_Number_Of_Members (Type_Id);
      if Result = -1 then
         Throw (Illegal_Value_Exception'Identity,
                "No such type ("
                & Safir.Dob.Typesystem.Type_Id'Image (Type_Id) & ")");
      end if;
      return Result;
   end Get_Number_Of_Members;

   --------------
   -- GetIndex --
   --------------

   function Get_Index
     (Type_Id     : Safir.Dob.Typesystem.Type_Id;
      Member_Name : Unbounded_Wide_String)
      return Safir.Dob.Typesystem.Member_Index is

      Result : Safir.Dob.Typesystem.Int_32;
   begin
      Result := Safir.Dob.Typesystem.Kernel.Get_Member_Id
        (Type_Id, C.To_C (To_Utf_8 (Member_Name)));
      if Result = -1 then
         Throw (Illegal_Value_Exception'Identity,
                "There is no such type or member defined ("
                & Safir.Dob.Typesystem.Type_Id'Image (Type_Id)
                & " - " & To_Utf_8 (Member_Name) & ")");
      end if;
      return Member_Index (Result);
   end Get_Index;


   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Type_Id : Safir.Dob.Typesystem.Type_Id;
      Member : Safir.Dob.Typesystem.Member_Index)
      return Unbounded_Wide_String is

      L_Val : C.Strings.chars_ptr;
   begin
      L_Val := Safir.Dob.Typesystem.Kernel.Get_Member_Name (Type_Id, Member);
      if L_Val = C.Strings.Null_Ptr then
         Throw (Illegal_Value_Exception'Identity,
                "There is no such type or member defined ("
                & Safir.Dob.Typesystem.Type_Id'Image (Type_Id)
                & " -" & Safir.Dob.Typesystem.Member_Index'Image (Member) & ")");
      end if;
      return From_Utf_8 (C.To_Ada (C.Strings.Value (L_Val)));
   end Get_Name;

   -----------------
   -- Get_Type_Id --
   -----------------

   function Get_Type_Id
     (Type_Id : Safir.Dob.Typesystem.Type_Id;
      Member  : Safir.Dob.Typesystem.Member_Index)
      return Safir.Dob.Typesystem.Type_Id is

      Result : Safir.Dob.Typesystem.Type_Id;
   begin
      Result := Safir.Dob.Typesystem.Kernel.Get_Complex_Member_Type_Id (Type_Id, Member);
      if Result = -1 then
         Throw (Illegal_Value_Exception'Identity,
                "There is no such type or member defined ("
                & Safir.Dob.Typesystem.Type_Id'Image (Type_Id)
                & " -" & Safir.Dob.Typesystem.Member_Index'Image (Member) & ")");
      end if;
      return Result;
   end Get_Type_Id;

   -------------
   -- GetInfo --
   -------------

   procedure Get_Info
     (Type_Id        : in  Safir.Dob.Typesystem.Type_Id;
      Member         : in  Safir.Dob.Typesystem.Member_Index;
      Member_Type    : out Safir.Dob.Typesystem.Member_Type;
      Member_Name    : out Unbounded_Wide_String;
      Member_Type_Id : out Safir.Dob.Typesystem.Type_Id;
      String_Length  : out Safir.Dob.Typesystem.Int_32;
      Is_Array       : out Boolean;
      Array_Length   : out Safir.Dob.Typesystem.Int_32)
   is
      L_Member_Name : C.Strings.chars_ptr;
      L_Is_Array : C.char;

   begin
      Safir.Dob.Typesystem.Kernel.Get_Member_Info
        (Type_Id, Member, Member_Type,
         L_Member_Name, Member_Type_Id,
         String_Length, L_Is_Array, Array_Length);
      if L_Member_Name = C.Strings.Null_Ptr then
         Throw (Illegal_Value_Exception'Identity,
                "There is no such type or member defined ("
                & Safir.Dob.Typesystem.Type_Id'Image (Type_Id)
                & " - " & Safir.Dob.Typesystem.Member_Index'Image (Member) & ")");
      end if;
      Member_Name := From_Utf_8 (C.To_Ada (C.Strings.Value (L_Member_Name)));
      Is_Array := C.char'Pos (L_Is_Array) /= 0;
   end Get_Info;

   --------------------
   -- Get_Array_Size --
   --------------------

   function Get_Array_Size
     (Type_Id : Safir.Dob.Typesystem.Type_Id;
      Member : Safir.Dob.Typesystem.Member_Index)
      return Int_32 is

      Result : Safir.Dob.Typesystem.Int_32;
   begin
      Result := Safir.Dob.Typesystem.Kernel.Get_Member_Array_Size (Type_Id, Member);
      if Result = -1 then
         Throw (Illegal_Value_Exception'Identity,
                "No such type or array defined ("
                & Safir.Dob.Typesystem.Type_Id'Image (Type_Id)
                & " - " & Safir.Dob.Typesystem.Member_Index'Image (Member) & ")");
      end if;
      return Result;
   end Get_Array_Size;

   ---------------------------
   -- Get_Max_String_Length --
   ---------------------------

   function Get_Max_String_Length
     (Type_Id : Safir.Dob.Typesystem.Type_Id;
      Member : Safir.Dob.Typesystem.Member_Index)
      return Int_32 is

      Result       : Safir.Dob.Typesystem.Int_32;
   begin
      Result := Safir.Dob.Typesystem.Kernel.Get_String_Member_Max_Length (Type_Id, Member);
      if Result = -1 then
         Throw (Illegal_Value_Exception'Identity,
                "There is no such type or member defined ("
                & Safir.Dob.Typesystem.Type_Id'Image (Type_Id)
                & " - " & Safir.Dob.Typesystem.Member_Index'Image (Member) & ")");
      end if;
      return Result;
   end Get_Max_String_Length;

   -------------------
   -- Get_Type_Name --
   -------------------

   function Get_Type_Name
     (Type_Id : Safir.Dob.Typesystem.Type_Id;
      Member : Safir.Dob.Typesystem.Member_Index)
      return Unbounded_Wide_String is

      L_Val : C.Strings.chars_ptr;
   begin
      L_Val := Safir.Dob.Typesystem.Kernel.Get_Member_Type_Name (Type_Id, Member);
      if L_Val = C.Strings.Null_Ptr then
         Throw (Illegal_Value_Exception'Identity,
                "There is no such type or member defined ("
                & Safir.Dob.Typesystem.Type_Id'Image (Type_Id)
                & " - " & Safir.Dob.Typesystem.Member_Index'Image (Member) & ")");
      end if;
      return From_Utf_8 (C.To_Ada (C.Strings.Value (L_Val)));
   end Get_Type_Name;


end Safir.Dob.Typesystem.Members;
