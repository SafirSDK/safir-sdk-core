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
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Safir.Dob.Typesystem.Object;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Typesystem.Channel_Id;
with Safir.Dob.Typesystem.Handler_Id;

pragma Warnings ("H");  -- turn off warnings for hiding variable

package Safir.Dob.Typesystem.Properties is

   -- Get the array size of a property member.
   --
   -- Parameters: Class_Id - Type id of a class that supports the specified property.
   --             Property_Id - Type_Id of the property.
   --             Member - Index of the property member.
   -- Returns: The array size of the property member.
   -- Exceptions: Illegal_Value_Exception - There is no such type or member defined.
   --
   function Get_Array_Size
     (Class_Id        : Safir.Dob.Typesystem.Type_Id;
      Property_Id     : Safir.Dob.Typesystem.Type_Id;
      Member          : Safir.Dob.Typesystem.Member_Index) return Safir.Dob.Typesystem.Int_32;

   -- Get the string max length of a property member.
   --
   -- Parameters: Class_Id - Type id of a class that supports the specified property.
   --             Property_Id - Type_Id of the property.
   --             Member - Index of the property member.
   -- Returns: The string max length of the property member.
   -- Exceptions: Illegal_Value_Exception - There is no such type or member defined.
   --
   function Get_String_Max_Length
     (Class_Id        : Safir.Dob.Typesystem.Type_Id;
      Property_Id     : Safir.Dob.Typesystem.Type_Id;
      Member          : Safir.Dob.Typesystem.Member_Index) return Safir.Dob.Typesystem.Int_32;

   -- Set a property member to null.
   --
   -- Parameters: Obj - The object to modify.
   --             Property_Id - Type_Id of the property to use.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The property member is read-only.
   --
   procedure Set_Null (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                       Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                       Member             : in Safir.Dob.Typesystem.Member_Index;
                       Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Is the property member null.
   --
   -- Parameters: Obj - The object to check inside.
   --             Property_Id - Type_Id of the property to use.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Returns: True if the property member (or a parent item of it) was null.
   --
   function Is_Null (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                     Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                     Member             : in Safir.Dob.Typesystem.Member_Index;
                     Index              : in Safir.Dob.Typesystem.Array_Index)
                     return Boolean;

   -- Is the property member changed.
   --
   -- Parameters: Obj - The object to check inside.
   --             Property_Id - Type_Id of the property to use.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Returns: True if the property member (or a parent item of it) was changed.
   --
   function Is_Changed (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                        Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                        Member             : in Safir.Dob.Typesystem.Member_Index;
                        Index              : in Safir.Dob.Typesystem.Array_Index)
                        return Boolean;

   -- Is the property member read-only.
   --
   -- A property member is read-only if it
   --
   -- 1. Is mapped to null or
   -- 2. Is mapped to a parameter or
   -- 3. The item containing the member in the object is null.
   --
   -- Parameters: Obj - The object to check inside.
   --             Property_Id - Type_Id of the property to use.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Returns: True if the property member is read only.
   --
   function Is_Read_Only (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                          Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                          Member             : in Safir.Dob.Typesystem.Member_Index;
                          Index              : in Safir.Dob.Typesystem.Array_Index)
                          return Boolean;

   -- Set a Boolean property member in the object using a property.
   --
   -- Parameters: Obj - The object to modify.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value to set the member to.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The property member is read-only.
   --
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Boolean;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Get a Boolean property member from the object using a property.
   --
   -- Parameters: Obj - The object to read from.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value of the member.
   --             Member - Index of the property member to read from.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The member is inaccessible. Some "parent" item is null.
   --             Null_Exception - The member is null.
   --
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Boolean;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Set an enumeration property member in the object using a property.
   --
   -- Parameters: Obj - The object to modify.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value to set the member to.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The property member is read-only.
   --
   procedure Set_Enum (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                       Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                       Value              : in Safir.Dob.Typesystem.Enumeration_Value;
                       Member             : in Safir.Dob.Typesystem.Member_Index;
                       Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Get an enumeration property member from the object using a property.
   --
   -- Parameters: Obj - The object to read from.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value of the member.
   --             Member - Index of the property member to read from.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The member is inaccessible. Some "parent" item is null.
   --             Null_Exception - The member is null.
   --
   procedure Get_Enum (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                       Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                       Value              : out Safir.Dob.Typesystem.Enumeration_Value;
                       Member             : in Safir.Dob.Typesystem.Member_Index;
                       Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Set an Int_32 property member in the object using a property.
   --
   -- Parameters: Obj - The object to modify.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value to set the member to.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The property member is read-only.
   --
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Int_32;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Get an Int_32 property member from the object using a property.
   --
   -- Parameters: Obj - The object to read from.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value of the member.
   --             Member - Index of the property member to read from.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The member is inaccessible. Some "parent" item is null.
   --             Null_Exception - The member is null.
   --
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Int_32;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Set an Int_64 property member in the object using a property.
   --
   -- Parameters: Obj - The object to modify.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value to set the member to.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The property member is read-only.
   --
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Int_64;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Get an Int_64 property member from the object using a property.
   --
   -- Parameters: Obj - The object to read from.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value of the member.
   --             Member - Index of the property member to read from.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The member is inaccessible. Some "parent" item is null.
   --             Null_Exception - The member is null.
   --
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Int_64;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Set a Float_32 property member in the object using a property.
   --
   -- Parameters: Obj - The object to modify.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value to set the member to.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The property member is read-only.
   --
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Float_32;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Get a Float_32 property member from the object using a property.
   --
   -- Parameters: Obj - The object to read from.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value of the member.
   --             Member - Index of the property member to read from.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The member is inaccessible. Some "parent" item is null.
   --             Null_Exception - The member is null.
   --
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Float_32;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Set a Float_64 property member in the object using a property.
   --
   -- Parameters: Obj - The object to modify.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value to set the member to.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The property member is read-only.
   --
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Float_64;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Get a Float_64 property member from the object using a property.
   --
   -- Parameters: Obj - The object to read from.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value of the member.
   --             Member - Index of the property member to read from.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The member is inaccessible. Some "parent" item is null.
   --             Null_Exception - The member is null.
   --
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Float_64;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Set an Instance_Id property member in the object using a property.
   --
   -- Parameters: Obj - The object to modify.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value to set the member to.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The property member is read-only.
   --
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Get an Instance_Id property member from the object using a property.
   --
   -- Parameters: Obj - The object to read from.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value of the member.
   --             Member - Index of the property member to read from.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The member is inaccessible. Some "parent" item is null.
   --             Null_Exception - The member is null.
   --
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Set an Entity_Id property member in the object using a property.
   --
   -- Parameters: Obj - The object to modify.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value to set the member to.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The property member is read-only.
   --
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Get an Entity_Id property member from the object using a property.
   --
   -- Parameters: Obj - The object to read from.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value of the member.
   --             Member - Index of the property member to read from.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The member is inaccessible. Some "parent" item is null.
   --             Null_Exception - The member is null.
   --
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Set a Channel_Id property member in the object using a property.
   --
   -- Parameters: Obj - The object to modify.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value to set the member to.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The property member is read-only.
   --
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Get a Channel_Id property member from the object using a property.
   --
   -- Parameters: Obj - The object to read from.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value of the member.
   --             Member - Index of the property member to read from.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The member is inaccessible. Some "parent" item is null.
   --             Null_Exception - The member is null.
   --
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Set a Handler_Id property member in the object using a property.
   --
   -- Parameters: Obj - The object to modify.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value to set the member to.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The property member is read-only.
   --
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Get a Handler_Id property member from the object using a property.
   --
   -- Parameters: Obj - The object to read from.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value of the member.
   --             Member - Index of the property member to read from.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The member is inaccessible. Some "parent" item is null.
   --             Null_Exception - The member is null.
   --
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Set a string property member in the object using a property.
   --
   -- Parameters: Obj - The object to modify.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value to set the member to.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The property member is read-only.
   --
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Unbounded_Wide_String;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Get a string property member from the object using a property.
   --
   -- Parameters: Obj - The object to read from.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value of the member.
   --             Member - Index of the property member to read from.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The member is inaccessible. Some "parent" item is null.
   --             Null_Exception - The member is null.
   --
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Unbounded_Wide_String;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Set an object property member in the object using a property.
   --
   -- Parameters: Obj - The object to modify.
   --             Property_Id - Type_Id of the property to use.
   --             Ptr - The object to set the member to.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The property member is read-only.
   --
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Ptr                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Get an object property member from the object using a property.
   --
   -- Parameters: Obj - The object to read from.
   --             Property_Id - Type_Id of the property to use.
   --             Ptr - The value of the member.
   --             Member - Index of the property member to read from.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The member is inaccessible. Some "parent" item is null.
   --             Null_Exception - The member is null.
   --
   function Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                 Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                 Member             : in Safir.Dob.Typesystem.Member_Index;
                 Index              : in Safir.Dob.Typesystem.Array_Index)
                 return Safir.Dob.Typesystem.Object.Smart_Pointer'Class;

   -- Set a binary property member in the object using a property.
   --
   -- Parameters: Obj - The object to modify.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value to set the member to.
   --             Member - Index of the property member to modify.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The property member is read-only.
   --
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Binary_Vectors.Vector;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

   -- Get a binary property member from the object using a property.
   --
   -- Parameters: Obj - The object to read from.
   --             Property_Id - Type_Id of the property to use.
   --             Value - The value of the member.
   --             Member - Index of the property member to read from.
   --             Index - Array index.
   -- Exceptions: Read_Only_Exception - The member is inaccessible. Some "parent" item is null.
   --             Null_Exception - The member is null.
   --
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Binary_Vectors.Vector;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index);

end Safir.Dob.Typesystem.Properties;
