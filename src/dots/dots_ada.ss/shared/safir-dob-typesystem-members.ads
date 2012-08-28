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
pragma Warnings ("H");  -- turn off warnings for hiding variable

-- Functions for getting member information from types.
--
-- With these operations you can get information on types regarding
-- their members. You can get member names and indexes. You can
-- get TypeIds of members etc.
--
package Safir.Dob.Typesystem.Members is
   pragma Preelaborate (Safir.Dob.Typesystem.Members);

   -- Get the number of members for a class or property.
   --
   -- Note that parameters are not included.
   --
   -- Parameters: Type_Id - TypeId of class or property.
   -- Returns: Number of members in the class.
   -- Exceptions: Illegal_Value_Exception - There is no such type defined.
   --
   function Get_Number_Of_Members
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Safir.Dob.Typesystem.Int_32;

   -- Gets the member id for a member.
   --
   -- Parameters:  Type_Id - Id of class.
   --              Member _Name  - Name of member as specified in xml
   --                              description, case Sensitive,
   -- Returns: Id of member.
   -- Exceptions: Illegal_Value_Exception
   --
   function Get_Index
     (Type_Id     : Safir.Dob.Typesystem.Type_Id;
      Member_Name : Unbounded_Wide_String) return Safir.Dob.Typesystem.Member_Index;

   -- Get the name of the specified member as it was defined in the xml description.
   --
   -- Parameters:  Type_Id - TypeId of class or property.
   --              Member - Index of member.
   -- Returns: Name of member.
   -- Exceptions: Illegal_Value_Exception - There is no such type defined or
   --                                       there is no such member in the type.
   --
   function Get_Name
     (Type_Id : Safir.Dob.Typesystem.Type_Id;
      Member : Safir.Dob.Typesystem.Member_Index) return Unbounded_Wide_String;

   -- Get type id of object or enumeration member.
   --
   -- If a member is of type object or enumeration, this method can be used to
   -- get the typeId for the class or enum that the member is of.
   --
   -- Parameters:  Type_Id - TypeId of class or property.
   --              Member - Index of member.
   -- Returns: The type id for the object or enumeration member.
   -- Exceptions: Illegal_Value_Exception - There is no such type defined or
   --                                       there is no such member in the type or
   --                                       the member is not an enum or object.
   --
   function Get_Type_Id
     (Type_Id : Safir.Dob.Typesystem.Type_Id;
      Member : Safir.Dob.Typesystem.Member_Index) return Safir.Dob.Typesystem.Type_Id;

   -- Get information about a specific class member.
   --
   -- Parameters: Type_Id - Type id of class or property.
   --             Member - Index of member.
   --             Member_Type - The type of the member.
   --             Member_Name - The name of the member.
   --             Member_Type_Id - If Member_Type is object or enumeration, this
   --                              is the Type_Id of that type. If memberType is
   --                              something else the value is -1.
   --             String_Length - If Member_Type is string and the type is a class
   --                             (not property) then this is the length of the string.
   --             Is_Array - True if member is an array. Not applicable if type id is a property.
   --             Array_Length - Maximum capacity of array if the member is an array(1 if not an array). Not applicable if type id is a property.
   --                            (1 if not an array). Not applicable if type id is a property.
   -- Exceptions: Illegal_Value_Exception - There is no such type defined or
   --                                       there is no such member in the type.
   --
   procedure Get_Info
     (Type_Id        : in  Safir.Dob.Typesystem.Type_Id;
      Member         : in  Safir.Dob.Typesystem.Member_Index;
      Member_Type    : out Safir.Dob.Typesystem.Member_Type;
      Member_Name    : out Unbounded_Wide_String;
      Member_Type_Id : out Safir.Dob.Typesystem.Type_Id;
      String_Length  : out Safir.Dob.Typesystem.Int_32;
      Is_Array       : out Boolean;
      Array_Length   : out Safir.Dob.Typesystem.Int_32);

   -- Get the array size of a member.
   --
   -- Parameters: Type_Id - Type id of class.
   --             Member - member index.
   -- Returns: The array size of the member.
   -- Exceptions: Illegal_Value_Exception - There is no such class defined or
   --                                       there is no such member in the type.
   --
   function Get_Array_Size
     (Type_Id : Safir.Dob.Typesystem.Type_Id;
      Member : Safir.Dob.Typesystem.Member_Index) return Int_32;

   -- Get the maximum string length of a member.
   --
   -- Parameters: Type_Id - Type id of class.
   --             Member - member index.
   -- Returns: The maximum length of the string member.
   -- Exceptions: Illegal_Value_Exception
   --
   function Get_Max_String_Length
     (Type_Id : Safir.Dob.Typesystem.Type_Id;
      Member : Safir.Dob.Typesystem.Member_Index) return Int_32;

   -- Get the name of the type as it was defined in the xml description.
   --
   -- Parameters: Type_Id - Type id of class.
   --             Member - member index.
   -- Returns: The name of the type.
   -- Exceptions: Illegal_Value_Exception - There is no such class defined or
   --                                       there is no such member in the type.
   --
   function Get_Type_Name
     (Type_Id : Safir.Dob.Typesystem.Type_Id;
      Member : Safir.Dob.Typesystem.Member_Index) return Unbounded_Wide_String;

end Safir.Dob.Typesystem.Members;
