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

package Safir.Dob.Typesystem.Operations is

   -- Get the number of type id's defined in the system.
   -- This is equal to Get_Number_Of_Classes + Get_Number_Of_Properties + Get_Number_Of_Enumerations.
   --
   -- Returns: Number of existing types.
   --
   function Get_Number_Of_Type_Ids return Safir.Dob.Typesystem.Int_32;

   -- Get the number of classes defined in the system.
   --
   -- Returns: Number of existing classes.
   --
   function Get_Number_Of_Classes return Safir.Dob.Typesystem.Int_32;

   -- Get the number of properties defined in the system.
   --
   -- Returns: Number of existing properties.
   --
   function Get_Number_Of_Properties return Safir.Dob.Typesystem.Int_32;

   -- Get the number of enumeration types defined in the system.
   --
   -- Returns: Number of existing enumeration types.
   --
   function Get_Number_Of_Enumerations return Safir.Dob.Typesystem.Int_32;

   -- Get all type id's that exists in the system.
   --
   -- Returns: A vector containing all the type ids in the system.
   --
   function Get_All_Type_Ids return Safir.Dob.Typesystem.Type_Id_Vectors.Vector;


   -- Check if class with specified type id exist.
   --
   -- Parameters: Type_Id - Type id of class.
   -- Returns: True if the type exists.
   --
   function Exists
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Boolean;

   -- Check if a type represented by a type id is a class.
   --
   -- Using this function on a typeid that does not exist at all in the system
   -- will give false as return value.
   --
   -- Parameters: Type_Id - Type id to check.
   -- Returns: True if the type exists as a class.
   --
   function Is_Class
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Boolean;

   -- Check if a type represented by a type id is a property.
   --
   -- Using this function on a typeid that does not exist at all in the system
   -- will give false as return value.
   --
   -- Parameters: Type_Id - Type id to check.
   -- Returns: True if the type exists as a property.
   --
   function Is_Property
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Boolean;

   -- Check if a type represented by a type id is an enumeration.
   --
   -- Using this function on a typeid that does not exist at all in the system
   -- will give false as return value.
   --
   -- Parameters: Type_Id - Type id to check.
   -- Returns: True if the type exists as an enumeration.
   --
   function Is_Enumeration
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Boolean;

   -- Check if a type represented by a type id is an exception.
   --
   -- Using this function on a typeid that does not exist at all in the system
   -- will give false as return value.
   --
   -- Parameters: Type_Id - Type id to check.
   -- Returns: True if the type exists as an exception.
   --
   function Is_Exception
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Boolean;

   -- Calculates the type id for the given name.
   --
   -- Note that this is a pure mathematical function and will always return
   -- the correct typeId for a class with the specified name. This function
   -- does not give any information about whether a class with the specified
   -- name actually exist in the system. Use Exists to check if a class with
   -- the Type_Id returned from this function exists.
   -- TypeIds for classes and properties are based on namespaces and class
   -- name like this:
   -- typeName="MyNamespace1.MyNamespace2.MyClass"
   -- However for enumeration types the type id also contains the enum values:
   -- typeName="MyNamespace1.MyNamespace2.MyEnumType.EnumValue1.EnumValue2.EnumValue3"
   --
   -- Parameters: Type_Name - The name shall contain namespaces and class name
   --                         with '.' as separator, example "MyNamespace1.MyNamespace2.MyClass"
   -- Returns: Generated Type_Id.
   --
   function Get_Type_Id
     (Type_Name : Unbounded_Wide_String) return Safir.Dob.Typesystem.Type_Id;

   -- Get the name associated with the specified type id.
   --
   -- Parameters: Type_Id - The type id to get the real name of.
   -- Returns: Name of the type.
   -- Exceptions: Illegal_Value_Exception - There is no such type defined.
   --
   function Get_Name
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Unbounded_Wide_String;

   -- Get the number of enumeration values the specified enumeration type has.
   --
   -- Parameters: Type id of enum type.
   -- Returns : Number of enumeration values.
   -- Exceptions: Illegal_Value_Exception - There is no such type defined or
   --                                       it is not an enumeration.
   --
   function Get_Number_Of_Enumeration_Values
     (Enumeration_Id : Safir.Dob.Typesystem.Type_Id) return Safir.Dob.Typesystem.Int_32;

   -- Get the string representation of the specified value for an enumeration type.
   --
   -- Note that the returned string corresponds to the value given in the dou-file,
   -- which is NOT the same as 'Image on the enumeration value in the generated
   -- Ada-code.
   --
   -- Parameters: Enumeration_Id - Type id of the enumeration type.
   --             Value - The ordinal value.
   -- Returns: String representation of the enumeration value.
   -- Exceptions: Illegal_Value_Exception - There is no such enumeration or
   --                                       value defined.
   --
   function Get_Enumeration_Value_Name
     (Enumeration_Id : Safir.Dob.Typesystem.Type_Id;
      Value          : Safir.Dob.Typesystem.Int_32) return Unbounded_Wide_String;

   -- Get integer value associated with the enumeration value for the specified
   -- enumeration type.
   --
   -- Note that Enumeration_Value_Name shall correspond to the value given in the dou-file,
   -- which is NOT the same as 'Image on the enumeration value in the generated
   -- Ada-code.
   --
   -- Parameters: Enumeration_Id - Type id of the enumeration type.
   --             Enumeration_Value_Name - String representation of the desired value.
   -- Returns: Integer value for the enumeration value name.
   -- Exceptions: Illegal_Value_Exception - There is no such type defined or
   --                                       it is not an enumeration.
   function Get_Enumeration_Value
     (Enumeration_Id         : Safir.Dob.Typesystem.Type_Id;
      Enumeration_Value_Name : Unbounded_Wide_String) return Safir.Dob.Typesystem.Enumeration_Value;


   -- Get the Checksum over all enumeration members for an enumeration type.
   --
   -- Parameters: Type_Id - Type id of enum type.
   -- Returns: Checksum of the enumeration type.
   -- Exceptions: Illegal_Value_Exception - There is no such type defined or
   --                                       it is not an enumeration.
   function Get_Enumeration_Checksum
     (Enumeration_Id : Safir.Dob.Typesystem.Type_Id) return Safir.Dob.Typesystem.Type_Id;

   ---------------------
   -- Type compatibility
   ---------------------

   -- Checks if Type_Id is Of_Type_Id or a subclass of Of_Type_Id.
   --
   -- Parameters: Type_Id - The type to check if it is of another type.
   --             Of_Type_Id - The type to compare to.
   -- Returns: True if Type_Id is Of_Type_Id or a subclass of Of_Type_Id.
   --          False if one of the types are not a class type id (i.e. a random
   --          number or a property or enumeration id).
   --
   function Is_Of_Type
     (Type_Id    : Safir.Dob.Typesystem.Type_Id;
      Of_Type_Id : Safir.Dob.Typesystem.Type_Id) return Boolean;

   -- Returns a list of all type id's that is of the given type by inheritance.
   -- The type Root_Class will also be inserted in the list.
   --
   -- Parameters: Root_Class - The base type.
   -- Returns: A vector of type ids.
   --
   function Get_Class_Tree
     (Root_Class : Safir.Dob.Typesystem.Type_Id) return Safir.Dob.Typesystem.Type_Id_Vectors.Vector;

   -- Returns the type id of the base class to the argument type.
   --
   -- If Type_Id represents Object, then the type id for Object is returned.
   --
   -- Parameters: Type_Id - The type for which the parent type is requested.
   -- Returns: Parent type id.
   --
   function Get_Parent_Type
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Safir.Dob.Typesystem.Type_Id;

   -- Checks if a class has a property.
   --
   -- Parameters: Class_Type - The type of the class to check if it has a specific property.
   --             Property_Type - The type id of the property.
   -- Returns: True if the class has the property.
   --
   function Has_Property
     (Class_Type    : Safir.Dob.Typesystem.Type_Id;
      Property_Type : Safir.Dob.Typesystem.Type_Id) return Boolean;

   -- Checks if the a class has a property and if it is inherited.
   --
   -- Parameters: Class_Type - The type id of the class to check if it has a specific property.
   --             Property_Type - The type id of the property.
   --             Has_Property - True if the class has the property.
   --             Is_Inherited - Indicates whether the property is set on
   --                            the class itself or whether it was inherited
   --                            from a parent class.
   --
   procedure Has_Property
     (Class_Type     : in Safir.Dob.Typesystem.Type_Id;
      Property_Type  : in Safir.Dob.Typesystem.Type_Id;
      Has_Property   : out Boolean;
      Is_Inherited   : out Boolean);

end Safir.Dob.Typesystem.Operations;
