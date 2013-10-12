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

with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Safir.Dob.Typesystem.Object;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Typesystem.Channel_Id;
with Safir.Dob.Typesystem.Handler_Id;


--  Functions for getting parameter information from types.
--
--  With these operations you can get parameter values from types.
--  You can also get information about the parameters in a type, such as
--  parameter names and indexes, TypeIds of parameters etc.
--
package Safir.Dob.Typesystem.Parameters is

   -- Get the number of parameters defined in a class.
   --
   -- Parameters: Type_Id - Type id of class.
   -- Returns: The number of parameters.
   -- Exceptions: Illegal_Value_Exception - There is no such type.
   --
   function Get_Number_Of_Parameters
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Safir.Dob.Typesystem.Int_32;

   -- Get index of a named parameter.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter_Name - Name of parameter.
   -- Returns: Index of the named parameter.
   -- Exceptions: Illegal_Value_Exception - There is no such type or parameter defined.
   --
   function Get_Index
     (Type_Id       : Safir.Dob.Typesystem.Type_Id;
      Parameter_Name : Unbounded_Wide_String) return Safir.Dob.Typesystem.Parameter_Index;

   -- Get the name of the specified parameter as it was defined in the xml description
   --
   -- If the parameter does not exist the returned value is undefined. Use
   -- Get_Index to get a valid ParameterIndex, which is guaranteed to exist.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   -- Returns: The name of the parameter.
   --
   function Get_Name
     (Type_Id    : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index) return Unbounded_Wide_String;

   -- Get the type of a parameter.
   --
   -- If the parameter does not exist the returned value is undefined. Use
   -- Get_Index to get a valid ParameterIndex, which is guaranteed to exist.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   -- Returns: The type of the parameter.
   --
   function Get_Type
     (Type_Id    : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index) return Safir.Dob.Typesystem.Member_Type;

   -- Gets a string representation of the type of a parameter.
   --
   -- If the parameter is not an object or enumeration the result is undefined.
   --
   -- If the parameter does not exist the returned value is undefined. Use
   -- Get_Index to get a valid ParameterIndex, which is guaranteed to exist.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   -- Returns: Name of the parameter type.
   --
   function Get_Type_Name
     (Type_Id    : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index) return Unbounded_Wide_String;


   -- Get the array size of a parameter.
   --
   -- If the parameter does not exist the returned value is undefined. Use
   -- Get_Index to get a valid Parameter_Index, which is guaranteed to exist.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   -- Returns: The array size of the parameter, or 1 if it is not an array.
   --
   function Get_Array_Size
     (Type_Id : in Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index) return Safir.Dob.Typesystem.Int_32;


   -- =========================================================================
   -- Parameter values.
   -- =========================================================================

   -- Get a boolean parameter value.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   --             Index - Array index. If parameter is not an array this shall be 0.
   -- Returns: Parameter value.
   --
   function Get_Boolean
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Boolean;

   -- Get an enumeration parameter value.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   --             Index - Array index. If parameter is not an array this shall be 0.
   -- Returns: Parameter value.
   --
   function Get_Enumeration
     (Type_Id       : Safir.Dob.Typesystem.Type_Id;
      Parameter     : Safir.Dob.Typesystem.Parameter_Index;
      Index         : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Int_32;

   -- Get an Int_32 parameter value.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   --             Index - Array index. If parameter is not an array this shall be 0.
   -- Returns: Parameter value.
   --
   function Get_Int_32
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Int_32;

   -- Get an Int_64 parameter value.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   --             Index - Array index. If parameter is not an array this shall be 0.
   -- Returns: Parameter value.
   --
   function Get_Int_64
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Int_64;

   -- Get a Float_32 parameter value.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   --             Index - Array index. If parameter is not an array this shall be 0.
   -- Returns: Parameter value.
   function Get_Float_32
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Float_32;

   -- Get a Float_64 parameter value.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   --             Index - Array index. If parameter is not an array this shall be 0.
   -- Returns: Parameter value.
   function Get_Float_64
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Float_64;

   -- Get a string parameter value.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   --             Index - Array index. If parameter is not an array this shall be 0.
   -- Returns: Parameter value.
   --
   function Get_String
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Unbounded_Wide_String;

   -- Get a Type_Id parameter value.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   --             Index - Array index. If parameter is not an array this shall be 0.
   -- Returns: Parameter value.
   --
   function Get_Type_Id
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Type_Id;

   -- Get an Instance_Id parameter value.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   --             Index - Array index. If parameter is not an array this shall be 0.
   -- Returns: Parameter value.
   --
   function Get_Instance_Id
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;

   -- Get an Entity Id parameter value.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   --             Index - Array index. If parameter is not an array this shall be 0.
   -- Returns: Parameter value.
   --
   function Get_Entity_Id
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;

   -- Get a Channel_Id parameter value.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   --             Index - Array index. If parameter is not an array this shall be 0.
   -- Returns: Parameter value.
   --
   function Get_Channel_Id
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;

   -- Get a Handler_Id parameter value.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   --             Index - Array index. If parameter is not an array this shall be 0.
   -- Returns: Parameter value.
   --
   function Get_Handler_Id
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

   -- Get an Object parameter value.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   --             Index - Array index. If parameter is not an array this shall be 0.
   -- Returns: Parameter value.
   --
   function Get_Object
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Object.Smart_Pointer'Class;

   -- Get a Binary parameter value.
   --
   -- Parameters: Type_Id - Type id.
   --             Parameter - Index of parameter.
   --             Index - Array index. If parameter is not an array this shall be 0.
   -- Returns: Parameter value.
   --
   function Get_Binary
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Binary_Vectors.Vector;

end Safir.Dob.Typesystem.Parameters;
