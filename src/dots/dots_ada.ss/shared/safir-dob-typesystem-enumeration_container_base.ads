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
with Safir.Dob.Typesystem.Container_Base; use Safir.Dob.Typesystem.Container_Base;

-- Package containing a base type for containers of enumeration values.
-- The containers for enumerations are defined in the automatically generated
-- code, but this type defines the common functionality for them.
-- Enumeration containers really store the ordinal values (integer
-- representation of the enumeration), and this type has methods for setting
-- and getting the ordinal. The derived type (in the generated code) has methods
-- for setting and getting the value as an enumeration value.
-- Most applications should not use the GetOrdinal/SetOrdinal functions,
-- but should use the SetVal and GetVal methods defined in the derived classes.
--
package Safir.Dob.Typesystem.Enumeration_Container_Base is
   pragma Preelaborate (Safir.Dob.Typesystem.Enumeration_Container_Base);

   type Enumeration_Container_Base_Type is interface and Container_Base_Type;

   type Enumeration_Container_Base_Access is
     access all Enumeration_Container_Base_Type'Class;

   -- Set the ordinal value of the enumeration container.
   --
   -- Note: Only applications that need to use "anonymous enums" should
   -- use this method. All other applications should be using the SetVal method.
   --
   -- Parameters: Value - The new value.
   -- Exceptions: Illegal_Value_Exception - The value is not in the range of
   --                                       the enumeration.
   --
   procedure Set_Ordinal (Self : in out Enumeration_Container_Base_Type;
                          Value : in Safir.Dob.Typesystem.Enumeration_Value) is abstract;

   -- Get the ordinal value of the enumeration container.
   --
   -- Note: Only applications that need to use "anonymous enums" should
   -- use this method. All other applications should be using the GetVal method.
   --
   -- Returns: The ordinal value of the container.
   -- Exceptions: Null_Exception - The container is null.
   --
   function Get_Ordinal (Self : in Enumeration_Container_Base_Type)
                         return Safir.Dob.Typesystem.Enumeration_Value is abstract;


   -- For internal usage only! (Needed in the public interface because Ada has no
   -- "friend" mechanism)
   procedure Init (Self       : in out Enumeration_Container_Base_Type;
                   Value      : in Safir.Dob.Typesystem.Enumeration_Value;
                   Is_Null    : in Boolean;
                   Is_Changed : in Boolean) is abstract;

end Safir.Dob.Typesystem.Enumeration_Container_Base;
