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
limited with Safir.Dob.Typesystem.Object;
with Safir.Dob.Typesystem.Container_Base;

package Safir.Dob.Typesystem.Object_Container_Base is
   pragma Preelaborate (Safir.Dob.Typesystem.Object_Container_Base);

   -- Base class for all object containers.
   --
   -- The reason for the existence of this class is that code that uses the reflection
   -- functionality must be able to get hold of members of items.
   --
   type Object_Container_Base_Type is interface and
     Safir.Dob.Typesystem.Container_Base.Container_Base_Type;
   type Object_Container_Base_Access is access all Object_Container_Base_Type'Class;

   -- Set the smart pointer in the container.
   --
   -- This method will set the contained pointer to point to another
   -- object. Checks are always made to see that it is of the correct type.
   -- The change flag of the container will be updated.
   --
   -- Parameters: Ptr - A pointer to the new object to point to.
   -- Exceptions: Constraint_Error - If Ptr is not of the type contained by the container.
   --
   procedure Set_Ptr (Self : in out Object_Container_Base_Type;
                      Ptr  : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class) is abstract;

   -- Is the change flag in the container set?
   --
   -- This method is like IsChanged without the recursion (on object containers
   -- Is_Changed is recursive).
   --
   -- Returns: True if the containers change flag is set.
   --
   function Is_Changed_Here (Self : in Object_Container_Base_Type)
                             return Boolean is abstract;

   -- Set the change flag in the container.
   --
   -- This method is like SetChanged without the recursion (on object containers
   -- Set_Changed is recursive).
   --
   -- Parameters: To - The value to set the change flag to.
   --
   procedure Set_Changed_Here (Self    : in out Object_Container_Base_Type;
                               To      : in Boolean) is abstract;

   -- ===============
   -- Reflection part.
   -- These methods allow applications to manipulate the members of objects
   -- without having been compiled against it.
   -- There should be no reason for most applications to use these methods.

   -- Get a reference to a member container from an object.
   --
   -- Use the methods in Members to get member indices and array sizes for use
   -- with this method.
   --
   -- Note: Do not use this method unless you're very sure it is the one you need!
   --
   -- Parameters: Member - The index of the member to get.
   --             Index - he array index of the member to get.
   -- Returns: The member container.
   -- Exceptions: Illegal_Value_Exception - the index is not in the range of the array.
   -- Software_Violation_Exception - the element is not an array and the index is not 0.
   --
   function Get_Member (Self   : in Object_Container_Base_Type;
                        Member : in Safir.Dob.Typesystem.Member_Index;
                        Idx    : in Safir.Dob.Typesystem.Array_Index) return
     Safir.Dob.Typesystem.Container_Base.Container_Base_Access is abstract;

   -- Get an access value to a smart pointer to the contained object.
   --
   -- This method does not check if the container is null!
   --
   -- Note: Do not use this method unless you're very sure it is the one you need!
   --
   function Get_Object_Pointer (Self : in Object_Container_Base_Type) return
   access Safir.Dob.Typesystem.Object.Smart_Pointer'Class is abstract;

   -- Set the smart pointer in the container.
   --
   -- This method will set the contained pointer to point to another
   -- object. Checks are NOT always made to see that it is of the correct type.
   --
   -- Warning: This method does not update the change flag!
   --
   -- Note: Do not use this method unless you're very sure it is the one you need!
   --
   -- Parameters: Obj_Ptr - An access value to a pointer to the object.
   --
   procedure Set_Object_Pointer (Self       : in out Object_Container_Base_Type;
                                 Object_Ptr : access Safir.Dob.Typesystem.Object.Smart_Pointer'Class) is abstract;

   -- Reset (ie set to null) the contained pointer.
   -- Warning: This method does not update the change flag!
   --
   -- Note: Do not use this method unless you're very sure it is the one you need!
   --
   procedure Reset_Object_Pointer (Self : in out Object_Container_Base_Type) is abstract;

end Safir.Dob.Typesystem.Object_Container_Base;
