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


-- Container and related container proxy for base types.
--
-- Holds a value of the generic parameter type and a null flag.
-- The operations that modify the value update the null flag and the change flag.
--
-- This container is intended for the simple types of the DOB typesystem.
-- There should be no need to use this type, since all the relevant
-- instantiations are already made.
--
generic
   type Contained_Type is private;
   Initial_Value : Contained_Type;
package Safir.Dob.Typesystem.Value_Container is

   ------------------
   -- Value Container
   ------------------
   type Container is new Container_Base_Type with private;

   type Container_Access is access all Container;

   -- Set the value of the container.
   -- Null and change flags are updated accordingly.
   --
   -- Parameters: Value - The new value.
   --
   procedure Set_Val (Self  : in out Container;
                      Value : in Contained_Type);

   -- Get the value of the container.
   --
   -- Returns: The value of the container.
   -- Exceptions: Null_Exception - The container is null.
   --
   function Get_Val (Self : in Container) return Contained_Type;

   -- Equality operator.
   -- This operator lets you compare two containers. It will return false if
   -- one, and only one, container is null and true if both containers are null.
   -- The change flags are ignored.
   --
   function "=" (Left : in Container; Right : in Container) return Boolean;

   -- Equality operator.
   -- This operator lets you compare the container with a value of
   -- The Contained type. It will return false if the container is null or
   -- the values are not equal. The change flag is ignored.
   --
   function "=" (Self : in Container; Right : in Contained_Type) return Boolean;

   -- Equality operator that enables expressions of the form
   -- Contained_Type = Container.
   --
   function "=" (Left : in Contained_Type; Self : in Container) return Boolean;

   -- Implementation of Container_Base interface.
   --
   overriding function Is_Null (Self : in Container) return Boolean;

   -- Implementation of Container_Base interface.
   --
   overriding procedure Set_Null (Self : in out Container);

   -- Implementation of Container_Base interface.
   --
   overriding function Is_Changed (Self : in Container) return Boolean;

   -- Implementation of Container_Base interface.
   --
   overriding procedure Set_Changed (Self : in out Container;
                                     To   : in     Boolean);

   -- Implementation of Container_Base interface.
   --
   overriding procedure Copy (Self : in out Container;
                              That : in Container_Base_Type'Class);

   ------------------------
   -- Value Container Proxy
   --
   -- This is the type returned from generated member functions. The main
   -- purpose is to encapsulate the pointer to the Value_Container. If
   -- this pointer were directly accessible assignments of the form
   -- A.My_Member.all := B.Other_Member.all would be allowed and we don't want
   -- that. For instance, it will cause the change flags not to be handled
   -- correctly.
   --
   -- Note that this type is tagged only to enable prefix notation. You are not
   -- supposed to inherit from this type.
   ------------------------
   type Container_Proxy is tagged private;

   function Create_Container_Proxy (Container_Ptr : in Container_Access)
                                    return Container_Proxy;

   -- Set the value of the container.
   -- Null and change flags are updated accordingly.
   --
   -- Parameters: Value - The new value.
   --
   procedure Set_Val (Self  : in Container_Proxy'Class;
                      Value : in Contained_Type);

   -- Get the value of the container.
   --
   -- Returns: The value of the container.
   -- Exceptions: Null_Exception - The container is null.
   --
   function Get_Val (Self : in Container_Proxy'Class) return Contained_Type;

   -- Equality operator.
   -- This operator lets you compare two containers. It will return false if
   -- one, and only one, container is null and true if both containers are null.
   -- The change flags are ignored.
   --
   function "=" (Left : in Container_Proxy'Class; Right : in Container_Proxy'Class) return Boolean;

   -- Equality operator.
   -- This operator lets you compare the container with a value of
   -- The Contained type. It will return false if the container is null or
   -- the values are not equal. The change flag is ignored.
   --
   function "=" (Self : in Container_Proxy'Class; Right : in Contained_Type) return Boolean;

   -- Equality operator that enables expressions of the form
   -- Contained_Type = Container.
   --
   function "=" (Left : in Contained_Type; Self : in Container_Proxy'Class) return Boolean;

   -- Is the container set to null?
   --
   -- Returns: True if the container is set to null.
   --
   function Is_Null (Self : in Container_Proxy'Class) return Boolean;

   -- Set the container to null.
   --
   procedure Set_Null (Self : in Container_Proxy'Class);

   -- Is the change flag set on the container?
   --
   -- The change flag gets updated every time the contained value changes.
   -- Note: If this is a container containing an object this call will recursively
   -- check change flags in the contained object.
   --
   function Is_Changed (Self : in Container_Proxy'Class) return Boolean;

   -- Set the container's change flag.
   --
   -- It should be fairly unusual for an application to have to use this
   -- operation. There is nothing dangerous about it, but are you sure this
   -- is the operation you were after?
   --
   -- The change flag is how receivers of objects can work out what the
   -- sender really wanted done on the object.
   --
   -- Note: If this is a container containing an object this call will recursively
   -- set all the change flags in the contained object.
   --
   procedure Set_Changed (Self : in  Container_Proxy'Class;
                          To   : in     Boolean);

   ------------------------
   -- Value Array Container
   ------------------------
   type Array_Container (Max_Index : Safir.Dob.Typesystem.Index) is tagged private;

   type Array_Container_Access is access all Array_Container;

   function Element (Self : in Array_Container; Idx : in Safir.Dob.Typesystem.Array_Index)
                           return Container_Access;

   function Is_Changed (Self : in Array_Container) return Boolean;

   procedure Set_Changed (Self : in out Array_Container;
                          To   : in  Boolean);

   ------------------------------
   -- Value Array Container Proxy
   ------------------------------
   type Array_Container_Proxy is tagged private;

   function Create_Array_Container_Proxy (Array_Container_Ptr : in Array_Container_Access)
                                                return Array_Container_Proxy;

   --
   function Element (Self : in Array_Container_Proxy'Class; Idx : in Safir.Dob.Typesystem.Array_Index)
                        return Container_Proxy;

   -- Check if any element in the array has a change flag set on it.
   --
   -- The change flag gets updated every time the contained value changes.
   --
   function Is_Changed (Self : in Array_Container_Proxy'Class) return Boolean;

   -- Set the change flag on all elements in the array.
   --
   -- It should be fairly unusual for an application to have to use this
   -- operation. There is nothing dangerous about it, but are you sure this
   -- is the operation you were after?
   --
   -- The change flag is how receivers of objects can work out what the
   -- sender really wanted done on the object.
   --
   procedure Set_Changed (Self : in  Array_Container_Proxy'Class;
                          To   : in  Boolean);


   ---------------
   -- Constructor
   ---------------
   function Create (Val        : in Contained_Type;
                    IsNull     : in Boolean;
                    IsChanged  : in Boolean) return Container;

private
   type Container is new Container_Base_Type with record
      Is_Changed : Boolean := False;
      Is_Null    : Boolean := True;
      Value      : Contained_Type := Initial_Value;
   end record;

   type Container_Proxy is tagged record
      Container_Ptr : Container_Access;
   end record;

   type Array_T is array (Safir.Dob.Typesystem.Array_Index range <>) of aliased Container;

   type Array_Container (Max_Index : Safir.Dob.Typesystem.Index) is tagged record
      Arr : Array_T (0 .. Max_Index);
   end record;

   type Array_Container_Proxy is tagged record
      Array_Container_Ptr : Array_Container_Access;
   end record;

end Safir.Dob.Typesystem.Value_Container;
