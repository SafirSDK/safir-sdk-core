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
with Ada.Finalization;
with Safir.Dob.Typesystem.Container_Base; use Safir.Dob.Typesystem.Container_Base;
with Safir.Dob.Typesystem.Object_Container_Base;

package Safir.Dob.Typesystem.Object is

   type Object_Type is tagged private;

   type Object_Class_Access is access all Object_Type'Class;
   type Object_Access is access all Object_Type;

   type Smart_Pointer is tagged private;

   --  Type id for Object
   Class_Type_Id : constant Safir.Dob.Typesystem.Type_Id := 5955188366590963785;

   -- Get the type id of the object.
   --
   -- This method is overridden by all auto-generated classes.
   --
   -- Returns: The TypeId of the object.
   --
   function Get_Type_Id (Self : in Object_Type) return Safir.Dob.Typesystem.Type_Id;

   -- Check if any member of this object is changed.
   --
   -- This method will recursively check if any member of the object has its
   -- change flag set.
   --
   -- This method is overridden by all auto-generated classes.
   --
   -- Returns: True if any member has changed.
   --
   function Is_Changed (Self : in Object_Type) return Boolean;

   -- Recursively set change flags in all members of this object.
   --
   -- This method is overridden by all auto-generated classes.
   --
   -- Parameters: Changed - The value to set the change flags to
   --
   procedure Set_Changed (Self : in out Object_Type; Changed : in Boolean);

   -- ==================
   -- Containers package
   -- ==================
   package Containers is

      type Container is new
        Ada.Finalization.Controlled and
        Safir.Dob.Typesystem.Object_Container_Base.Object_Container_Base_Type with private;
      type Container_Access is access all Container;

      -- The type returned from generated member functions.
      type Container_Proxy is tagged private;

      ---------------------
      -- Container proxy --
      ---------------------

      function Create_Container_Proxy (Container_Ptr : in Container_Access)
                                    return Container_Proxy;

      -- Set the smart pointer in the container.
      --
      -- This method will set the contained pointer to point to another object.
      -- The change flag of the container will be updated.
      --
      -- Parameters: Ptr - A pointer to the new object to point to.
      --
      procedure Set_Ptr (Self : in Container_Proxy'Class;
                         Ptr  : in Smart_Pointer'Class);

      -- Get the smart pointer from the container.
      --
      -- This method will return the contained smart pointer unless the container is null, then
      -- an exception will be thrown.
      --
      -- Returns: A smart pointer to the contained object.
      -- Exceptions: Null_Exception - The container is null.
      --
      function Get_Ptr (Self : in Container_Proxy'Class) return Smart_Pointer'Class;

      -- Returns the raw access value.
      --
      -- Use it to get access to the pointed to Type.
      -- Usually you don't want to store the access value in a variable because then
      -- all bets are off regarding refrence counting. In this case you are
      -- responsible for not using the access value after the corresponding
      -- container has gone out of scope.
      --
      function Ref (Self : in Container_Proxy'Class) return Object_Class_Access;

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
      -- Note: This call will recursively check change flags in the contained object.
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
      -- Note: This call will recursively set all the change flags in the contained object.
      --
      procedure Set_Changed (Self : in  Container_Proxy'Class;
                             To   : in  Boolean);

      -- Is the change flag in the container set?
      --
      -- This method is like IsChanged without the recursion (on object containers
      -- Is_Changed is recursive).
      --
      -- Returns: True if the containers change flag is set.
      --
      function Is_Changed_Here (Self : in Container_Proxy'Class) return Boolean;

      -- Set the change flag in the container.
      --
      -- This method is like SetChanged without the recursion (on object containers
      -- Set_Changed is recursive).
      --
      -- Parameters: Changed - The value to set the change flag to.
      --
      procedure Set_Changed_Here (Self    : in out Container_Proxy'Class;
                                  To      : in Boolean);

      --------------------------------------------------------------------------
      -- Container.
      -- A container is returned only for self-referencing members so in most --
      -- cases you don't care about these operations.
      --------------------------------------------------------------------------

      function Get_Ptr (Self : in Container) return Smart_Pointer'Class;

      function Ref (Self : in Container) return Object_Class_Access;

      overriding
      function Is_Null (Self : in Container) return Boolean;

      overriding
      procedure Set_Null (Self : in out Container);

      overriding
      function Is_Changed (Self : in Container) return Boolean;

      overriding
      procedure Set_Changed (Self : in out Container;
                             To   : in     Boolean);

      overriding
      procedure Copy (Self : in out Container;
                      That : in Container_Base_Type'Class);

      overriding
      procedure Set_Ptr (Self : in out Container;
                         Ptr  : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class);

      overriding
      function Is_Changed_Here (Self : in Container)
                             return Boolean;

      overriding
      procedure Set_Changed_Here (Self    : in out Container;
                                  To      : in Boolean);

      function Calculate_Blob_Size (Self : in Container)
                                   return Safir.Dob.Typesystem.Int_32;

      overriding
      function Get_Member (Self         : in Container;
                           Member       : in Safir.Dob.Typesystem.Member_Index;
                           Idx          : in Safir.Dob.Typesystem.Array_Index)
                           return Safir.Dob.Typesystem.Container_Base.Container_Base_Access;

      overriding
      function Get_Object_Pointer (Self : in Container) return
      access Safir.Dob.Typesystem.Object.Smart_Pointer'Class;

      overriding
      procedure Set_Object_Pointer (Self       : in out Container;
                                    Object_Ptr : access Safir.Dob.Typesystem.Object.Smart_Pointer'Class);

      overriding
      procedure Reset_Object_Pointer (Self : in out Container);

      overriding
      procedure Adjust (Self : in out Container);

   private
      type Container is new
        Ada.Finalization.Controlled and
        Safir.Dob.Typesystem.Object_Container_Base.Object_Container_Base_Type with record
         Obj_Ptr : aliased Smart_Pointer;
         Is_Changed : Boolean := False;
      end record;

      type Container_Proxy is tagged record
         Container_Ptr : Container_Access;
      end record;

   end Containers;

   -- ========================
   -- Array containers package
   -- ========================
   package Array_Containers is

      type Array_Container (Max_Index : Safir.Dob.Typesystem.Index) is tagged private;
      type Array_Container_Access is access all Array_Container;

      type Array_Container_Proxy is tagged private;

      ---------------------------
      -- Array container proxy --
      ---------------------------

      function Create_Array_Container_Proxy (Array_Container_Ptr : in Array_Container_Access)
                                                return Array_Container_Proxy;

      -- Access to an array element
      --
      function Element (Self : in Array_Container_Proxy'Class; Idx : in Safir.Dob.Typesystem.Array_Index)
                           return Containers.Container_Proxy;

      -- Check if any element in the array has a change flag set on it.
      --
      -- The change flag gets updated every time the contained value changes.
      -- Note: This call will recursively check change flags in the contained object.
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
      -- Note: This call will recursively set all the change flags in the contained object.
      --
      procedure Set_Changed (Self : in  Array_Container_Proxy'Class;
                             To   : in  Boolean);

      ---------------------
      -- Array container --
      ---------------------

      function Element (Self : in Array_Container; Idx : in Safir.Dob.Typesystem.Array_Index)
                           return Containers.Container_Access;

      function Is_Changed (Self : in Array_Container) return Boolean;

      procedure Set_Changed (Self : in out Array_Container;
                             To   : in  Boolean);

   private

      type Array_T is array (Safir.Dob.Typesystem.Array_Index range <>) of aliased Containers.Container;

      type Array_Container (Max_Index : Safir.Dob.Typesystem.Index) is tagged record
         Arr : Array_T (0 .. Max_Index);
      end record;

      type Array_Container_Proxy is tagged record
         Array_Container_Ptr : Array_Container_Access;
      end record;

   end Array_Containers;

   -- ================
   -- Reflection part.
   -- These methods allow applications to manipulate the members of objects
   -- without having been compiled against it.
   -- There should be no reason for most applications to use these methods.

   -- Get a pointer to a member container from an object.
   --
   -- Use the methods in Members to get member indices and array sizes for use
   -- with this method.
   --
   -- Note: Do not use this method unless you're very sure it is the one you need!
   --
   -- Parameters: Member - The index of the member to get.
   --             Index - The array index of the member to get.
   -- Returns: A pointer to a member container.
   -- Exceptions: Illegal_Value_Exception - The index is not in the range of the array.
   --
   function Get_Member (Self   : in Object_Type;
                        Member : in Safir.Dob.Typesystem.Member_Index;
                        Idx    : in Safir.Dob.Typesystem.Array_Index)
                        return Safir.Dob.Typesystem.Container_Base.Container_Base_Access;

   -- ========================================
   -- Blob serialization/deserialization part.
   -- These subprograms are for internal use only!
   -- Their names and functionality are likely to change in the near future!


   -- Initialize an Object from a blob
   --
   -- Parameters : Blob - The blob to deserialize.
   --
   procedure Initialize (Self : in out Object_Type;
                         Blob : in Safir.Dob.Typesystem.Blob_T);

   -- Calculate the size of the blob-serialized form of this object.
   --
   -- Returns: The needed size in bytes.
   --
   function Calculate_Blob_Size (Self : in Object_Type)
                                 return Safir.Dob.Typesystem.Int_32;

   -- Write the object to a blob.
   --
   -- Note that the size of the blob is assumed to be correct! No checks are made!
   --
   -- Parameters: Blob - The blob to write to.
   --             Beginning_Of_Unused - The beginning of unused dynamic blob space.
   --
   procedure Write_To_Blob (Self                : in Object_Type;
                            Blob                : in Safir.Dob.Typesystem.Blob_T;
                            Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T);

   -- Returns the reference count.
   --
   function Use_Count (Self : in Smart_Pointer) return Natural;

   -- Create a copy of the object.
   --
   -- Will create a copy of the object on the heap and return a smart pointer to it.
   --
   -- Returns:  A smart pointer to the copy of the object.
   --
   function Clone (Self : in Object_Type) return Smart_Pointer'Class;

   -- Creates an Object_Type and returns a Smart_Pointer to it.
   --
   function Create return Smart_Pointer;

   -- Returns the raw access value.
   --
   -- Use it to get access to the pointed to Type like this:
   -- My_Ptr.Ref.My_Member or My_Ptr.Ref.all.
   -- Usually you don't want to store the access value in a variable because then
   -- all bets are off regarding refrence counting. In this case you are
   -- responsible for not using the access value after the corresponding
   -- Smart_Pointer has gone out of scope.
   --
   function Ref (Self : in Smart_Pointer) return Object_Class_Access;

   ----------------------
   -- Internal subprograms and types. NOT FOR PUBLIC USAGE!!
   --
   type Counter_Access is access all Natural;
   procedure Internal_Initialize (Smart_Ptr : in out Smart_Pointer'Class;
                                  Data_Ptr  : in Object_Class_Access);
   procedure Internal_Initialize_From_Existing (Dest : in out Smart_Pointer'Class;
                                                Source : in Smart_Pointer'Class);

   function Internal_Get_Count_Ptr (Smart_Ptr : in Smart_Pointer'Class)
                                    return Counter_Access;
   function Internal_Get_Raw_Ptr (Smart_Ptr : in Smart_Pointer'Class) return Object_Class_Access;
   function Create_Object (Blob : in Safir.Dob.Typesystem.Blob_T)
                           return Smart_Pointer'Class;
   function Create_Smart_Ptr return Smart_Pointer'Class;

private

   type Object_Type is tagged null record;

   type Smart_Pointer is new Ada.Finalization.Controlled with
      record
         Data_Ptr    : Object_Class_Access;
         Counter_Ptr : Counter_Access;
      end record;

   pragma Warnings (Off); -- declaration of "Finalize" and "Adjust" hides one
   procedure Finalize (Self : in out Smart_Pointer);
   procedure Adjust (Self : in out Smart_Pointer);
   pragma Warnings (On);

end Safir.Dob.Typesystem.Object;
