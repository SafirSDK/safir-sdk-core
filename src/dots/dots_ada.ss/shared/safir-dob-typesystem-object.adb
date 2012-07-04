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
with Ada.Unchecked_Deallocation;
--with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with System.Address_To_Access_Conversions;
with Safir.Dob.Typesystem.Container_Instantiations; use Safir.Dob.Typesystem.Container_Instantiations;
with Safir.Dob.Typesystem.Kernel;
with Safir.Dob.Typesystem.Object.Factory;

pragma Warnings ("D");  -- turn off warnings for implicit dereference

package body Safir.Dob.Typesystem.Object is

   package Arr_Conv is new System.Address_To_Access_Conversions (Array_Containers.Array_Container);

   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String := "")
                  renames  Ada.Exceptions.Raise_Exception;
   pragma No_Return (Throw);

   procedure Free_Item is
     new Ada.Unchecked_Deallocation (Object_Type'Class, Object_Class_Access);

   procedure Free_Counter is
     new Ada.Unchecked_Deallocation (Natural,
                                     Counter_Access);

   Initial_Size : Safir.Dob.Typesystem.Int_32;

   function Get_Type_Id (Self : in Object_Type) return Safir.Dob.Typesystem.Type_Id is
      pragma Unreferenced (Self);
   begin
      return Class_Type_Id;
   end Get_Type_Id;

   function Is_Changed (Self : in Object_Type) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Changed;

   procedure Set_Changed (Self : in out Object_Type; Changed : in Boolean) is
      pragma Unreferenced (Self, Changed);
   begin
      null;
   end Set_Changed;

   package body Containers is

      package Conv is new System.Address_To_Access_Conversions (Container);

      ---------------------
      -- Container proxy --
      ---------------------

      function Create_Container_Proxy (Container_Ptr : in Container_Access)
                                      return Container_Proxy is
         Proxy : Container_Proxy;
      begin
         Proxy.Container_Ptr := Container_Ptr;
         return Proxy;
      end Create_Container_Proxy;

      procedure Set_Ptr (Self : in Container_Proxy'Class;
                         Ptr  : in Smart_Pointer'Class) is
      begin
         Self.Container_Ptr.Set_Ptr (Ptr);
      end Set_Ptr;

      function Get_Ptr (Self : in Container_Proxy'Class) return Smart_Pointer'Class is
      begin
         return Self.Container_Ptr.Get_Ptr;
      end Get_Ptr;

      function Ref (Self : in Container_Proxy'Class) return Object_Class_Access is
      begin
         return Self.Container_Ptr.Ref;
      end Ref;

      function Is_Null (Self : in Container_Proxy'Class) return Boolean is
      begin
         return Self.Container_Ptr.Is_Null;
      end Is_Null;

      procedure Set_Null (Self : in Container_Proxy'Class) is
      begin
         Self.Container_Ptr.Set_Null;
      end Set_Null;

      function Is_Changed (Self : in Container_Proxy'Class) return Boolean is
      begin
         return Is_Changed (Self.Container_Ptr.all);
      end Is_Changed;

      procedure Set_Changed (Self : in  Container_Proxy'Class;
                             To   : in     Boolean) is
      begin
         Self.Container_Ptr.Set_Changed (To);
      end Set_Changed;

      function Is_Changed_Here (Self : in Container_Proxy'Class) return Boolean is
      begin
         return Self.Container_Ptr.Is_Changed_Here;
      end Is_Changed_Here;

      procedure Set_Changed_Here (Self    : in Container_Proxy'Class;
                                  To      : in Boolean) is
      begin
         Self.Container_Ptr.Set_Changed_Here (To);
      end Set_Changed_Here;

      ---------------
      -- Container --
      ---------------

      function Get_Ptr (Self : in Container) return Smart_Pointer'Class is
      begin
         if Self.Is_Null then
            Throw (Safir.Dob.Typesystem.Null_Exception'Identity, "Object is null");
         end if;
         declare
            Raw_Obj_Ptr : constant Safir.Dob.Typesystem.Object.Object_Class_Access :=
                            Self.Obj_Ptr.Ref;
            -- Create a smart pointer with the correct tag.
            Smart_Ptr : Safir.Dob.Typesystem.Object.Smart_Pointer'Class :=
                    Safir.Dob.Typesystem.Object.Factory.Create_Smart_Ptr
                              (Raw_Obj_Ptr.Get_Type_Id);
         begin
            Internal_Initialize_From_Existing (Smart_Ptr, Self.Obj_Ptr);
            return Smart_Ptr;
         end;
      end Get_Ptr;

      function Ref (Self : in Container) return Object_Class_Access is
      begin
         if Self.Is_Null then
            Throw (Safir.Dob.Typesystem.Null_Exception'Identity, "Object is null");
         end if;
         return Ref (Self.Obj_Ptr);
      end Ref;

      function Is_Null (Self : in Container) return Boolean is
      begin
         return Ref (Self.Obj_Ptr) = null;
      end Is_Null;

      procedure Set_Null (Self : in out Container) is
         Null_Smart_Ptr : Smart_Pointer;
      begin
         Self.Is_Changed := True;
         Self.Obj_Ptr := Null_Smart_Ptr;
      end Set_Null;

      function Is_Changed (Self : in Container) return Boolean is
      begin
         return Self.Is_Changed or else (not Self.Is_Null and then Ref (Self.Obj_Ptr).Is_Changed);
      end Is_Changed;

      procedure Set_Changed (Self : in out Container;
                             To   : in     Boolean) is
      begin
         Self.Is_Changed := To;
         if not Self.Is_Null then
            Ref (Self.Obj_Ptr).Set_Changed (To);
         end if;
      end Set_Changed;

      procedure Copy (Self : in out Container;
                      That : in Container_Base_Type'Class) is
         Converted_That : constant Container := Container (That);
         Null_Smart_Ptr : Smart_Pointer;
         Obj_Access : constant Object_Class_Access := Ref (Converted_That.Obj_Ptr);
      begin
         Self.Is_Changed := Converted_That.Is_Changed;
         if That.Is_Null then
            Self.Obj_Ptr := Null_Smart_Ptr;
         else
            Self.Obj_Ptr := Smart_Pointer (Obj_Access.Clone);
         end if;
      end Copy;

      procedure Set_Ptr (Self : in out Container;
                         Ptr  : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class) is
      begin
         Self.Is_Changed := True;
         Self.Obj_Ptr := Smart_Pointer (Ptr);
      end Set_Ptr;

      function Is_Changed_Here (Self : in Container)
                               return Boolean is
      begin
         return Self.Is_Changed;
      end Is_Changed_Here;

      procedure Set_Changed_Here (Self    : in out Container;
                                  To      : in Boolean) is
      begin
         Self.Is_Changed := To;
      end Set_Changed_Here;

      function Get_Member (Self         : in Container;
                           Member       : in Safir.Dob.Typesystem.Member_Index;
                           Idx          : in Safir.Dob.Typesystem.Array_Index) return
        Safir.Dob.Typesystem.Container_Base.Container_Base_Access is
      begin
         if Self.Is_Null then
            Throw (Safir.Dob.Typesystem.Null_Exception'Identity, "Object is null");
         end if;
         return Ref (Self.Obj_Ptr).Get_Member (Member, Idx);
      end Get_Member;

      function Get_Object_Pointer (Self : in Container) return
      access Safir.Dob.Typesystem.Object.Smart_Pointer'Class is
         Self_Ptr : constant Conv.Object_Pointer := Conv.To_Pointer (Self'Address);
      begin
         return Self_Ptr.Obj_Ptr'Access;
      end Get_Object_Pointer;

      procedure Set_Object_Pointer (Self       : in out Container;
                                    Object_Ptr : access Safir.Dob.Typesystem.Object.Smart_Pointer'Class) is
      begin
         Self.Obj_Ptr := Smart_Pointer (Object_Ptr.all);
      end Set_Object_Pointer;

      procedure Reset_Object_Pointer (Self : in out Container) is
         Null_Smart_Ptr : Smart_Pointer;
      begin
         Self.Obj_Ptr := Null_Smart_Ptr;
      end Reset_Object_Pointer;

      function Calculate_Blob_Size (Self : in Container)
                                    return Safir.Dob.Typesystem.Int_32 is
      begin
         if Self.Is_Null then
            return 0;
         end if;
         return Ref (Self.Obj_Ptr).Calculate_Blob_Size;
      end Calculate_Blob_Size;

      procedure Adjust (Self : in out Container) is
         Obj_Access : constant Object_Class_Access := Ref (Self.Obj_Ptr);
      begin
         if not Self.Is_Null then
            Self.Obj_Ptr := Smart_Pointer (Obj_Access.Clone);
         end if;
      end Adjust;

   end Containers;

   -- ========================
   -- Array containers package
   -- ========================
   package body Array_Containers is

      ---------------------------
      -- Array container proxy --
      ---------------------------

      function Create_Array_Container_Proxy (Array_Container_Ptr : in Array_Container_Access)
                                             return Array_Container_Proxy is
         Proxy : Array_Container_Proxy;
      begin
         Proxy.Array_Container_Ptr := Array_Container_Ptr;
         return Proxy;
      end Create_Array_Container_Proxy;

      function Element (Self : in Array_Container_Proxy'Class; Idx : in Safir.Dob.Typesystem.Array_Index)
                        return Containers.Container_Proxy is
      begin
         return Containers.Create_Container_Proxy (Self.Array_Container_Ptr.Element (Idx));
      end Element;

      function Is_Changed (Self : in Array_Container_Proxy'Class) return Boolean is
      begin
         return Array_Containers.Is_Changed (Self.Array_Container_Ptr.all);
      end Is_Changed;

      procedure Set_Changed (Self : in  Array_Container_Proxy'Class; To   : in  Boolean) is
      begin
         Array_Containers.Set_Changed (Self.Array_Container_Ptr.all, To);
      end Set_Changed;

      ---------------------
      -- Array container --
      ---------------------

      function Element (Self : in Array_Container; Idx : in Safir.Dob.Typesystem.Array_Index)
                        return Containers.Container_Access is
         Self_Ptr : constant Arr_Conv.Object_Pointer := Arr_Conv.To_Pointer (Self'Address);
      begin
         return Self_Ptr.Arr (Idx)'Access;
      end Element;

      function Is_Changed (Self : in Array_Container) return Boolean is
      begin
         for Idx in Self.Arr'Range loop
            if Self.Arr (Idx).Is_Changed then
               return True;
            end if;
         end loop;
         return False;
      end Is_Changed;

      procedure Set_Changed (Self : in out Array_Container; To : in  Boolean) is
      begin
         for Idx in Self.Arr'Range loop
            Self.Arr (Idx).Set_Changed (To);
         end loop;
      end Set_Changed;

   end Array_Containers;

   Dummy : aliased Int_32_Container.Container;
   function Get_Member (Self   : in Object_Type;
                        Member : in Safir.Dob.Typesystem.Member_Index;
                        Idx    : in Safir.Dob.Typesystem.Array_Index)
                        return Safir.Dob.Typesystem.Container_Base.Container_Base_Access is

      pragma Unreferenced (Self, Member);
   begin
      Throw (Software_Violation_Exception'Identity, "Object does not have any members!");
      -- Must return something from this function even though the procedure Throw
      -- has a No_Return pragma. Strange, but this dummy solves it for
      return Dummy'Access;
   end Get_Member;

   procedure Initialize (Self : in out Object_Type;
                         Blob : in Safir.Dob.Typesystem.Blob_T) is
      pragma Unreferenced (Self, Blob);
   begin
      null;
   end Initialize;

   function Calculate_Blob_Size (Self : in Object_Type) return Safir.Dob.Typesystem.Int_32 is
      pragma Unreferenced (Self);
   begin
      return Initial_Size;
   end Calculate_Blob_Size;

   procedure Write_To_Blob (Self                : in Object_Type;
                            Blob                : in Safir.Dob.Typesystem.Blob_T;
                            Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T) is
      pragma Unreferenced (Self, Blob, Beginning_Of_Unused);
   begin
      null;
   end Write_To_Blob;

   function Clone (Self : in Object_Type) return Smart_Pointer'Class is
      Smart_Ptr : Smart_Pointer;
   begin
      Internal_Initialize (Smart_Ptr, new Object_Type'(Self));
      return Smart_Ptr;
   end Clone;

   function Create return Smart_Pointer is
      Smart_Ptr : Smart_Pointer;
   begin
      Internal_Initialize (Smart_Ptr, new Object_Type);
      return Smart_Ptr;
   end Create;

   function Ref (Self : in Smart_Pointer) return Object_Class_Access is
--        function To_Ref is new Ada.Unchecked_Conversion
--          (Safir.Dob.Typesystem.Object.Object_Class_Access,
--           Object_Access);
   begin
      --return To_Ref (Internal_Get_Raw_Ptr (Self));
      return Internal_Get_Raw_Ptr (Self);
   end Ref;

   function Use_Count (Self : in Smart_Pointer) return Natural is
   begin
      if Self.Counter_Ptr = null then
         return 0;
      end if;

      return Self.Counter_Ptr.all;
   end Use_Count;

   procedure Internal_Initialize (Smart_Ptr : in out Smart_Pointer'Class;
                                  Data_Ptr  : in Object_Class_Access) is
   begin
      Smart_Ptr.Data_Ptr := Data_Ptr;
      Smart_Ptr.Counter_Ptr := new Natural;
      Smart_Ptr.Counter_Ptr.all := 1;
   end Internal_Initialize;

   procedure Internal_Initialize_From_Existing (Dest   : in out Smart_Pointer'Class;
                                                Source : in Smart_Pointer'Class) is
   begin
      Dest.Data_Ptr := Source.Data_Ptr;
      Dest.Counter_Ptr := Source.Counter_Ptr;
      if Dest.Counter_Ptr /= null then
         Dest.Counter_Ptr.all := Dest.Counter_Ptr.all + 1;
      end if;
   end Internal_Initialize_From_Existing;

   function Internal_Get_Count_Ptr (Smart_Ptr : in Smart_Pointer'Class)
                                    return Counter_Access is
   begin
      return Smart_Ptr.Counter_Ptr;
   end Internal_Get_Count_Ptr;

   function Internal_Get_Raw_Ptr (Smart_Ptr : in Smart_Pointer'Class)
                                  return Object_Class_Access is
   begin
      return Smart_Ptr.Data_Ptr;
   end Internal_Get_Raw_Ptr;

   overriding procedure Finalize (Self : in out Smart_Pointer) is
   begin
      if Self.Data_Ptr = null then
         return;
      end if;

      Self.Counter_Ptr.all := Self.Counter_Ptr.all - 1;
      if Self.Counter_Ptr.all = 0 then
         Free_Item (Self.Data_Ptr);
         Free_Counter (Self.Counter_Ptr);
      end if;
   end Finalize;

   overriding procedure Adjust (Self : in out Smart_Pointer) is
   begin
      if Self.Data_Ptr /= null then
         Self.Counter_Ptr.all := Self.Counter_Ptr.all + 1;
      end if;
   end Adjust;

   function Create_Object (Blob : in Safir.Dob.Typesystem.Blob_T)
                              return Safir.Dob.Typesystem.Object.Smart_Pointer'Class is
      Ptr              : Object_Access;
      Object_Smart_Ptr : Smart_Pointer;
   begin
      Ptr := new Object_Type;
      Initialize (Ptr.all, Blob);
      Safir.Dob.Typesystem.Object.Internal_Initialize (Object_Smart_Ptr,
                                                       Object_Class_Access (Ptr));
      return Object_Smart_Ptr;
   end Create_Object;

   function Create_Smart_Ptr return Smart_Pointer'Class is
      P : Smart_Pointer;
   begin
      return P;
   end Create_Smart_Ptr;

begin
   Initial_Size := Safir.Dob.Typesystem.Kernel.Get_Initial_Size (Class_Type_Id);
end Safir.Dob.Typesystem.Object;
