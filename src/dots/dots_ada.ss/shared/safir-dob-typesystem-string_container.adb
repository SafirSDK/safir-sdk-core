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
with Ada.Exceptions;
with System.Address_To_Access_Conversions;
with Safir.Dob.Typesystem.Utilities; use Safir.Dob.Typesystem.Utilities;
pragma Warnings ("D");  -- turn off warnings for implicit dereference

package body Safir.Dob.Typesystem.String_Container is

   package Conv is new System.Address_To_Access_Conversions (Container);
   package Arr_Conv is new System.Address_To_Access_Conversions (Array_Container);

   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String := "")
                       renames  Ada.Exceptions.Raise_Exception;

   -----------------------------
   -- String_Container operations
   -----------------------------

   procedure Set_Val (Self      : in out Container;
                      Value     : in     Unbounded_Wide_String) is
   begin
      Self.Is_Changed := True;
      Self.Is_Null := False;
      Self.Value := Value;
   end Set_Val;


   function Get_Val (Self : in Container) return Unbounded_Wide_String is
   begin
      if Self.Is_Null then
         raise Null_Exception;
      end if;

      return Self.Value;
   end Get_Val;


   function "=" (Left : in Container; Right : in Container) return Boolean is
   begin
      if not Left.Is_Null and not Right.Is_Null then
         return Left.Value = Right.Value;
      elsif Left.Is_Null and Right.Is_Null then
         return True;
      else
         return False;
      end if;
   end "=";


   function "=" (Self : in Container; Right : in Unbounded_Wide_String) return Boolean is
   begin
      return not Self.Is_Null and then Self.Value = Right;
   end "=";

   function "=" (Left : in Unbounded_Wide_String; Self : in Container) return Boolean is
   begin
      return not Self.Is_Null and then Self.Value = Left;
   end "=";

   overriding function Is_Null (Self : in Container) return Boolean is
   begin
      return Self.Is_Null;
   end Is_Null;


   overriding procedure Set_Null (Self : in out Container) is
   begin
      Self.Is_Changed := True;
      Self.Is_Null := True;
   end Set_Null;


   overriding function Is_Changed (Self : in Container) return Boolean is
   begin
      return Self.Is_Changed;
   end Is_Changed;


   overriding procedure Set_Changed (Self : in out Container;
                                     To   : in     Boolean) is
   begin
      Self.Is_Changed := To;
   end Set_Changed;

   overriding procedure Copy (Self : in out Container;
                              That : in Container_Base_Type'Class) is
   begin
      Self := Container (That);
   end Copy;

   ------------------
   -- Utf8 operations
   ------------------

   function Utf_8_String_Length (Self : in Container)
                                 return Safir.Dob.Typesystem.Int_32 is
      Self_Ptr : constant Conv.Object_Pointer := Conv.To_Pointer (Self'Address);
   begin
      if Self.Is_Null then
         return 0;
      end if;

      if Length (Self.Value) = 0 then
         return 1;
      end if;

      if Length (Self.Cached_Utf_8_String) = 0 then
         Self_Ptr.Cached_Utf_8_String :=
           To_Unbounded_String (To_Utf_8 (Self.Value));
      end if;

      return Safir.Dob.Typesystem.Int_32 (Length (Self.Cached_Utf_8_String) + 1);
   end Utf_8_String_Length;


   function Utf_8_String (Self : in Container) return Unbounded_String is
      Self_Ptr : constant Conv.Object_Pointer := Conv.To_Pointer (Self'Address);
   begin
      if Self.Is_Null then
         Throw (Null_Exception'Identity, "The string is null, cannot convert!");
      end if;

      if Length (Self.Value) /= 0 and Length (Self.Cached_Utf_8_String) = 0 then
         Self_Ptr.Cached_Utf_8_String :=
           To_Unbounded_String (To_Utf_8 (Self.Value));
      end if;

      return Self.Cached_Utf_8_String;
   end Utf_8_String;


   ------------------------------------
   -- String_Container_Proxy operations
   ------------------------------------

   function Create_Container_Proxy (Container_Ptr : in Container_Access)
                                    return Container_Proxy is
      Proxy : Container_Proxy;
   begin
      Proxy.Container_Ptr := Container_Ptr;
      return Proxy;
   end Create_Container_Proxy;

   procedure Set_Val (Self  : in Container_Proxy'Class;
                      Value : in Unbounded_Wide_String) is
   begin
      Self.Container_Ptr.all.Set_Val (Value);
   end Set_Val;


   function Get_Val (Self : in Container_Proxy'Class) return Unbounded_Wide_String is
   begin
      return Self.Container_Ptr.all.Get_Val;
   end Get_Val;


   function "=" (Left : in Container_Proxy'Class; Right : in Container_Proxy'Class) return Boolean is
   begin
      return Left.Container_Ptr.all = Right.Container_Ptr.all;
   end "=";


   function "=" (Self : in Container_Proxy'Class; Right : in Unbounded_Wide_String) return Boolean is
   begin
      return Self.Container_Ptr.all = Right;
   end "=";


   function "=" (Left : in Unbounded_Wide_String; Self : in Container_Proxy'Class) return Boolean is
   begin
      return Left = Self.Container_Ptr.all;
   end "=";


   function Is_Null (Self : in Container_Proxy'Class) return Boolean is
   begin
      return Self.Container_Ptr.all.Is_Null;
   end Is_Null;


   procedure Set_Null (Self : in Container_Proxy'Class) is
   begin
      Self.Container_Ptr.all.Set_Null;
   end Set_Null;


   function Is_Changed (Self : in Container_Proxy'Class) return Boolean is
   begin
      return Self.Container_Ptr.all.Is_Changed;
   end Is_Changed;


   procedure Set_Changed (Self : in Container_Proxy'Class;
                          To   : in     Boolean) is
   begin
      Self.Container_Ptr.all.Set_Changed (To);
   end Set_Changed;

   ------------------------
   -- String Array Container
   ------------------------

   function Element (Self : in Array_Container; Idx : in Safir.Dob.Typesystem.Array_Index)
                           return Container_Access is
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

   ------------------------------
   -- String Array Container Proxy
   ------------------------------

   function Create_Array_Container_Proxy (Array_Container_Ptr : in Array_Container_Access)
                                             return Array_Container_Proxy is
      Proxy : Array_Container_Proxy;
   begin
      Proxy.Array_Container_Ptr := Array_Container_Ptr;
      return Proxy;
   end Create_Array_Container_Proxy;

   function Element (Self : in Array_Container_Proxy'Class; Idx : in Safir.Dob.Typesystem.Array_Index)
                        return Container_Proxy is
   begin
      return Create_Container_Proxy (Self.Array_Container_Ptr.Element (Idx));
   end Element;

   function Is_Changed (Self : in Array_Container_Proxy'Class) return Boolean is
   begin
      return Is_Changed (Self.Array_Container_Ptr.all);
   end Is_Changed;

   procedure Set_Changed (Self : in  Array_Container_Proxy'Class; To : in  Boolean) is
   begin
      Set_Changed (Self.Array_Container_Ptr.all, To);
   end Set_Changed;

   --------------
   -- Constructor
   --------------
   function Create (Val        : in Unbounded_Wide_String;
                    IsNull     : in Boolean;
                    IsChanged  : in Boolean) return Container is
      C : Container;
   begin
      C.Value := Val;
      C.Is_Null := IsNull;
      C.Is_Changed := IsChanged;
      return C;
   end Create;

end Safir.Dob.Typesystem.String_Container;
