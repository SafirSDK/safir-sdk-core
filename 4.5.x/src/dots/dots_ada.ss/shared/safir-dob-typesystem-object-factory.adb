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
with Ada.Containers.Hashed_Maps;
with Ada.Exceptions;
with Safir.Dob.Typesystem.Blob_Operations;

package body Safir.Dob.Typesystem.Object.Factory is

   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String := "")
                  renames  Ada.Exceptions.Raise_Exception;
   pragma No_Return (Throw);

   -- Object map
   function Type_Id_Hash (Type_Id : in Safir.Dob.Typesystem.Type_Id)
                          return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type'Mod (Type_Id);
   end Type_Id_Hash;

   function Equivalent_Type_Ids (Left, Right : in Safir.Dob.Typesystem.Type_Id)
                                 return Boolean is
   begin
      return Left = Right;
   end Equivalent_Type_Ids;

   type Callback_Element is record
      Create_Obj : Create_Object_Callback;
      Create_Smart_Ptr : Create_Smart_Ptr_Callback;
   end record;

   package Callback_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Safir.Dob.Typesystem.Type_Id,
      Element_Type    => Callback_Element,
      Hash            => Type_Id_Hash,
      Equivalent_Keys => Equivalent_Type_Ids);

   Callback_Map : Callback_Maps.Map;

   function Create_Object (Type_Id : in Safir.Dob.Typesystem.Type_Id)
                              return Object.Smart_Pointer'Class is
      use type Callback_Maps.Cursor;
      C       : Callback_Maps.Cursor;
   begin
      C := Callback_Map.Find (Type_Id);
      if C = Callback_Maps.No_Element then
         Throw (Illegal_Value_Exception'Identity, "There is no such type registered in the ObjectFactory");
      end if;

      -- Invoke the callback.
      return Callback_Maps.Element (C).Create_Obj (null);

   end Create_Object;

   procedure Register_Type (Type_Id                : in Safir.Dob.Typesystem.Type_Id;
                            Obj_Callback           : in not null Create_Object_Callback;
                            Smart_Ptr_Callback     : in not null Create_Smart_Ptr_Callback) is
      CB : Callback_Element;
   begin
      CB.Create_Obj := Obj_Callback;
      CB.Create_Smart_Ptr := Smart_Ptr_Callback;

      Callback_Map.Insert (Type_Id, CB);
   end Register_Type;


   function Create_Object (Blob   : in Safir.Dob.Typesystem.Blob_T)
                           return Object.Smart_Pointer'Class is
      use type Safir.Dob.Typesystem.Blob_T;
      use type Callback_Maps.Cursor;

      Type_Id : Safir.Dob.Typesystem.Type_Id;
      C       : Callback_Maps.Cursor;
   begin
      if Blob = null then
         Throw (Software_Violation_Exception'Identity, "Cannot create object from NULL blob!");
      end if;

      Type_Id := Safir.Dob.Typesystem.Blob_Operations.Get_Type_Id (Blob);
      C := Callback_Map.Find (Type_Id);
      if C = Callback_Maps.No_Element then
         Throw (Illegal_Value_Exception'Identity, "Can't create object, there is no such type registered in the ObjectFactory");
      end if;

      -- Invoke the callback.
      return Callback_Maps.Element (C).Create_Obj.all (Blob);
   end Create_Object;

   function Create_Smart_Ptr (Type_Id : in Safir.Dob.Typesystem.Type_Id)
                              return Object.Smart_Pointer'Class is
      use type Callback_Maps.Cursor;
      C : Callback_Maps.Cursor;
   begin
      C := Callback_Map.Find (Type_Id);
      if C = Callback_Maps.No_Element then
         Throw (Illegal_Value_Exception'Identity, "Can't create smart pointer, there is no such type registered in the ObjectFactory");
      end if;

      -- Invoke the callback.
      return Callback_Maps.Element (C).Create_Smart_Ptr.all;
   end Create_Smart_Ptr;

end Safir.Dob.Typesystem.Object.Factory;
