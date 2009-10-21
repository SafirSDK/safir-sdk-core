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
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Characters.Latin_1;   use Ada.Characters.Latin_1;
with Interfaces.C.Strings;
with Safir.Dob.Typesystem.Kernel;

package body Safir.Dob.Typesystem.Library_Exceptions is

   package C renames Interfaces.C;
   use type C.Strings.chars_ptr;

   --  Aid is AdaExceptionId ==> Ada.Exceptions.Exception_Id
   --  Eid is ExceptionId ==> Safir.Dob.Typesystem.TypeId

   function Hash (Key : in Ada.Exceptions.Exception_Id)
                  return Ada.Containers.Hash_Type;

   function Hash (Key : in Safir.Dob.Typesystem.Type_Id)
                  return Ada.Containers.Hash_Type;
   function Equivalent (Left, Right : in Ada.Exceptions.Exception_Id)
                        return Boolean;
   function Equivalent (Left, Right : in Safir.Dob.Typesystem.Type_Id)
                        return Boolean;

   function Hash (Key : in Ada.Exceptions.Exception_Id)
                  return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Ada.Exceptions.Exception_Name (Key));
   end Hash;

   function Hash (Key : in Safir.Dob.Typesystem.Type_Id)
                  return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key mod 2 ** 32);
   end Hash;

   function Equivalent (Left, Right : in Ada.Exceptions.Exception_Id)
                        return Boolean is
      use type Ada.Exceptions.Exception_Id;
   begin
      return Left = Right;
   end Equivalent;

   function Equivalent (Left, Right : in Safir.Dob.Typesystem.Type_Id)
                        return Boolean is
   begin
      return Left = Right;
   end Equivalent;

   package AidToEidMaps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Exceptions.Exception_Id,
      Element_Type    => Safir.Dob.Typesystem.Type_Id,
      Hash            => Hash,
      Equivalent_Keys => Equivalent);

   use type Ada.Exceptions.Exception_Id;

   package EidToAidMaps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Safir.Dob.Typesystem.Type_Id,
      Element_Type    => Ada.Exceptions.Exception_Id,
      Hash            => Hash,
      Equivalent_Keys => Equivalent);

   AidToEidMap : AidToEidMaps.Map;
   EidToAidMap : EidToAidMaps.Map;


   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String)
                    renames  Ada.Exceptions.Raise_Exception;

   ---------
   -- Set --
   ---------

   procedure Set (X : in Ada.Exceptions.Exception_Occurrence) is
      Cursor               : AidToEidMaps.Cursor;
      Ada_Exception_Id     : constant Ada.Exceptions.Exception_Id := Ada.Exceptions.Exception_Identity (X);
      Exception_Id         : Safir.Dob.Typesystem.Type_Id;

      use type AidToEidMaps.Cursor;
   begin
      Cursor := AidToEidMap.Find (Ada_Exception_Id);
      if AidToEidMaps.No_Element /= Cursor then
         Exception_Id := AidToEidMaps.Element (Cursor);
      else
         Exception_Id := 0;
      end if;
      Safir.Dob.Typesystem.Kernel.Set_Exception
        (Exception_Id,
         C.To_C (Ada.Exceptions.Exception_Information (X)));
   end Set;

   -----------
   -- Throw --
   -----------

   procedure Throw is

      Exception_Id : Safir.Dob.Typesystem.Type_Id;
      Description_Ptr : C.Strings.chars_ptr;

      Deleter     : Safir.Dob.Typesystem.Kernel.String_Deleter_Cb_Type;
      pragma Convention (Convention => C,
                         Entity => Deleter);

      L_Was_Set      : C.char;

      use type EidToAidMaps.Cursor;
   begin
      Safir.Dob.Typesystem.Kernel.Get_And_Clear_Exception
        (Exception_Id, Description_Ptr, Deleter, L_Was_Set);
      if C.char'Pos (L_Was_Set) /= 0 then
         declare
            Description : constant String := C.To_Ada (C.Strings.Value (Description_Ptr));
            Cursor      : EidToAidMaps.Cursor;
         begin
            Deleter.all (Description_Ptr);
            if Exception_Id = 0 then
               Throw (Program_Error'Identity, Description);
            else
               Cursor := EidToAidMap.Find (Exception_Id);

               if EidToAidMaps.No_Element /= Cursor then
                  Throw (EidToAidMaps.Element (Cursor), Description);
               else
                  Throw (Software_Violation_Exception'Identity,
                         "Library_Exceptions.Throw was called when an exception that was not registered in the exception-factory was set in dots_kernel" & LF &
                         "exceptionId = " & Type_Id'Image (Exception_Id) &
                         ", description = '" & Description & "'." & LF &
                         "Please report this to your nearest DOB developer!");
               end if;
            end if;
         end;
      else
         Throw (Software_Violation_Exception'Identity,
                "There was no exception set when Library_Exceptions.Throw was called!");
      end if;
   end Throw;

   ---------------------------
   -- Get_Exception_Type_Id --
   ---------------------------

   function Get_Exception_Type_Id (Ada_Exception_Id  : in Ada.Exceptions.Exception_Id)
                                   return Safir.Dob.Typesystem.Type_Id is
      Cursor : AidToEidMaps.Cursor;
      use type AidToEidMaps.Cursor;
   begin
      Cursor := AidToEidMap.Find (Ada_Exception_Id);
      if AidToEidMaps.No_Element = Cursor then
         Throw (Software_Violation_Exception'Identity,
                "LibraryExceptions. Get_Exception_Type_Id called with an exception that is not registered!");
      end if;
      return AidToEidMaps.Element (Cursor);
   end Get_Exception_Type_Id;

   -----------------------
   -- RegisterException --
   -----------------------

   procedure Register_Exception
     (Exception_Type_Id : in Safir.Dob.Typesystem.Type_Id;
      Ada_Exception_Id  : in Ada.Exceptions.Exception_Id) is
   begin
      AidToEidMap.Insert (Key => Ada_Exception_Id, New_Item => Exception_Type_Id);
      EidToAidMap.Insert (Key => Exception_Type_Id, New_Item => Ada_Exception_Id);
   end Register_Exception;

begin
   Library_Exceptions.Register_Exception (2909620812590558895, Configuration_Error_Exception'Identity);
   Library_Exceptions.Register_Exception (-3653935143986901894, Illegal_Value_Exception'Identity);
   Library_Exceptions.Register_Exception (-5150658527844777416, Incompatible_Types_Exception'Identity);
   Library_Exceptions.Register_Exception (-6392953138294149211, Null_Exception'Identity);
   Library_Exceptions.Register_Exception (-2318636033853590373, Software_Violation_Exception'Identity);
   Library_Exceptions.Register_Exception (-4804695341042352897, Read_Only_Exception'Identity);
end Safir.Dob.Typesystem.Library_Exceptions;
