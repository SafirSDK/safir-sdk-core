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

with Safir.Dob.Typesystem.Operations;
with Ada.Strings.Wide_Fixed;

package body Safir.Dob.Typesystem.Entity_Id is

   function Trim (Source : in Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
                  Side   : in Ada.Strings.Trim_End := Ada.Strings.Both)
                 return Ada.Strings.Wide_Unbounded.Unbounded_Wide_String
      renames Ada.Strings.Wide_Unbounded.Trim;

   function Trim (Source : in Wide_String;
                  Side   : in Ada.Strings.Trim_End := Ada.Strings.Both)
                 return Wide_String
      renames Ada.Strings.Wide_Fixed.Trim;



   function Create_Entity_Id
     (Type_Id     : in Safir.Dob.Typesystem.Type_Id;
      Instance_Id : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type)
      return Entity_Id_Type is
   begin
      return Entity_Id_Type'(Type_Id     => Type_Id,
                             Instance_Id => Instance_Id);
   end Create_Entity_Id;


   function Get_Instance_Id
     (Self : in Entity_Id_Type)
      return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type is
   begin
      return Self.Instance_Id;
   end Get_Instance_Id;


   procedure Set_Instance_Id
     (Self        : in out Entity_Id_Type;
      Instance_Id : in     Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type) is
   begin
      Self.Instance_Id := Instance_Id;
   end Set_Instance_Id;


   function Get_Type_Id (Self : in Entity_Id_Type) return Safir.Dob.Typesystem.Type_Id is
   begin
      return Self.Type_Id;
   end Get_Type_Id;


   procedure Set_Type_Id
     (Self    : in out Entity_Id_Type;
      Type_Id : in     Safir.Dob.Typesystem.Type_Id) is
   begin
      Self.Type_Id := Type_Id;
   end Set_Type_Id;


   procedure Remove_String (Self   : in out Entity_Id_Type) is
   begin
      Safir.Dob.Typesystem.Instance_Id.Remove_String (Self.Instance_Id);
   end Remove_String;


   function To_String (Entity_Id : in Entity_Id_Type)
                       return Ada.Strings.Wide_Unbounded.Unbounded_Wide_String is
      Str : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   begin
      Ada.Strings.Wide_Unbounded.Append (Str, "(");
      if Safir.Dob.Typesystem.Operations.Exists (Entity_Id.Type_Id) then
         Ada.Strings.Wide_Unbounded.Append (Str, Safir.Dob.Typesystem.Operations.Get_Name (Entity_Id.Type_Id));
      else
         Ada.Strings.Wide_Unbounded.Append (Str, "Unknown type: " &
                                               Trim (Safir.Dob.Typesystem.Type_Id'Wide_Image (Entity_Id.Type_Id)));
      end if;

      Ada.Strings.Wide_Unbounded.Append (Str, ", ");


      Ada.Strings.Wide_Unbounded.Append
         (Str, Trim (Safir.Dob.Typesystem.Instance_Id.To_String (Entity_Id.Instance_Id)));

      Ada.Strings.Wide_Unbounded.Append (Str, ")");
      return Str;
   end To_String;


   function To_String_Numeric (Entity_Id : in Entity_Id_Type)
                               return Ada.Strings.Wide_Unbounded.Unbounded_Wide_String is
      Str : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   begin
      Ada.Strings.Wide_Unbounded.Append (Str, "(");
      Ada.Strings.Wide_Unbounded.Append (Str, Trim
                                            (Safir.Dob.Typesystem.Type_Id'Wide_Image (Entity_Id.Type_Id)));
      Ada.Strings.Wide_Unbounded.Append (Str, ",");
      Ada.Strings.Wide_Unbounded.Append
         (Str, Trim
             (Safir.Dob.Typesystem.Int_64'Wide_Image (Safir.Dob.Typesystem.Instance_Id.Get_Raw_Value (Entity_Id.Instance_Id))));

      Ada.Strings.Wide_Unbounded.Append (Str, ")");
      return Str;
   end To_String_Numeric;


   function "<" (L, R : Entity_Id_Type) return Boolean
   is
      use type Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
   begin
      if L.Type_Id /= R.Type_Id then
         return L.Type_Id < R.Type_Id;
      else
         return L.Instance_Id < R.Instance_Id;
      end if;
   end "<";

end Safir.Dob.Typesystem.Entity_Id;
