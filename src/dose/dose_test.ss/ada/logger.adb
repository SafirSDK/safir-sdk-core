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
with Ada.Characters.Wide_Latin_1;
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Ada.Wide_Text_IO;
with Ada.Wide_Text_IO.Wide_Unbounded_IO;
with Safir.Dob.Typesystem.Serialization;
pragma Warnings ("D"); -- turn off warnings for implicit dereference

package body Logger is

   Buf : Unbounded_Wide_String;

   function To_String (Conn_Info : in Safir.Dob.Connection_Info.Smart_Pointer)
                       return Ada.Strings.Wide_Unbounded.Unbounded_Wide_String is
      Ix : Natural;
   begin
      Conn_Info.Ref.Connection_Id.Set_Null;
      if not Conn_Info.Ref.Connection_Name.Is_Null then
         Ix := Index (Conn_Info.Ref.Connection_Name.Get_Val, "#", Ada.Strings.Backward);
         Conn_Info.Ref.Connection_Name.Set_Val
           (Unbounded_Slice (Conn_Info.Ref.Connection_Name.Get_Val, 1, Ix - 1));
      end if;

      return Safir.Dob.Typesystem.Serialization.To_Xml (Conn_Info);

   end To_String;

   procedure Put_Line (Str : in Wide_String) is
   begin
      Put (Str);
      New_Line;
   end Put_Line;

   procedure Put_Line
     (Str : in Ada.Strings.Wide_Unbounded.Unbounded_Wide_String) is
   begin
      Put (Str);
      New_Line;
   end Put_Line;

   procedure Put (Str : in Wide_String) is
   begin
      Ada.Wide_Text_IO.Put (Str);
      Append (Buf, Str);
   end Put;


   procedure Put
     (Str : in Ada.Strings.Wide_Unbounded.Unbounded_Wide_String) is
   begin
      Ada.Wide_Text_IO.Wide_Unbounded_IO.Put (Str);
      Append (Buf, Str);
   end Put;


   procedure New_Line is
   begin
      Ada.Wide_Text_IO.New_Line;
      Append (Buf, Ada.Characters.Wide_Latin_1.LF);
   end New_Line;


   function Get_String return Unbounded_Wide_String is
   begin
      return Buf;
   end Get_String;

   procedure Clear is
   begin
      Buf := Null_Unbounded_Wide_String;
   end Clear;

end Logger;
