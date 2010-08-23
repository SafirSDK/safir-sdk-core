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
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Utilities; use Safir.Dob.Typesystem.Utilities;
with Safir.Dob.Typesystem.Library_Exceptions;
with Safir.Dob.Interf;
with Interfaces.C.Strings;

package body Safir.Dob.Connection_Aspect_Miscs is

   package C renames Interfaces.C;

   function Create
     (Connection_Base : in  Safir.Dob.Connection_Bases.Connection_Base'Class) return Connection_Aspect_Misc is

      Aspect : Connection_Aspect_Misc;
   begin
      Aspect.Controller_Id := Connection_Base.Get_Controller_Id;
      return Aspect;
   end Create;

   function Get_Current_Callback_Id (Self : in Connection_Aspect_Misc)
                                     return Safir.Dob.Callback_Id.Enumeration is
      Callback : Safir.Dob.Typesystem.Int_32;
      Success  : C.char;
   begin
      Safir.Dob.Interf.Get_Current_Callback_Id (Self.Controller_Id,
                                                Callback,
                                                Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      return Safir.Dob.Callback_Id.Enumeration'Val (Callback);

   end Get_Current_Callback_Id;


   function Get_Connection_Name (Self : in Connection_Aspect_Misc)
                                 return Unbounded_Wide_String is
      Name    : C.Strings.chars_ptr;
      Success : C.char;
   begin
      Safir.Dob.Interf.Get_Connection_Name (Self.Controller_Id,
                                            Name,
                                            Success);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      return From_Utf_8 (C.To_Ada (C.Strings.Value (Name)));

   end Get_Connection_Name;

   function Get_Connection_Name_Common_Part (Self : in Connection_Aspect_Misc)
                                             return Unbounded_Wide_String is
      Name    : C.Strings.chars_ptr;
      Success : C.char;
   begin
      Safir.Dob.Interf.Get_Connection_Name_Common_Part (Self.Controller_Id,
                                                        Name,
                                                        Success);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      return From_Utf_8 (C.To_Ada (C.Strings.Value (Name)));

   end Get_Connection_Name_Common_Part;

   function Get_Connection_Name_Instance_Part (Self : in Connection_Aspect_Misc)
                                               return Unbounded_Wide_String is
      Name    : C.Strings.chars_ptr;
      Success : C.char;
   begin
      Safir.Dob.Interf.Get_Connection_Name_Instance_Part (Self.Controller_Id,
                                                          Name,
                                                          Success);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      return From_Utf_8 (C.To_Ada (C.Strings.Value (Name)));

   end Get_Connection_Name_Instance_Part;

   function Get_Context (Self : in Connection_Aspect_Misc)
                         return Safir.Dob.Typesystem.Int_32 is
      Success : C.char;
      Context : Safir.Dob.Typesystem.Int_32;
   begin
      Safir.Dob.Interf.Get_Context (Self.Controller_Id,
                                    Context,
                                    Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
      return Context;
   end Get_Context;

   function Get_Queue_Capacity (Self  : in Connection_Aspect_Misc;
                                Queue : in Safir.Dob.Connection_Queue_Id.Enumeration)
                                return Safir.Dob.Typesystem.Int_32 is
      Success : C.char;
      Queue_Capacity : Safir.Dob.Typesystem.Int_32;
   begin
      Safir.Dob.Interf.Get_Queue_Capacity (Self.Controller_Id,
                                           Safir.Dob.Connection_Queue_Id.Enumeration'Pos (Queue),
                                           Queue_Capacity,
                                           Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
      return Queue_Capacity;
   end Get_Queue_Capacity;

   function Get_Queue_Size (Self  : in Connection_Aspect_Misc;
                            Queue : in Safir.Dob.Connection_Queue_Id.Enumeration)
                            return Safir.Dob.Typesystem.Int_32 is
      Success : C.char;
      Queue_Size : Safir.Dob.Typesystem.Int_32;
   begin
      Safir.Dob.Interf.Get_Queue_Size (Self.Controller_Id,
                                       Safir.Dob.Connection_Queue_Id.Enumeration'Pos (Queue),
                                       Queue_Size,
                                       Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
      return Queue_Size;
   end Get_Queue_Size;

   procedure Simulate_Overflows (Self       : in Connection_Aspect_Misc;
                                 In_Queues  : in Boolean;
                                 Out_Queues : in Boolean) is
      Success : C.char;
   begin
      Safir.Dob.Interf.Simulate_Overflows (Self.Controller_Id,
                                           C.char'Val (Boolean'Pos (In_Queues)),
                                           C.char'Val (Boolean'Pos (Out_Queues)),
                                           Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Simulate_Overflows;

end Safir.Dob.Connection_Aspect_Miscs;

