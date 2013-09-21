-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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
with Interfaces.C;

with Safir.Dob.Typesystem.Utilities;
with Safir.Dob.Typesystem.Library_Exceptions;
with Ada.Characters.Latin_1;

package body Safir.Application.Tracers is
   package C renames Interfaces.C;

   function Create (Prefix : in Wide_String) return Tracer is
      A_Tracer : Tracer := (Prefix => Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String (Prefix),
                            PrefixId => 0);
   begin
      A_Tracer.Add_Prefix;
      return A_Tracer;
   end Create;

   procedure Put (Self : in out Tracer;
                  Item : in     Ada.Strings.Wide_Unbounded.Unbounded_Wide_String) is

      procedure SwreC_TraceAppendString (PrefixId : in     Safir.Dob.Typesystem.Int_64;
                                         Str      : in     Interfaces.C.char_array;
                                         Success  :    out C.char);

      pragma Import (C, SwreC_TraceAppendString, "SwreC_TraceAppendString");

      Str : constant String := Safir.Dob.Typesystem.Utilities.To_Utf_8 (Item);
      L_Success : C.char;
   begin
      if Self.Is_Enabled then
         SwreC_TraceAppendString (PrefixId => Self.PrefixId,
                                  Str => Interfaces.C.To_C (Str),
                                  Success => L_Success);
         if not (C.char'Pos (L_Success) /= 0) then
            Safir.Dob.Typesystem.Library_Exceptions.Throw;
         end if;
      end if;
   end Put;

   procedure Put (Self : in out Tracer;
                  Item : in     Wide_String) is
   begin
      Self.Put (Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String (Item));
   end Put;

   procedure Put_Line (Self : in out Tracer;
                       Item : in     Wide_String) is
   begin
      if Self.Is_Enabled then
         Self.Put (Item);
         Self.New_Line;
      end if;
   end Put_Line;


   procedure Put_Line (Self : in out Tracer;
                       Item : in     Ada.Strings.Wide_Unbounded.Unbounded_Wide_String)
   is
   begin
      if Self.Is_Enabled then
         Self.Put (Item);
         Self.New_Line;
      end if;
   end Put_Line;

   procedure New_Line (Self : in out Tracer) is

      procedure SwreC_TraceAppendChar (PrefixId : in     Safir.Dob.Typesystem.Int_64;
                                       Item     : in     Interfaces.C.char;
                                       Success  :    out C.char);

      pragma Import (C, SwreC_TraceAppendChar, "SwreC_TraceAppendChar");

      procedure SwreC_TraceFlush (Success : out C.char);

      pragma Import (C, SwreC_TraceFlush, "SwreC_TraceFlush");

      L_Success : C.char;
   begin
      SwreC_TraceAppendChar (PrefixId => Self.PrefixId,
                             Item => Interfaces.C.To_C (Ada.Characters.Latin_1.LF),
                             Success => L_Success);
      if not (C.char'Pos (L_Success) /= 0) then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      SwreC_TraceFlush (Success => L_Success);

      if not (C.char'Pos (L_Success) /= 0) then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

   end New_Line;

   function Is_Enabled (Self : in Tracer) return Boolean is
      function SwreC_TracePrefixIsEnabled (PrefixId : in Safir.Dob.Typesystem.Int_64) return C.char;
      pragma Import (C, SwreC_TracePrefixIsEnabled, "SwreC_TracePrefixIsEnabled");

      use type Safir.Dob.Typesystem.Int_64;
   begin
      if Self.PrefixId = 0 then
         return False;
      end if;
      return C.char'Pos (SwreC_TracePrefixIsEnabled (Self.PrefixId)) /= 0;

   end Is_Enabled;

   procedure Enable (Self    : in out Tracer;
                     Enabled : in     Boolean) is
      procedure SwreC_TracePrefixSetEnabled (PrefixId : in     Safir.Dob.Typesystem.Int_64;
                                             SetEnabled  : in     C.char;
                                             Success  :    out C.char);
      pragma Import (C, SwreC_TracePrefixSetEnabled, "SwreC_TracePrefixSetEnabled");

      L_Success : C.char;
      L_Enabled : constant C.char := C.char'Val (Boolean'Pos (Enabled));
   begin
      SwreC_TracePrefixSetEnabled (PrefixId => Self.PrefixId,
                                   SetEnabled => L_Enabled,
                                   Success => L_Success);
      if not (C.char'Pos (L_Success) /= 0) then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Enable;

   procedure Add_Prefix (Self : in out Tracer) is
      procedure SwreC_TracePrefixAdd (Prefix   : in     Interfaces.C.char_array;
                                      PrefixId :    out Safir.Dob.Typesystem.Int_64;
                                      Success  :    out C.char);

      pragma Import (C, SwreC_TracePrefixAdd, "SwreC_TracePrefixAdd");

      L_Success : C.char;

      use type Safir.Dob.Typesystem.Int_64;
   begin
      if Self.PrefixId = 0 then
         SwreC_TracePrefixAdd (Prefix => Interfaces.C.To_C (Safir.Dob.Typesystem.Utilities.To_Utf_8 (Self.Prefix)),
                               PrefixId => Self.PrefixId,
                               Success => L_Success);
         if not (C.char'Pos (L_Success) /= 0) then
            Safir.Dob.Typesystem.Library_Exceptions.Throw;
         end if;
      end if;

      return;
   end Add_Prefix;


end Safir.Application.Tracers;
