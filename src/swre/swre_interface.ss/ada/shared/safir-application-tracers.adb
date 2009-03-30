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
with Safir.Dob.Typesystem.LibraryExceptions;
with Ada.Characters.Latin_1;
with Ada.Command_Line;
package body Safir.Application.Tracers is
    package C renames Interfaces.C;

   function Create (Prefix : in Wide_String) return Tracer is
      procedure SwreC_SetProgramName (ProgramName : in     Interfaces.C.char_array;
                                      Success     :    out C.char);
      pragma Import (C, SwreC_SetProgramName, "SwreC_SetProgramName");

      L_Success : C.char;
   begin
      SwreC_SetProgramName (Interfaces.C.To_C (Ada.Command_Line.Command_Name), L_Success);
      if not (C.char'Pos (L_Success) /= 0) then
         Safir.Dob.Typesystem.LibraryExceptions.Throw;
      end if;

      return (Prefix => Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String (Prefix),
              PrefixId => 0);
   end Create;

   procedure Put (Self : in out Tracer;
                  Item : in     Ada.Strings.Wide_Unbounded.Unbounded_Wide_String) is

      procedure SwreC_TraceAppendStringPrefix (PrefixId : in     Safir.Dob.Typesystem.Int64;
                                               Str      : in     Interfaces.C.char_array;
                                               Success  :    out C.char);

      pragma Import (C, SwreC_TraceAppendStringPrefix, "SwreC_TraceAppendStringPrefix");

      Str : constant String := Safir.Dob.Typesystem.Utilities.ToUtf8 (Item);
      L_Success : C.char;
   begin
      Self.AddPrefix;
      if Self.IsEnabled then
         SwreC_TraceAppendStringPrefix (PrefixId => Self.PrefixId,
                                        Str => Interfaces.C.To_C (Str),
                                        Success => L_Success);
         if not (C.char'Pos (L_Success) /= 0) then
            Safir.Dob.Typesystem.LibraryExceptions.Throw;
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
      if Self.IsEnabled then
         Self.Put (Item);
         Self.New_Line;
      end if;
   end Put_Line;


   procedure Put_Line (Self : in out Tracer;
                       Item : in     Ada.Strings.Wide_Unbounded.Unbounded_Wide_String)
   is
   begin
      if Self.IsEnabled then
         Self.Put (Item);
         Self.New_Line;
      end if;
   end Put_Line;

   procedure New_Line (Self : in out Tracer) is

      procedure SwreC_TraceAppendCharPrefix (PrefixId : in     Safir.Dob.Typesystem.Int64;
                                             Item     : in     Interfaces.C.char;
                                             Success  :    out C.char);

      pragma Import (C, SwreC_TraceAppendCharPrefix, "SwreC_TraceAppendCharPrefix");

      procedure SwreC_TraceSyncBuffer (Success : out C.char);

      pragma Import (C, SwreC_TraceSyncBuffer, "SwreC_TraceSyncBuffer");

      L_Success : C.char;
   begin
      Self.AddPrefix;
      SwreC_TraceAppendCharPrefix (PrefixId => Self.PrefixId,
                                   Item => Interfaces.C.To_C (Ada.Characters.Latin_1.LF),
                                   Success => L_Success);
      if not (C.char'Pos (L_Success) /= 0) then
         Safir.Dob.Typesystem.LibraryExceptions.Throw;
      end if;

      SwreC_TraceSyncBuffer (Success => L_Success);

      if not (C.char'Pos (L_Success) /= 0) then
         Safir.Dob.Typesystem.LibraryExceptions.Throw;
      end if;

   end New_Line;

   function IsEnabled (Self : in Tracer) return Boolean is
      function SwreC_TracePrefixIsEnabled (PrefixId : in Safir.Dob.Typesystem.Int64) return C.char;
      pragma Import (C, SwreC_TracePrefixIsEnabled, "SwreC_TracePrefixIsEnabled");

      use type Safir.Dob.Typesystem.Int64;
   begin
      if Self.PrefixId = 0 then
         return False;
      end if;
      return C.char'Pos (SwreC_TracePrefixIsEnabled (Self.PrefixId)) /= 0;

   end IsEnabled;

   procedure Enable (Self    : in out Tracer;
                     Enabled : in     Boolean) is
      procedure SwreC_TracePrefixSetEnabled (PrefixId : in     Safir.Dob.Typesystem.Int64;
                                             SetEnabled  : in     C.char;
                                             Success  :    out C.char);
      pragma Import (C, SwreC_TracePrefixSetEnabled, "SwreC_TracePrefixSetEnabled");

      L_Success : C.char;
      L_Enabled : C.char := C.char'Val (Boolean'Pos (Enabled));
   begin
      Self.AddPrefix;
      SwreC_TracePrefixSetEnabled (PrefixId => Self.PrefixId,
                                   SetEnabled => L_Enabled,
                                   Success => L_Success);
      if not (C.char'Pos (L_Success) /= 0) then
         Safir.Dob.Typesystem.LibraryExceptions.Throw;
      end if;
   end Enable;


   procedure Flush is
      procedure SwreC_TraceFlushBuffer (Success  :    out C.char);
      pragma Import (C, SwreC_TraceFlushBuffer, "SwreC_TraceFlushBuffer");
      L_Success : C.char;
   begin
      if Self.IsEnabled then
         SwreC_TraceFlushBuffer (Success => L_Success);
         if not (C.char'Pos (L_Success) /= 0) then
            Safir.Dob.Typesystem.LibraryExceptions.Throw;
         end if;
      end if;
   end Flush;



   procedure AddPrefix (Self : in out Tracer) is
      procedure SwreC_TracePrefixAdd (Prefix   : in     Interfaces.C.char_array;
                                      PrefixId :    out Safir.Dob.Typesystem.Int64;
                                      Success  :    out C.char);

      pragma Import (C, SwreC_TracePrefixAdd, "SwreC_TracePrefixAdd");

      L_Success : C.char;

      use type Safir.Dob.Typesystem.Int64;
   begin
      if Self.PrefixId = 0 then
         SwreC_TracePrefixAdd (Prefix => Interfaces.C.To_C (Safir.Dob.Typesystem.Utilities.ToUtf8 (Self.Prefix)),
                               PrefixId => Self.PrefixId,
                               Success => L_Success);
         if not (C.char'Pos (L_Success) /= 0) then
            Safir.Dob.Typesystem.LibraryExceptions.Throw;
         end if;
      end if;

      return;
   end AddPrefix;


end Safir.Application.Tracers;
