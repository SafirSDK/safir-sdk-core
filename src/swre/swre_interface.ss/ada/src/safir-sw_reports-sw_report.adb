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
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Utilities;
with Safir.Dob.Typesystem.Library_Exceptions;
with Interfaces.C;
package body Safir.Sw_Reports.Sw_Report is
   package C renames Interfaces.C;
   function Convert (Str : in Wide_String) return Interfaces.C.char_array;

   function Convert (Str : in Wide_String) return Interfaces.C.char_array is
      function ToUtf8 (WStr : in Unbounded_Wide_String) return String renames Safir.Dob.Typesystem.Utilities.To_Utf_8;
   begin
      return Interfaces.C.To_C (ToUtf8 (To_Unbounded_Wide_String (Str)));
   end Convert;


   ----------------------------------------------------------------------------
   -- Stop
   ----------------------------------------------------------------------------
   procedure Stop is
      procedure SwreC_Stop;
      pragma Import (C, SwreC_Stop, "SwreC_Stop");
   begin
      SwreC_Stop;
   end Stop;

   ----------------------------------------------------------------------------
   -- Enable_Crash_Reporting
   ----------------------------------------------------------------------------
   procedure Enable_Crash_Reporting is

      procedure SwreC_EnableCrashReporting (C_Success : out C.char);
      pragma Import (C, SwreC_EnableCrashReporting, "SwreC_EnableCrashReporting");

      L_Success : C.char;
   begin
      SwreC_EnableCrashReporting (C_Success => L_Success);
      if not (C.char'Pos (L_Success) /= 0) then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Enable_Crash_Reporting;

   ----------------------------------------------------------------------------
   -- Send_Fatal_Error_Report
   ----------------------------------------------------------------------------
   procedure Send_Fatal_Error_Report (Error_Code : in Wide_String;
                                      Location   : in Wide_String;
                                      Text       : in Wide_String) is

      procedure SwreC_SendFatalErrorReport (C_Error_Code : in     Interfaces.C.char_array;
                                            C_Location   : in     Interfaces.C.char_array;
                                            C_Text       : in     Interfaces.C.char_array;
                                            C_Success    :    out C.char);
      pragma Import (C, SwreC_SendFatalErrorReport, "SwreC_SendFatalErrorReport");

      L_Success : C.char;
   begin
      SwreC_SendFatalErrorReport (C_Error_Code => Convert (Error_Code),
                                  C_Location => Convert (Location),
                                  C_Text => Convert (Text),
                                  C_Success => L_Success);
      if not (C.char'Pos (L_Success) /= 0) then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Send_Fatal_Error_Report;

   ----------------------------------------------------------------------------
   -- Send_Error_Report
   ----------------------------------------------------------------------------
   procedure Send_Error_Report (Error_Code : in Wide_String;
                                Location   : in Wide_String;
                                Text       : in Wide_String) is

      procedure SwreC_SendErrorReport (C_Error_Code : in     Interfaces.C.char_array;
                                       C_Location   : in     Interfaces.C.char_array;
                                       C_Text       : in     Interfaces.C.char_array;
                                       C_Success    :    out C.char);
      pragma Import (C, SwreC_SendErrorReport, "SwreC_SendErrorReport");

      L_Success : C.char;
   begin
      SwreC_SendErrorReport (C_Error_Code => Convert (Error_Code),
                             C_Location => Convert (Location),
                             C_Text => Convert (Text),
                             C_Success => L_Success);
      if not (C.char'Pos (L_Success) /= 0) then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Send_Error_Report;

   ----------------------------------------------------------------------------
   -- Send_Resource_Report
   ----------------------------------------------------------------------------
   procedure Send_Resource_Report (Resource_Id : in Wide_String;
                                   Allocated   : in Boolean;
                                   Text        : in Wide_String) is

      procedure SwreC_SendResourceReport (C_Resource_Id : in     Interfaces.C.char_array;
                                          C_Allocated   : in     C.char;
                                          C_Text        : in     Interfaces.C.char_array;
                                          C_Success     :    out C.char);
      pragma Import (C, SwreC_SendResourceReport, "SwreC_SendResourceReport");

      L_Success : C.char;
      L_Allocated : constant C.char := C.char'Val (Boolean'Pos (Allocated));
   begin
      SwreC_SendResourceReport (C_Resource_Id => Convert (Resource_Id),
                                C_Allocated => L_Allocated,
                                C_Text => Convert (Text),
                                C_Success => L_Success);
      if not (C.char'Pos (L_Success) /= 0) then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Send_Resource_Report;

   ----------------------------------------------------------------------------
   -- Send_Programming_Error_Report
   ----------------------------------------------------------------------------
   procedure Send_Programming_Error_Report (Error_Code : in Wide_String;
                                            Location   : in Wide_String;
                                            Text       : in Wide_String) is


      procedure SwreC_SendProgrammingErrorReport (C_Error_Code : in     Interfaces.C.char_array;
                                                  C_Location   : in     Interfaces.C.char_array;
                                                  C_Text       : in     Interfaces.C.char_array;
                                                  C_Success    :    out C.char);
      pragma Import (C, SwreC_SendProgrammingErrorReport, "SwreC_SendErrorReport");

      L_Success : C.char;
   begin
      SwreC_SendProgrammingErrorReport (C_Error_Code => Convert (Error_Code),
                                        C_Location => Convert (Location),
                                        C_Text => Convert (Text),
                                        C_Success => L_Success);
      if not (C.char'Pos (L_Success) /= 0) then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Send_Programming_Error_Report;

   ----------------------------------------------------------------------------
   -- Send_Program_Info_Report
   ----------------------------------------------------------------------------
   procedure Send_Program_Info_Report (Text : in Wide_String) is

      procedure SwreC_SendProgramInfoReport (C_Text    : in     Interfaces.C.char_array;
                                             C_Success :    out C.char);
      pragma Import (C, SwreC_SendProgramInfoReport, "SwreC_SendProgramInfoReport");

      L_Success : C.char;
   begin
      SwreC_SendProgramInfoReport (C_Text => Convert (Text),
                                   C_Success => L_Success);
      if not (C.char'Pos (L_Success) /= 0) then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Send_Program_Info_Report;

end Safir.Sw_Reports.Sw_Report;
