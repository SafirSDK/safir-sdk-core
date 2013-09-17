-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2013 (http://www.safirsdk.com)
--
--  Created by: Lars Hagström / lars.hagstrom@consoden.se
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
with Ada.Command_Line;
with Safir.Sw_Reports.Sw_Report;
pragma Wide_Character_Encoding (UTF8);
procedure Sender is
begin
   Safir.Sw_Reports.Sw_Report.Send_Fatal_Error_Report ("FatalError",
                                                       "here",
                                                       "Testing SendFatalErrorReport");
   Safir.Sw_Reports.Sw_Report.Send_Error_Report ("Error",
                                                 "there",
                                                 "Testing SendErrorReport");
   Safir.Sw_Reports.Sw_Report.Send_Resource_Report ("ResourceReport",
                                                    False,
                                                    "Testing SendResourceReport");
   Safir.Sw_Reports.Sw_Report.Send_Resource_Report ("ResourceReport",
                                                    True,
                                                    "Testing SendResourceReport");
   Safir.Sw_Reports.Sw_Report.Send_Programming_Error_Report ("ProgrammingError",
                                                             "everywhere",
                                                             "Testing SendProgrammingErrorReport");

   Safir.Sw_Reports.Sw_Report.Send_Program_Info_Report ("Testing SendProgramInfoReport");

   Safir.Sw_Reports.Sw_Report.Send_Error_Report ("brynanuppafjässasponken", --ä
                                                "Don't know" & Wide_Character'Val (16#203d#), -- interrobang
                                                "Testing" & Wide_Character'Val (10) & "funny characters");

   Safir.Sw_Reports.Sw_Report.Send_Program_Info_Report ("Finished!");
   Ada.Command_Line.Set_Exit_Status (0);
end Sender;
