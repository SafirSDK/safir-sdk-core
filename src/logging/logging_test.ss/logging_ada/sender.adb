-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2013 (http://www.safirsdk.com)
--
--  Created by: Anders Widén <anders.widen@consoden.se>
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
with Safir.Logging;
pragma Wide_Character_Encoding (UTF8);

procedure Sender is
begin
   Safir.Logging.Send_System_Log (Severity=>Safir.Logging.Emergency,
                                  Message=>"This is an emergency log!");
   Safir.Logging.Send_System_Log (Severity=>Safir.Logging.Alert,
                                  Message=>"This is an alert log!");
   Safir.Logging.Send_System_Log (Severity=>Safir.Logging.Critical,
                                  Message=>"This is a critical log!");
   Safir.Logging.Send_System_Log (Severity=>Safir.Logging.Error,
                                  Message=>"This is an error log!");
   Safir.Logging.Send_System_Log (Severity=>Safir.Logging.Warning,
                                  Message=>"This is a warning log!");
   Safir.Logging.Send_System_Log (Severity=>Safir.Logging.Notice,
                                  Message=>"This is a notice log!");
   Safir.Logging.Send_System_Log (Severity=>Safir.Logging.Informational,
                                  Message=>"This is an informational log!");
   Safir.Logging.Send_System_Log (Severity=>Safir.Logging.Debug,
                                  Message=>"This is a debug log!");

   Ada.Command_Line.Set_Exit_Status (0);
end Sender;

