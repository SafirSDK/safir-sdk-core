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
with Ada.Exceptions;
with Ada.Command_Line;
with Executor;
with Logger;
with Safir.Dob.Typesystem.Utilities;
with Safir.Application.Crash_Reporter;

procedure Dose_Test_Ada is
begin
   -- Set up exit status to be something bad, in case we exit ungracefully
   Ada.Command_Line.Set_Exit_Status (1);
   begin
      Safir.Application.Crash_Reporter.Start;

      Executor.Run;
   exception
      when E : others =>
         Logger.Put ("Caught some exception: ");
         Logger.Put_Line (Safir.Dob.Typesystem.Utilities.From_Utf_8 (Ada.Exceptions.Exception_Name (E)));
   end;
      Safir.Application.Crash_Reporter.Stop;
end Dose_Test_Ada;
