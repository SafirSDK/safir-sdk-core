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
with Ada.Exceptions;
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Safir.Dob.Consumer;
with Safir.Dob.Connection;
with Safir.Application.Tracers;
with Safir.SwReports.SwReport; use Safir.SwReports;
with Ada.Text_IO;

procedure Swre_Test_Ada is

   task MainLoop is
      entry Run;
      entry Dispatch;
   end MainLoop;


   type Dispatcher is limited new
     Safir.Dob.Consumer.Dispatcher with null record;

   procedure OnDoDispatch
      (Self : in out Dispatcher) is
      pragma Unreferenced (Self);
   begin
      MainLoop.Dispatch;
   end OnDoDispatch;

   type StopHandler is limited new
      Safir.Dob.Consumer.StopHandler with
   record
      Stop : Boolean := False;
   end record;

   procedure OnStopOrder
      (Self : in out StopHandler);


   procedure OnStopOrder
      (Self : in out StopHandler) is
   begin
      Self.Stop := True;
   end OnStopOrder;

   Debug : Safir.Application.Tracers.Tracer;

   procedure Timeout is
   begin
      SwReport.Send_Fatal_Error_Report ("FatalErrorCode99", "AdaTestApp", "Fatal Error text99");
      SwReport.Send_Fatal_Error_Report ("FatalErrorCode100", "AdaTestApp", "Fatal Error text100");

      SwReport.Send_Error_Report ("ErrorCode44", "AdaTestApp", "Error text44");
      SwReport.Send_Error_Report ("ErrorCode55", "AdaTestApp", "Error text55");

      SwReport.Send_Resource_Report ("Resource77", true, "Resource text77");
      SwReport.Send_Resource_Report ("Resource88", true, "Resource text88");

      SwReport.Send_Program_Info_Report ("Important information from the Ada application");
      SwReport.Send_Program_Info_Report ("More Important information from the Ada application");

      SwReport.Send_Programming_Error_Report ("ProgrammingErrorCode33", "AdaTestApp", "Ada programming error33");
      SwReport.Send_Programming_Error_Report ("ProgrammingErrorCode34", "AdaTestApp", "Ada programming error34");

      SwReport.Send_Programming_Error_Report ("ProgrammingErrorCode35", "AdaTestApp", "Ada programming error35");
      SwReport.Send_Programming_Error_Report ("ProgrammingErrorCode36", "AdaTestApp", "Ada programming error36");

      for Index in 0 .. 100 loop
         Debug.Put_Line ("Testing logging to tracer " & Integer'Wide_Image (Index));
      end loop;
   end Timeout;

   task body MainLoop is
      Connection      : Safir.Dob.Connection.Class;
      Disp : aliased Dispatcher;
      Stop : aliased StopHandler;
   begin
      accept Run;
      Connection.Open (To_Unbounded_Wide_String ("SwreAdaTest"),
                       To_Unbounded_Wide_String (""),
                       0,
                       Stop'Access,
                       Disp'Access);

      Debug := Safir.Application.Tracers.Create ("AdaTest");
      Debug.Enable (True);
      Ada.Text_IO.Put_Line ("tracing something");
      Debug.Put_Line ("Hello");
      while not Stop.Stop loop
         select
            accept Dispatch;
            Connection.Dispatch;
         or
            delay 10.0;
            Timeout;
         end select;
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Exception: " &
                               Ada.Exceptions.Exception_Information (E));
   end  MainLoop;

begin
   MainLoop.Run;
end Swre_Test_Ada;
