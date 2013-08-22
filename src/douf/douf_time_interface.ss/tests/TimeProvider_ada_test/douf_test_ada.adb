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
with Ada.Calendar;
with Ada.Calendar.Time_Zones;
with Ada.Command_Line;
with Ada.Text_IO;
with Safir.Time.Time_Provider;
with Safir.Dob.Typesystem.Si_64;

procedure Douf_Test_Ada is
   use type Ada.Calendar.Time;
   use type Safir.Dob.Typesystem.Si_64.Second;

   A_Date : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (Year => 2010,
                                                                Month => 2,
                                                                Day => 24,
                                                                Seconds => 3900.0);
   As_Double : constant Safir.Dob.Typesystem.Si_64.Second := Safir.Time.Time_Provider.Seconds_Of (A_Date);

   Back_To_Calendar : constant Ada.Calendar.Time := Safir.Time.Time_Provider.Calendar_Time_Of (As_Double);

   As_Local_Time : constant Ada.Calendar.Time := Safir.Time.Time_Provider.To_Local_Time (As_Double);

   Back_To_UTC_Seconds : constant Safir.Dob.Typesystem.Si_64.Second := Safir.Time.Time_Provider.To_UTC_Time (As_Local_Time);

   Now_Safir : constant Safir.Dob.Typesystem.Si_64.Second := Safir.Time.Time_Provider.Get_UTC_Time;

   Now_Ada : constant Duration := Ada.Calendar.Clock - Ada.Calendar.Time_Of (1970, 1, 1);

   Diff : constant Safir.Dob.Typesystem.Si_64.Second := abs Now_Safir - Safir.Dob.Typesystem.Si_64.Second (Now_Ada);
begin
   Ada.Command_Line.Set_Exit_Status (1);

   if As_Double /= 1266973500.0 then
      Ada.Text_IO.Put_Line ("Safir.Time.TimeProvider.To_Seconds(...) returned incorrect value");
      return;
   end if;

   if Back_To_Calendar /= A_Date then
      Ada.Text_IO.Put_Line ("Safir.Time.Time_Provider.Calendar_Time_Of (...) returned incorrect value");
      return;
   end if;

   if As_Local_Time - A_Date /= Duration (Ada.Calendar.Time_Zones.UTC_Time_Offset) * 60 then
      Ada.Text_IO.Put_Line ("Safir.Time.Time_Provider.To_Local_Time (...) returned incorrect value");
      return;
   end if;

   if Back_To_UTC_Seconds /= As_Double then
      Ada.Text_IO.Put_Line ("Safir.Time.Time_Provider.To_UTC_Time (...) returned incorrect value");
      return;
   end if;

   if Diff > 0.1 then
      Ada.Text_IO.Put_Line ("Safir.Time.Time_Provider.Get_UTC_Time returned incorrect value");
      return;
   end if;

   Ada.Command_Line.Set_Exit_Status (0);
end Douf_Test_Ada;
