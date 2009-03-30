-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
--
--  Created by: Erik Adolfsson / sterad
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
with Ada.Text_IO;                         use Ada.Text_IO;
with Safir;
with Safir.Dob.Typesystem;
with Safir.Time.TimeProvider;

procedure Douf_Time_Test_Program is

      TimeAda    : Ada.Calendar.Time := Ada.Calendar.Clock;
      TimeDouble : Safir.Dob.Typesystem.Float64;

      ADA_1_JAN_1970 : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (
                                                                Year => 1970,
                                                                Month => 1,
                                                                Day   => 1);

      ADA_1_FEB_1902 : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (
                                                                Year => 1902,
                                                                Month => 2,
                                                                Day   => 1);

      ADA_1_FEB_2039 : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (
                                                                Year => 2039,
                                                                Month => 2,
                                                                Day   => 1);

      procedure Print_Time (TimeAda : in Ada.Calendar.Time) is
         Hour : Integer;
         Min : Integer;
         Sec : Duration;
         package Integer_Text is new Ada.Text_IO.Integer_IO (Integer);
         package Duration_Text is new Ada.Text_IO.Fixed_IO (Duration);

         procedure Put_Int ( Val : in Integer) is
         begin
            if Val < 10 then
               Put("0");
               Integer_Text.Put ( Val, 1);
            else
               Integer_Text.Put ( Val, 2);
            end if;
         end;
         procedure Put_Duration ( Val : in Duration) is
         begin
            if Val < 10.0 then
               Put("0");
               Duration_Text.Put ( Val, 1, 3);
            else
               Duration_Text.Put ( Val, 2, 3);
            end if;
         end;

      begin
         Hour := Integer( Ada.Calendar.Seconds ( TimeAda )) /3600;
         Min  := Integer( Ada.Calendar.Seconds ( TimeAda )) / 60 - Hour * 60;
         Sec  := Ada.Calendar.Seconds ( TimeAda ) - (Hour * 3600.0 + Min * 60.0);

         Integer_Text.Put( Ada.Calendar.Year( TimeAda ),4);
         Put ("-");
         Put_Int( Ada.Calendar.Month ( TimeAda ));
         Put("-");
         Put_Int( Ada.Calendar.Day ( TimeAda ));
         Put(" ");
         Put_Int( Hour );
         Put(":");
         Put_Int( Min );
         Put(":");
         Put_Duration ( Sec );
         Put_Line(" ");
      end;

begin

      Ada.Text_IO.Put ("Current local Time    - ");

      Print_Time(TimeAda);

      Ada.Text_IO.Put ("Converted to double   - ");

      TimeDouble := Safir.Time.TimeProvider.DoubleOf (TimeAda);
      TimeAda := Safir.Time.TimeProvider.CalendarTimeOf (TimeDouble);
      Print_Time(TimeAda);

      Ada.Text_IO.Put ("Current UTC Time      - ");

      TimeDouble := Safir.Time.TimeProvider.GetUtcTime;
      TimeAda := Safir.Time.TimeProvider.CalendarTimeOf (TimeDouble);
      Print_Time(TimeAda);

      Ada.Text_IO.Put ("To Local Time         - ");

      TimeAda := Safir.Time.TimeProvider.ToLocalTime (TimeDouble);
      Print_Time(TimeAda);

      Ada.Text_IO.Put ("To Utc Time           - ");

      TimeDouble := Safir.Time.TimeProvider.ToUtcTime (TimeAda);
      TimeAda := Safir.Time.TimeProvider.CalendarTimeOf (TimeDouble);
      Print_Time(TimeAda);

      Ada.Text_IO.Put ("1 Jan 1970            - ");

      TimeDouble := Safir.Time.TimeProvider.DoubleOf( ADA_1_JAN_1970 );
      TimeAda := Safir.Time.TimeProvider.CalendarTimeOf (TimeDouble);
      Print_Time(TimeAda);

      Ada.Text_IO.Put ("1 Feb 1902            - ");

      TimeDouble := Safir.Time.TimeProvider.DoubleOf( ADA_1_FEB_1902 );
      TimeAda := Safir.Time.TimeProvider.CalendarTimeOf (TimeDouble);
      Print_Time(TimeAda);

      Ada.Text_IO.Put ("1 Feb 2039            - ");

      TimeDouble := Safir.Time.TimeProvider.DoubleOf( ADA_1_FEB_2039 );
      TimeAda := Safir.Time.TimeProvider.CalendarTimeOf (TimeDouble);
      Print_Time(TimeAda);

end Douf_Time_Test_Program;


