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
with Dose_Pkg;
with Safir.Test.TimeConversion;
with Safir.Time.TimeProvider;
with Safir.Dob.Typesystem;

procedure Douf_Time_Test_Program_Send is

   procedure Set_Time is
      TimeAda  : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Time_Conv : Safir.Test.TimeConversion.Class;
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
            Duration_Text.Put ( Val, 1, 6);
         else
            Duration_Text.Put ( Val, 2, 6);
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

      Safir.Test.TimeConversion.SetTimeStamp (Time_Conv,
                                              Safir.Time.TimeProvider.DoubleOf (TimeAda));

      Time_Conv.SetInstanceNumber(0);
      Dose_Pkg.Set(Time_Conv);

   end Set_Time;

   objId : Safir.Dob.Typesystem.ObjectId;

begin

   Dose_Pkg.Start;

   objId.TypeId := Safir.Test.TimeConversion.ClassId;
   objId.Instance := Safir.Dob.Typesystem.Whole_Class;

   Dose_Pkg.Register_Entity (objId);

   while True loop
      Set_Time;
      delay 5.13;
   end loop;

end Douf_Time_Test_Program_Send;


