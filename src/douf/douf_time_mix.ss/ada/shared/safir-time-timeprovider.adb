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
package body Safir.Time.TimeProvider is

   use type Safir.Dob.Typesystem.Si64.Second;
   use type Ada.Calendar.Time;

   type Float64_P is access all Safir.Dob.Typesystem.Float64;

   type Int32_P is access all Safir.Dob.Typesystem.Int32;
   procedure GetUtcTime (utcTime : out Float64_P);
   pragma Import (C,GetUtcTime,"DoufTimeC_GetUtcTime");
   procedure GetLocalTimeOffset (offset : out Int32_P);
   pragma Import (C,GetLocalTimeOffset,"DoufTimeC_GetLocalTimeOffset");

   ADA_1_JAN_1970 : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (
                                                                Year => 1970,
                                                                Month => 1,
                                                                Day   => 1);

   ----------------------------------------------------------------------------
   function GetUtcTime return Safir.Dob.Typesystem.Si64.Second is
      UtcTime : Float64_P := new Safir.Dob.Typesystem.Float64;
   begin
      -- Get current Utc time
      GetUtcTime(UtcTime);
      return UtcTime.all;
   end;

   ----------------------------------------------------------------------------
   function ToLocalTime (UtcTime : in Safir.Dob.Typesystem.Si64.Second) return Ada.Calendar.Time is
      Offset : Int32_P := new Safir.Dob.Typesystem.Int32;
   begin
      GetLocalTimeOffset (Offset);
      return CalendarTimeOf(UtcTime + Safir.Dob.Typesystem.Float64(Offset.all));
   end ToLocalTime;

   ----------------------------------------------------------------------------
   function ToUtcTime (LocalTime : in Ada.Calendar.Time) return Safir.Dob.Typesystem.Si64.Second is
      Offset    : Int32_P := new Safir.Dob.Typesystem.Int32;
   begin
      GetLocalTimeOffset (Offset);
      -- Return seconds since 01 Jan 1970
      return Safir.Dob.Typesystem.Float64(LocalTime - ADA_1_JAN_1970 - Duration(Offset.all));
   end ToUtcTime;

   ----------------------------------------------------------------------------
   function CalendarTimeOf (UtcTime : in Safir.Dob.Typesystem.Si64.Second) return Ada.Calendar.Time is
   begin
      return ADA_1_JAN_1970 + Duration(UtcTime);
   end CalendarTimeOf;

   ----------------------------------------------------------------------------
   function DoubleOf (UtcTime : in Ada.Calendar.Time) return Safir.Dob.Typesystem.Si64.Second is
   begin
      return Safir.Dob.Typesystem.Si64.Second(UtcTime - ADA_1_JAN_1970);
   end DoubleOf;

end Safir.Time.TimeProvider;
