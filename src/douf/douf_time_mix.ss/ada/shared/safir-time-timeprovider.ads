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
with Safir.Dob.Typesystem.Si_64;

package Safir.Time.TimeProvider is

   Time_Error : exception renames Ada.Calendar.Time_Error;
   ----------------------------------------------------------------------------
   -- Get current Utc time.
   -- Returns seconds and fraction since jan 1 1970 00:00
   ----------------------------------------------------------------------------
   function GetUtcTime return Safir.Dob.Typesystem.Si_64.Second;

   ----------------------------------------------------------------------------
   -- Converts Utc time to local time
   -- If UtcTime represents a year ouside Ada.Calendar.Year_Number range
   -- exception Time_Error is raised.
   ----------------------------------------------------------------------------
   function ToLocalTime (UtcTime : in Safir.Dob.Typesystem.Si_64.Second) return Ada.Calendar.Time;

   ----------------------------------------------------------------------------
   -- Converts local time to UTC time
   -- Returns seconds and fraction since jan 1 1970 00:00
   ----------------------------------------------------------------------------
   function ToUtcTime (LocalTime : in Ada.Calendar.Time) return Safir.Dob.Typesystem.Si_64.Second;

   ----------------------------------------------------------------------------
   -- Gets specified utc time in Calendar representation
   -- If UtcTime represents a year ouside Ada.Calendar.Year_Number range
   -- exception Time_Error is raised.
   ----------------------------------------------------------------------------
   function CalendarTimeOf (UtcTime : in Safir.Dob.Typesystem.Si_64.Second) return Ada.Calendar.Time;

   ----------------------------------------------------------------------------
   -- Convert specified UTC time to a Double
   -- Returns seconds and fraction since jan 1 1970 00:00
   ----------------------------------------------------------------------------
   function DoubleOf (UtcTime : in Ada.Calendar.Time) return Safir.Dob.Typesystem.Si_64.Second;


end Safir.Time.TimeProvider;

