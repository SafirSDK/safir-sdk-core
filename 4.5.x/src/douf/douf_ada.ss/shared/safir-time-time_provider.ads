-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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

package Safir.Time.Time_Provider is

   Time_Error : exception renames Ada.Calendar.Time_Error;
   ----------------------------------------------------------------------------
   -- Get current UTC time.
   -- Returns seconds and fraction since jan 1 1970 00:00
   ----------------------------------------------------------------------------
   function Get_UTC_Time return Safir.Dob.Typesystem.Si_64.Second;

   ----------------------------------------------------------------------------
   -- Converts UTC time to local time
   -- If UTC_Time represents a year ouside Ada.Calendar.Year_Number range
   -- exception Time_Error is raised.
   ----------------------------------------------------------------------------
   function To_Local_Time (UTC_Time : in Safir.Dob.Typesystem.Si_64.Second) return Ada.Calendar.Time;

   ----------------------------------------------------------------------------
   -- Converts local time to UTC time
   -- Returns seconds and fraction since jan 1 1970 00:00
   ----------------------------------------------------------------------------
   function To_UTC_Time (Local_Time : in Ada.Calendar.Time) return Safir.Dob.Typesystem.Si_64.Second;

   ----------------------------------------------------------------------------
   -- Gets specified UTC time in Calendar representation
   -- If UTCTime represents a year ouside Ada.Calendar.Year_Number range
   -- exception Time_Error is raised.
   ----------------------------------------------------------------------------
   function Calendar_Time_Of (UTC_Time : in Safir.Dob.Typesystem.Si_64.Second) return Ada.Calendar.Time;

   ----------------------------------------------------------------------------
   -- Convert specified UTC time to a Double
   -- Returns seconds and fraction since jan 1 1970 00:00
   ----------------------------------------------------------------------------
   function Seconds_Of (UTC_Time : in Ada.Calendar.Time) return Safir.Dob.Typesystem.Si_64.Second;


end Safir.Time.Time_Provider;

