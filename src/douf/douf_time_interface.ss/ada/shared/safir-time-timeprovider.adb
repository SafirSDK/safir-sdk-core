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
with Ada.Exceptions;
with Interfaces.C;

package body Safir.Time.TimeProvider is

   package C renames Interfaces.C;

   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String := "")
     renames  Ada.Exceptions.Raise_Exception;

   use type Safir.Dob.Typesystem.Si_64.Second;
   use type Ada.Calendar.Time;

   procedure GetUtcTime (utcTime : out Safir.Dob.Typesystem.Si_64.Second; Success : out C.char);
   pragma Import (C, GetUtcTime, "DoufTimeC_GetUtcTime");

   procedure GetLocalTimeOffset (offset : out Safir.Dob.Typesystem.Int_32; Success : out C.char);
   pragma Import (C, GetLocalTimeOffset, "DoufTimeC_GetLocalTimeOffset");

   ADA_1_JAN_1970 : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (
                                                                Year => 1970,
                                                                Month => 1,
                                                                Day   => 1);

   ----------------------------------------------------------------------------
   function GetUtcTime return Safir.Dob.Typesystem.Si_64.Second is
      UtcTime : Safir.Dob.Typesystem.Si_64.Second;
      Success : C.char;
   begin
      -- Get current Utc time
      GetUtcTime (UtcTime, Success);

      if C.char'Pos (Success) = 0 then
         Throw (Safir.Dob.Typesystem.Configuration_Error_Exception'Identity,
                "Configuration error in TimeProvider, please check your logs!");
      end if;

      return UtcTime;
   end GetUtcTime;

   ----------------------------------------------------------------------------
   function ToLocalTime (UtcTime : in Safir.Dob.Typesystem.Si_64.Second) return Ada.Calendar.Time is
      Offset : Safir.Dob.Typesystem.Int_32;
      Success : C.char;
   begin
      GetLocalTimeOffset (Offset, Success);

      if C.char'Pos (Success) = 0 then
         Throw (Safir.Dob.Typesystem.Configuration_Error_Exception'Identity,
                "Configuration error in TimeProvider, please check your logs!");
      end if;

      return CalendarTimeOf (UtcTime + Safir.Dob.Typesystem.Float_64 (Offset));
   end ToLocalTime;

   ----------------------------------------------------------------------------
   function ToUtcTime (LocalTime : in Ada.Calendar.Time) return Safir.Dob.Typesystem.Si_64.Second is
      Offset    : Safir.Dob.Typesystem.Int_32;
      Success : C.char;
   begin
      GetLocalTimeOffset (Offset, Success);

      if C.char'Pos (Success) = 0 then
         Throw (Safir.Dob.Typesystem.Configuration_Error_Exception'Identity,
                "Configuration error in TimeProvider, please check your logs!");
      end if;

      -- Return seconds since 01 Jan 1970
      return Safir.Dob.Typesystem.Float_64 (LocalTime - ADA_1_JAN_1970 - Duration (Offset));
   end ToUtcTime;

   ----------------------------------------------------------------------------
   function CalendarTimeOf (UtcTime : in Safir.Dob.Typesystem.Si_64.Second) return Ada.Calendar.Time is
   begin
      return ADA_1_JAN_1970 + Duration (UtcTime);
   end CalendarTimeOf;

   ----------------------------------------------------------------------------
   function DoubleOf (UtcTime : in Ada.Calendar.Time) return Safir.Dob.Typesystem.Si_64.Second is
   begin
      return Safir.Dob.Typesystem.Si_64.Second (UtcTime - ADA_1_JAN_1970);
   end DoubleOf;

end Safir.Time.TimeProvider;
