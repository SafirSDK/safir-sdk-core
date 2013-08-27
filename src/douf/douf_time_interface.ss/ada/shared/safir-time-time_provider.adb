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

package body Safir.Time.Time_Provider is

   package C renames Interfaces.C;

   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String := "")
     renames  Ada.Exceptions.Raise_Exception;

   use type Safir.Dob.Typesystem.Si_64.Second;
   use type Ada.Calendar.Time;

   procedure Get_UTC_Time (UTCTime : out Safir.Dob.Typesystem.Si_64.Second; Success : out C.char);
   pragma Import (C, Get_UTC_Time, "DoufTimeC_GetUtcTime");

   procedure Get_Local_Time_Offset (offset : out Safir.Dob.Typesystem.Int_32; Success : out C.char);
   pragma Import (C, Get_Local_Time_Offset, "DoufTimeC_GetLocalTimeOffset");

   ADA_1_JAN_1970 : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (
                                                                Year => 1970,
                                                                Month => 1,
                                                                Day   => 1);

   ----------------------------------------------------------------------------
   function Get_UTC_Time return Safir.Dob.Typesystem.Si_64.Second is
      UTC_Time : Safir.Dob.Typesystem.Si_64.Second;
      Success : C.char;
   begin
      -- Get current UTC time
      Get_UTC_Time (UTC_Time, Success);

      if C.char'Pos (Success) = 0 then
         Throw (Safir.Dob.Typesystem.Configuration_Error_Exception'Identity,
                "Configuration error in TimeProvider, please check your logs!");
      end if;

      return UTC_Time;
   end Get_UTC_Time;

   ----------------------------------------------------------------------------
   function To_Local_Time (UTC_Time : in Safir.Dob.Typesystem.Si_64.Second) return Ada.Calendar.Time is
      Offset : Safir.Dob.Typesystem.Int_32;
      Success : C.char;
   begin
      Get_Local_Time_Offset (Offset, Success);

      if C.char'Pos (Success) = 0 then
         Throw (Safir.Dob.Typesystem.Configuration_Error_Exception'Identity,
                "Configuration error in TimeProvider, please check your logs!");
      end if;

      return Calendar_Time_Of (UTC_Time + Safir.Dob.Typesystem.Float_64 (Offset));
   end To_Local_Time;

   ----------------------------------------------------------------------------
   function To_UTC_Time (Local_Time : in Ada.Calendar.Time) return Safir.Dob.Typesystem.Si_64.Second is
      Offset    : Safir.Dob.Typesystem.Int_32;
      Success : C.char;
   begin
      Get_Local_Time_Offset (Offset, Success);

      if C.char'Pos (Success) = 0 then
         Throw (Safir.Dob.Typesystem.Configuration_Error_Exception'Identity,
                "Configuration error in TimeProvider, please check your logs!");
      end if;

      -- Return seconds since 01 Jan 1970
      return Safir.Dob.Typesystem.Float_64 (Local_Time - ADA_1_JAN_1970 - Duration (Offset));
   end To_UTC_Time;

   ----------------------------------------------------------------------------
   function Calendar_Time_Of (UTC_Time : in Safir.Dob.Typesystem.Si_64.Second) return Ada.Calendar.Time is
   begin
      return ADA_1_JAN_1970 + Duration (UTC_Time);
   end Calendar_Time_Of;

   ----------------------------------------------------------------------------
   function Seconds_Of (UTC_Time : in Ada.Calendar.Time) return Safir.Dob.Typesystem.Si_64.Second is
   begin
      return Safir.Dob.Typesystem.Si_64.Second (UTC_Time - ADA_1_JAN_1970);
   end Seconds_Of;

end Safir.Time.Time_Provider;
