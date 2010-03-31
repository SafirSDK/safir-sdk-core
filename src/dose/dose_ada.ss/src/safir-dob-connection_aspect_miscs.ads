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
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Safir.Dob.Typesystem;
with Safir.Dob.Defs;
with Safir.Dob.Connection_Bases;
with Safir.Dob.Callback_Id;
with Safir.Dob.Connection_Queue_Id;

package Safir.Dob.Connection_Aspect_Miscs is

   type Connection_Aspect_Misc is tagged private;

   function Create
     (Connection_Base : in  Safir.Dob.Connection_Bases.Connection_Base'Class)
      return Connection_Aspect_Misc;

   -- Get info about which callback you are currently executing in.
   --
   -- Returns: Id of the callback you are currently inside, or None if not in a callback.
   --
   function Get_Current_Callback_Id (Self : in Connection_Aspect_Misc)
                                     return Safir.Dob.Callback_Id.Enumeration;

   ------------------
   -- Connection name
   ------------------

   -- Get the name for this connection used in the system.
   --
   -- The connection name is composed of the name parts given by the application
   -- when opening the connection, with some additional decoration made by the DOB.
   --
   -- Returns: The connection name.
   --
   function Get_Connection_Name (Self : in Connection_Aspect_Misc)
                                 return Unbounded_Wide_String;

   -- Get the common part of the connection name.
   --
   -- Returns: The connection name common part specified when opening the connection.
   --
   function Get_Connection_Name_Common_Part (Self : in Connection_Aspect_Misc)
                                             return Unbounded_Wide_String;

   -- Get the instance part of the connection name.
   --
   -- Returns: The connection name instance part specified when opening the connection.
   function Get_Connection_Name_Instance_Part (Self : in Connection_Aspect_Misc)
                                               return Unbounded_Wide_String;


   ---------------
   -- Queue status
   ---------------

   -- Get the capacity of the specified queue.
   --
   -- This method returns the maximum number of items that the queue can hold.
   --
   -- Parameters: Queue - The queue to get info for.
   -- Returns: The capacity of the queue.
   --
   function Get_Queue_Capacity (Self  : in Connection_Aspect_Misc;
                                Queue : in Safir.Dob.Connection_Queue_Id.Enumeration)
     return Safir.Dob.Typesystem.Int_32;

   -- Get the number of items currently in the queue.
   --
   -- This method returns the number of items that is currently in the specified queue.
   --
   -- Parameters: Queue - The queue to get info for.
   -- Returns: The current size of the queue.
   --
   function Get_Queue_Size (Self  : in Connection_Aspect_Misc;
                            Queue : in Safir.Dob.Connection_Queue_Id.Enumeration)
     return Safir.Dob.Typesystem.Int_32;

   --------
   -- Debug
   --------

   -- Turn simulation of overflow on/off. For test purposes.
   --
   -- Setting In_Queues to True means that no messages or requests are handled by the application.
   -- An incoming request will result in an overflow, and an incoming message will be discarded.
   -- Setting Out_Queues to true means that no messages or requests can be sent from the application,
   -- instead these calls will throw an Overflow_Exception. When reset to false
   -- On_Xxxx_Not_Overflow will be called as expected.
   -- Use this to verify that your application handles overflows correctly.
   --
   -- Note that the In_Queues flag is not applied to new consumers added after this call.
   --
   -- Parameters: In_Queues - If True all incoming queues are simulated full.
   --             Out_Queues - If True all outgoing queues are simulated full.
   --
   procedure Simulate_Overflows (Self       : in Connection_Aspect_Misc;
                                 In_Queues  : in Boolean;
                                 Out_Queues : in Boolean);
private

   use type Safir.Dob.Defs.Controller_Id;

   type Connection_Aspect_Misc is tagged record
      Controller_Id : Safir.Dob.Defs.Controller_Id := -1;
   end record;

end Safir.Dob.Connection_Aspect_Miscs;
