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

   -- AWI:todo comment
   function Get_Connection_Name (Self : in Connection_Aspect_Misc)
                                 return Unbounded_Wide_String;

   -- AWI:todo comment
   function Get_Connection_Name_Common_Part (Self : in Connection_Aspect_Misc)
                                             return Unbounded_Wide_String;

   -- AWI:todo comment
   function Get_Connection_Name_Instance_Part (Self : in Connection_Aspect_Misc)
                                               return Unbounded_Wide_String;

   -- AWI:todo comment
   function Get_Queue_Capacity (Self  : in Connection_Aspect_Misc;
                                Queue : in Safir.Dob.Connection_Queue_Id.Enumeration)
     return Safir.Dob.Typesystem.Int_32;

   -- AWI:todo comment
   function Get_Queue_Size (Self  : in Connection_Aspect_Misc;
                            Queue : in Safir.Dob.Connection_Queue_Id.Enumeration)
     return Safir.Dob.Typesystem.Int_32;

   --------
   -- Debug
   --------

   -- AWI:todo comment
   procedure Simulate_Overflows (Self       : in Connection_Aspect_Misc;
                                 In_Queues  : in Boolean;
                                 Out_Queues : in Boolean);
private

   use type Safir.Dob.Defs.Controller_Id;

   type Connection_Aspect_Misc is tagged record
      Controller_Id : Safir.Dob.Defs.Controller_Id := -1;
   end record;

end Safir.Dob.Connection_Aspect_Miscs;
