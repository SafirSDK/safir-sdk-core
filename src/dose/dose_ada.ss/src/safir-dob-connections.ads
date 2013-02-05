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
with Safir.Dob.Defs;
with Safir.Dob.Consumers;
with Safir.Dob.Connection_Bases;
with Safir.Dob.Typesystem;

package Safir.Dob.Connections is

   -- A connection to the DOB.
   -- This type represents a "real" (as opposed to Secondary Connection) connection to the dob.
   -- Each DOB application must have at least one connection. Connections are not thread safe.
   --
   type Connection is limited new
     Safir.Dob.Connection_Bases.Connection_Base with private;


   -- Open a connection to the DOB.
   --
   -- The connection uses the On_Do_Dispatch callback to signal that there is incoming data available.
   -- When On_Do_Dispatch is called the application shall trigg the thread (task)
   -- that owns (has called Open) the connection. When trigged the thread shall
   -- call the Dispatch operation.
   --
   -- There can be a number of contexts in the DOB. A connection is linked to the context specified in Open.
   -- All operations using a connection is affecting only the context linked to that connection.
   -- The intended primary usage is for recording/replay functionality. 0 is defined as the default
   -- context.
   --
   -- Note that Connection_Name_Common_Part together with Connection_Name_Instance_Part must be unique
   -- in the node.
   --
   -- If NULL  is passed as the Stop_Handler argument the connection will not receive a stop order.
   -- Normally only the main thread of an application should pass a non-NULL stopHandler, and it
   -- should then tell other parts of the application to exit. If multiple stop handlers are specified
   -- there is NO guaranteed order between which gets called first when a process receives a stop signal.
   --
   -- Parameters: Connection_Name_Common_Part - Name that identifies the program but not any particular
   --                                           program instance.
   --             Connection_Name_Instance_Part - Name that identifies a particular program instance.
   --             Context - Context functionality not implemented yet!
   --             Stop_Handler - Object that implements the Stop_Handler interface.
   --             Dispatcher - Object that implements the Dispatcher interface.
   -- Exceptions: Not_Open_Exception - The connection name is already used by someone else.
   --                                  Try another!
   --
   procedure Open
     (Self                          : in Connection;
      Connection_Name_Common_Part   : in Unbounded_Wide_String;
      Connection_Name_Instance_Part : in Unbounded_Wide_String;
      Context                       : in Safir.Dob.Typesystem.Int_32;
      Stop_Handler                  : access Safir.Dob.Consumers.Stop_Handler'Class;
      Dispatcher                    : access Safir.Dob.Consumers.Dispatcher'Class);

   -- Close the connection to the DOB.
   --
   -- Closes the connection to the DOB and deallocates all resources. All subscriptions
   -- and registrations will automatically be deleted and there is no need to call
   -- Unsubscribe and Unregister before calling Close.
   -- Note that all connections that were set up using Attach will also be closed after
   -- a call to this method.
   --
   procedure Close
     (Self : in Connection);

   -- Check if this Connection instance is open.
   --
   -- Returns: True if the connection is open.
   --
   function Is_Open
     (Self : in Connection) return Boolean;


   -- After receiving a Do_Dispatch callback the application MUST call this
   -- method. A call to Dispatch will result in that all queues for this connection
   -- are emptied and that each message in the queues are passed to the associated
   -- consumer. The call must be from the thread of the connection, not directly
   -- from Do_Dispatch.
   --
   -- Calls to dispatch from connection instances that are not open will be ignored.
   --
   procedure Dispatch
     (Self : in Connection);

   -----------------------
   -- Get_Controller_Id --
   -----------------------
   overriding
   function Get_Controller_Id (Self : in Connection) return Safir.Dob.Defs.Controller_Id;


   procedure Initialize (Self : in out Connection);
   procedure Finalize (Self : in out Connection);

private

   --type Do_Dispatch_Cb_T is access procedure;

   use type Safir.Dob.Defs.Controller_Id;

   type Connection is new Safir.Dob.Connection_Bases.Connection_Base with record
      --Do_Dispatch   : Do_Dispatch_Cb_T;
      Controller_Id : Safir.Dob.Defs.Controller_Id := -1;
   end record;

   procedure Close
     (Self         : in Connection;
      Check_Thread : in Boolean);

end Safir.Dob.Connections;
