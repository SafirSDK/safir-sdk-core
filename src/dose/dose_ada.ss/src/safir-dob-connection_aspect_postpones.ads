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
with Safir.Dob.Defs;
with Safir.Dob.Connection_Bases;

package Safir.Dob.Connection_Aspect_Postpones is

   -- Type that provides operations to postpone the reception of data from the Dob.
   --
   -- Sometimes it can by handy for an application to postpone the reception of data
   -- received in callbacks from the Dob.
   --
   -- For instance, setting up a subscription can potentially give a lot of initial On_New_Entity callbacks.
   -- If the application, for each subscription response, sends a request, the out-queue will probably fill up
   -- giving an Overflow exception. In this situation the application can postpone further callbacks.
   --
   type Connection_Aspect_Postpone is tagged private;

   function Create
     (Connection_Base : in  Safir.Dob.Connection_Bases.Connection_Base'Class)
      return Connection_Aspect_Postpone;

   -- Postpone dispatching of current callback method for the dispatched type and its subclasses.
   --
   -- Used by a consumer to postpone dispatching of current callback method for the dispatched type and its subclasses.
   --
   -- This method can be called from within the following callbacks:
   -- * Entity_Subscriber.On_New_Entity
   -- * Entity_Subscriber.On_Updated_Entity
   -- * Entity_Subscriber.On_Deleted_Entity
   -- * Entity_Injection_Base.On_Injected_New_Entity
   -- * Entity_Injection_Base.On_Injected_Updated_Entity
   -- * Entity_Injection_Base.On_Injected_Deleted_Entity
   -- * Entity_Request_Base.On_Create_Request
   -- * Entity_Request_Base.On_Update_Request
   -- * Entity_Request_Base.On_Delete_Request
   -- * Service_Request_Base.On_Service_Request
   -- * Message_Subscriber.On_Message
   --
   -- but NOT from the following callbacks:
   -- * Revoked_Registration_Base.On_Revoked_Registration
   -- * Completed_Registration_Base.On_Completed_Registration
   -- * Entity_Injection_Base.On_Initial_Injections_Done
   -- * Stop_Handler.On_Stop_Order
   -- * Dispatcher.On_Do_Dispatch
   -- * Requestor.On_Response
   -- * Requestor.On_Not_Request_Overflow
   -- * Message_Sender.On_Not_Message_Overflow
   -- * Registration_Subscriber.On_Registered
   -- * Registration_Subscriber.On_Unregistered
   --
   -- The dispatching is automatically resumed when an On_Not_Request_Overflow or On_Not_Message_Overflow is
   -- dispatched to the application which means that the application doesn't need to invoke Resume_Postponed
   -- by itself when the original postpone has been made because of an overflow situation.
   --
   -- Note that the postpone only applies to the currently dispatching consumer, other consumers will not
   -- be affected by the postpone.
   --
   -- If you are postponing a request (On_Create_Request, On_Update_Request, On_Delete_Request or On_Service_Request),
   -- special care must be taken to the handling of the Response_Sender object. If Redispatch_Current is set to True
   -- the current Response_Sender must be discarded (you will get a new Response_Sender). If Redispatch_Current
   -- is set to False, a response for the current request must be sent.
   --
   -- Parameters: Redispatch_Current - True indicates that the currently dispatched object shall be dispatched again
   --                                  once the dispatching is resumed.
   --
   procedure Postpone (Self               : in Connection_Aspect_Postpone;
                       Redispatch_Current : in Boolean);

   -- Resume dispatching of postponed objects.
   --
   -- Allows the application to explicitly resume dispatching of postponed objects.
   --
   -- Needs to be inoked only if the original postpone is not related to an overflow towards the Dob.
   -- See the Postpone operation above.
   --
   procedure Resume_Postponed (Self : in Connection_Aspect_Postpone);

   -- Discard the currently dispatched injected entity instance and wait for an update.
   --
   -- A create or update of an object from an external source can have produced an inconsistent entity state
   -- (from the perspective of the application, not the Dob) and this method gives the local owner the
   -- possibility to wait for the entity instance to be completly updated.
   --
   -- This method can be called from within the following callbacks:
   -- * Entity_Injection_Base.On_Injected_New_Entity
   -- * Entity_Injection_Base.On_Injected_Updated_Entity
   --
   --  The dispatching of the injected entity instance is resumed when it is updated by the external source.
   --
   procedure Incomplete_Injection_State (Self : in Connection_Aspect_Postpone);

private

   use type Safir.Dob.Defs.Controller_Id;

   type Connection_Aspect_Postpone is tagged record
      Controller_Id : Safir.Dob.Defs.Controller_Id := -1;
   end record;

end Safir.Dob.Connection_Aspect_Postpones;
