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
with Safir.Dob.Response;
with Safir.Dob.Response_Sender_Impl_Pointers;

package Safir.Dob.Response_Senders is

   -- Type used for responding to received requests.
   --
   -- The purpose of this type is to allow responses to be sent either from
   -- within the request callback, or at a later time (in which case you have
   -- to keep the Response_Sender you received in the callback "for later").
   --
   -- The Response_Sender is reference counted and if it is destroyed (when the
   -- last reference is dropped) without being used, an error will be reported
   -- (a PanicLog!).
   -- Not using a Response_Sender is considered a programming error.

   -- Note that you still have to send the response within the timout period,
   -- or the response will not be delivered to the requestor (who will have
   -- received a timeout response instead).
   --
   -- Note: The type is tagged only to enable prefix notation. You are not
   --       supposed to inherit from it!
   --
   type Response_Sender is tagged private;

   type Response_Sender_Access is access all Response_Sender;

   -- Sends a response for the request that this instance was obtained with.
   --
   -- This operation may only be called once on any given instance! Calling it twice
   -- amounts to trying to send two responses to one request, which is considered
   -- a programming error.
   --
   -- Parameters: Response - The response to be sent.
   -- Exceptions: Not_Open_Exception - The connection is not open.
   --
   procedure Send (Self     : in Response_Sender;
                   Response : in Safir.Dob.Response.Smart_Pointer'Class);

   -- Check if an instance is still waiting for a response to be sent.
   --
   -- Returns: True if a response has been sent using this instance
   --          (instance is consumed), otherwise false.
   --
   function Is_Done (Self : in Response_Sender) return Boolean;

   -- Discard this ResponseSender.
   --
   -- Calling this function means that you forfeit your chance to send a response
   -- to the request. It will disable the checks in the Response_Sender is destroyed
   -- (see above).
   --
   -- The typical case when you must discard the Response_Sender is when calling
   -- Postpone with Redispatch_Current set to True. In this case you will get
   -- the request again together with a new Response_Sender.
   --
   procedure Discard (Self : in Response_Sender);

   -- For internal usage only!
   function Create (Response_Sender_Impl_Ptr : in Response_Sender_Impl_Pointers.Smart_Pointer)
                    return Response_Sender;
private

   type Response_Sender is tagged record
      Impl_Ptr : Safir.Dob.Response_Sender_Impl_Pointers.Smart_Pointer;
   end record;

end Safir.Dob.Response_Senders;
