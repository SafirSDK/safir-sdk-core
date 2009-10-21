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

   -- Note: The type is tagged only to enable prefix notation. You are not
   --       supposed to inherit from it!
   type Response_Sender is tagged private;

   type Response_Sender_Access is access all Response_Sender;

   -- AWI:todo comment
   procedure Send (Self     : in Response_Sender;
                   Response : in Safir.Dob.Response.Smart_Pointer'Class);

   -- AWI:todo comment
   function Is_Done (Self : in Response_Sender) return Boolean;

   -- AWI:todo comment
   procedure Discard (Self : in Response_Sender);

   -- For internal usage only!
   function Create (Response_Sender_Impl_Ptr : in Response_Sender_Impl_Pointers.Smart_Pointer)
                    return Response_Sender;
private

   type Response_Sender is tagged record
      Impl_Ptr : Safir.Dob.Response_Sender_Impl_Pointers.Smart_Pointer;
   end record;

end Safir.Dob.Response_Senders;
