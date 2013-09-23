-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2009-2013 (http://safir.sourceforge.net)
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
with Safir.Dob.Message;
with Safir.Dob.Typesystem.Channel_Id;
with Safir.Dob.Connection_Info;
with Safir.Dob.Message_Proxy_Impl_Pointers;
with Safir.Dob.Typesystem;

package Safir.Dob.Message_Proxies is
   -- Note: The type is tagged only to enable prefix notation. You are not
   --       supposed to inherit from this type!
   type Message_Proxy is tagged private;

   function Get_Type_Id (Self : in Message_Proxy)
                         return Safir.Dob.Typesystem.Type_Id;

   -- Retrieves a smart pointer to the message.
   --
   function Get_Message (Self : in Message_Proxy) return
     Safir.Dob.Message.Smart_Pointer'Class;

   -- Get info about the sender.
   --
   -- Retrieves a smart pointer to info about the connection sending the message.
   --
   function Get_Sender_Connection_Info (Self : in Message_Proxy) return
     Safir.Dob.Connection_Info.Smart_Pointer;

   -- Get channel id.
   --
   -- Retrieves the channel on which the message is sent.
   --
   function Get_Channel_Id (Self : in Message_Proxy) return
     Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;

   -- Get binary blob of the received message.
   --
   -- This method will give you a pointer to the underlying representation of the object.
   --
   -- Note that this pointer is only valid while the Message_Proxy is in scope.
   -- If you want to keep the blob you must copy it using methods in Safir.Dob.Typesystem.
   --
   -- This method is mainly useful if all you want to do with a received object is to write it
   -- to a database or pass it over a C-interface to a library or plugin.
   --
   function Get_Blob (Self : in Message_Proxy) return
     Safir.Dob.Typesystem.Blob_T;

   ------------------------
   -- Trace and Debug stuff
   ------------------------

   -- Get channel id that also contains the string representation.
   --
   -- Mainly for trace and debug purposes.
   --
   function Get_Channel_Id_With_String_Representation (Self : in Message_Proxy) return
     Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;


   -- For internal usage only!
   function Create (Proxy_Impl_Ptr : in Safir.Dob.Message_Proxy_Impl_Pointers.Smart_Pointer)
     return Message_Proxy;

private

   type Message_Proxy is tagged record
      Impl_Ptr : Safir.Dob.Message_Proxy_Impl_Pointers.Smart_Pointer;
   end record;

end Safir.Dob.Message_Proxies;
