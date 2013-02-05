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
with Safir.Dob.Service_Request_Proxy_Impl_Pointers;
with Safir.Dob.Typesystem;
with Safir.Dob.Connection_Info;
with Safir.Dob.Service;
with Safir.Dob.Typesystem.Handler_Id;

package Safir.Dob.Service_Request_Proxies is

   -- Note: The type is tagged only to enable prefix notation. You are not
   --       supposed to inherit from this type!
   type Service_Request_Proxy is tagged private;

   function Get_Type_Id (Self : in Service_Request_Proxy)
                         return Safir.Dob.Typesystem.Type_Id;

   -- Retrieves a smart pointer to the service request.
   --
   function Get_Request (Self : in Service_Request_Proxy) return
     Safir.Dob.Service.Smart_Pointer'Class;

   -- Get info about the sender.
   --
   -- Retrieves a smart pointer to info about the connection sending the request.
   --
   function Get_Sender_Connection_Info (Self : in Service_Request_Proxy) return
     Safir.Dob.Connection_Info.Smart_Pointer;

   -- Get id of receiving handler
   --
   -- Can be handy when one consumer is used for several handlers.
   --
   function Get_Receiving_Handler_Id (Self : in Service_Request_Proxy) return
     Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

   -- Get binary blob of the received service request.
   --
   -- This method will give you a pointer to the underlying representation of the object.
   --
   -- Note that this pointer is only valid while the Service_Request_Proxy is in scope.
   -- If you want to keep the blob you must copy it using methods in Safir.Dob.Typesystem.
   --
   -- This method is mainly useful if all you want to do with a received object is to write it
   -- to a database or pass it over a C-interface to a library or plugin.
   --
   function Get_Blob (Self : in Service_Request_Proxy) return
     Safir.Dob.Typesystem.Blob_T;

   ------------------------
   -- Trace and Debug stuff
   ------------------------

   -- Get receiver handler id that also contains the string representation.
   --
   -- Mainly for trace and debug purposes.
   --
   function Get_Receiver_With_String_Representation (Self : in Service_Request_Proxy) return
     Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

   -- For internal usage only!
   function Create (Proxy_Impl_Ptr : in Safir.Dob.Service_Request_Proxy_Impl_Pointers.Smart_Pointer)
     return Service_Request_Proxy;

private

   type Service_Request_Proxy is tagged record
      Impl_Ptr : Safir.Dob.Service_Request_Proxy_Impl_Pointers.Smart_Pointer;
   end record;

end Safir.Dob.Service_Request_Proxies;
