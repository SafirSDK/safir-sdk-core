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
with Safir.Dob.Entity_Request_Proxy_Impl_Pointers;
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Typesystem.Handler_Id;
with Safir.Dob.Connection_Info;
with Safir.Dob.Entity;

package Safir.Dob.Entity_Request_Proxies is

   -- Note: The type is tagged only to enable prefix notation. You are not
   --       supposed to inherit from this type!
   type Entity_Request_Proxy is tagged private;

   -- Retrieves type id of the entity request.
   --
   -- Returns: Type id.
   --
   function Get_Type_Id (Self : in Entity_Request_Proxy)
                         return Safir.Dob.Typesystem.Type_Id;

   -- Retrieves instance id of the entity request.
   --
   -- Note that it is illegal to call this operation on proxies received in On_Create_Request
   -- callbacks if the handler is registered as "Handler_Decides_Instance_Id".
   -- This is because there is no instance id in the request in this case...
   --
   -- Returns: Instance id.
   --
   function Get_Instance_Id (Self : in Entity_Request_Proxy)
                              return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;

   -- Aggregation of type id and instance id.
   --
   -- Note that it is illegal to call this operation on proxies received in On_Create_Request
   -- callbacks if the handler is registered as "Handler_Decides_Instance_Id".
   -- This is because there is no instance id in the request in this case...
   --
   -- Returns: Entity id.
   --
   function Get_Entity_Id (Self : in Entity_Request_Proxy)
                         return Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;

   -- Retrieves a smart pointer to the entity request.
   --
   -- Note that it is not valid to call this for a Delete_Request.
   --
   -- Returns: Entity request.
   --
   function Get_Request (Self : in Entity_Request_Proxy)
                        return Safir.Dob.Entity.Smart_Pointer'Class;

   -- Get info about the sender.
   --
   -- Retrieves a smart pointer to info about the connection sending the request.
   --
   -- Returns: Connection info
   --
   function Get_Sender_Connection_Info (Self : in Entity_Request_Proxy) return
     Safir.Dob.Connection_Info.Smart_Pointer;

   -- Get id of receiving handler.
   --
   -- Can be handy when one consumer is used for several handlers.
   --
   -- Returns : Handler id.
   --
   function Get_Receiving_Handler_Id (Self : in Entity_Request_Proxy) return
     Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

   -- Get binary blob of the received entity request.
   --
   -- This operation will give you a pointer to the underlying representation of the object.
   -- Note that this pointer is only valid while the Entity_Request_Proxy is in scope.
   -- If you want to keep the blob you must copy it using opeartions in Safir.Dob.Typesystem.
   --
   -- This operation is mainly useful if all you want to do with a received object is to write it
   -- to a database or pass it over a C-interface to a library or plugin.
   --
   -- Returns: Binary blob of the received entity request.
   --
   function Get_Blob (Self : in Entity_Request_Proxy) return
     Safir.Dob.Typesystem.Blob_T;

   ------------------------
   -- Trace and Debug stuff
   ------------------------

   -- Get receiver handler id that also contains the string representation.
   --
   -- Mainly for trace and debug purposes.
   -- See Get_Receiving_Handler_Id above.
   --
   -- Returns: Handler id.
   --
   function Get_Receiver_With_String_Representation (Self : in Entity_Request_Proxy) return
     Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

   -- For internal usage only!
   function Create (Proxy_Impl_Ptr : in Safir.Dob.Entity_Request_Proxy_Impl_Pointers.Smart_Pointer)
     return Entity_Request_Proxy;

private

   type Entity_Request_Proxy is tagged record
      Impl_Ptr : Safir.Dob.Entity_Request_Proxy_Impl_Pointers.Smart_Pointer;
   end record;

end Safir.Dob.Entity_Request_Proxies;
