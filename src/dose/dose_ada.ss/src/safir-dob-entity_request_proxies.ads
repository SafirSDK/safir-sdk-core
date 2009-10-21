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

   -- AWI:todo comment
   function Get_Type_Id (Self : in Entity_Request_Proxy)
                         return Safir.Dob.Typesystem.Type_Id;

   -- AWI:todo comment
   function Get_Instance_Id (Self : in Entity_Request_Proxy)
                              return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;

   -- AWI:todo comment
   function Get_Entity_Id (Self : in Entity_Request_Proxy)
                         return Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;

   -- AWI:todo comment
   function Get_Request (Self : in Entity_Request_Proxy)
                        return Safir.Dob.Entity.Smart_Pointer'Class;

   -- AWI:todo comment
   function Get_Sender_Connection_Info (Self : in Entity_Request_Proxy) return
     Safir.Dob.Connection_Info.Smart_Pointer;

   -- AWI:todo comment
   function Get_Receiving_Handler_Id (Self : in Entity_Request_Proxy) return
     Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

   -- AWI:todo comment
   function Get_Blob (Self : in Entity_Request_Proxy) return
     Safir.Dob.Typesystem.Blob_T;

   -- AWI:todo comment
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
