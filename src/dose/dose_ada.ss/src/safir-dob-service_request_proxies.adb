-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2009 (http://www.safirsdk.com)
--
--  Created by: Anders Wid�n / stawi
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
with Safir.Dob.Service_Request_Proxy_Impls;

package body Safir.Dob.Service_Request_Proxies is

   function Get_Type_Id (Self : in Service_Request_Proxy)
                         return Safir.Dob.Typesystem.Type_Id is

   begin
      return Safir.Dob.Service_Request_Proxy_Impls.Get_Type_Id (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Type_Id;

   function Get_Request (Self : in Service_Request_Proxy) return
     Safir.Dob.Service.Smart_Pointer'Class is
   begin
      return Safir.Dob.Service_Request_Proxy_Impls.Get_Request (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Request;

   function Get_Sender_Connection_Info (Self : in Service_Request_Proxy) return
     Safir.Dob.Connection_Info.Smart_Pointer is
   begin
      return Safir.Dob.Service_Request_Proxy_Impls.Get_Sender_Connection_Info (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Sender_Connection_Info;

   function Get_Receiving_Handler_Id (Self : in Service_Request_Proxy) return
     Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type is
   begin
      return Safir.Dob.Service_Request_Proxy_Impls.Get_Receiving_Handler_Id (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Receiving_Handler_Id;

   function Get_Blob (Self : in Service_Request_Proxy) return
     Safir.Dob.Typesystem.Blob_T is
   begin
      return Safir.Dob.Service_Request_Proxy_Impls.Get_Blob (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Blob;

   function Get_Receiver_With_String_Representation (Self : in Service_Request_Proxy) return
     Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type is
   begin
      return Safir.Dob.Service_Request_Proxy_Impls.Get_Receiver_With_String_Representation (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Receiver_With_String_Representation;

   function Create (Proxy_Impl_Ptr : in Safir.Dob.Service_Request_Proxy_Impl_Pointers.Smart_Pointer)
                    return Service_Request_Proxy is
      Proxy : Service_Request_Proxy;
   begin
      Proxy.Impl_Ptr := Proxy_Impl_Ptr;
      return Proxy;
   end Create;

end Safir.Dob.Service_Request_Proxies;
