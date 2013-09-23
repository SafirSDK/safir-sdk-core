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
with Safir.Dob.Response_Proxy_Impls;

package body Safir.Dob.Response_Proxies is

   function Is_Success (Self : in Response_Proxy) return Boolean is
   begin
      return Safir.Dob.Response_Proxy_Impls.Is_Success (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Is_Success;

   function Get_Type_Id (Self : in Response_Proxy)
                         return Safir.Dob.Typesystem.Type_Id is
   begin
      return Safir.Dob.Response_Proxy_Impls.Get_Type_Id (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Type_Id;

   function Get_Response (Self : in Response_Proxy)
                          return Safir.Dob.Response.Smart_Pointer'Class is
   begin
      return Safir.Dob.Response_Proxy_Impls.Get_Response (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Response;

   function Get_Response_Sender_Connection_Info (Self : in Response_Proxy)
                                                 return Safir.Dob.Connection_Info.Smart_Pointer is
   begin
      return Safir.Dob.Response_Proxy_Impls.Get_Response_Sender_Connection_Info (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Response_Sender_Connection_Info;

   function Get_Blob (Self : in Response_Proxy)
                      return Safir.Dob.Typesystem.Blob_T is
   begin
      return Safir.Dob.Response_Proxy_Impls.Get_Blob (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Blob;

   function Get_Request_Id (Self : in Response_Proxy)
                            return Safir.Dob.Defs.Request_Id is
   begin
      return Safir.Dob.Response_Proxy_Impls.Get_Request_Id (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Request_Id;

   function Get_Request_Type_Id (Self : in Response_Proxy)
                                 return Safir.Dob.Typesystem.Type_Id is
   begin
      return Safir.Dob.Response_Proxy_Impls.Get_Request_Type_Id (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Request_Type_Id;

   function Get_Request_Instance_Id (Self : in Response_Proxy)
                                     return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type is
   begin
      return Safir.Dob.Response_Proxy_Impls.Get_Request_Instance_Id (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Request_Instance_Id;

   function Get_Request (Self : in Response_Proxy)
                         return Safir.Dob.Typesystem.Object.Smart_Pointer'Class is
   begin
      return Safir.Dob.Response_Proxy_Impls.Get_Request (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Request;

   function Get_Request_Blob (Self : in Response_Proxy)
                              return Safir.Dob.Typesystem.Blob_T is
   begin
      return Safir.Dob.Response_Proxy_Impls.Get_Request_Blob (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Request_Blob;

   function Get_Request_Handler_Id (Self : in Response_Proxy)
                                    return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type is
   begin
      return Safir.Dob.Response_Proxy_Impls.Get_Request_Handler_Id (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Request_Handler_Id;

   function Create (Proxy_Impl_Ptr : in Safir.Dob.Response_Proxy_Impl_Pointers.Smart_Pointer)
                    return Response_Proxy is
      Proxy : Response_Proxy;
   begin
      Proxy.Impl_Ptr := Proxy_Impl_Ptr;
      return Proxy;
   end Create;

end Safir.Dob.Response_Proxies;
