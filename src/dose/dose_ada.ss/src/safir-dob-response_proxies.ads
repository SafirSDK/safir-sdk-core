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
with Safir.Dob.Response_Proxy_Impl_Pointers;
with Safir.Dob.Typesystem;
with Safir.Dob.Response;
with Safir.Dob.Connection_Info;
with Safir.Dob.Defs;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Typesystem.Handler_Id;
with Safir.Dob.Typesystem.Object;

package Safir.Dob.Response_Proxies is
   -- Note: The type is tagged only to enable prefix notation. You are not
   --       supposed to inherit from this type!
   type Response_Proxy is tagged private;

   -- AWI:todo comment
   function Is_Success (Self : in Response_Proxy) return Boolean;

   -- AWI:todo comment
   function Get_Type_Id (Self : in Response_Proxy)
                         return Safir.Dob.Typesystem.Type_Id;

   -- AWI:todo comment
   function Get_Response (Self : in Response_Proxy)
                          return Safir.Dob.Response.Smart_Pointer'Class;

   -- AWI:todo comment
   function Get_Response_Sender_Connection_Info (Self : in Response_Proxy)
                                                 return Safir.Dob.Connection_Info.Smart_Pointer;

   -- AWI:todo comment
   function Get_Blob (Self : in Response_Proxy)
                      return Safir.Dob.Typesystem.Blob_T;

   -- AWI:todo comment
   function Get_Request_Id (Self : in Response_Proxy)
                            return Safir.Dob.Defs.Request_Id;

   -- AWI:todo comment
   function Get_Request_Type_Id (Self : in Response_Proxy)
                                 return Safir.Dob.Typesystem.Type_Id;

   -- AWI:todo comment
   function Get_Request_Instance_Id (Self : in Response_Proxy)
                                     return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;

   -- AWI:todo comment
   function Get_Request (Self : in Response_Proxy)
                         return Safir.Dob.Typesystem.Object.Smart_Pointer'Class;

   -- AWI:todo comment
   function Get_Request_Blob (Self : in Response_Proxy)
                              return Safir.Dob.Typesystem.Blob_T;

   -- AWI:todo comment
   function Get_Request_Handler_Id (Self : in Response_Proxy)
                                     return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

   -- For internal usage only!
   function Create (Proxy_Impl_Ptr : in Safir.Dob.Response_Proxy_Impl_Pointers.Smart_Pointer)
     return Response_Proxy;

private

   type Response_Proxy is tagged record
      Impl_Ptr : Safir.Dob.Response_Proxy_Impl_Pointers.Smart_Pointer;
   end record;

end Safir.Dob.Response_Proxies;
