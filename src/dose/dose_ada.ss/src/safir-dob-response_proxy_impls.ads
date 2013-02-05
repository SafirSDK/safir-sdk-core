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
with Safir.Dob.Typesystem;
with Safir.Dob.Defs;
with Safir.Dob.Typesystem.Object;
with Safir.Dob.Response;
with Safir.Dob.Connection_Info;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Typesystem.Handler_Id;

package Safir.Dob.Response_Proxy_Impls is

   type Response_Proxy_Impl is limited private;

   type Response_Proxy_Impl_Access is access all Response_Proxy_Impl;

   function Create (Request_Id            : in Safir.Dob.Defs.Request_Id;
                    Response_Blob         : in Safir.Dob.Typesystem.Blob_T;
                    Response_State        : in Safir.Dob.Typesystem.Char_Star;
                    Request_Blob          : in Safir.Dob.Typesystem.Blob_T;
                    Request_State         : in Safir.Dob.Typesystem.Char_Star)
                    return Response_Proxy_Impl_Access;

   function Is_Success (Self : in Response_Proxy_Impl) return Boolean;

   function Get_Type_Id (Self : in Response_Proxy_Impl)
                         return Safir.Dob.Typesystem.Type_Id;

   function Get_Response (Self : in Response_Proxy_Impl)
                          return Safir.Dob.Response.Smart_Pointer'Class;

   function Get_Response_Sender_Connection_Info (Self : in Response_Proxy_Impl)
                                                 return Safir.Dob.Connection_Info.Smart_Pointer;

   function Get_Blob (Self : in Response_Proxy_Impl)
                      return Safir.Dob.Typesystem.Blob_T;

   function Get_Request_Id (Self : in Response_Proxy_Impl)
                            return Safir.Dob.Defs.Request_Id;

   function Get_Request_Type_Id (Self : in Response_Proxy_Impl)
                                 return Safir.Dob.Typesystem.Type_Id;

   function Get_Request_Instance_Id (Self : in Response_Proxy_Impl)
                                     return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;

   function Get_Request (Self : in Response_Proxy_Impl)
                         return Safir.Dob.Typesystem.Object.Smart_Pointer'Class;

   function Get_Request_Blob (Self : in Response_Proxy_Impl)
                              return Safir.Dob.Typesystem.Blob_T;

   function Get_Request_Handler_Id (Self : in Response_Proxy_Impl)
                                     return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

private

   type Response_Proxy_Impl is record
      Request_Id      : Safir.Dob.Defs.Request_Id;
      Response_Blob   : Safir.Dob.Typesystem.Blob_T;
      Response_State  : Safir.Dob.Typesystem.Char_Star;
      Request_Blob    : Safir.Dob.Typesystem.Blob_T;
      Request_State   : Safir.Dob.Typesystem.Char_Star;
   end record;

end Safir.Dob.Response_Proxy_Impls;
