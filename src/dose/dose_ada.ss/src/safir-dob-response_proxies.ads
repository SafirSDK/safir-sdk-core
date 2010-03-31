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

   -- Get response success or failure status.
   --
   -- Returns: Success or failure.
   --
   function Is_Success (Self : in Response_Proxy) return Boolean;

   -- Retrieves type id of the response.
   --
   -- Returns: Type id.
   --
   function Get_Type_Id (Self : in Response_Proxy)
                         return Safir.Dob.Typesystem.Type_Id;

   -- Retrieves a smart pointer to the response.
   --
   -- Returns: Response.
   --
   function Get_Response (Self : in Response_Proxy)
                          return Safir.Dob.Response.Smart_Pointer'Class;

   -- Get info about the response sender.
   --
   -- Retrieves a smart pointer to info about the connection sending the response.
   --
   -- Returns: Connection info.
   --
   function Get_Response_Sender_Connection_Info (Self : in Response_Proxy)
                                                 return Safir.Dob.Connection_Info.Smart_Pointer;

   -----------------------
   -- Retrieve binary blob
   -----------------------

   -- Get binary blob of the received response.
   --
   -- This operation will give you a pointer to the underlying representation of the object.
   -- Note that this pointer is only valid while the Response_Proxy is in scope.
   -- If you want to keep the blob you must copy it using opeartions in Safir.Dob.Typesystem.
   --
   -- This operation is mainly useful if all you want to do with a received object is to write it
   -- to a database or pass it over a C-interface to a library or plugin.
   --
   -- Returns: Binary blob of the received response.
   --
   function Get_Blob (Self : in Response_Proxy)
                      return Safir.Dob.Typesystem.Blob_T;

   ----------------------------------------------------------
   -- Operations to retrieve info about the original request.
   ----------------------------------------------------------

   -- Retrieves the request id generated when the request was sent.
   --
   -- Returns: Request id.
   --
   function Get_Request_Id (Self : in Response_Proxy)
                            return Safir.Dob.Defs.Request_Id;

   -- Get type id of the entity or service sent in the original request.
   --
   -- Returns: Type id.
   --
   function Get_Request_Type_Id (Self : in Response_Proxy)
                                 return Safir.Dob.Typesystem.Type_Id;

   -- Get the instance id used in the original request. (Only for entity requests)
   --
   -- Returns: Instance id.
   --
   function Get_Request_Instance_Id (Self : in Response_Proxy)
                                     return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;

   -- Get the original request.
   --
   -- Retrieves the original request. Depending on the type of request this
   -- can be a Safir.Dob.Entity.Smart_Pointer or a Safir.Dob.Service.Smart_Pointer
   --
   -- Returns: Original request.
   --
   function Get_Request (Self : in Response_Proxy)
                         return Safir.Dob.Typesystem.Object.Smart_Pointer'Class;

   -- Get the original request blob.
   --
   -- Retrieves the blob of the original request.
   -- If the request was a delete request NULL will be returned.
   --
   -- This operation will give you a pointer to the underlying representation of the object.
   -- Note that this pointer is only valid while the Response_Proxy is in scope.
   -- If you want to keep the blob you must copy it using opeartions in Safir.Dob.Typesystem.
   --
   -- This operation is mainly useful if all you want to do with a received object is to write it
   -- to a database or pass it over a C-interface to a library or plugin.
   --
   -- Returns: Original request blob.
   --
   function Get_Request_Blob (Self : in Response_Proxy)
                              return Safir.Dob.Typesystem.Blob_T;

   -- Get the handler id to which the original request was sent.
   --
   -- Returns: Handler id.
   --
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
