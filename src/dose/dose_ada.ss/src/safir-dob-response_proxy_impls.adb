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
with Ada.Exceptions;
with Safir.Dob.Typesystem.Blob_Operations;
with Safir.Dob.Typesystem.Operations;
with Safir.Dob.Success_Response;
with Safir.Dob.Typesystem.Object.Factory;
with Safir.Dob.Interf;
with Safir.Dob.Typesystem.Library_Exceptions;
with Safir.Dob.Blob_References;
with Interfaces.C;

package body Safir.Dob.Response_Proxy_Impls is

   package C renames Interfaces.C;

   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String := "")
                    renames  Ada.Exceptions.Raise_Exception;

   function Create (Request_Id            : in Safir.Dob.Defs.Request_Id;
                    Response_Blob         : in Safir.Dob.Typesystem.Blob_T;
                    Response_State        : in Safir.Dob.Typesystem.Char_Star;
                    Request_Blob          : in Safir.Dob.Typesystem.Blob_T;
                    Request_State         : in Safir.Dob.Typesystem.Char_Star)
                    return Response_Proxy_Impl_Access is
      Impl_Ptr : Response_Proxy_Impl_Access;
   begin
      Impl_Ptr := new Response_Proxy_Impl;
      Impl_Ptr.all.Request_Id := Request_Id;
      Impl_Ptr.all.Response_Blob := Response_Blob;
      Impl_Ptr.all.Response_State := Response_State;
      Impl_Ptr.all.Request_Blob := Request_Blob;
      Impl_Ptr.all.Request_State := Request_State;
      return Impl_Ptr;
   end Create;

   function Is_Success (Self : in Response_Proxy_Impl) return Boolean is
   begin
      return Safir.Dob.Typesystem.Operations.Is_Of_Type
        (Get_Type_Id (Self),
         Safir.Dob.Success_Response.Class_Type_Id);
   end Is_Success;

   function Get_Type_Id (Self : in Response_Proxy_Impl)
                         return Safir.Dob.Typesystem.Type_Id is
   begin
         return Safir.Dob.Typesystem.Blob_Operations.Get_Type_Id (Self.Response_Blob);
   end Get_Type_Id;

   function Get_Response (Self : in Response_Proxy_Impl)
                          return Safir.Dob.Response.Smart_Pointer'Class is
   begin
      return Safir.Dob.Response.Smart_Pointer'Class
        (Safir.Dob.Typesystem.Object.Factory.Create_Object (Self.Response_Blob));
   end Get_Response;

   function Get_Response_Sender_Connection_Info (Self : in Response_Proxy_Impl) return
     Safir.Dob.Connection_Info.Smart_Pointer is

      use type Safir.Dob.Typesystem.Blob_T;

      Blob : Safir.Dob.Typesystem.Blob_T;
      Blob_Deleter : Safir.Dob.Blob_References.Blob_Deleter_Proc;
      Connection_Info : Safir.Dob.Connection_Info.Smart_Pointer;
      Success : C.char;
   begin
      Safir.Dob.Interf.Get_Connection_Info (Self.Response_State, Blob, Blob_Deleter, Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      pragma Assert (Blob /= null, "Got NULL blob from DoseC_GetConnectionInfo");

      begin
         Connection_Info := Safir.Dob.Connection_Info.Smart_Pointer
           (Safir.Dob.Typesystem.Object.Factory.Create_Object (Blob));
         Blob_Deleter (Blob);
         return Connection_Info;
      exception
         when others =>
            Blob_Deleter (Blob);
            raise;
      end;

   end Get_Response_Sender_Connection_Info;

   function Get_Blob (Self : in Response_Proxy_Impl)
                      return Safir.Dob.Typesystem.Blob_T is
   begin
      return Self.Response_Blob;
   end Get_Blob;

   function Get_Request_Id (Self : in Response_Proxy_Impl)
                            return Safir.Dob.Defs.Request_Id is
   begin
      return Self.Request_Id;
   end Get_Request_Id;

   function Get_Request_Type_Id (Self : in Response_Proxy_Impl)
                                 return Safir.Dob.Typesystem.Type_Id is
      Type_Id : Safir.Dob.Typesystem.Type_Id;
      Success : C.char;
   begin
      Safir.Dob.Interf.Get_Type_Id (Self.Request_State,
                                    Type_Id,
                                    Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
      return Type_Id;
   end Get_Request_Type_Id;


   function Get_Request_Instance_Id (Self : in Response_Proxy_Impl)
                                     return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type is
      Instance_Id : Safir.Dob.Typesystem.Int_64;
      Success : C.char;
   begin
      Safir.Dob.Interf.Get_Instance_Id (Self.Request_State,
                                        Instance_Id,
                                        Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
      return Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id (Instance_Id);
   end Get_Request_Instance_Id;

   function Get_Request (Self : in Response_Proxy_Impl)
                         return Safir.Dob.Typesystem.Object.Smart_Pointer'Class is
      use type Safir.Dob.Typesystem.Blob_T;
   begin
      if Self.Request_Blob = null then
         Throw (Safir.Dob.Typesystem.Software_Violation_Exception'Identity,
                "Cannot get Request on ResponseProxies for DeleteRequests");
      end if;
      return Safir.Dob.Typesystem.Object.Factory.Create_Object (Self.Request_Blob);
   end Get_Request;

   function Get_Request_Blob (Self : in Response_Proxy_Impl)
                              return Safir.Dob.Typesystem.Blob_T is
   begin
      return Self.Request_Blob;
   end Get_Request_Blob;

   function Get_Request_Handler_Id (Self : in Response_Proxy_Impl) return
     Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type is

      Handler_Id : Safir.Dob.Typesystem.Int_64;
      Success    : C.char;
   begin
      Safir.Dob.Interf.Get_Handler_Id (Self.Request_State, Handler_Id, Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
      return Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id (Handler_Id);
   end Get_Request_Handler_Id;

end Safir.Dob.Response_Proxy_Impls;
