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
with Safir.Dob.Typesystem.Blob_Operations;
with Safir.Dob.Interf;
with Interfaces.C;
with Safir.Dob.Typesystem.Library_Exceptions;
with Safir.Dob.Typesystem.Object_Factory;
with Safir.Dob.Blob_References;

package body Safir.Dob.Entity_Request_Proxy_Impls is

   package C renames Interfaces.C;

   function Create (Request_Blob         : in Safir.Dob.Typesystem.Blob_T;
                    State                : in Safir.Dob.Typesystem.Char_Star)
                    return Entity_Request_Proxy_Impl_Access is
      Impl_Ptr : Entity_Request_Proxy_Impl_Access;
   begin
      Impl_Ptr := new Entity_Request_Proxy_Impl;
      Impl_Ptr.all.Request_Blob := Request_Blob;
      Impl_Ptr.all.State := State;
      return Impl_Ptr;
   end Create;

   function Get_Type_Id (Self : in Entity_Request_Proxy_Impl)
                         return Safir.Dob.Typesystem.Type_Id is

      use type Safir.Dob.Typesystem.Blob_T;

      Type_Id : Safir.Dob.Typesystem.Type_Id;
      Success : C.char;
   begin
      if Self.Request_Blob = null then
         Safir.Dob.Interf.Get_Type_Id (Self.State, Type_Id, Success);
         if C.char'Pos (Success) = 0 then
            Safir.Dob.Typesystem.Library_Exceptions.Throw;
         end if;
         return Type_Id;
      else
         return Safir.Dob.Typesystem.Blob_Operations.Get_Type_Id (Self.Request_Blob);
      end if;

   end Get_Type_Id;

   function Get_Instance_Id (Self : in Entity_Request_Proxy_Impl)
                             return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type is
      Instance_Id : Safir.Dob.Typesystem.Int_64;
      Success : C.char;
   begin
      Safir.Dob.Interf.Get_Instance_Id (Self.State,
                                        Instance_Id,
                                        Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
      return Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id (Instance_Id);
   end Get_Instance_Id;

   function Get_Entity_Id (Self : in Entity_Request_Proxy_Impl)
                           return Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type is

   begin
      return Safir.Dob.Typesystem.Entity_Id.Create_Entity_Id
        (Get_Type_Id (Self), Get_Instance_Id (Self));
   end Get_Entity_Id;

   function Get_Request (Self : in Entity_Request_Proxy_Impl) return
     Safir.Dob.Entity.Smart_Pointer'Class is

      use type Safir.Dob.Typesystem.Blob_T;
   begin
      pragma Assert (Self.Request_Blob /= null, "Not possible to Get_Request on DeleteRequests!");
      return Safir.Dob.Entity.Smart_Pointer'Class
        (Safir.Dob.Typesystem.Object_Factory.Create_Object (Self.Request_Blob));
   end Get_Request;

   function Get_Sender_Connection_Info (Self : in Entity_Request_Proxy_Impl) return
     Safir.Dob.Connection_Info.Smart_Pointer is

      use type Safir.Dob.Typesystem.Blob_T;

      Blob : Safir.Dob.Typesystem.Blob_T;
      Blob_Deleter : Safir.Dob.Blob_References.Blob_Deleter_Proc;
      Connection_Info : Safir.Dob.Connection_Info.Smart_Pointer;
      Success : C.char;
   begin
      Safir.Dob.Interf.Get_Connection_Info (Self.State, Blob, Blob_Deleter, Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      pragma Assert (Blob /= null, "Got NULL blob from DoseC_GetConnectionInfo");

      begin
         Connection_Info := Safir.Dob.Connection_Info.Smart_Pointer
           (Safir.Dob.Typesystem.Object_Factory.Create_Object (Blob));
         Blob_Deleter (Blob);
         return Connection_Info;
      exception
         when others =>
            Blob_Deleter (Blob);
            raise;
      end;

   end Get_Sender_Connection_Info;

   function Get_Receiving_Handler_Id (Self : in Entity_Request_Proxy_Impl) return
     Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type is
      Handler_Id : Safir.Dob.Typesystem.Int_64;
      Success    : C.char;
   begin
      Safir.Dob.Interf.Get_Handler_Id (Self.State, Handler_Id, Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
      return Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id (Handler_Id);
   end Get_Receiving_Handler_Id;

   function Get_Blob (Self : in Entity_Request_Proxy_Impl) return
     Safir.Dob.Typesystem.Blob_T is
      use type Safir.Dob.Typesystem.Blob_T;
   begin
      pragma Assert (Self.Request_Blob /= null, "No blob available on DeleteRequests!");
      return Self.Request_Blob;
   end Get_Blob;

   function Get_Receiver_With_String_Representation (Self : in Entity_Request_Proxy_Impl) return
     Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type is
   begin

      return Get_Receiving_Handler_Id (Self);
      -- AWI:todo Try to obtain string representation
   end Get_Receiver_With_String_Representation;

end Safir.Dob.Entity_Request_Proxy_Impls;
