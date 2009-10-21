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
with Safir.Dob.Typesystem.Object_Factory;
with Safir.Dob.Typesystem.Blob_Operations;
with Safir.Dob.Interf;
with Safir.Dob.Blob_References;
with Interfaces.C;
with Safir.Dob.Typesystem.Library_Exceptions;


package body Safir.Dob.Message_Proxy_Impls is

   package C renames Interfaces.C;

   function Create (Message_Blob : in Safir.Dob.Typesystem.Blob_T;
                    State        : in Safir.Dob.Typesystem.Char_Star)
                    return Message_Proxy_Impl_Access is
      Impl_Ptr : Message_Proxy_Impl_Access;
   begin
      Impl_Ptr := new Message_Proxy_Impl;
      Impl_Ptr.all.Message_Blob := Message_Blob;
      Impl_Ptr.all.State := State;
      return Impl_Ptr;
   end Create;

   function Get_Type_Id (Self : in Message_Proxy_Impl)
                         return Safir.Dob.Typesystem.Type_Id is

   begin
      return Safir.Dob.Typesystem.Blob_Operations.Get_Type_Id (Self.Message_Blob);
   end Get_Type_Id;

   function Get_Message (Self : in Message_Proxy_Impl)
                         return Safir.Dob.Message.Smart_Pointer'Class is
   begin
      return Safir.Dob.Message.Smart_Pointer'Class
        (Safir.Dob.Typesystem.Object_Factory.Create_Object (Self.Message_Blob));
   end Get_Message;

   function Get_Sender_Connection_Info (Self : in Message_Proxy_Impl) return
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


   function Get_Channel_Id (Self : in Message_Proxy_Impl) return
     Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type is

      Channel_Id : Safir.Dob.Typesystem.Int_64;
      Success : C.char;
   begin
      Safir.Dob.Interf.Get_Channel_Id (Self.State, Channel_Id, Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
      return Safir.Dob.Typesystem.Channel_Id.Create_Channel_Id (Channel_Id);
   end Get_Channel_Id;


   function Get_Blob (Self : in Message_Proxy_Impl) return
     Safir.Dob.Typesystem.Blob_T is
   begin
      return Self.Message_Blob;
   end Get_Blob;

   function Get_Channel_Id_With_String_Representation (Self : in Message_Proxy_Impl) return
     Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type is
   begin
      return Get_Channel_Id (Self);
      -- AWI:todo Try to obtain string representation
   end Get_Channel_Id_With_String_Representation;


end Safir.Dob.Message_Proxy_Impls;
