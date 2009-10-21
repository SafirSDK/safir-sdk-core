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
with Safir.Dob.Message_Proxy_Impls;

package body Safir.Dob.Message_Proxies is

   function Get_Type_Id (Self : in Message_Proxy)
                         return Safir.Dob.Typesystem.Type_Id is

   begin
      return Safir.Dob.Message_Proxy_Impls.Get_Type_Id (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Type_Id;

   function Get_Message (Self : in Message_Proxy)
                         return Safir.Dob.Message.Smart_Pointer'Class is
   begin
      return Safir.Dob.Message_Proxy_Impls.Get_Message (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Message;

   function Get_Sender_Connection_Info (Self : in Message_Proxy) return
     Safir.Dob.Connection_Info.Smart_Pointer is
   begin
      return Safir.Dob.Message_Proxy_Impls.Get_Sender_Connection_Info (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Sender_Connection_Info;

   function Get_Channel_Id (Self : in Message_Proxy) return
     Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type is
   begin
      return Safir.Dob.Message_Proxy_Impls.Get_Channel_Id (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Channel_Id;

   function Get_Blob (Self : in Message_Proxy) return
     Safir.Dob.Typesystem.Blob_T is
   begin
      return Safir.Dob.Message_Proxy_Impls.Get_Blob (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Blob;

   function Get_Channel_Id_With_String_Representation (Self : in Message_Proxy) return
     Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type is
   begin
      return Safir.Dob.Message_Proxy_Impls.Get_Channel_Id_With_String_Representation (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Channel_Id_With_String_Representation;

   function Create (Proxy_Impl_Ptr : in Safir.Dob.Message_Proxy_Impl_Pointers.Smart_Pointer)
                    return Message_Proxy is
      Proxy : Message_Proxy;
   begin
      Proxy.Impl_Ptr := Proxy_Impl_Ptr;
      return Proxy;
   end Create;

end Safir.Dob.Message_Proxies;
