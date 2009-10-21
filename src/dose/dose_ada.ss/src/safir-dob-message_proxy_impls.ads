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
with Safir.Dob.Message;
with Safir.Dob.Connection_Info;
with Safir.Dob.Typesystem.Channel_Id;

package Safir.Dob.Message_Proxy_Impls is

   type Message_Proxy_Impl is limited private;

   type Message_Proxy_Impl_Access is access all Message_Proxy_Impl;

   function Create (Message_Blob : in Safir.Dob.Typesystem.Blob_T;
                    State        : in Safir.Dob.Typesystem.Char_Star)
                    return Message_Proxy_Impl_Access;

   function Get_Type_Id (Self : in Message_Proxy_Impl)
                         return Safir.Dob.Typesystem.Type_Id;

   function Get_Message (Self : in Message_Proxy_Impl)
                         return Safir.Dob.Message.Smart_Pointer'Class;

   function Get_Sender_Connection_Info (Self : in Message_Proxy_Impl) return
     Safir.Dob.Connection_Info.Smart_Pointer;

   function Get_Channel_Id (Self : in Message_Proxy_Impl) return
     Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;

   function Get_Blob (Self : in Message_Proxy_Impl) return
     Safir.Dob.Typesystem.Blob_T;

   function Get_Channel_Id_With_String_Representation (Self : in Message_Proxy_Impl) return
     Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
private

   type Message_Proxy_Impl is record
      Message_Blob : Safir.Dob.Typesystem.Blob_T;
      State :  Safir.Dob.Typesystem.Char_Star;
   end record;

end Safir.Dob.Message_Proxy_Impls;
