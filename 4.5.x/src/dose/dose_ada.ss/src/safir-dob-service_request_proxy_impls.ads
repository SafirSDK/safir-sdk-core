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
with Safir.Dob.Typesystem;
with Safir.Dob.Service;
with Safir.Dob.Connection_Info;
with Safir.Dob.Typesystem.Handler_Id;

package Safir.Dob.Service_Request_Proxy_Impls is

   type Service_Request_Proxy_Impl is limited private;

   type Service_Request_Proxy_Impl_Access is access all Service_Request_Proxy_Impl;

   function Create (Request_Blob         : in Safir.Dob.Typesystem.Blob_T;
                    State                : in Safir.Dob.Typesystem.Char_Star)
                    return Service_Request_Proxy_Impl_Access;

   function Get_Type_Id (Self : in Service_Request_Proxy_Impl)
                         return Safir.Dob.Typesystem.Type_Id;

   function Get_Request (Self : in Service_Request_Proxy_Impl) return
     Safir.Dob.Service.Smart_Pointer'Class;

   function Get_Sender_Connection_Info (Self : in Service_Request_Proxy_Impl) return
     Safir.Dob.Connection_Info.Smart_Pointer;

   function Get_Receiving_Handler_Id (Self : in Service_Request_Proxy_Impl) return
     Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

   function Get_Blob (Self : in Service_Request_Proxy_Impl) return
     Safir.Dob.Typesystem.Blob_T;

   function Get_Receiver_With_String_Representation (Self : in Service_Request_Proxy_Impl) return
     Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

private

   type Service_Request_Proxy_Impl is record
      Request_Blob   : Safir.Dob.Typesystem.Blob_T;
      State          : Safir.Dob.Typesystem.Char_Star;
   end record;

end Safir.Dob.Service_Request_Proxy_Impls;
