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
with Ada.Finalization;
with Safir.Dob.Defs;
limited with Safir.Dob.Consumer_Bases;
with Safir.Dob.Response;

package Safir.Dob.Response_Sender_Impls is

   type Response_Sender_Impl is limited private;

   type Response_Sender_Impl_Access is access all Response_Sender_Impl;

   function Create
     (Controller_Id : in Safir.Dob.Defs.Controller_Id;
      Consumer      : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Response_Id   : in Safir.Dob.Defs.Response_Id)
      return Response_Sender_Impl_Access;

   procedure Send (Self     : in out Response_Sender_Impl;
                   Response : in Safir.Dob.Response.Smart_Pointer'Class);

   function Is_Done (Self : in Response_Sender_Impl) return Boolean;

   procedure Discard (Self : in out Response_Sender_Impl);

private
   type Response_Sender_Impl is new Ada.Finalization.Limited_Controlled with record
      Is_Valid      : Boolean;
      Controller_Id : Safir.Dob.Defs.Controller_Id;
      Consumer      : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Response_Id   : Safir.Dob.Defs.Response_Id;
   end record;

   overriding
   procedure Finalize (Self : in out Response_Sender_Impl);

end Safir.Dob.Response_Sender_Impls;
