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
with Safir.Dob.Response_Sender_Impls;

package body Safir.Dob.Response_Senders is

   procedure Send (Self     : in Response_Sender;
                   Response : in Safir.Dob.Response.Smart_Pointer'Class) is
   begin
      Safir.Dob.Response_Sender_Impls.Send (Self.Impl_Ptr.Get_Raw_Ptr.all,
                                            Response);
   end Send;

   function Is_Done (Self : in Response_Sender) return Boolean is
   begin
      return Safir.Dob.Response_Sender_Impls.Is_Done
        (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Is_Done;

   procedure Discard (Self : in Response_Sender) is
   begin
      Safir.Dob.Response_Sender_Impls.Discard (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Discard;

   function Create (Response_Sender_Impl_Ptr : in Response_Sender_Impl_Pointers.Smart_Pointer)
                    return Response_Sender is
      Resp_Sender : Response_Sender;
   begin
      Resp_Sender.Impl_Ptr := Response_Sender_Impl_Ptr;
      return Resp_Sender;
   end Create;

end Safir.Dob.Response_Senders;
