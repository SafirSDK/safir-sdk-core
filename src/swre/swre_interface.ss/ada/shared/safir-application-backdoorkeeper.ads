-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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
with Safir.Application.Backdoor;
with Safir.Dob.SecondaryConnection;
with Safir.Dob.Consumer;
with Safir.Dob.Message;


package Safir.Application.BackdoorKeeper is

   type Class is limited new Safir.Dob.Consumer.MessageSubscriber with private;

   type ClassAccess is access all Class'Class;

   procedure Start (Self       : in out Class;
                    Backdoor   : in     not null Safir.Application.Backdoor.ClassAccess);

   procedure Stop (Self : in out Class);

   --  Must not be overridden by users.
   procedure OnMessage (Self    : in out Class;
                        Message : in     Safir.Dob.Message.Class);

private

   type Class is limited new Safir.Dob.Consumer.MessageSubscriber with
      record
        Connection  : Safir.Dob.SecondaryConnection.Class;
        Started : Boolean := False;
        Backdoor : Safir.Application.Backdoor.ClassAccess := null;
      end record;

end Safir.Application.BackdoorKeeper;
