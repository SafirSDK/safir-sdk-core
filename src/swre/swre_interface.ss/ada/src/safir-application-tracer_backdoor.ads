-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2013 (http://www.safirsdk.com)
--
--  Created by: Lars Hagström / lars.hagstrom@consoden.se
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
with Safir.Dob.Connection_Bases;

package Safir.Application.Tracer_Backdoor is
   procedure Start (Connection : in Safir.Dob.Connection_Bases.Connection_Base);
   procedure Stop;
end Safir.Application.Tracer_Backdoor;
