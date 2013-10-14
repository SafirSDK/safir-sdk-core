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
with Safir.Dob.Smart_Pointers;
pragma Elaborate_All (Safir.Dob.Smart_Pointers);
with Safir.Dob.Message_Proxy_Impls;

package Safir.Dob.Message_Proxy_Impl_Pointers is new Safir.Dob.Smart_Pointers
  (Safir.Dob.Message_Proxy_Impls.Message_Proxy_Impl,
   Safir.Dob.Message_Proxy_Impls.Message_Proxy_Impl_Access);
