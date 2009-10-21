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
with Safir.Dob.Defs;
with Safir.Dob.Connection_Bases;

package Safir.Dob.Connection_Aspect_Postpones is

   type Connection_Aspect_Postpone is tagged private;

   function Create
     (Connection_Base : in  Safir.Dob.Connection_Bases.Connection_Base'Class)
      return Connection_Aspect_Postpone;

   -- AWI:todo comment
   procedure Postpone (Self               : in Connection_Aspect_Postpone;
                       Redispatch_Current : in Boolean);

   -- AWI:todo comment
   procedure Resume_Postponed (Self : in Connection_Aspect_Postpone);

   -- AWI:todo comment
   procedure Incomplete_Injection_State (Self : in Connection_Aspect_Postpone);

private

   use type Safir.Dob.Defs.Controller_Id;

   type Connection_Aspect_Postpone is tagged record
      Controller_Id : Safir.Dob.Defs.Controller_Id := -1;
   end record;

end Safir.Dob.Connection_Aspect_Postpones;
