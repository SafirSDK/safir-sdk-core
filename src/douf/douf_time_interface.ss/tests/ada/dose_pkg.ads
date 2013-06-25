-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
--
--  Created by: Erik Adolfsson / sterad
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
with Safir.Dob.Entity;
with Safir.Dob.Typesystem;
with Safir.Dob.Connection;
package Dose_Pkg is

   procedure Open
     (Connection  : in out Safir.Dob.Connection.Class);

   procedure Start;
   procedure Stop;

   procedure Register_Entity (ObjectId    : in Safir.Dob.Typesystem.ObjectId);

--   procedure Send_Update_Request ( Request : in Safir.Dob.Entity.Class'Class; ok : out boolean );

    procedure Subscribe_Entity
     (EntityId : in Safir.Dob.Typesystem.TypeId;
      Instance   : in Safir.Dob.Typesystem.Int32);

   procedure Set (Entity     : in Safir.Dob.Entity.Class'Class);

--   procedure Scan ( Entity_Id : in Safir.Dob.Typesystem.TypeId );

end Dose_Pkg;
