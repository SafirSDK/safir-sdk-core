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

-- Package that contains an interface to be implemented by all
-- Dob Typesystem Containers.
--
package Safir.Dob.Typesystem.Container_Base is
   pragma Preelaborate (Safir.Dob.Typesystem.Container_Base);

   type Container_Base_Type is interface;

   type Container_Base_Access is access all Container_Base_Type'Class;

   -- Is the container set to null?
   --
   -- Returns: True if the container is set to null.
   --
   function Is_Null (Self : in Container_Base_Type) return Boolean is abstract;

   -- Set the container to null.
   --
   procedure Set_Null (Self : in out Container_Base_Type) is abstract;

   -- Is the change flag set on the container?
   --
   -- The change flag gets updated every time the contained value changes.
   -- Note: If this is a container containing an object this call will recursively
   -- check change flags in the contained object.
   --
   function Is_Changed (Self : in Container_Base_Type) return Boolean is abstract;

   -- Set the container's change flag.
   --
   -- It should be fairly unusual for an application to have to use this
   -- operation. There is nothing dangerous about it, but are you sure this
   -- is the operation you were after?
   --
   -- The change flag is how receivers of objects can work out what the
   -- sender really wanted done on the object.
   --
   -- Note: If this is a container containing an object this call will recursively
   -- set all the change flags in the contained object.
   --
   procedure Set_Changed (Self : in out Container_Base_Type;
                          To   : in     Boolean) is abstract;

   -- Virtual assignment.
   --
   -- Copy all the members from "that" into "this". Types must be the same for this to work!
   --
   -- Parameters: That - The object to copy into Self
   --
   procedure Copy (Self : in out Container_Base_Type;
                   That : in Container_Base_Type'Class) is abstract;

end Safir.Dob.Typesystem.Container_Base;
