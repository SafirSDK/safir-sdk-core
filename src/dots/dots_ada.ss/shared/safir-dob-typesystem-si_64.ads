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
with Safir.Dob.Typesystem.Container_Instantiations; use Safir.Dob.Typesystem.Container_Instantiations;

package Safir.Dob.Typesystem.Si_64 is

   subtype Ampere is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity electric current.

   subtype Cubic_Meter is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity volume.

   subtype Hertz is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity frequency.

   subtype Joule is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity energy.

   subtype Kelvin is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity thermodynamic temperature.

   subtype Kilogram is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity mass.

   subtype Meter is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity length.

   subtype Meter_Per_Second is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity velocity.

   subtype Meter_Per_Second_Squared is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity acceleration.

   subtype Newton is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity force.

   subtype Pascal is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity pressure.

   subtype Radian is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity plane angle.
   --  WARNING: Be careful if you compare two angles.
   --  The values might first have to be normalized.

   subtype Radian_Per_Second is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity angular velocity.

   subtype Radian_Per_Second_Squared is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity angular acceleration.

   subtype Second is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity time.

   subtype Square_Meter is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity area.

   subtype Steradian is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity solid angle.

   subtype Volt is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity electric potential.

   subtype Watt is Safir.Dob.Typesystem.Float_64;
   --  The SI unit for the quantity power.

      -- Containers
   package Ampere_Container renames Float_64_Container;
   package Cubic_Meter_Container renames Float_64_Container;
   package Hertz_Container renames Float_64_Container;
   package Joule_Container renames Float_64_Container;
   package Kelvin_Container renames Float_64_Container;
   package Kilogram_Container renames Float_64_Container;
   package Meter_Container renames Float_64_Container;
   package Meter_Per_Second_Container renames Float_64_Container;
   package Meter_Per_Second_Squared_Container renames Float_64_Container;
   package Newton_Container renames Float_64_Container;
   package Pascal_Container renames Float_64_Container;
   package Radian_Container renames Float_64_Container;
   package Radian_Per_Second_Container renames Float_64_Container;
   package Radian_Per_Second_Squared_Container renames Float_64_Container;
   package Second_Container renames Float_64_Container;
   package Square_Meter_Container renames Float_64_Container;
   package Steradian_Container renames Float_64_Container;
   package Volt_Container renames Float_64_Container;
   package Watt_Container renames Float_64_Container;

end Safir.Dob.Typesystem.Si_64;
