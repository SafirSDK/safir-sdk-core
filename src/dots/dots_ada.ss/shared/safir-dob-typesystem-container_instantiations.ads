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
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Safir.Dob.Typesystem.Value_Container;
pragma Elaborate_All (Safir.Dob.Typesystem.Value_Container);
with Safir.Dob.Typesystem.Entity_Id; use Safir.Dob.Typesystem.Entity_Id;
pragma Elaborate_All (Safir.Dob.Typesystem.Entity_Id);
with Safir.Dob.Typesystem.Instance_Id; use Safir.Dob.Typesystem.Instance_Id;
pragma Elaborate_All (Safir.Dob.Typesystem.Instance_Id);
with Safir.Dob.Typesystem.Channel_Id; use Safir.Dob.Typesystem.Channel_Id;
pragma Elaborate_All (Safir.Dob.Typesystem.Channel_Id);
with Safir.Dob.Typesystem.Handler_Id; use Safir.Dob.Typesystem.Handler_Id;
pragma Elaborate_All (Safir.Dob.Typesystem.Handler_Id);

package Safir.Dob.Typesystem.Container_Instantiations is

   package Int_32_Container is new Value_Container (Contained_Type => Int_32,
                                                   Initial_Value  => 0);

   package Int_64_Container is new Value_Container (Contained_Type => Int_64,
                                                   Initial_Value  => 0);

   package Float_32_Container is new Value_Container (Contained_Type => Float_32,
                                                      Initial_Value => 0.0);

   package Float_64_Container is new Value_Container (Contained_Type => Float_64,
                                                      Initial_Value => 0.0);

   package Boolean_Container is new Value_Container (Contained_Type => Boolean,
                                                     Initial_Value => False);

   package Entity_Id_Container is new
     Value_Container (Contained_Type => Entity_Id_Type,
                      Initial_Value => Create_Entity_Id (0, Create_Instance_Id (-1)));

--     package Type_Id_Container is new
--       Value_Container (Contained_Type => Type_Id,
--                        Initial_Value  => 0);

   package Type_Id_Container renames Int_64_Container;

   package Instance_Id_Container is new
     Value_Container (Contained_Type => Instance_Id_Type,
                      Initial_Value  => Create_Instance_Id (-1));

   package Channel_Id_Container is new
     Value_Container (Contained_Type => Channel_Id_Type,
                      Initial_Value  => Create_Channel_Id
                        (To_Unbounded_Wide_String ("DEFAULT_CHANNEL")));

   package Handler_Id_Container is new
     Value_Container (Contained_Type => Handler_Id_Type,
                      Initial_Value  => Create_Handler_Id
                        (To_Unbounded_Wide_String ("DEFAULT_HANDLER")));

   package Binary_Container is new
     Value_Container (Contained_Type => Binary_Vectors.Vector,
                      Initial_Value  => Binary_Vectors.Empty_Vector);


end Safir.Dob.Typesystem.Container_Instantiations;
