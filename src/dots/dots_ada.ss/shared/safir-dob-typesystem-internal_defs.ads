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
--with Interfaces.C.Pointers;
--with Interfaces.C.Strings;

package Safir.Dob.Typesystem.Internal_Defs is

   type DotsC_Property_Mapping_Kind is (Mapped_To_Null,
                                        Mapped_To_Member,
                                        Mapped_To_Parameter);
   for DotsC_Property_Mapping_Kind'Size use 32;

   type DotsC_Error_Code is (No_Error,
                             Read_Only_Property,
                             Unable_To_Dereference_Property,
                             Illegal_Value);
   for DotsC_Error_Code'Size use 32;

   type DotsC_Entity_Id is
      record
         Type_Id     : Safir.Dob.Typesystem.Type_Id;
         Instance_Id : Safir.Dob.Typesystem.Int_64;
      end record;
   for DotsC_Entity_Id'Size use 128;

   type Underlying_Entity_Id_Type is record
      Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Instance  : Safir.Dob.Typesystem.Int_64;
   end record;
   for Underlying_Entity_Id_Type use record
      Type_Id   at 0 range 0 .. 63;
      Instance at 8 range 0 .. 63;
   end record;
   for Underlying_Entity_Id_Type'Size use 16*8;

end Safir.Dob.Typesystem.Internal_Defs;
