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

with Ada.Exceptions;

package Safir.Dob.Typesystem.Library_Exceptions is

   procedure Set (X : in Ada.Exceptions.Exception_Occurrence);

   procedure Throw;

   function Get_Exception_Type_Id (Ada_Exception_Id  : in Ada.Exceptions.Exception_Id)
                         return Safir.Dob.Typesystem.Type_Id;

   --  Only for use by automatically generated code! Do not call this yourself!
   procedure Register_Exception (Exception_Type_Id : in Safir.Dob.Typesystem.Type_Id;
                                 Ada_Exception_Id  : in Ada.Exceptions.Exception_Id);

end Safir.Dob.Typesystem.Library_Exceptions;
