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
with Ada.Finalization;

generic
   type T is limited private;
   type Access_T is access all T;
package Safir.Dob.Smart_Pointers is

   type Smart_Pointer is tagged private;

   Null_Pointer : constant Smart_Pointer;

   function Create (Raw_Ptr : in Access_T) return Smart_Pointer;

   function Get_Raw_Ptr (Self : in Smart_Pointer) return Access_T;

   function Use_Count (Self : in Smart_Pointer) return Natural;

   procedure Set_Adjust_Limitation (Self : in out Smart_Pointer;
                                    Max  : in Natural);

private
   type Counter_Access is access all Natural;

   type Smart_Pointer is new Ada.Finalization.Controlled with
      record
         Data_Ptr          : Access_T;
         Counter_Ptr       : Counter_Access;
         Adjust_Limitation : Boolean := False;
         Max_Adjusts       : Natural;
      end record;

   pragma Warnings (Off); -- declaration of "Finalize" and "Adjust" hides one ...
   overriding
   procedure Finalize (Self : in out Smart_Pointer);
   overriding
   procedure Adjust (Self : in out Smart_Pointer);
   pragma Warnings (On);

   Null_Pointer : constant Smart_Pointer :=
                    (Ada.Finalization.Controlled with null, null, False, 0);

end Safir.Dob.Smart_Pointers;
