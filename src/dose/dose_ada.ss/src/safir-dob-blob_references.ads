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
with Ada.Finalization; use Ada.Finalization;
with Safir.Dob.Typesystem;

package Safir.Dob.Blob_References is

   type Blob_Reference is tagged private;

   type Blob_Deleter_Proc is access procedure (Ptr : in out Safir.Dob.Typesystem.Blob_T);
   pragma Convention (C, Blob_Deleter_Proc);

   function Create (Ptr : in Safir.Dob.Typesystem.Char_Star;
                    Deleter : Blob_Deleter_Proc) return Blob_Reference;

   function Get_Ptr (Self : in Blob_Reference) return Safir.Dob.Typesystem.Blob_T;

private

   type Counter_Access is access all Natural;

   type Blob_Reference is new Ada.Finalization.Controlled with record
      Ptr               : Safir.Dob.Typesystem.Char_Star;
      Deleter           : Blob_Deleter_Proc;
      Counter_Ptr       : Counter_Access;
   end record;

   pragma Warnings (Off); -- declaration of "Finalize" and "Adjust" hides one ...
   overriding
   procedure Finalize (Self : in out Blob_Reference);
   overriding
   procedure Adjust (Self : in out Blob_Reference);
   pragma Warnings (On);

end Safir.Dob.Blob_References;
