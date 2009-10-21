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
with Safir.Dob.Typesystem.Object;

pragma Warnings ("H");  -- turn off warnings for hiding variable

-- For internal usage. Use only if you know what you are doing.
--
package Safir.Dob.Typesystem.Internal_Operations is

   -- Format a piece of blank memory to be a blob of a desired type.
   -- Does not check that the size of the blob is correct.
   --
   -- Parameters : Blob - Blob to format.
   --              Blob_Size - Size of blob to format.
   --              Type_Id - Type id.
   --              Beginning_Of_Unused - Pointer to unused part
   --
   procedure Format_Blob (Blob                : in Safir.Dob.Typesystem.Blob_T;
                          Blob_Size           : in Safir.Dob.Typesystem.Int_32;
                          Type_Id             : in Safir.Dob.Typesystem.Type_Id;
                          Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T);

   -- Merge the changed members (recursively) from one object into another.
   --
   -- This procedure will recurse through the members of the "from" object and
   -- take all the members that have a change flag set and copy them into the "into"
   -- object.
   --
   -- Parameters: Into - Object to merge into.
   --             From - Object whose changes shall be merged into "into".
   --
   procedure Merge_Changes
     (Into : in out Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
      From : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class);

end Safir.Dob.Typesystem.Internal_Operations;
