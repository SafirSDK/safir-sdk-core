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
with Ada.Unchecked_Deallocation;

package body Safir.Dob.Shm_References is

   procedure Free_Counter is
     new Ada.Unchecked_Deallocation (Natural,
                                     Counter_Access);

   function Create (Ptr : in Safir.Dob.Typesystem.Char_Star;
                    Deleter : Shm_Deleter_Proc) return Shm_Reference is
      Shm_Ref : Shm_Reference;
   begin
      Shm_Ref.Ptr := Ptr;
      Shm_Ref.Deleter := Deleter;
      Shm_Ref.Counter_Ptr := new Natural;
      Shm_Ref.Counter_Ptr.all := 1;
      return Shm_Ref;
   end Create;

   function Get_Ptr (Self : in Shm_Reference) return Safir.Dob.Typesystem.Char_Star is
   begin
      return Self.Ptr;
   end Get_Ptr;

   overriding procedure Finalize (Self : in out Shm_Reference) is
   begin
      if Self.Counter_Ptr = null then
         return;
      end if;

      Self.Counter_Ptr.all := Self.Counter_Ptr.all - 1;
      if Self.Counter_Ptr.all = 0 then
         if Self.Deleter /= null then
            Self.Deleter (Self.Ptr);
         end if;

         Free_Counter (Self.Counter_Ptr);
      end if;
   end Finalize;

   overriding procedure Adjust (Self : in out Shm_Reference) is
   begin
      if Self.Counter_Ptr = null then
         return;
      end if;
      Self.Counter_Ptr.all := Self.Counter_Ptr.all + 1;
   end Adjust;

end Safir.Dob.Shm_References;
