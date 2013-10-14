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
with Ada.Unchecked_Deallocation;
with Safir.Logging;

package body Safir.Dob.Smart_Pointers is

   procedure Free_Item is
     new Ada.Unchecked_Deallocation (T, Access_T);

   procedure Free_Counter is
     new Ada.Unchecked_Deallocation (Natural,
                                     Counter_Access);

   function Create (Raw_Ptr : in Access_T)
                    return Smart_Pointer is
      Ptr : Smart_Pointer;
   begin
      Ptr.Data_Ptr := Raw_Ptr;
      Ptr.Counter_Ptr := new Natural;
      Ptr.Counter_Ptr.all := 1;
      return Ptr;
   end Create;

   function Get_Raw_Ptr (Self : in Smart_Pointer) return Access_T is
   begin
      return Self.Data_Ptr;
   end Get_Raw_Ptr;

   function Use_Count (Self : in Smart_Pointer) return Natural is
   begin
      if Self.Counter_Ptr = null then
         return 0;
      end if;
      return Self.Counter_Ptr.all;
   end Use_Count;

   procedure Set_Adjust_Limitation (Self : in out Smart_Pointer;
                                    Max  : in Natural) is
   begin
      Self.Adjust_Limitation := True;
      Self.Max_Adjusts := Max;
   end Set_Adjust_Limitation;

   overriding procedure Finalize (Self : in out Smart_Pointer) is
   begin
      if Self.Data_Ptr = null then
         return;
      end if;

      Self.Counter_Ptr.all := Self.Counter_Ptr.all - 1;
      if Self.Counter_Ptr.all = 0 then
         Free_Item (Self.Data_Ptr);
         Free_Counter (Self.Counter_Ptr);
      end if;
   end Finalize;

   overriding procedure Adjust (Self : in out Smart_Pointer) is
   begin
      if Self.Data_Ptr /= null then
         if Self.Adjust_Limitation then
            if Self.Max_Adjusts < 1 then
               Safir.Logging.Send_System_Log (Safir.Logging.Critical,
                                              "You are not allowed to make a copy of the proxy object!");
            else
               Self.Max_Adjusts := Self.Max_Adjusts - 1;
            end if;
         end if;

         Self.Counter_Ptr.all := Self.Counter_Ptr.all + 1;
      end if;
   end Adjust;

end Safir.Dob.Smart_Pointers;
