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
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Library_Exceptions;
with Safir.Dob.Interf;
with Interfaces.C;

package body Safir.Dob.Connection_Aspect_Postpones is

   package C renames Interfaces.C;

   function Create
     (Connection_Base : in  Safir.Dob.Connection_Bases.Connection_Base'Class)
      return Connection_Aspect_Postpone is

      Aspect : Connection_Aspect_Postpone;
   begin
      Aspect.Controller_Id := Connection_Base.Get_Controller_Id;
      return Aspect;
   end Create;

   procedure Postpone (Self               : in Connection_Aspect_Postpone;
                       Redispatch_Current : in Boolean) is
      Success  : C.char;
   begin
      Safir.Dob.Interf.Postpone (Self.Controller_Id,
                                 C.char'Val (Boolean'Pos (Redispatch_Current)),
                                 Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Postpone;

   procedure Resume_Postponed (Self : in Connection_Aspect_Postpone) is
      Success  : C.char;
   begin
      Safir.Dob.Interf.Resume_Postponed (Self.Controller_Id,
                                         Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Resume_Postponed;

   procedure Incomplete_Injection_State (Self : in Connection_Aspect_Postpone) is
      Success  : C.char;
   begin
      Safir.Dob.Interf.Incomplete_Injection_State (Self.Controller_Id,
                                                   Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Incomplete_Injection_State;

end Safir.Dob.Connection_Aspect_Postpones;
