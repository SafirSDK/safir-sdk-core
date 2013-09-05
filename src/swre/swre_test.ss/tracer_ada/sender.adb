-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2013 (http://www.safirsdk.com)
--
--  Created by: Lars Hagström / lars.hagstrom@consoden.se
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
with Ada.Command_Line;
with Safir.Application.Tracers;
pragma Wide_Character_Encoding (UTF8);
procedure Sender is
   Razor : Safir.Application.Tracers.Tracer := Safir.Application.Tracers.Create ("Razor");
   Rb : Safir.Application.Tracers.Tracer := Safir.Application.Tracers.Create ("Rymd-Börje");
begin
   Razor.Enable (True);
   Rb.Enable (True);
   Rb.Put_Line ("blahonga");
   Rb.Put_Line ("blahong®a");
   Rb.Put_Line ("blahongaåäö");
   Razor.Put_Line ("brynanuppafjässasponken");
   Razor.Put_Line (Wide_Character'Val (16#202e#) & "reversed");
   Rb.Put_Line ("skull and crossbones: " & Wide_Character'Val (16#2620#));
   Rb.Put_Line ("interrobang: " & Wide_Character'Val (16#203d#));
   Razor.Put_Line (Integer'Wide_Image (1) &
                   Integer'Wide_Image (2) &
                   Float'Wide_Image (3.1));
   Razor.Put ("foo");
   Razor.Put_Line ("bar");
   Razor.Put_Line ("this is the end" & Wide_Character'Val (10) & "my only friend, the end");
   Rb.Put_Line ("of our elaborate plans");

   Ada.Command_Line.Set_Exit_Status (0);
end Sender;
