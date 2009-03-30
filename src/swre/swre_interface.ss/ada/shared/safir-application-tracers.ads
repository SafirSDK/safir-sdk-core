-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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
with Ada.Strings.Wide_Unbounded;
with Safir.Dob.Typesystem;

package Safir.Application.Tracers is
   type Tracer is tagged private;

   function Create (Prefix : in Wide_String) return Tracer;

   --  This method cannot be called before either Enable, Put, Put_Line or New_Line have been called
   --  If it is called before this false will be returned.
   function IsEnabled (Self : in Tracer) return Boolean;

   procedure Enable (Self    : in out Tracer;
                     Enabled : in     Boolean);

   procedure Put (Self : in out Tracer;
                  Item : in     Wide_String);

   procedure Put_Line (Self : in out Tracer;
                       Item : in     Ada.Strings.Wide_Unbounded.Unbounded_Wide_String);

   procedure Put (Self : in out Tracer;
                  Item : in     Ada.Strings.Wide_Unbounded.Unbounded_Wide_String);

   procedure Put_Line (Self : in out Tracer;
                       Item : in     Wide_String);

   procedure New_Line (Self : in out Tracer);

   procedure Flush;

private
   type Tracer is tagged record
      Prefix : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      PrefixId : Safir.Dob.Typesystem.Int64;
   end record;

   procedure AddPrefix (Self : in out Tracer);

end Safir.Application.Tracers;
