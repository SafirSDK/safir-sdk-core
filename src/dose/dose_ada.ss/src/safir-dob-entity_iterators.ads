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
with Ada.Finalization;
with Safir.Dob.Typesystem;
with Safir.Dob.Entity_Proxies;
with Safir.Dob.Smart_Pointers;
pragma Elaborate_All (Safir.Dob.Smart_Pointers);
with Safir.Dob.Defs;
with Interfaces.C;

package Safir.Dob.Entity_Iterators is

   type Entity_Iterator is private;

   function Done (Self : in Entity_Iterator) return Boolean;

   procedure Next (Self : in out Entity_Iterator);

   function "=" (L, R : Entity_Iterator) return Boolean;

   function Get_Entity_Proxy (Self : in Entity_Iterator)
                              return Safir.Dob.Entity_Proxies.Entity_Proxy;

   -- not for public usage!
   function Create (Controller_Id      : in Safir.Dob.Defs.Controller_Id;
                    Type_Id            : in Safir.Dob.Typesystem.Type_Id;
                    Include_Subclasses : in Boolean) return Entity_Iterator;

private

   use type Safir.Dob.Typesystem.Int_32;
   use type Interfaces.C.long;

   type Entity_Proxy_Access is access all Safir.Dob.Entity_Proxies.Entity_Proxy;

   package Entity_Proxy_Pointers is new Safir.Dob.Smart_Pointers
  (Safir.Dob.Entity_Proxies.Entity_Proxy, Entity_Proxy_Access);

   type Entity_Iterator is new Ada.Finalization.Controlled with
      record
         Controller_Id : Safir.Dob.Defs.Controller_Id := -1;
         Iterator_Id   : Safir.Dob.Typesystem.Int_32 := -1;
      end record;

   pragma Warnings (Off); -- declaration of "Finalize" and "Adjust" hides one ...
   overriding
   procedure Finalize (Self : in out Entity_Iterator);
   overriding
   procedure Adjust (Self : in out Entity_Iterator);
   pragma Warnings (On);

end Safir.Dob.Entity_Iterators;
