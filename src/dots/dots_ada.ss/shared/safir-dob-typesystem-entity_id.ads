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

with Ada.Strings.Wide_Unbounded;
with Safir.Dob.Typesystem.Instance_Id;
pragma Warnings ("H");  -- turn off warnings for hiding variable

-- Package containing  a type that represents the identity of an entity.
-- The identity consists of a type identifier (Type_Id) and an instance number.
package Safir.Dob.Typesystem.Entity_Id is

   type Entity_Id_Type is private;

   -- Constructor with type id and instance arguments.
   --
   -- Creates an Entity_Id with the given type id and instance number.
   --
   -- Type_Id - The type id of the entity that the EntityId is to refer to.
   -- Instance - The instance of the entity that the EntityId is to refer to.
   --
   function Create_Entity_Id
     (Type_Id        : in Safir.Dob.Typesystem.Type_Id;
      Instance_Id    : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type)
      return Entity_Id_Type;

   -- Get the instance number out of the Entity_Id.
   --
   function Get_Instance_Id (Self : in Entity_Id_Type)
                             return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;

   -- Set the instance number of the EntityId.
   --
   -- Instance_Id - The new instance number.
   --
   procedure Set_Instance_Id
     (Self        : in out Entity_Id_Type;
      Instance_Id : in     Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type);

   -- Get the type id out of the Entity_Id.
   --
   function Get_Type_Id (Self : in Entity_Id_Type) return Safir.Dob.Typesystem.Type_Id;

   -- Set the type id of the Entity_Id.
   --
   -- Type_Id - The new type id.
   --
   procedure Set_Type_Id (Self    : in out Entity_Id_Type;
                          Type_Id : in     Safir.Dob.Typesystem.Type_Id);


   -- Remove the included string from the entity id.
   --
   -- This is meant to be used when this type is used as a member of a Dob object.
   -- Using this call before the object gets serialized to binary or xml (i.e.
   -- also before sending it anywhere) means that the string will not be included
   -- when the object is sent.
   --
   procedure Remove_String (Self   : in out Entity_Id_Type);

   -- Convert an entity id to a string.
   --
   -- Will convert the entity id to a string on the form "(Safir.Dob.Entity, 10)".
   -- This is meant to be used for debug output only.
   -- If the type does not exist output will be on the form "(Unknown type: 32873478348, 10)"
   -- If the string representation of the instance exists, the numerical instance id may be
   -- replaced by that string.
   --
   -- The purpose of this function is for debug output and such.
   -- The resulting string can *not* reliably be parsed or passed to constructors to recreate the same
   -- entity id.
   --
   function To_String (Entity_Id : in Entity_Id_Type)
                       return Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   -- Convert an entity id to a string that has only numeric parts.
   --
   -- Will convert the entity id to a string on the form "(10109232329848, 2884849309093)".
   -- Use the normal ToString method if you need something for debug output. This is intended
   -- to be used when a consistent string is needed.
   --
   function To_String_Numeric (Entity_Id : in Entity_Id_Type)
                               return Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   -- Less-than operator.
   -- This is provided to allow EntityIds to be stored in containers.
   --
   function "<" (L, R : Entity_Id_Type) return Boolean;

private

   type Entity_Id_Type is
      record
         Type_Id     : Safir.Dob.Typesystem.Type_Id := 0;
         Instance_Id : Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      end record;

end Safir.Dob.Typesystem.Entity_Id;
