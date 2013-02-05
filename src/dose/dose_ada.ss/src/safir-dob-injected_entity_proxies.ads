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
with Safir.Dob.Injected_Entity_Proxy_Impl_Pointers;
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Entity;

package Safir.Dob.Injected_Entity_Proxies is

   -- Note: The type is tagged only to enable prefix notation. You are not
   --       supposed to inherit from this type!
   type Injected_Entity_Proxy is tagged private;

   -- Retrieves type id of the Entity that is about to be injected.
   --
   -- Returns: Type id.
   --
   function Get_Type_Id (Self : in Injected_Entity_Proxy)
                         return Safir.Dob.Typesystem.Type_Id;

   -- Retrieves instance id of the Entity that is about to be injected.
   --
   -- Returns: Instance id.
   --
   function Get_Instance_Id (Self : in Injected_Entity_Proxy)
                              return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;

   -- Aggregation of type id and instance id.
   --
   -- Returns: Entity id.
   function Get_Entity_Id (Self : in Injected_Entity_Proxy)
                         return Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;

   -- Get the entity state that is about to be injected.
   --
   -- Retrieves a smart pointer to the entity that is about to be injected.
   --
   -- Change flags will be set in the entity to indicate which members
   -- are part of the injection.
   --
   -- Note that this method cannot be called in an On_Injected_Deleted_Entity,
   -- since there then is no entity to get...
   --
   -- Returns: Entity.
   function Get_Injection (Self : in Injected_Entity_Proxy)
                        return Safir.Dob.Entity.Smart_Pointer'Class;

   -- Get binary blob of the entity that is about to be injected.
   --
   -- This operation will give you a pointer to the underlying representation of the object.
   -- Note that this pointer is only valid while the Injected_Entity_Proxy is in scope.
   -- If you want to keep the blob you must copy it using opeartions in Safir.Dob.Typesystem.
   --
   -- This operation is mainly useful if all you want to do with a received object is to write it
   -- to a database or pass it over a C-interface to a library or plugin.
   --
   -- Change flags will be set in the entity to indicate which members
   -- are part of the injection.
   --
   -- Returns: Binary blob of the entity that is about to be injected.
   --
   function Get_Injection_Blob (Self : in Injected_Entity_Proxy)
                                return Safir.Dob.Typesystem.Blob_T;

   -- Get the current entity state.
   --
   -- This method retrieves the entity as it is before the injection has been completed.
   --
   -- Can be used when a "current" state exists, i.e. from within the following callbacks:
   -- * Entity_Injection_Handler.On_Injected_Updated_Entity
   -- * Entity_Injection_Handler.On_Injected_Deleted_Entity
   --
   -- No change flags will be set in the returned entity.
   --
   -- Returns: Previous entity.
   --
   function Get_Current (Self : in Injected_Entity_Proxy)
                        return Safir.Dob.Entity.Smart_Pointer'Class;

   -- TODO: does anyone need a GetCurrentWithChangeInfo? Please tell your nearest
   -- Dob developer if so.

   -- For internal usage only!
   function Create (Proxy_Impl_Ptr : in Safir.Dob.Injected_Entity_Proxy_Impl_Pointers.Smart_Pointer)
     return Injected_Entity_Proxy;

private

   type Injected_Entity_Proxy is tagged record
      Impl_Ptr : Safir.Dob.Injected_Entity_Proxy_Impl_Pointers.Smart_Pointer;
   end record;

end Safir.Dob.Injected_Entity_Proxies;
