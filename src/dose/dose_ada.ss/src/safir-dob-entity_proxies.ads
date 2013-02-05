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
with Safir.Dob.Entity_Proxy_Impl_Pointers;
with Safir.Dob.Typesystem;
with Safir.Dob.Entity;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Typesystem.Handler_Id;
with Safir.Dob.Connection_Info;
with Safir.Dob.Previous_Entity_Proxies;

package Safir.Dob.Entity_Proxies is

   -- Note: The type is tagged only to enable prefix notation. You are not
   --       supposed to inherit from this type!
   type Entity_Proxy is tagged private;

   -- Retrieves type id of the Entity.
   --
   -- Returns: Type id.
   --
   function Get_Type_Id (Self : in Entity_Proxy)
                         return Safir.Dob.Typesystem.Type_Id;

   -- Retrieves instance id of the Entity.
   --
   -- Returns: Instance id.
   --
   function Get_Instance_Id (Self : in Entity_Proxy)
                             return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;

   -- Aggregation of type id and instance id.
   --
   -- Returns: Entity id.
   --
   function Get_Entity_Id (Self : in Entity_Proxy)
                         return Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;

   -- Retrieves a smart pointer to the entity.
   --
   -- No change flags will be set in the returned entity.
   --
   -- Returns: Entity.
   --
   function Get_Entity (Self : in Entity_Proxy)
                        return Safir.Dob.Entity.Smart_Pointer'Class;

   -- Retrieves the entity with change flags set to indicate which members have
   -- changed since the last subscription response.
   --
   -- Returns: Entity.
   --
   function Get_Entity_With_Change_Info (Self : in Entity_Proxy)
                                         return Safir.Dob.Entity.Smart_Pointer'Class;

   -- Retrieves the handler id of the handler that owns (has created) this entity instance.
   --
   -- Returns: Handler id.
   --
   function Get_Owner (Self : in Entity_Proxy)
                       return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

   -- Get info about the connection to which the owner handler is related.
   --
   -- Returns: Connection ifo.
   function Get_Owner_Connection_Info (Self : in Entity_Proxy)
                                       return Safir.Dob.Connection_Info.Smart_Pointer;

   -----------------------
   -- Retrieve binary blob
   -----------------------

   -- Get binary blob of the received entity without changeflags set.
   --
   -- This operation will give you a pointer to the underlying representation of the object.
   -- Note that this pointer is only valid while the Entity_Proxy is in scope.
   -- If you want to keep the blob you must copy it using opeartions in Safir.Dob.Typesystem.
   --
   -- This operation is mainly useful if all you want to do with a received object is to write it
   -- to a database or pass it over a C-interface to a library or plugin.
   --
   -- Returns: Binary blob of the received entity.
   --
   function Get_Blob (Self : in Entity_Proxy)
                      return Safir.Dob.Typesystem.Blob_T;

   -- Get binary blob with change information.
   --
   -- Retrieves the entity with change flags set to indicate which members have
   -- changed since the last subscription response.
   -- See Get_Blob above.
   --
   -- Returns: Binary blob of the received entity.
   --
   function Get_Blob_With_Change_Info (Self : in Entity_Proxy)
                                       return Safir.Dob.Typesystem.Blob_T;

   ---------------------------------
   -- Retrieve previous entity state
   ---------------------------------

   -- Get previous entity state.
   --
   -- Used to get the entity state that preceeded this state.
   --
   -- Can be used when a "previous" state exists, that is, from within the following callbacks:
   -- * Entity_Subscriber.On_Updated_Entity
   -- * Entity_Subscriber.On_Deleted_Entity
   --
   -- No change flags will be set in the returned entity.
   --
   -- Returns: Previous entity.
   --
   function Get_Previous (Self : in Entity_Proxy)
                          return Safir.Dob.Previous_Entity_Proxies.Previous_Entity_Proxy;

   ------------------------
   -- Trace and Debug stuff
   ------------------------

   -- Get owner handler id that also contains the string representation.
   --
   -- Mainly for trace and debug purposes.
   -- See Get_Owner above.
   --
   -- Returns: Handler id.
   --
   function Get_Owner_With_String_Representation (Self : in Entity_Proxy)
                                                  return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

   -------------
   -- Retrieve timestamps. (Extended info for applications with special need)
   -- Note that timestamps is only available for types configured with this option.
   -------------

   -- Retrieves the timestamp for the latest create, update or delete.
   --
   -- Note that this operation is only valid for Injectable types.
   --
   -- Returns: Timestamp
   --
   function Get_Timestamp (Self : in Entity_Proxy)
                           return Safir.Dob.Typesystem.Int_64;

   -- Retrieves the timestamp for the given top member.
   --
   -- Note that this operation is only valid for Injectable types.
   --
   -- Parameters: Member - Top level member index.
   -- Returns: Timestamp.
   --
   function Get_Timestamp (Self : in Entity_Proxy;
                           Member : in Safir.Dob.Typesystem.Member_Index)
                           return Safir.Dob.Typesystem.Int_64;


   -- For internal usage only!
   function Create (Proxy_Impl_Ptr : in Safir.Dob.Entity_Proxy_Impl_Pointers.Smart_Pointer)
     return Entity_Proxy;

private

   type Entity_Proxy is tagged record
      Impl_Ptr : Safir.Dob.Entity_Proxy_Impl_Pointers.Smart_Pointer;
   end record;

end Safir.Dob.Entity_Proxies;
