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
with Safir.Dob.Shm_References;
with Safir.Dob.Blob_References;
with Safir.Dob.Entity;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Typesystem.Handler_Id;
with Safir.Dob.Connection_Info;
with Safir.Dob.Previous_Entity_Proxies;

package Safir.Dob.Entity_Proxy_Impls is

   type Entity_Proxy_Impl is limited private;

   type Entity_Proxy_Impl_Access is access all Entity_Proxy_Impl;

   function Create (Current_Blob          : in Safir.Dob.Typesystem.Blob_T;
                    Current_State         : in Safir.Dob.Typesystem.Char_Star;
                    Previous_Blob         : in Safir.Dob.Typesystem.Blob_T;
                    Previous_State        : in Safir.Dob.Typesystem.Char_Star;
                    Add_Reference         : in Boolean;
                    Timestamp_Diff        : in Boolean)
                    return Entity_Proxy_Impl_Access;

   function Get_Type_Id (Self : in Entity_Proxy_Impl)
                         return Safir.Dob.Typesystem.Type_Id;

   function Get_Instance_Id (Self : in Entity_Proxy_Impl)
                              return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;

   function Get_Entity_Id (Self : in Entity_Proxy_Impl)
                         return Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;

   function Get_Entity (Self : in Entity_Proxy_Impl)
                        return Safir.Dob.Entity.Smart_Pointer'Class;

   function Get_Entity_With_Change_Info (Self : in Entity_Proxy_Impl)
                                         return Safir.Dob.Entity.Smart_Pointer'Class;

   function Get_Owner (Self : in Entity_Proxy_Impl)
                       return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

   function Get_Owner_Connection_Info (Self : in Entity_Proxy_Impl)
                                       return Safir.Dob.Connection_Info.Smart_Pointer;

   function Get_Blob (Self : in Entity_Proxy_Impl)
                      return Safir.Dob.Typesystem.Blob_T;

   function Get_Blob_With_Change_Info (Self : in Entity_Proxy_Impl)
                                       return Safir.Dob.Typesystem.Blob_T;

   function Get_Previous (Self : in Entity_Proxy_Impl)
                          return Safir.Dob.Previous_Entity_Proxies.Previous_Entity_Proxy;

   function Get_Owner_With_String_Representation (Self : in Entity_Proxy_Impl)
                                                  return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

   function Get_Timestamp (Self : in Entity_Proxy_Impl)
                           return Safir.Dob.Typesystem.Int_64;

   function Get_Timestamp (Self : in Entity_Proxy_Impl;
                           Member : in Safir.Dob.Typesystem.Member_Index)
                           return Safir.Dob.Typesystem.Int_64;

private

   type Entity_Proxy_Impl is record
      Current_Blob                  : Safir.Dob.Typesystem.Blob_T;
      Current_State                 : Safir.Dob.Shm_References.Shm_Reference;
      Previous_Blob                 : Safir.Dob.Typesystem.Blob_T;
      Previous_State                : Safir.Dob.Shm_References.Shm_Reference;
      Timestamp_Diff                : Boolean;
      Current_Blob_With_Change_Info : Safir.Dob.Blob_References.Blob_Reference;
   end record;

end Safir.Dob.Entity_Proxy_Impls;
