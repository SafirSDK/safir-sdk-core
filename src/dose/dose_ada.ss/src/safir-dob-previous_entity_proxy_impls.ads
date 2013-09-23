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
with Safir.Dob.Connection_Info;
with Safir.Dob.Typesystem;
with Safir.Dob.Shm_References;
with Safir.Dob.Blob_References;
with Safir.Dob.Entity;
with Safir.Dob.Typesystem.Handler_Id;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Typesystem.Instance_Id;

package Safir.Dob.Previous_Entity_Proxy_Impls is

   type Previous_Entity_Proxy_Impl is limited private;

   type Previous_Entity_Proxy_Impl_Access is access all Previous_Entity_Proxy_Impl;

   function Create (Current_Blob          : in Safir.Dob.Typesystem.Blob_T;
                    Current_State         : in Safir.Dob.Shm_References.Shm_Reference;
                    Previous_Blob         : in Safir.Dob.Typesystem.Blob_T;
                    Previous_State        : in Safir.Dob.Shm_References.Shm_Reference;
                    Timestamp_Diff        : in Boolean)
                    return Previous_Entity_Proxy_Impl_Access;

   function Get_Type_Id (Self : in Previous_Entity_Proxy_Impl)
                         return Safir.Dob.Typesystem.Type_Id;

   function Get_Instance_Id (Self : in Previous_Entity_Proxy_Impl)
                              return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;

   function Get_Entity_Id (Self : in Previous_Entity_Proxy_Impl)
                         return Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;

   function Get_Entity (Self : in Previous_Entity_Proxy_Impl)
                        return Safir.Dob.Entity.Smart_Pointer'Class;

   function Get_Entity_With_Change_Info (Self : in Previous_Entity_Proxy_Impl)
                                         return Safir.Dob.Entity.Smart_Pointer'Class;

   function Get_Owner (Self : in Previous_Entity_Proxy_Impl)
                       return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

   function Get_Owner_Connection_Info (Self : in Previous_Entity_Proxy_Impl)
                                       return Safir.Dob.Connection_Info.Smart_Pointer;

   function Get_Blob (Self : in Previous_Entity_Proxy_Impl)
                      return Safir.Dob.Typesystem.Blob_T;

   function Get_Blob_With_Change_Info (Self : in Previous_Entity_Proxy_Impl)
                                       return Safir.Dob.Typesystem.Blob_T;

   function Get_Owner_With_String_Representation (Self : in Previous_Entity_Proxy_Impl)
                                                  return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

   function Get_Timestamp (Self : in Previous_Entity_Proxy_Impl)
                           return Safir.Dob.Typesystem.Int_64;

   function Get_Timestamp (Self : in Previous_Entity_Proxy_Impl;
                           Member : in Safir.Dob.Typesystem.Member_Index)
                           return Safir.Dob.Typesystem.Int_64;

private

   type Previous_Entity_Proxy_Impl is record
      Current_Blob                   : Safir.Dob.Typesystem.Blob_T;
      Current_State                  : Safir.Dob.Shm_References.Shm_Reference;
      Previous_Blob                  : Safir.Dob.Typesystem.Blob_T;
      Previous_State                 : Safir.Dob.Shm_References.Shm_Reference;
      Timestamp_Diff                 : Boolean;
      Previous_Blob_With_Change_Info : Safir.Dob.Blob_References.Blob_Reference;
   end record;

end Safir.Dob.Previous_Entity_Proxy_Impls;
