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
with Safir.Dob.Previous_Entity_Proxy_Impls;

package body Safir.Dob.Previous_Entity_Proxies is

   function Get_Type_Id (Self : in Previous_Entity_Proxy)
                         return Safir.Dob.Typesystem.Type_Id is

   begin
      return Safir.Dob.Previous_Entity_Proxy_Impls.Get_Type_Id (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Type_Id;

   function Get_Instance_Id (Self : in Previous_Entity_Proxy)
                             return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type is
   begin
      return Safir.Dob.Previous_Entity_Proxy_Impls.Get_Instance_Id (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Instance_Id;

   function Get_Entity_Id (Self : in Previous_Entity_Proxy)
                         return Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type is
   begin
      return Safir.Dob.Previous_Entity_Proxy_Impls.Get_Entity_Id (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Entity_Id;

   function Get_Entity (Self : in Previous_Entity_Proxy)
                           return Safir.Dob.Entity.Smart_Pointer'Class is
   begin
      return Safir.Dob.Previous_Entity_Proxy_Impls.Get_Entity (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Entity;

   function Get_Entity_With_Change_Info (Self : in Previous_Entity_Proxy)
                                         return Safir.Dob.Entity.Smart_Pointer'Class is
   begin
      return Safir.Dob.Previous_Entity_Proxy_Impls.Get_Entity_With_Change_Info (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Entity_With_Change_Info;

   function Get_Owner (Self : in Previous_Entity_Proxy)
                       return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type is
   begin
      return Safir.Dob.Previous_Entity_Proxy_Impls.Get_Owner (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Owner;

   function Get_Owner_Connection_Info (Self : in Previous_Entity_Proxy)
                                       return Safir.Dob.Connection_Info.Smart_Pointer is
   begin
      return Safir.Dob.Previous_Entity_Proxy_Impls.Get_Owner_Connection_Info (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Owner_Connection_Info;

   function Get_Blob (Self : in Previous_Entity_Proxy)
                      return Safir.Dob.Typesystem.Blob_T is
   begin
      return Safir.Dob.Previous_Entity_Proxy_Impls.Get_Blob (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Blob;

   function Get_Blob_With_Change_Info (Self : in Previous_Entity_Proxy)
                                       return Safir.Dob.Typesystem.Blob_T is
   begin
      return Safir.Dob.Previous_Entity_Proxy_Impls.Get_Blob_With_Change_Info (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Blob_With_Change_Info;

   function Get_Owner_With_String_Representation (Self : in Previous_Entity_Proxy)
                                                  return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type is
   begin
      return Safir.Dob.Previous_Entity_Proxy_Impls.Get_Owner_With_String_Representation (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Owner_With_String_Representation;

   function Get_Timestamp (Self : in Previous_Entity_Proxy)
                           return Safir.Dob.Typesystem.Int_64 is
   begin
      return Safir.Dob.Previous_Entity_Proxy_Impls.Get_Timestamp (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Timestamp;

   function Get_Timestamp (Self : in Previous_Entity_Proxy;
                           Member : in Safir.Dob.Typesystem.Member_Index)
                           return Safir.Dob.Typesystem.Int_64 is
   begin
      return Safir.Dob.Previous_Entity_Proxy_Impls.Get_Timestamp (Self.Impl_Ptr.Get_Raw_Ptr.all,
                                                                  Member);
   end Get_Timestamp;

   function Create (Proxy_Impl_Ptr : in Safir.Dob.Previous_Entity_Proxy_Impl_Pointers.Smart_Pointer)
                    return Previous_Entity_Proxy is
      Proxy : Previous_Entity_Proxy;
   begin
      Proxy.Impl_Ptr := Proxy_Impl_Ptr;
      return Proxy;
   end Create;

end Safir.Dob.Previous_Entity_Proxies;
