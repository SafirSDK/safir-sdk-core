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
with Safir.Dob.Injected_Entity_Proxy_Impls;

package body Safir.Dob.Injected_Entity_Proxies is

   function Get_Type_Id (Self : in Injected_Entity_Proxy)
                         return Safir.Dob.Typesystem.Type_Id is
   begin
      return Safir.Dob.Injected_Entity_Proxy_Impls.Get_Type_Id (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Type_Id;

   function Get_Instance_Id (Self : in Injected_Entity_Proxy)
                             return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type is
   begin
      return Safir.Dob.Injected_Entity_Proxy_Impls.Get_Instance_Id (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Instance_Id;

   function Get_Entity_Id (Self : in Injected_Entity_Proxy)
                           return Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type is
   begin
      return Safir.Dob.Injected_Entity_Proxy_Impls.Get_Entity_Id (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Entity_Id;

   function Get_Injection (Self : in Injected_Entity_Proxy)
                           return Safir.Dob.Entity.Smart_Pointer'Class is
   begin
      return Safir.Dob.Injected_Entity_Proxy_Impls.Get_Injection (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Injection;

   function Get_Injection_Blob (Self : in Injected_Entity_Proxy)
                                return Safir.Dob.Typesystem.Blob_T is
   begin
      return Safir.Dob.Injected_Entity_Proxy_Impls.Get_Injection_Blob (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Injection_Blob;

   function Get_Current (Self : in Injected_Entity_Proxy)
                         return Safir.Dob.Entity.Smart_Pointer'Class is
   begin
      return Safir.Dob.Injected_Entity_Proxy_Impls.Get_Current (Self.Impl_Ptr.Get_Raw_Ptr.all);
   end Get_Current;

   function Create (Proxy_Impl_Ptr : in Safir.Dob.Injected_Entity_Proxy_Impl_Pointers.Smart_Pointer)
                    return Injected_Entity_Proxy is
      Proxy : Injected_Entity_Proxy;
   begin
      Proxy.Impl_Ptr := Proxy_Impl_Ptr;
      return Proxy;
   end Create;

end Safir.Dob.Injected_Entity_Proxies;
