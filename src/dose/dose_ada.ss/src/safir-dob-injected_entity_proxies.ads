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

   -- AWI:todo comment
   function Get_Type_Id (Self : in Injected_Entity_Proxy)
                         return Safir.Dob.Typesystem.Type_Id;

   -- AWI:todo comment
   function Get_Instance_Id (Self : in Injected_Entity_Proxy)
                              return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;

   -- AWI:todo comment
   function Get_Entity_Id (Self : in Injected_Entity_Proxy)
                         return Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;

   -- AWI:todo comment
   function Get_Injection (Self : in Injected_Entity_Proxy)
                        return Safir.Dob.Entity.Smart_Pointer'Class;

   -- AWI:todo comment
   function Get_Injection_Blob (Self : in Injected_Entity_Proxy)
                                return Safir.Dob.Typesystem.Blob_T;

   -- AWI:todo comment
   function Get_Current (Self : in Injected_Entity_Proxy)
                        return Safir.Dob.Entity.Smart_Pointer'Class;

   -- For internal usage only!
   function Create (Proxy_Impl_Ptr : in Safir.Dob.Injected_Entity_Proxy_Impl_Pointers.Smart_Pointer)
     return Injected_Entity_Proxy;

private

   type Injected_Entity_Proxy is tagged record
      Impl_Ptr : Safir.Dob.Injected_Entity_Proxy_Impl_Pointers.Smart_Pointer;
   end record;

end Safir.Dob.Injected_Entity_Proxies;
