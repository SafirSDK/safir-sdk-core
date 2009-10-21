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
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Entity;

package Safir.Dob.Injected_Entity_Proxy_Impls is

   type Injected_Entity_Proxy_Impl is limited private;

   type Injected_Entity_Proxy_Impl_Access is access all Injected_Entity_Proxy_Impl;

   function Create (Injection_Blob         : in Safir.Dob.Typesystem.Blob_T;
                    Injection_State        : in Safir.Dob.Typesystem.Char_Star;
                    Current_Blob           : in Safir.Dob.Typesystem.Blob_T;
                    Current_State          : in Safir.Dob.Typesystem.Char_Star)
                    return Injected_Entity_Proxy_Impl_Access;

   function Get_Type_Id (Self : in Injected_Entity_Proxy_Impl)
                         return Safir.Dob.Typesystem.Type_Id;

   function Get_Instance_Id (Self : in Injected_Entity_Proxy_Impl)
                              return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;

   function Get_Entity_Id (Self : in Injected_Entity_Proxy_Impl)
                         return Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;

   function Get_Injection (Self : in Injected_Entity_Proxy_Impl)
                        return Safir.Dob.Entity.Smart_Pointer'Class;

   function Get_Injection_Blob (Self : in Injected_Entity_Proxy_Impl)
                                return Safir.Dob.Typesystem.Blob_T;

   function Get_Current (Self : in Injected_Entity_Proxy_Impl)
                        return Safir.Dob.Entity.Smart_Pointer'Class;

private

   type Injected_Entity_Proxy_Impl is record
      Injection_Blob   : Safir.Dob.Typesystem.Blob_T;
      Injection_State  : Safir.Dob.Typesystem.Char_Star;
      Current_Blob     : Safir.Dob.Typesystem.Blob_T;
      Current_State    : Safir.Dob.Typesystem.Char_Star;
   end record;

end Safir.Dob.Injected_Entity_Proxy_Impls;
