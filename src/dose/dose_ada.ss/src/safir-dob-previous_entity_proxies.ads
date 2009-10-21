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
with Safir.Dob.Previous_Entity_Proxy_Impl_Pointers;
with Safir.Dob.Typesystem;
with Safir.Dob.Entity;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Typesystem.Handler_Id;
with Safir.Dob.Connection_Info;

package Safir.Dob.Previous_Entity_Proxies is

   -- Note: The type is tagged only to enable prefix notation. You are not
   --       supposed to inherit from this type!
   type Previous_Entity_Proxy is tagged private;

   -- AWI:todo comment
   function Get_Type_Id (Self : in Previous_Entity_Proxy)
                         return Safir.Dob.Typesystem.Type_Id;

   -- AWI:todo comment
   function Get_Instance_Id (Self : in Previous_Entity_Proxy)
                              return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
   -- AWI:todo comment
   function Get_Entity_Id (Self : in Previous_Entity_Proxy)
                         return Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;

   -- Retrieves a smart pointer to the entity.
   --
   -- No change flags will be set in the returned entity.
   --
   function Get_Entity (Self : in Previous_Entity_Proxy)
                        return Safir.Dob.Entity.Smart_Pointer'Class;

   -- AWI:todo comment
   function Get_Entity_With_Change_Info (Self : in Previous_Entity_Proxy)
                                         return Safir.Dob.Entity.Smart_Pointer'Class;

   -- AWI:todo comment
   function Get_Owner (Self : in Previous_Entity_Proxy)
                       return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

   -- AWI:todo comment
   function Get_Owner_Connection_Info (Self : in Previous_Entity_Proxy)
                                       return Safir.Dob.Connection_Info.Smart_Pointer;

   -- AWI:todo comment
   function Get_Blob (Self : in Previous_Entity_Proxy)
                      return Safir.Dob.Typesystem.Blob_T;

   -- AWI:todo comment
   function Get_Blob_With_Change_Info (Self : in Previous_Entity_Proxy)
                                       return Safir.Dob.Typesystem.Blob_T;

   -- AWI:todo comment
   function Get_Owner_With_String_Representation (Self : in Previous_Entity_Proxy)
                                                  return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;

   -- AWI:todo comment
   function Get_Timestamp (Self : in Previous_Entity_Proxy)
                           return Safir.Dob.Typesystem.Int_64;

   -- AWI:todo comment
   function Get_Timestamp (Self : in Previous_Entity_Proxy;
                           Member : in Safir.Dob.Typesystem.Member_Index)
                           return Safir.Dob.Typesystem.Int_64;

   -- For internal usage only!
   function Create (Proxy_Impl_Ptr : in Safir.Dob.Previous_Entity_Proxy_Impl_Pointers.Smart_Pointer)
     return Previous_Entity_Proxy;

private

   type Previous_Entity_Proxy is tagged record
      Impl_Ptr : Safir.Dob.Previous_Entity_Proxy_Impl_Pointers.Smart_Pointer;
   end record;

end Safir.Dob.Previous_Entity_Proxies;
