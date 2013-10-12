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
with Safir.Dob.Typesystem.Blob_Operations;
with Interfaces.C;
with Safir.Dob.Typesystem.Library_Exceptions;
with Safir.Dob.Typesystem.Object.Factory;
with Safir.Dob.Interf;

package body Safir.Dob.Injected_Entity_Proxy_Impls is

   package C renames Interfaces.C;

   function Create (Injection_Blob         : in Safir.Dob.Typesystem.Blob_T;
                    Injection_State        : in Safir.Dob.Typesystem.Char_Star;
                    Current_Blob           : in Safir.Dob.Typesystem.Blob_T;
                    Current_State          : in Safir.Dob.Typesystem.Char_Star)
                    return Injected_Entity_Proxy_Impl_Access is
      Impl_Ptr : Injected_Entity_Proxy_Impl_Access;
   begin
      Impl_Ptr := new Injected_Entity_Proxy_Impl;
      Impl_Ptr.all.Injection_Blob := Injection_Blob;
      Impl_Ptr.all.Injection_State := Injection_State;
      Impl_Ptr.all.Current_Blob := Current_Blob;
      Impl_Ptr.all.Current_State := Current_State;
      return Impl_Ptr;
   end Create;

   function Get_Type_Id (Self : in Injected_Entity_Proxy_Impl)
                         return Safir.Dob.Typesystem.Type_Id is

      use type Safir.Dob.Typesystem.Blob_T;

      Type_Id : Safir.Dob.Typesystem.Type_Id;
      Success : C.char;
   begin
      if Self.Injection_Blob = null then
         Safir.Dob.Interf.Get_Type_Id (Self.Injection_State,
                                       Type_Id,
                                       Success);
         if C.char'Pos (Success) = 0 then
            Safir.Dob.Typesystem.Library_Exceptions.Throw;
         end if;
         return Type_Id;
      else
         return Safir.Dob.Typesystem.Blob_Operations.Get_Type_Id (Self.Injection_Blob);
      end if;
   end Get_Type_Id;

   function Get_Instance_Id (Self : in Injected_Entity_Proxy_Impl)
                             return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type is
      Instance_Id : Safir.Dob.Typesystem.Int_64;
      Success : C.char;
   begin
      Safir.Dob.Interf.Get_Instance_Id (Self.Injection_State,
                                        Instance_Id,
                                        Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
      return Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id (Instance_Id);
   end Get_Instance_Id;

   function Get_Entity_Id (Self : in Injected_Entity_Proxy_Impl)
                           return Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type is

   begin
      return Safir.Dob.Typesystem.Entity_Id.Create_Entity_Id
        (Get_Type_Id (Self), Get_Instance_Id (Self));
   end Get_Entity_Id;

   function Get_Injection (Self : in Injected_Entity_Proxy_Impl)
                        return Safir.Dob.Entity.Smart_Pointer'Class is
      use type Safir.Dob.Typesystem.Blob_T;
   begin
      pragma Assert (Self.Injection_Blob /= null, "Not possible to do Get_Injection on InjectDeletes!");

      return Safir.Dob.Entity.Smart_Pointer'Class
        (Safir.Dob.Typesystem.Object.Factory.Create_Object (Self.Injection_Blob));

   end Get_Injection;

   function Get_Injection_Blob (Self : in Injected_Entity_Proxy_Impl) return
     Safir.Dob.Typesystem.Blob_T is
      use type Safir.Dob.Typesystem.Blob_T;
   begin
      pragma Assert (Self.Injection_Blob /= null, "No blob available on InjectDeletes!");
      return Self.Injection_Blob;
   end Get_Injection_Blob;

   function Get_Current (Self : in Injected_Entity_Proxy_Impl)
                        return Safir.Dob.Entity.Smart_Pointer'Class is
      use type Safir.Dob.Typesystem.Blob_T;
   begin
      pragma Assert (Self.Current_Blob /= null, "Not possible to do Get_Current on InjectNews!");

      return Safir.Dob.Entity.Smart_Pointer'Class
        (Safir.Dob.Typesystem.Object.Factory.Create_Object (Self.Current_Blob));

   end Get_Current;

end Safir.Dob.Injected_Entity_Proxy_Impls;
