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
with Ada.Exceptions;
with Safir.Dob.Typesystem.Blob_Operations;
with Safir.Dob.Interf;
with Safir.Dob.Typesystem.Library_Exceptions;
with Safir.Dob.Typesystem.Utilities; use Safir.Dob.Typesystem.Utilities;
with Safir.Dob.Typesystem.Object.Factory;
with Safir.Dob.Previous_Entity_Proxy_Impls;
with Safir.Dob.Previous_Entity_Proxy_Impl_Pointers;
with Interfaces.C;
with System.Address_To_Access_Conversions;
pragma Warnings ("D");  -- turn off warnings for implicit dereference

package body Safir.Dob.Entity_Proxy_Impls is

   package Conv is new System.Address_To_Access_Conversions (Entity_Proxy_Impl);

   package C renames Interfaces.C;

   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String := "")
                    renames  Ada.Exceptions.Raise_Exception;

   function Create (Current_Blob          : in Safir.Dob.Typesystem.Blob_T;
                    Current_State         : in Safir.Dob.Typesystem.Char_Star;
                    Previous_Blob         : in Safir.Dob.Typesystem.Blob_T;
                    Previous_State        : in Safir.Dob.Typesystem.Char_Star;
                    Add_Reference         : in Boolean;
                    Timestamp_Diff        : in Boolean)
                    return Entity_Proxy_Impl_Access is
      Impl_Ptr : Entity_Proxy_Impl_Access;
   begin
      Impl_Ptr := new Entity_Proxy_Impl;
      Impl_Ptr.all.Current_Blob := Current_Blob;
      Impl_Ptr.all.Current_State :=
        Safir.Dob.Shm_References.Create (Ptr => Current_State,
                                         Deleter => Safir.Dob.Interf.Drop_Reference'Access);
      Impl_Ptr.all.Previous_Blob := Previous_Blob;
      Impl_Ptr.all.Previous_State :=
        Safir.Dob.Shm_References.Create (Ptr => Previous_State,
                                         Deleter => Safir.Dob.Interf.Drop_Reference'Access);
      Impl_Ptr.all.Timestamp_Diff := Timestamp_Diff;

      if Add_Reference then
         Safir.Dob.Interf.Add_Reference (Current_State);
         Safir.Dob.Interf.Add_Reference (Previous_State);
      end if;

      return Impl_Ptr;
   end Create;

   function Get_Type_Id (Self : in Entity_Proxy_Impl)
                         return Safir.Dob.Typesystem.Type_Id is

      use type Safir.Dob.Typesystem.Blob_T;

      Type_Id : Safir.Dob.Typesystem.Type_Id;
      Success : C.char;
   begin
      if Self.Current_Blob = null then
         Safir.Dob.Interf.Get_Type_Id (Self.Current_State.Get_Ptr,
                                       Type_Id,
                                       Success);
         if C.char'Pos (Success) = 0 then
            Safir.Dob.Typesystem.Library_Exceptions.Throw;
         end if;
         return Type_Id;
      else
         return Safir.Dob.Typesystem.Blob_Operations.Get_Type_Id (Self.Current_Blob);
      end if;
   end Get_Type_Id;

   function Get_Instance_Id (Self : in Entity_Proxy_Impl)
                             return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type is
      Instance_Id : Safir.Dob.Typesystem.Int_64;
      Success : C.char;
   begin
      Safir.Dob.Interf.Get_Instance_Id (Self.Current_State.Get_Ptr,
                                        Instance_Id,
                                        Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
      return Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id (Instance_Id);
   end Get_Instance_Id;

   function Get_Entity_Id (Self : in Entity_Proxy_Impl)
                           return Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type is

   begin
      return Safir.Dob.Typesystem.Entity_Id.Create_Entity_Id
        (Get_Type_Id (Self), Get_Instance_Id (Self));
   end Get_Entity_Id;

   function Get_Entity (Self : in Entity_Proxy_Impl)
                        return Safir.Dob.Entity.Smart_Pointer'Class is
      use type Safir.Dob.Typesystem.Blob_T;
   begin
      if Self.Current_Blob = null then
         Throw (Safir.Dob.Typesystem.Software_Violation_Exception'Identity,
                "Not possible to do Get_Entity on proxies from On_Deleted_Entity (entity = "
                & To_Utf_8 (Safir.Dob.Typesystem.Entity_Id.To_String (Get_Entity_Id (Self)))
                & ")!");
      end if;

      return Safir.Dob.Entity.Smart_Pointer'Class
        (Safir.Dob.Typesystem.Object.Factory.Create_Object (Self.Current_Blob));

   end Get_Entity;

   function Get_Entity_With_Change_Info (Self : in Entity_Proxy_Impl)
                                         return Safir.Dob.Entity.Smart_Pointer'Class is
      use type Safir.Dob.Typesystem.Blob_T;

      Self_Ptr : constant Conv.Object_Pointer := Conv.To_Pointer (Self'Address);

      Diff_Blob : Safir.Dob.Typesystem.Blob_T;
      Blob_Deleter : Safir.Dob.Blob_References.Blob_Deleter_Proc;
      Success : C.char;
   begin
      if Self.Current_Blob_With_Change_Info.Get_Ptr = null then
         Safir.Dob.Interf.Diff (Self.Previous_State.Get_Ptr,
                                Self.Current_State.Get_Ptr,
                                C.char'Val (Boolean'Pos (True)),  -- true => want current
                                C.char'Val (Boolean'Pos (Self.Timestamp_Diff)),
                                Diff_Blob,
                                Blob_Deleter,
                                Success);

         if C.char'Pos (Success) = 0 then
            Safir.Dob.Typesystem.Library_Exceptions.Throw;
         end if;

         Self_Ptr.Current_Blob_With_Change_Info :=
           Safir.Dob.Blob_References.Create (Diff_Blob, Blob_Deleter);
      end if;

      return Safir.Dob.Entity.Smart_Pointer'Class
        (Safir.Dob.Typesystem.Object.Factory.Create_Object (Self.Current_Blob_With_Change_Info.Get_Ptr));

   end Get_Entity_With_Change_Info;

   function Get_Owner (Self : in Entity_Proxy_Impl)
                       return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type is

      Handler_Id : Safir.Dob.Typesystem.Int_64;
      Success    : C.char;
   begin
      Safir.Dob.Interf.Get_Handler_Id (Self.Current_State.Get_Ptr, Handler_Id, Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
      return Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id (Handler_Id);
   end Get_Owner;

   function Get_Owner_Connection_Info (Self : in Entity_Proxy_Impl)
                                       return Safir.Dob.Connection_Info.Smart_Pointer is
      use type Safir.Dob.Typesystem.Blob_T;

      Blob : Safir.Dob.Typesystem.Blob_T;
      Blob_Deleter : Safir.Dob.Blob_References.Blob_Deleter_Proc;
      Connection_Info : Safir.Dob.Connection_Info.Smart_Pointer;
      Success : C.char;
   begin
      Safir.Dob.Interf.Get_Connection_Info (Self.Current_State.Get_Ptr, Blob, Blob_Deleter, Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      pragma Assert (Blob /= null, "Got NULL blob from DoseC_GetConnectionInfo");

      begin
         Connection_Info := Safir.Dob.Connection_Info.Smart_Pointer
           (Safir.Dob.Typesystem.Object.Factory.Create_Object (Blob));
         Blob_Deleter (Blob);
         return Connection_Info;
      exception
         when others =>
            Blob_Deleter (Blob);
            raise;
      end;
   end Get_Owner_Connection_Info;

   function Get_Blob (Self : in Entity_Proxy_Impl)
                      return Safir.Dob.Typesystem.Blob_T is
      use type Safir.Dob.Typesystem.Blob_T;
   begin
      if Self.Current_Blob = null then
         Throw (Safir.Dob.Typesystem.Software_Violation_Exception'Identity,
                "Not possible to do Get_Blob on proxies from OnDeletedEntity (entity = "
                & To_Utf_8 (Safir.Dob.Typesystem.Entity_Id.To_String (Get_Entity_Id (Self)))
                & ")!");
      end if;

      return Self.Current_Blob;
   end Get_Blob;

   function Get_Blob_With_Change_Info (Self : in Entity_Proxy_Impl)
                      return Safir.Dob.Typesystem.Blob_T is
      use type Safir.Dob.Typesystem.Blob_T;

      Self_Ptr : constant Conv.Object_Pointer := Conv.To_Pointer (Self'Address);

      Diff_Blob : Safir.Dob.Typesystem.Blob_T;
      Blob_Deleter : Safir.Dob.Blob_References.Blob_Deleter_Proc;
      Success : C.char;
   begin
      if Self.Current_Blob_With_Change_Info.Get_Ptr = null then
         Safir.Dob.Interf.Diff (Self.Previous_State.Get_Ptr,
                                Self.Current_State.Get_Ptr,
                                C.char'Val (Boolean'Pos (True)),  -- true => want current
                                C.char'Val (Boolean'Pos (Self.Timestamp_Diff)),
                                Diff_Blob,
                                Blob_Deleter,
                                Success);

         if C.char'Pos (Success) = 0 then
            Safir.Dob.Typesystem.Library_Exceptions.Throw;
         end if;

         Self_Ptr.Current_Blob_With_Change_Info :=
           Safir.Dob.Blob_References.Create (Diff_Blob, Blob_Deleter);

      end if;

      return Self.Current_Blob_With_Change_Info.Get_Ptr;
   end Get_Blob_With_Change_Info;

   function Get_Previous (Self : in Entity_Proxy_Impl)
                       return Safir.Dob.Previous_Entity_Proxies.Previous_Entity_Proxy is
   begin
      return Safir.Dob.Previous_Entity_Proxies.Create (Safir.Dob.Previous_Entity_Proxy_Impl_Pointers.Create
        (Safir.Dob.Previous_Entity_Proxy_Impls.Create
           (Self.Current_Blob,
            Self.Current_State,
            Self.Previous_Blob,
            Self.Previous_State,
            Self.Timestamp_Diff)));
   end Get_Previous;

   function Get_Owner_With_String_Representation (Self : in Entity_Proxy_Impl)
                                                  return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type is
   begin
      return Get_Owner (Self);
   end Get_Owner_With_String_Representation;

   function Get_Timestamp (Self : in Entity_Proxy_Impl)
                           return Safir.Dob.Typesystem.Int_64 is

      Timestamp : Safir.Dob.Typesystem.Int_64;
      Success : C.char;
   begin
      Safir.Dob.Interf.Get_Top_Timestamp (Self.Current_State.Get_Ptr,
                                          Timestamp,
                                          Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
      return Timestamp;
   end Get_Timestamp;

   function Get_Timestamp (Self   : in Entity_Proxy_Impl;
                           Member : in Safir.Dob.Typesystem.Member_Index)
                           return Safir.Dob.Typesystem.Int_64 is

      Timestamp : Safir.Dob.Typesystem.Int_64;
      Success : C.char;
   begin
      Safir.Dob.Interf.Get_Member_Timestamp (Self.Current_State.Get_Ptr,
                                             Member,
                                             Timestamp,
                                             Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
      return Timestamp;
   end Get_Timestamp;

end Safir.Dob.Entity_Proxy_Impls;
