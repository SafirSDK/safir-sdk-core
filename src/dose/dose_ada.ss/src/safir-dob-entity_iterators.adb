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
with Ada.Exceptions;
with Safir.Dob.Typesystem.Library_Exceptions;
with Safir.Dob.Interf;
with Safir.Dob.Entity_Proxy_Impl_Pointers;
with Safir.Dob.Entity_Proxy_Impls;
with Interfaces.C;

package body Safir.Dob.Entity_Iterators is

   package C renames Interfaces.C;

   procedure Throw (E : Ada.Exceptions.Exception_Id; Msg : String := "")
                    renames  Ada.Exceptions.Raise_Exception;

   function Create (Controller_Id      : in Safir.Dob.Defs.Controller_Id;
                    Type_Id            : in Safir.Dob.Typesystem.Type_Id;
                    Include_Subclasses : in Boolean) return Entity_Iterator is
      It : Entity_Iterator;
      End_It : Entity_Iterator;
      Success : C.char;
      Is_End_It  : C.char;
   begin
      It.Controller_Id := Controller_Id;

      Safir.Dob.Interf.Entity_Iterator_Create
        (Controller_Id,
         Type_Id,
         C.char'Val (Boolean'Pos (Include_Subclasses)),
         It.Iterator_Id,
         Is_End_It,
         Success);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      if C.char'Pos (Is_End_It) /= 0 then
         -- The assignment will make Finalize for the just created end iterator
         -- to be run, which is excatly what we want.
         It := End_It;
         return It;
      end if;

      return It;
   end Create;

   function Done (Self : in Entity_Iterator) return Boolean is
   begin
      return Self.Iterator_Id = -1;
   end Done;

   procedure Next (Self : in out Entity_Iterator) is
      Success : C.char;
      Is_End_It  : C.char;
      End_It : Entity_Iterator;
   begin
      if Self.Iterator_Id = -1 then
         Throw (Safir.Dob.Typesystem.Software_Violation_Exception'Identity,
                "Cannot increment a 'done' iterator");
      end if;

      Safir.Dob.Interf.Entity_Iterator_Increment
        (Self.Controller_Id,
         Self.Iterator_Id,
         Is_End_It,
         Success);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      if C.char'Pos (Is_End_It) /= 0 then
         -- The assignment will make Finalize for Self
         -- to be run, which is excatly what we want.
         Self := End_It;
      end if;
   end Next;

   function Get_Entity_Proxy (Self : in Entity_Iterator)
                              return Safir.Dob.Entity_Proxies.Entity_Proxy is
      Success : C.char;
      Entity_Blob : Safir.Dob.Typesystem.Blob_T;
      Entity_State : Safir.Dob.Typesystem.Char_Star;
      Entity_Proxy_Impl_Ptr : Safir.Dob.Entity_Proxy_Impl_Pointers.Smart_Pointer;
   begin
      if Self.Iterator_Id = -1 then
         Throw (Safir.Dob.Typesystem.Software_Violation_Exception'Identity,
                "This iterator appears to be at 'end', so dont try to dereference it!");
      end if;

      Safir.Dob.Interf.Entity_Iterator_Dereference
        (Self.Controller_Id,
         Self.Iterator_Id,
         Entity_Blob,
         Entity_State,
         Success);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      Entity_Proxy_Impl_Ptr := Safir.Dob.Entity_Proxy_Impl_Pointers.Create
        (Safir.Dob.Entity_Proxy_Impls.Create (Current_Blob => Entity_Blob,
                                              Current_State  => Entity_State,
                                              Previous_Blob  => null,
                                              Previous_State => null,
                                              Add_Reference  => True,
                                              Timestamp_Diff => False));

      -- Set the max number of Adjusts (copy operations) that is allowed. This is to ensure
      -- that the user can't hold any refrences to shared memory by copying
      -- the receivied proxy. Currently I (AWI) am not sure if the number of Adjusts
      -- on the way out of this function is implementation dependant. This have to be
      -- checked.
      Entity_Proxy_Impl_Ptr.Set_Adjust_Limitation (3);

      return Safir.Dob.Entity_Proxies.Create (Entity_Proxy_Impl_Ptr);

   end Get_Entity_Proxy;


   function "=" (L, R : Entity_Iterator) return Boolean is
      Success : C.char;
      Equal   : C.char;
   begin
      if L.Iterator_Id = -1 and R.Iterator_Id = -1 then
         return True;
      end if;
      if L.Iterator_Id = -1 or R.Iterator_Id = -1 then
         return False;
      end if;

      if L.Controller_Id /= R.Controller_Id then
         Throw (Safir.Dob.Typesystem.Software_Violation_Exception'Identity,
                "Cannot compare iterators from different connections!");
      end if;

      Safir.Dob.Interf.Entity_Iterator_Equal
        (L.Controller_Id,
         L.Iterator_Id,
         R.Iterator_Id,
         Equal,
         Success);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      return C.char'Pos (Equal) /= 0;

   end "=";

   overriding
   procedure Finalize (Self : in out Entity_Iterator) is
   begin
      if Self.Iterator_Id /= -1 then
         Safir.Dob.Interf.Entity_Iterator_Destroy (Self.Controller_Id,
                                                   Self.Iterator_Id);
         Self.Iterator_Id := -1;
      end if;
   end Finalize;

   overriding
   procedure Adjust (Self : in out Entity_Iterator) is
      Success : C.char;
   begin
      if Self.Iterator_Id /= -1 then
         Safir.Dob.Interf.Entity_Iterator_Copy
           (Self.Controller_Id,
            Self.Iterator_Id,
            Self.Iterator_Id,
            Success);

         if C.char'Pos (Success) = 0 then
            Safir.Dob.Typesystem.Library_Exceptions.Throw;
         end if;

      end if;

   end Adjust;



end Safir.Dob.Entity_Iterators;
