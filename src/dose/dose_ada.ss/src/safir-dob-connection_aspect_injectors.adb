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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Safir.Dob.Typesystem.Library_Exceptions;
with Safir.Dob.Typesystem.Internal_Operations;
with Safir.Dob.Interf;
with Interfaces.C.Strings;
pragma Warnings ("D");  -- turn off warnings for implicit dereference

package body Safir.Dob.Connection_Aspect_Injectors is

   package C renames Interfaces.C;

   function C_Malloc (Size : C.size_t) return Safir.Dob.Typesystem.Char_Star;
   pragma Import (C, C_Malloc, "malloc");

   procedure C_Free (X : in out Safir.Dob.Typesystem.Char_Star) is
      procedure Free (X : Safir.Dob.Typesystem.Char_Star);
      pragma Import (C, Free, "free");
   begin
      Free (X);
      X := null;
   end C_Free;

   function Create
     (Connection_Base : in  Safir.Dob.Connection_Bases.Connection_Base'Class) return Connection_Aspect_Injector is

      Aspect : Connection_Aspect_Injector;
   begin
      Aspect.Controller_Id := Connection_Base.Get_Controller_Id;
      return Aspect;
   end Create;

   procedure Inject_Changes
     (Self        : in Connection_Aspect_Injector;
      Entity      : in Safir.Dob.Entity.Smart_Pointer'Class;
      Instance_Id : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Timestamp   : in Safir.Dob.Typesystem.Int_64;
      Handler_Id  : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is

      use type Safir.Dob.Entity.Entity_Class_Access;
      use Safir.Dob.Typesystem.Instance_Id;
      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Blob : Safir.Dob.Typesystem.Blob_T;
      Blob_Size : Safir.Dob.Typesystem.Int_32;
      Beginning_Of_Unused : Safir.Dob.Typesystem.Blob_T;
      Entity_Ptr : constant Safir.Dob.Entity.Entity_Class_Access := Entity.Ref;
      Instance_Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Handler_Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      Blob_Size := Entity_Ptr.Calculate_Blob_Size;

      Blob := C_Malloc (C.size_t (Blob_Size));

      Safir.Dob.Typesystem.Internal_Operations.Format_Blob (Blob,
                                                            Blob_Size,
                                                            Entity_Ptr.Get_Type_Id,
                                                            Beginning_Of_Unused);
      Entity_Ptr.Write_To_Blob (Blob, Beginning_Of_Unused);


      Instance_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Instance_Id)));
      Handler_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Inject_Entity
        (Self.Controller_Id,
         Blob,
         Get_Raw_Value (Instance_Id),
         Instance_Utf_8_Str_Ptr,
         Get_Raw_Value (Handler_Id),
         Handler_Utf_8_Str_Ptr,
         Timestamp,
         Success);

      C_Free (Blob);
      C.Strings.Free (Instance_Utf_8_Str_Ptr);
      C.Strings.Free (Handler_Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Inject_Changes;

   procedure Inject_Delete
     (Self        : in Connection_Aspect_Injector;
      Entity_Id   : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
      Timestamp   : in Safir.Dob.Typesystem.Int_64;
      Handler_Id  : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is

      use Safir.Dob.Typesystem.Instance_Id;
      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Instance_Id : Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Instance_Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Handler_Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      Instance_Id := Safir.Dob.Typesystem.Entity_Id.Get_Instance_Id (Entity_Id);
      Instance_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Instance_Id)));
      Handler_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Inject_Deleted_Entity
        (Self.Controller_Id,
         Safir.Dob.Typesystem.Entity_Id.Get_Type_Id (Entity_Id),
         Get_Raw_Value (Instance_Id),
         Instance_Utf_8_Str_Ptr,
         Get_Raw_Value (Handler_Id),
         Handler_Utf_8_Str_Ptr,
         Timestamp,
         Success);

      C.Strings.Free (Instance_Utf_8_Str_Ptr);
      C.Strings.Free (Handler_Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Inject_Delete;

   procedure Initial_Set
     (Self        : in Connection_Aspect_Injector;
      Entity      : in Safir.Dob.Entity.Smart_Pointer'Class;
      Instance_Id : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Handler_Id  : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is

      use type Safir.Dob.Entity.Entity_Class_Access;
      use Safir.Dob.Typesystem.Instance_Id;
      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Blob : Safir.Dob.Typesystem.Blob_T;
      Blob_Size : Safir.Dob.Typesystem.Int_32;
      Beginning_Of_Unused : Safir.Dob.Typesystem.Blob_T;
      Entity_Ptr : constant Safir.Dob.Entity.Entity_Class_Access := Entity.Ref;
      Instance_Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Handler_Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      Blob_Size := Entity_Ptr.Calculate_Blob_Size;

      Blob := C_Malloc (C.size_t (Blob_Size));

      Safir.Dob.Typesystem.Internal_Operations.Format_Blob (Blob,
                                                            Blob_Size,
                                                            Entity_Ptr.Get_Type_Id,
                                                            Beginning_Of_Unused);
      Entity_Ptr.Write_To_Blob (Blob, Beginning_Of_Unused);


      Instance_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Instance_Id)));
      Handler_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Set_Entity
        (Self.Controller_Id,
         Blob,
         Get_Raw_Value (Instance_Id),
         Instance_Utf_8_Str_Ptr,
         Get_Raw_Value (Handler_Id),
         Handler_Utf_8_Str_Ptr,
         C.char'Val (Boolean'Pos (False)),  -- false => don't consider change flags
         C.char'Val (Boolean'Pos (True)),   -- true => this is an initial injection
         Success);

      C_Free (Blob);
      C.Strings.Free (Instance_Utf_8_Str_Ptr);
      C.Strings.Free (Handler_Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Initial_Set;

   procedure Subscribe_Entity
     (Self                                 : in Connection_Aspect_Injector;
      Type_Id                              : in Safir.Dob.Typesystem.Type_Id;
      Include_Updates                      : in Boolean;
      Include_Subclasses                   : in Boolean;
      Restart_Subscription                 : in Boolean;
      Wants_Ghost_Delete                   : in Boolean;
      Wants_Last_State                     : in Boolean;
      Doesnt_Want_Source_Is_PermanentStore : in Boolean;
      Wants_All_State_Changes              : in Boolean;
      Timestamp_Change_Info                : in Boolean;
      Entity_Subscriber                    : access Safir.Dob.Consumers.Entity_Subscriber'Class) is

      Success : C.char;
   begin
      Safir.Dob.Interf.Injector_Subscribe_Entity
        (Self.Controller_Id,
         Type_Id,
         C.char'Val (Boolean'Pos (Include_Updates)),
         C.char'Val (Boolean'Pos (Include_Subclasses)),
         C.char'Val (Boolean'Pos (Restart_Subscription)),
         C.char'Val (Boolean'Pos (Wants_Ghost_Delete)),
         C.char'Val (Boolean'Pos (Wants_Last_State)),
         C.char'Val (Boolean'Pos (Doesnt_Want_Source_Is_PermanentStore)),
         C.char'Val (Boolean'Pos (Wants_All_State_Changes)),
         C.char'Val (Boolean'Pos (Timestamp_Change_Info)),
         Safir.Dob.Interf.Language_Ada,
         Entity_Subscriber,
         Success);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Subscribe_Entity;


end Safir.Dob.Connection_Aspect_Injectors;
