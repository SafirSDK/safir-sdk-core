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
with GNAT.OS_Lib;
with Text_IO; use Text_IO;
with Ada.Exceptions;
with Interfaces.C;
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Internal_Operations;
with Safir.Dob.Typesystem.Library_Exceptions;
with Safir.Dob.Interf;
with Safir.Dob.This_Node_Parameters;

pragma Warnings ("D");  -- turn off warnings for implicit dereference

package body Safir.Dob.Response_Sender_Impls is

   package C renames Interfaces.C;

   procedure Throw (E : Ada.Exceptions.Exception_Id; Msg : String := "")
                    renames  Ada.Exceptions.Raise_Exception;

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
     (Controller_Id : in Safir.Dob.Defs.Controller_Id;
      Consumer      : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Response_Id   : in Safir.Dob.Defs.Response_Id)
      return Response_Sender_Impl_Access is

      Impl_Ptr : Response_Sender_Impl_Access;
   begin
      Impl_Ptr := new Response_Sender_Impl;
      Impl_Ptr.all.Is_Valid := True;
      Impl_Ptr.all.Controller_Id := Controller_Id;
      Impl_Ptr.all.Consumer := Consumer;
      Impl_Ptr.all.Response_Id := Response_Id;
      return Impl_Ptr;
   end Create;

   overriding
   procedure Finalize (Self : in out Response_Sender_Impl) is
   begin
      if Self.Is_Valid then
         Put_Line ("A ResponseSender was destroyed without having been used!");
         Put_Line ("This is usually due to a programming error in the application:");
         Put_Line ("Either the application did not send a response to a request, or ");
         Put_Line ("an exception was thrown inside a Service or Entity request callback, ");
         Put_Line ("causing the Response_Sender to be destroyed prematurely.");
         Put_Line ("In either case the application needs to be fixed and the system restarted");
         New_Line;
         Put_Line ("NodeNumber = " &
                   Safir.Dob.Typesystem.Int_32'Image (Safir.Dob.This_Node_Parameters.Node_Number));
         GNAT.OS_Lib.OS_Exit (101010);
      end if;
   end Finalize;

   procedure Send (Self     : in out Response_Sender_Impl;
                   Response : in Safir.Dob.Response.Smart_Pointer'Class) is

      Response_Ptr : constant Safir.Dob.Response.Response_Class_Access := Response.Ref;
      Blob_Size : constant Safir.Dob.Typesystem.Int_32 :=
                    Response_Ptr.Calculate_Blob_Size;
      Blob : Safir.Dob.Typesystem.Blob_T;
      Beginning_Of_Unused : Safir.Dob.Typesystem.Blob_T;
      Success : C.char;
   begin

      if not Self.Is_Valid then
         Throw (Safir.Dob.Typesystem.Software_Violation_Exception'Identity,
                "Response Sender object has already been used once.");
      end if;

      Blob := C_Malloc (C.size_t (Blob_Size));

      Safir.Dob.Typesystem.Internal_Operations.Format_Blob (Blob,
                                                            Blob_Size,
                                                            Response_Ptr.Get_Type_Id,
                                                            Beginning_Of_Unused);

      Response_Ptr.Write_To_Blob (Blob, Beginning_Of_Unused);

      Safir.Dob.Interf.Send_Response (Self.Controller_Id,
                                      Blob,
                                      Self.Consumer,
                                      Safir.Dob.Interf.Language_Ada,
                                      Self.Response_Id,
                                      Success);

      C_Free (Blob);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
      Self.Is_Valid := False;
   end Send;

   function Is_Done (Self : in Response_Sender_Impl) return Boolean is
   begin
      return not Self.Is_Valid;
   end Is_Done;


   procedure Discard (Self : in out Response_Sender_Impl) is
   begin
      if not Self.Is_Valid then
         Throw (Safir.Dob.Typesystem.Software_Violation_Exception'Identity,
                "Response_Sender object has already been used once.");
      end if;
      Self.Is_Valid := False;
   end Discard;

end Safir.Dob.Response_Sender_Impls;
