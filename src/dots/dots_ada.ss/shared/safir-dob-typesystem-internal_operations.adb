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
with Ada.Text_IO; use Ada.Text_IO;
with Safir.Dob.Typesystem.Kernel;
with Safir.Dob.Typesystem.Members;
with Safir.Dob.Typesystem.Container_Base;
with Safir.Dob.Typesystem.Object_Container_Base;
pragma Warnings ("D");  -- turn off warnings for implicit dereference

package body Safir.Dob.Typesystem.Internal_Operations is

   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String := "")
                       renames  Ada.Exceptions.Raise_Exception;

   procedure Format_Blob (Blob                : in Safir.Dob.Typesystem.Blob_T;
                          Blob_Size           : in Safir.Dob.Typesystem.Int_32;
                          Type_Id             : in Safir.Dob.Typesystem.Type_Id;
                          Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T) is
   begin
      Safir.Dob.Typesystem.Kernel.Format_Blob
        (Blob, Blob_Size, Type_Id, Beginning_Of_Unused);
   end Format_Blob;

   procedure Merge_Changes
     (Into : in out  Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
      From : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class) is

      use type Safir.Dob.Typesystem.Object.Object_Class_Access;
      use Safir.Dob.Typesystem.Container_Base;
      use Safir.Dob.Typesystem.Object_Container_Base;

      Into_Ptr : constant Safir.Dob.Typesystem.Object.Object_Class_Access := Into.Ref;
      From_Ptr : constant Safir.Dob.Typesystem.Object.Object_Class_Access := From.Ref;

   begin
      if From_Ptr = null or Into_Ptr = null then
         Throw (Software_Violation_Exception'Identity,
                "Objects must not be null in call to MergeChanges");
      end if;

      if From_Ptr.Get_Type_Id /= Into_Ptr.Get_Type_Id then
         Throw (Software_Violation_Exception'Identity,
                "Objects must have same TypeId for MergeChanges");
      end if;

      declare
         Num_Members : constant Safir.Dob.Typesystem.Member_Index :=
                         Safir.Dob.Typesystem.Members.Get_Number_Of_Members (Into_Ptr.Get_Type_Id);
         Member : Safir.Dob.Typesystem.Member_Index := 0;

      begin
         while Member < Num_Members loop
            declare
               Array_Size : constant Safir.Dob.Typesystem.Array_Index :=
                              Safir.Dob.Typesystem.Members.Get_Array_Size (Into_Ptr.Get_Type_Id, Member);
               Index      : Safir.Dob.Typesystem.Array_Index := 0;
               From_Container_Base_Ptr : Container_Base_Access;
               Into_Container_Base_Ptr : Container_Base_Access;
               From_Container_Obj_Base_Ptr : Object_Container_Base_Access;
               Into_Container_Obj_Base_Ptr : Object_Container_Base_Access;
               From_Object_Ptr             : Safir.Dob.Typesystem.Object.Object_Class_Access;
               Into_Object_Ptr             : Safir.Dob.Typesystem.Object.Object_Class_Access;
            begin
               while Index < Array_Size loop
                  From_Container_Base_Ptr := From_Ptr.Get_Member (Member, Index);
                  Into_Container_Base_Ptr := Into_Ptr.Get_Member (Member, Index);

                  -- Is it an object member?

                  if From_Container_Base_Ptr.all in Object_Container_Base_Type'Class then
                     From_Container_Obj_Base_Ptr :=
                       Object_Container_Base_Access (From_Container_Base_Ptr);
                     Into_Container_Obj_Base_Ptr :=
                       Object_Container_Base_Access (Into_Container_Base_Ptr);

                     if From_Container_Obj_Base_Ptr.Is_Changed_Here then
                        -- This specific memeber has changed.
                        if From_Container_Obj_Base_Ptr.Is_Null then
                           Into_Container_Obj_Base_Ptr.Set_Null;
                        else
                           From_Object_Ptr := From_Container_Obj_Base_Ptr.Get_Object_Pointer.Ref;
                           Into_Container_Obj_Base_Ptr.Set_Ptr (From_Object_Ptr.Clone);
                           Into_Container_Obj_Base_Ptr.Set_Changed_Here (True);
                        end if;

                     elsif From_Container_Obj_Base_Ptr.Is_Changed then
                        -- Some child has changed, we need to recurse ...
                        From_Object_Ptr := From_Container_Obj_Base_Ptr.Get_Object_Pointer.Ref;
                        Into_Object_Ptr := Into_Container_Obj_Base_Ptr.Get_Object_Pointer.Ref;
                        if Into_Container_Obj_Base_Ptr.Is_Null or else
                          Into_Object_Ptr.Get_Type_Id /= Into_Object_Ptr.Get_Type_Id
                        then
                           -- ... unless the type has changed or the into-member is null.
                           if not Into_Container_Obj_Base_Ptr.Is_Null then
                              Put_Line ("Warning (Contact a DOB developer if you do not understand it):");
                              Put_Line ("he type of a member has changed without the change flag being set in 'from'.");
                           end if;

                           -- if it was null we don't warn (even if it is a little bit suspicious to do that...)
                           -- AWI: Varför inte använda virtuella container-copy här?
                           Into_Container_Obj_Base_Ptr.Set_Ptr (From_Object_Ptr.Clone);
                           Into_Container_Obj_Base_Ptr.Set_Changed_Here (True);
                        else
                           -- recurse
                           Merge_Changes (Into_Container_Obj_Base_Ptr.Get_Object_Pointer.all,
                                          Into_Container_Obj_Base_Ptr.Get_Object_Pointer.all);
                        end if;

                     end if;

                  else
                     -- a normal member
                     if From_Container_Base_Ptr.Is_Changed then
                        Into_Container_Base_Ptr.Copy (From_Container_Base_Ptr.all);
                     end if;
                  end if;

                  Index := Index + 1;
               end loop;
            end;

            Member := Member + 1;
         end loop;
      end;
   exception
      when Constraint_Error =>
         Throw (Constraint_Error'Identity,
                "Type conversion failed inside Merge_Changes");
   end Merge_Changes;
end Safir.Dob.Typesystem.Internal_Operations;
