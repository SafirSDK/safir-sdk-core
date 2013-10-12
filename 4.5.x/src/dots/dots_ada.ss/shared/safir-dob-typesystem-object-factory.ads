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
with Safir.Dob.Typesystem.Object; use Safir.Dob.Typesystem.Object;
pragma Warnings ("H");  -- turn off warnings for hiding variable

-- This package implements an object factory for all automatically generated
-- DOB classes.
-- Each generated class automatically registers itself with this package.
-- Users can call the Create_Object (Type_Id) subprogram to create objects
-- of a desired type. (this is if they receive the type id from some other
-- application so that they cannot call the Create routine of the class itself
-- directly).
--
package Safir.Dob.Typesystem.Object.Factory is

   -- Create a new "empty" object from a type id.
   --
   -- This method takes a type id and calls the appropriate callback to create
   -- an object of the desired type.
   --
   -- Parameters: Type_Id - The TypeId of the object to create.
   -- Returns: A smart pointer to the object.
   -- Exceptions: Illegal_Value_Exception - If the type couldn't be found
   --                                       in the object factory.
   --
   function Create_Object (Type_Id : in Safir.Dob.Typesystem.Type_Id)
                           return Object.Smart_Pointer'Class;

   -- ==================
   -- Registration part.
   -- Stuff for registering classes with the object factory.
   -- ==================

   -- This is the signature of the function that the object factory will call to create an object.
   -- If blob = null then just create an empty object.
   --
   type Create_Object_Callback is access
     function (Blob : in Safir.Dob.Typesystem.Blob_T) return Object.Smart_Pointer'Class;

   -- This is the signature of the function that the object factory will call to
   -- create an empty smart pointer
   --
   type Create_Smart_Ptr_Callback is access
     function return Object.Smart_Pointer'Class;

   -- Register a type with the object factory.
   --
   -- Parameters: Type_Id - The Type_Id of the object that should be created
   --                       using the Obj_Callback function.
   --             Obj_Callback - The function to call to create the object.
   --             Smart_Ptr_Tag - Tag of the smart pointer that should be created
   --                             using the Smart_Ptr_Callback function.
   --             Smart_Ptr_Callback - The function to call to create a smart pointer.
   --
   procedure Register_Type (Type_Id : in Safir.Dob.Typesystem.Type_Id;
                            Obj_Callback : in not null Create_Object_Callback;
                            Smart_Ptr_Callback : in not null Create_Smart_Ptr_Callback);


   -- ==========================
   -- Blob deserialization part.
   -- ==========================

   -- Create a new object from a blob.
   --
   -- This method takes a blob and extracts the type id from it and then calls the
   -- appropriate callback to create the object.
   --
   -- Parameters: Blob - The blob to deserialize.
   -- Returns: A smart pointer to the object.
   -- Exceptions: Illegal_Value_Exception - If the type represented by the blob
   --                                       isn't found in the object factory.
   --
   function Create_Object (Blob   : in Safir.Dob.Typesystem.Blob_T)
                           return Object.Smart_Pointer'Class;

   -- Create an empty smart pointer.
   function Create_Smart_Ptr (Type_Id : in Safir.Dob.Typesystem.Type_Id)
                                 return Object.Smart_Pointer'Class;



end Safir.Dob.Typesystem.Object.Factory;
