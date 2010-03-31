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
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Safir.Dob.Typesystem.Object;
pragma Warnings ("H");  -- turn off warnings for hiding variable

-- Functions for serializing objects to binary and XML forms.
--
package Safir.Dob.Typesystem.Serialization is

   -- Serialize an object to XML.
   --
   -- Parameters: Object - The object to serialize.
   -- Returns: String containing the xml-serialized version of the object.
   -- Exceptions Illegal_Value_Exception - There is something wrong with the object.
   --
   function To_Xml
     (Object : Safir.Dob.Typesystem.Object.Smart_Pointer'Class)
      return Unbounded_Wide_String;

   -- Deserialize an XML serialization.
   --
   -- Creates a new object from a given xml serialization.
   --
   -- Parameters: Xml - The xml to convert.
   -- Returns: A smart pointer to the new object.
   -- Exceptions: Illegal_Value_Exception - If there is something wrong with the XML
   --                                       or if the type represented by the serialization
   --                                       isn't found in the ObjectFactory.
   function To_Object (Xml : in Unbounded_Wide_String) return
     Safir.Dob.Typesystem.Object.Smart_Pointer'Class;

   -- Convert a binary serialization to XML.
   --
   -- Parameters: Binary - The binary serialization to convert to xml.
   -- Returns: The xml of the binary serialization.
   --
   function To_Xml (Binary : in Safir.Dob.Typesystem.Binary_Vectors.Vector)
                    return Unbounded_Wide_String;

   -- Convert a blob to XML.
   --
   -- Parameters: Blob - The blob to convert to xml.
   -- Returns: The xml of the blob.
   --
   function To_Xml
     (Blob : Safir.Dob.Typesystem.Blob_T) return Unbounded_Wide_String;

   -- Serialize an object to binary form.
   --
   -- Parameters: Object - The object to serialize.
   --             Binary - The destination of the serialization.
   --
   procedure To_Binary (Object : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                        Binary : out Safir.Dob.Typesystem.Binary_Vectors.Vector);


   -- Deserialize a binary serialization and create an object.
   --
   -- Parameters: Binary - The binary serialization to deserialize.
   -- Returns: A smart pointer to the new object.
   -- Exceptions: Illegal_Value_Exception - The type represented by the serialization
   --                                       isn't found in the object factory.
   --
   function To_Object (Binary : in Safir.Dob.Typesystem.Binary_Vectors.Vector)
     return Safir.Dob.Typesystem.Object.Smart_Pointer'Class;

end Safir.Dob.Typesystem.Serialization;
