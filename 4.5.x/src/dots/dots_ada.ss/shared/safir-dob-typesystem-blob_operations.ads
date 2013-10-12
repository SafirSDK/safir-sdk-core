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
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Safir.Dob.Typesystem.Container_Instantiations; use Safir.Dob.Typesystem.Container_Instantiations;
with Safir.Dob.Typesystem.Enumeration_Container_Base; use Safir.Dob.Typesystem.Enumeration_Container_Base;
with Safir.Dob.Typesystem.String_Container; use Safir.Dob.Typesystem.String_Container;
with Safir.Dob.Typesystem.Binary_Container; use Safir.Dob.Typesystem.Binary_Container;
with Safir.Dob.Typesystem.Object_Container_Base; use Safir.Dob.Typesystem.Object_Container_Base;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Typesystem.Channel_Id;
with Safir.Dob.Typesystem.Handler_Id;

-- Operations on blobs.
--
-- Functions for getting information from blobs and setting and getting
-- values in blobs.
-- Currently this functionality is meant mainly for internal use, but
-- the functions can be used to modify binary serializations of objects.
-- But be careful if you do, and talk to your closest DOB expert first.
--
-- Note: Most of these methods have no checks on them to make sure that
-- everything has worked okay. They will just return unexpected values if
-- something went wrong.
--
package Safir.Dob.Typesystem.Blob_Operations is

   -- Extract the Type_Id from a blob.
   --
   -- Parameters: Blob - The blob to read from.
   -- Returns: The Type_Id from the blob.
   --
   function Get_Type_Id (Blob   : in Safir.Dob.Typesystem.Blob_T)
                         return Safir.Dob.Typesystem.Type_Id;


   -- Get the size of the blob contained by this object
   --
   -- Parameters: Blob - The blob
   -- Returns: The size of the blob, in bytes.
   --
   function Get_Size (Blob   : in Safir.Dob.Typesystem.Blob_T)
                     return Safir.Dob.Typesystem.Int_32;

   -- =========================================================================
   -- Value operations on blobs.
   -- =========================================================================

   -- Set a member to null.
   --
   -- This methods sets a given member (with index) to null in a blob.
   -- If the member is not an array the index must be 0.
   --
   -- Parameters: Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --
   procedure Set_Null (Blob   : in Safir.Dob.Typesystem.Blob_T;
                       Member : in Safir.Dob.Typesystem.Member_Index;
                       Index  : in Safir.Dob.Typesystem.Array_Index);

   -- Set a Boolean in a blob.
   --
   -- This procedure will set a Boolean member in a blob.
   -- If the Is_Null parameter is true then only the isChange and Is_Null flags
   -- are set in the blob, not the value (so it can be any value).
   --
   -- Parameters: Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value that the member is to be set to
   --                     (unless IsNull is True).
   --             Is_Null - Should the value be set to null.
   --             Is_Changed - Should the value be set to changed.
   --
   procedure Set (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : in Boolean;
                  Is_Null                : in Boolean;
                  Is_Changed             : in Boolean);

   -- Get a Boolean from a blob.
   --
   -- This procedure will get a Boolean member and the associated Is_Null
   -- and Is_Changed values from a blob.
   -- The value parameter is not valid if Is_Null is true.
   --
   -- Parameters: Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value of the member (invalid if Is_Null is true).
   --             Is_Null - The Is_Null flag of the member.
   --             Is_Changed - The Is_Changed flag of the member.
   --
   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Boolean;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean);

   -- Set an Int_32 or EnumerationValue in a blob.
   --
   -- This procedure will set an Int_32 member in a blob.
   -- If the Is_Null parameter is true then only the isChange and Is_Null flags
   -- are set in the blob, not the value (so it can be any value).
   --
   -- Parameters: Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value that the member is to be set to
   --                     (unless IsNull is True).
   --             Is_Null - Should the value be set to null.
   --             Is_Changed - Should the value be set to changed.
   --
   procedure Set (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : in Safir.Dob.Typesystem.Int_32;
                  Is_Null                : in Boolean;
                  Is_Changed             : in Boolean);

   -- Get an Int_32 or EnumerationValue from a blob.
   --
   -- This procedure will get an Int_32 or EnumerationValue member and
   -- the associated Is_Null and Is_Changed values from a blob.

   -- The value parameter is not valid if Is_Null is true.
   --
   -- Parameters: Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value of the member (invalid if Is_Null is true).
   --             Is_Null - The Is_Null flag of the member.
   --             Is_Changed - The Is_Changed flag of the member.
   --
   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Int_32;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean);

   -- Set an Int_64 or Type_Id in a blob.
   --
   -- This procedure will set an Int_64-based type member in a blob.
   -- If the Is_Null parameter is true then only the Is_Change and Is_Null flags
   -- are set in the blob, not the value (so it can be any value).
   --
   -- Parameters: Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value that the member is to be set to
   --                     (unless IsNull is True).
   --             Is_Null - Should the value be set to null.
   --             Is_Changed - Should the value be set to changed.
   --
   procedure Set (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : in Safir.Dob.Typesystem.Int_64;
                  Is_Null                : in Boolean;
                  Is_Changed             : in Boolean);

   -- Get an Int_64 or Type_Id from a blob.
   --
   -- This procedure will get an Int_32 or Type_Id member and
   -- the associated Is_Null and Is_Changed values from a blob.

   -- The value parameter is not valid if Is_Null is true.
   --
   -- Parameters: Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value of the member (invalid if Is_Null is true).
   --             Is_Null - The Is_Null flag of the member.
   --             Is_Changed - The Is_Changed flag of the member.
   --
   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Int_64;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean);

   -- Set a Float_32 in a blob.
   --
   -- This procedure will set a Float_32 member in a blob.
   -- If the Is_Null parameter is true then only the isChange and Is_Null flags
   -- are set in the blob, not the value (so it can be any value).
   --
   -- Parameters: Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value that the member is to be set to
   --                     (unless IsNull is True).
   --             Is_Null - Should the value be set to null.
   --             Is_Changed - Should the value be set to changed.
   --
   procedure Set (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : in Safir.Dob.Typesystem.Float_32;
                  Is_Null                : in Boolean;
                  Is_Changed             : in Boolean);

   -- Get a Float_32 from a blob.
   --
   -- This procedure will get a Float_32 and
   -- the associated Is_Null and Is_Changed values from a blob.

   -- The value parameter is not valid if Is_Null is true.
   --
   -- Parameters: Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value of the member (invalid if Is_Null is true).
   --             Is_Null - The Is_Null flag of the member.
   --             Is_Changed - The Is_Changed flag of the member.
   --
   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Float_32;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean);

   -- Set a Float_64 in a blob.
   --
   -- This procedure will set a Float_64 member in a blob.
   -- If the Is_Null parameter is true then only the isChange and Is_Null flags
   -- are set in the blob, not the value (so it can be any value).
   --
   -- Parameters: Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value that the member is to be set to
   --                     (unless IsNull is True).
   --             Is_Null - Should the value be set to null.
   --             Is_Changed - Should the value be set to changed.
   --
   procedure Set (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : in Safir.Dob.Typesystem.Float_64;
                  Is_Null                : in Boolean;
                  Is_Changed             : in Boolean);

   -- Get a Float_64 from a blob.
   --
   -- This procedure will get a Float_64 and
   -- the associated Is_Null and Is_Changed values from a blob.

   -- The value parameter is not valid if Is_Null is true.
   --
   -- Parameters: Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value of the member (invalid if Is_Null is true).
   --             Is_Null - The Is_Null flag of the member.
   --             Is_Changed - The Is_Changed flag of the member.
   --
   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Float_64;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean);

   -- Get an Instance_Id from a blob.
   --
   -- This procedure will get an Instance_Id and
   -- the associated Is_Null and Is_Changed values from a blob.

   -- The value parameter is not valid if Is_Null is true.
   --
   -- Parameters: Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value of the member (invalid if Is_Null is true).
   --             Is_Null - The Is_Null flag of the member.
   --             Is_Changed - The Is_Changed flag of the member.
   --
   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean);

   -- Get an Entity_Id from a blob.
   --
   -- This procedure will get an Entity_Id and
   -- the associated Is_Null and Is_Changed values from a blob.

   -- The value parameter is not valid if Is_Null is true.
   --
   -- Parameters: Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value of the member (invalid if Is_Null is true).
   --             Is_Null - The Is_Null flag of the member.
   --             Is_Changed - The Is_Changed flag of the member.
   --
   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean);

   -- Get a Channel_Id from a blob.
   --
   -- This procedure will get a Channel_Id and
   -- the associated Is_Null and Is_Changed values from a blob.

   -- The value parameter is not valid if Is_Null is true.
   --
   -- Parameters: Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value of the member (invalid if Is_Null is true).
   --             Is_Null - The Is_Null flag of the member.
   --             Is_Changed - The Is_Changed flag of the member.
   --
   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean);

   -- Get a Handler_Id from a blob.
   --
   -- This procedure will get a Handler_Id and
   -- the associated Is_Null and Is_Changed values from a blob.

   -- The value parameter is not valid if Is_Null is true.
   --
   -- Parameters: Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value of the member (invalid if Is_Null is true).
   --             Is_Null - The Is_Null flag of the member.
   --             Is_Changed - The Is_Changed flag of the member.
   --
   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean);

   -- Get an Unbounded_Wide_String from a blob.
   --
   -- This procedure will get an Unbounded_Wide_String and
   -- the associated Is_Null and Is_Changed values from a blob.

   -- The value parameter is not valid if Is_Null is true.
   --
   -- Parameters: Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value of the member (invalid if Is_Null is true).
   --             Is_Null - The Is_Null flag of the member.
   --             Is_Changed - The Is_Changed flag of the member.
   --
   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Unbounded_Wide_String;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean);

   -- Get a blob from a blob.
   --
   -- This procedure will get a blob and
   -- the associated Is_Null and Is_Changed values from a blob.

   -- The value parameter is not valid if Is_Null is true.
   --
   -- Parameters: Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value of the member (invalid if Is_Null is true).
   --             Is_Null - The Is_Null flag of the member.
   --             Is_Changed - The Is_Changed flag of the member.
   --
   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Blob_T;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean);

   -- Get a Binary from a blob.
   --
   -- This procedure will get a Binary and
   -- the associated Is_Null and Is_Changed values from a blob.

   -- The value parameter is not valid if Is_Null is true.
   --
   -- Parameters: Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   --             Value - The value of the member (invalid if Is_Null is true).
   --             Is_Null - The Is_Null flag of the member.
   --             Is_Changed - The Is_Changed flag of the member.
   --
   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Binary_Vectors.Vector;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean);

   -- =========================================================================
   -- Container operations on blobs.
   -- These operations converts between a container and a blob.
   -- =========================================================================

   -- Set a Boolean in a blob.
   --
   -- If the container is null then the member will be set to null in the blob.
   -- The change flag from the container will be set in the blob.
   --
   -- Parameters: Value - The container whose values to use.
   --             Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --
   procedure Set (Value               : in Boolean_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Get a Boolean from a blob.
   --
   -- Get the member and the associated isNull and isChange values from a blob
   -- and put them in the container.
   --
   -- Parameters: Value - The container in which to put the values.
   --             Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   procedure Get (Value               : out Boolean_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Set an Enumeration in a blob.
   --
   -- If the container is null then the member will be set to null in the blob.
   -- The change flag from the container will be set in the blob.
   --
   -- Parameters: Value - The container whose values to use.
   --             Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --
   procedure Set (Value               : in Enumeration_Container_Base_Type'Class;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Get an Enumeration from a blob.
   --
   -- Get the member and the associated isNull and isChange values from a blob
   -- and put them in the container.
   --
   -- Parameters: Value - The container in which to put the values.
   --             Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   procedure Get (Value               : out Enumeration_Container_Base_Type'Class;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Set an Int_32 in a blob.
   --
   -- If the container is null then the member will be set to null in the blob.
   -- The change flag from the container will be set in the blob.
   --
   -- Parameters: Value - The container whose values to use.
   --             Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --
   procedure Set (Value               : in Int_32_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Get an Int_32 from a blob.
   --
   -- Get the member and the associated isNull and isChange values from a blob
   -- and put them in the container.
   --
   -- Parameters: Value - The container in which to put the values.
   --             Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   procedure Get (Value               : out Int_32_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Set an Int_64 in a blob.
   --
   -- If the container is null then the member will be set to null in the blob.
   -- The change flag from the container will be set in the blob.
   --
   -- Parameters: Value - The container whose values to use.
   --             Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --
   procedure Set (Value               : in Int_64_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Get an Int_64 from a blob.
   --
   -- Get the member and the associated isNull and isChange values from a blob
   -- and put them in the container.
   --
   -- Parameters: Value - The container in which to put the values.
   --             Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   procedure Get (Value               : out Int_64_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Set a Float_32 in a blob.
   --
   -- If the container is null then the member will be set to null in the blob.
   -- The change flag from the container will be set in the blob.
   --
   -- Parameters: Value - The container whose values to use.
   --             Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --
   procedure Set (Value               : in Float_32_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Get a Float_32 from a blob.
   --
   -- Get the member and the associated isNull and isChange values from a blob
   -- and put them in the container.
   --
   -- Parameters: Value - The container in which to put the values.
   --             Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   procedure Get (Value               : out Float_32_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Set a Float_64 in a blob.
   --
   -- If the container is null then the member will be set to null in the blob.
   -- The change flag from the container will be set in the blob.
   --
   -- Parameters: Value - The container whose values to use.
   --             Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --
   procedure Set (Value               : in Float_64_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Get a Float_64 from a blob.
   --
   -- Get the member and the associated isNull and isChange values from a blob
   -- and put them in the container.
   --
   -- Parameters: Value - The container in which to put the values.
   --             Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   procedure Get (Value               : out Float_64_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Set an Instance_Id in a blob.
   --
   -- If the container is null then the member will be set to null in the blob.
   -- The change flag from the container will be set in the blob.
   --
   -- Parameters: Value - The container whose values to use.
   --             Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --
   procedure Set (Value               : in Instance_Id_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Get an Instance_Id from a blob.
   --
   -- Get the member and the associated isNull and isChange values from a blob
   -- and put them in the container.
   --
   -- Parameters: Value - The container in which to put the values.
   --             Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   procedure Get (Value               : out Instance_Id_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Set an Entity_Id in a blob.
   --
   -- If the container is null then the member will be set to null in the blob.
   -- The change flag from the container will be set in the blob.
   --
   -- Parameters: Value - The container whose values to use.
   --             Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --
   procedure Set (Value               : in Entity_Id_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Get an Entity_Id from a blob.
   --
   -- Get the member and the associated isNull and isChange values from a blob
   -- and put them in the container.
   --
   -- Parameters: Value - The container in which to put the values.
   --             Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   procedure Get (Value               : out Entity_Id_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Set a Channel_Id in a blob.
   --
   -- If the container is null then the member will be set to null in the blob.
   -- The change flag from the container will be set in the blob.
   --
   -- Parameters: Value - The container whose values to use.
   --             Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --
   procedure Set (Value               : in Channel_Id_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Get a Channel_Id from a blob.
   --
   -- Get the member and the associated isNull and isChange values from a blob
   -- and put them in the container.
   --
   -- Parameters: Value - The container in which to put the values.
   --             Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   procedure Get (Value               : out Channel_Id_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Set a Handler_Id in a blob.
   --
   -- If the container is null then the member will be set to null in the blob.
   -- The change flag from the container will be set in the blob.
   --
   -- Parameters: Value - The container whose values to use.
   --             Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --
   procedure Set (Value               : in Handler_Id_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Get a Handler_Id from a blob.
   --
   -- Get the member and the associated isNull and isChange values from a blob
   -- and put them in the container.
   --
   -- Parameters: Value - The container in which to put the values.
   --             Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   procedure Get (Value               : out Handler_Id_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Set a String in a blob.
   --
   -- If the container is null then the member will be set to null in the blob.
   -- The change flag from the container will be set in the blob.
   --
   -- Parameters: Value - The container whose values to use.
   --             Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --
   procedure Set (Value               : in String_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Get a String from a blob.
   --
   -- Get the member and the associated isNull and isChange values from a blob
   -- and put them in the container.
   --
   -- Parameters: Value - The container in which to put the values.
   --             Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   procedure Get (Value               : out String_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Set an Object in a blob.
   --
   -- If the container is null then the member will be set to null in the blob.
   -- The change flag from the container will be set in the blob.
   --
   -- Parameters: Value - The container whose values to use.
   --             Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --
   procedure Set (Value               : in Object_Container_Base_Type'Class;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Get an Object from a blob.
   --
   -- Get the member and the associated isNull and isChange values from a blob
   -- and put them in the container.
   --
   -- Parameters: Value - The container in which to put the values.
   --             Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   procedure Get (Value               : out Object_Container_Base_Type'Class;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Set Binary in a blob.
   --
   -- If the container is null then the member will be set to null in the blob.
   -- The change flag from the container will be set in the blob.
   --
   -- Parameters: Value - The container whose values to use.
   --             Blob - Blob to set the member in.
   --             Member - The member to be set.
   --             Index - Array index in member to set. Shall be 0 if
   --                     the member is not an array.
   --
   procedure Set (Value               : in Binary_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Get Binary from a blob.
   --
   -- Get the member and the associated isNull and isChange values from a blob
   -- and put them in the container.
   --
   -- Parameters: Value - The container in which to put the values.
   --             Blob - Blob to get the member from.
   --             Member - The member to get.
   --             Index - Array index in member to get. Shall be 0 if
   --                     the member is not an array.
   procedure Get (Value               : out Binary_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index);

   -- Get the static blob size of an type, but excluding the size that is
   -- inherited from parent classes.
   --
   -- This is very much an internal function!
   -- Unless you have a really good reason to use this function you should stay
   -- clear of it.
   --
   -- Parameters : Type_Id - The Type Id of a DOB class.
   -- Returns: The amount of space needed for the static part of the type.
   --
   function Get_Initial_Size (Type_Id : in Safir.Dob.Typesystem.Type_Id)
                              return Safir.Dob.Typesystem.Int_32;

end Safir.Dob.Typesystem.Blob_Operations;
