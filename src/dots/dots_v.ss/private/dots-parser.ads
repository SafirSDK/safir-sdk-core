-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
--
--  Created by: Henrik Sundberg / sthesu
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

with GNAT.Spitbol; use GNAT.Spitbol;
package Dots.Parser is

   Error : VString := Nul;

   type Element_Type_T is
     (stEmpty,
      stSummary,

      stUnitName,
      stClassName,
      stFullClassName,
      stFullParameterName,
      stMemberName,
      stParameterName,
      stArraySize,
      stArray,
      stStringLength,
      stType,
      stEnumerationValue,
      xsd_boolean,
      xsd_float,
      xsd_double,
      xsd_integer,
      xsd_positiveInteger,
      xsd_decimal,
      xsd_string,

      document, -- The first complex type
      ctClass,
      ctProperty,
      ctEnumeration,
      ctException,
      ctEnumerationValues,
      ctParameters,
      ctParameter,
      ctParameterArray,
      ctParameterArrayElement,
      ctEntityId,
      ctReference,
      ctObject,
      ctObjectMembers,
      ctObjectMember,
      ctMembers,
      ctMember,
      ctCreateRoutines,
      ctCreateRoutine,
      ctCreateParameters,
      ctCreateValues,
      ctCreateValue);

   procedure Start_Document;

   procedure Start_Element
     (Namespace_URI : String;
      Local_Name    : String;
      Entered_Element_Type : out Element_Type_T);

   procedure End_Element
     (Local_Name : String;
      Left_Element_Type : out Element_Type_T);

   procedure Characters
     (Ch : String);

   function Is_Inside
     (Element_Type : Element_Type_T) return Boolean;

   function Is_Child_Of
     (Element_Type : Element_Type_T) return Boolean;

   function Simple_Contents return VString;

end Dots.Parser;
