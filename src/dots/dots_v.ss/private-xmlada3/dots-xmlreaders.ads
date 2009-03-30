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

with Sax.Exceptions;
with Sax.Locators;
with Sax.Readers;
with Sax.Attributes;
with Unicode.CES;

package Dots.Xmlreaders is
   type Reader is new Sax.Readers.Reader with private;

   procedure Set_Silent
     (Handler : in out Reader; Silent : Boolean);
   --  If Silent is True, then nothing will be output on the console, except
   --  error messages

   procedure Set_Effort_Only
     (Handler : in out Reader; Effort_Only : Boolean);
   --  If Effort_Only is True, then nothing will happen, except
   --  syntax/semantic checks

   procedure Warning
     (Handler : in out Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class);
   procedure Error
     (Handler : in out Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   procedure Fatal_Error
     (Handler : in out Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   procedure Set_Document_Locator
     (Handler : in out Reader;
      Loc     : in out Sax.Locators.Locator);
   procedure Start_Document (Handler : in out Reader);
   procedure End_Document (Handler : in out Reader);
   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);
   procedure End_Element
     (Handler : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");
   procedure Characters
     (Handler : in out Reader; Ch : Unicode.CES.Byte_Sequence);

private
   type Reader is new Sax.Readers.Reader with record
      Locator : Sax.Locators.Locator;
      Silent  : Boolean := False;
      Effort_Only : Boolean := True;
   end record;
end Dots.Xmlreaders;
