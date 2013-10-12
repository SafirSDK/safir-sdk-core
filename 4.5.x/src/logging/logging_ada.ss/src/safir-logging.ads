-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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
package Safir.Logging is

   type Severity_T is (Emergency,     -- RFC 3164 Description: System is unusable.
                       Alert,         -- RFC 3164 Description: Action must be taken immediately.
                       Critical,      -- RFC 3164 Description: Critical conditions.
                       Error,         -- RFC 3164 Description: Error conditions.
                       Warning,       -- RFC 3164 Description: Warning conditions.
                       Notice,        -- RFC 3164 Description: Normal but significant condition.
                       Informational, -- RFC 3164 Description: Informational messages.
                       Debug);        -- RFC 3164 Description: Debug-level messages.
   --
   -- Severity level according to RFC 3164. Please read the section on Safir Logging
   -- in the Safir SDK Core User's Guide for some recommendations on when to use the
   -- various severity levels.
   --

   procedure Send_System_Log (Severity : in Severity_T;
                              Message  : in Wide_String);
   --
   -- Send log messages to the system logging mechanism.
   --
   -- The function takes a severity and an arbitrary string.
   -- The severity levels conforms to the ones used by the well known syslog format as specified
   -- in http://www.ietf.org/rfc/rfc3164.txt.
   --
   -- Severity  Severity according to RFC 3164.
   -- Message   Log text.
   --

end Safir.Logging;

