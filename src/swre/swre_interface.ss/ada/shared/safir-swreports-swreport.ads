-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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
package Safir.SwReports.SwReport is

   procedure Stop;
   --
   -- Stop the SwReports background thread.
   --
   -- This needs to be called before exiting an application to let SwReports stop
   -- its background thread. Failure to do this may cause problems if the
   -- thread is currently using its dob connection when it gets killed.
   --

   procedure Send_Fatal_Error_Report (Error_Code : in Wide_String;
                                      Location   : in Wide_String;
                                      Text       : in Wide_String);
   --
   -- Sends a Fatal Error report.
   --
   -- Use it to report static conditions that must be fulfilled to be able to start/continue
   -- executing the program, for example missing static resources or invalid configuration.
   -- Normally the program should not continue to execute.
   --
   -- Error_Code Application defined error code (mnemonic).
   -- Location   Source code location.
   -- Text       Application defined text.
   --

   procedure Send_Error_Report (Error_Code : in Wide_String;
                                Location   : in Wide_String;
                                Text       : in Wide_String);
   --
   -- Sends an Error report.
   --
   -- Use it to report detected runtime errors, for example a message from an external system
   -- in an invalid format. Normally the program continues to execute, possibly in a degraded state.
   --
   -- Error_Code Application defined error code (mnemonic).
   -- Location   Source code location.
   -- Text       Application defined text.
   --

   procedure Send_Resource_Report (Resource_Id : in Wide_String;
                                   Allocated   : in Boolean;
                                   Text        : in Wide_String);
    --
    -- Sends a Resource report.
    --
    -- Use it to report a missing/acquired dynamic resource. Note that it is ok
    -- for dynamic resource to be temporary missing which means that a Resource Report
    -- should be sent only after a reasonably number of retries to acquire it.
    --
    -- Resource_Id Application defined resource id (mnemonic).
    -- Allocated   True if the resource is allocated, otherwise false.
    -- Text        Application defined text.
    --

   procedure Send_Programming_Error_Report (Error_Code : in Wide_String;
                                            Location   : in Wide_String;
                                            Text       : in Wide_String);
    --
    -- Sends a Programming Error report.
    --
    -- Use it to report programming errors, that is, errors of assert-type.
    -- Normally the program should not continue to execute, in that way enabling
    -- a redundant program instance to start.
    --
    -- Error_Code Application defined error code (mnemonic).
    -- Location   Source code location.
    -- Text       Application defined text.
    --

   procedure Send_Program_Info_Report (Text : in Wide_String);
   --
   -- Sends a Programming Info report.
   --
   -- Use it to report internal program information for debugging purposes.
   -- Normally the sending of this report type is controlled by internal status variables
   -- that are set by sending backdoor commands to the program.
   --
   -- Text Application defined text.
   --

end Safir.SwReports.SwReport;

