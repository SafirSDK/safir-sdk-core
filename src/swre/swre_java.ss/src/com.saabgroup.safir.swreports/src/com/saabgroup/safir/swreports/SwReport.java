// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / stlrha
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
package com.saabgroup.safir.swreports;

/**
Provides methods for sending sofware reports.
<p>
There are five predefined report types: Fatal Error Report, Error Report, Resource Report,
Program Info Report and Programming Error Report.
<p>
See the corresponding method for a description of the intended usage.
<p>
Some methods have both an error code and a text parameter (both of type string).
The intended usage is that the error code parameter should be a short mnemonic string
and the text parameter should be a more elaborated description. The mnemonic strings
are preferably defined as Dob parameters.
<p>
From the location parameter string it should be easy to identify the exact code location
where the report is generated.
<p>
All methods are thread safe.
<p>
@deprecated use com.saabgroup.safir.logging.SendSystemLog() instead.
*/
@Deprecated
public class SwReport
{
    /**
     * Sends a Fatal Error report.
     *
     * Use it to report static conditions that must be fulfilled to be able to start/continue
     * executing the program, for example missing static resources or invalid configuration.
     * Normally the program should not continue to execute.
     *
     * @param errorCode Application defined error code (mnemonic).
     * @param location  Source code location.
     * @param text      Application defined text.
     *
     * @deprecated use Safir::Logging::SendSystemLog() instead.
     */
    @Deprecated
    public static void SendFatalErrorReport(String errorCode,
                                            String location,
                                            String text)
    {
        boolean [] success = new boolean [1];
        Library.SendFatalErrorReport(errorCode,
                                           location,
                                           text,
                                           success);
        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    /**
     * Sends an Error report.
     *
     * Use it to report detected runtime errors, for example a message from an external system
     * in an invalid format. Normally the program continues to execute, possibly in a degraded state.
     *
     * @param errorCode Application defined error code (mnemonic).
     * @param location  Source code location.
     * @param text      Application defined text.
     *
     * @deprecated use Safir::Logging::SendSystemLog() instead.
     */
    @Deprecated
    public static void SendErrorReport(String errorCode,
                                       String location,
                                       String text)
    {
        boolean [] success = new boolean [1];
        Library.SendErrorReport(errorCode,
                                      location,
                                      text,
                                      success);
        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

    }

    /**
     * Sends a Resource report.
     *
     * Use it to report a missing/acquired dynamic resource. Note that it is ok
     * for dynamic resource to be temporary missing which means that a Resource Report
     * should be sent only after a reasonably number of retries to acquire it.
     *
     * @param resourceId Application defined resource id (mnemonic).
     * @param allocated  True if the resource is allocated, otherwise false.
     * @param text       Application defined text.
     *
     * @deprecated use Safir::Logging::SendSystemLog() instead.
     */
    @Deprecated
    public static void SendResourceReport(String resourceId,
                                          boolean allocated,
                                          String text)
    {
        boolean [] success = new boolean [1];
        Library.SendResourceReport(resourceId,
                                   allocated,
                                   text,
                                   success);
        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

    }

    /**
     * Sends a Programming Error report.
     *
     * Use it to report programming errors, that is, errors of assert-type.
     * Normally the program should not continue to execute, in that way enabling
     * a redundant program instance to start.
     *
     * @param errorCode Application defined error code (mnemonic).
     * @param location  Source code location.
     * @param text      Application defined text.
     *
     * @deprecated use Safir::Logging::SendSystemLog() instead.
     */
    @Deprecated
    public static void SendProgrammingErrorReport(String errorCode,
                                                  String location,
                                                  String text)
    {
        boolean [] success = new boolean [1];
        Library.SendProgrammingErrorReport(errorCode,
                                                 location,
                                                 text,
                                                 success);
        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    /**
     * Sends a Programming Info report.
     *
     * Use it to report internal program information for debugging purposes.
     * Normally the sending of this report type is controlled by internal status variables
     * that are set by sending backdoor commands to the program.
     *
     * @param text Application defined text.
     *
     * @deprecated use Safir::Logging::SendSystemLog() instead.
     */
    @Deprecated
    public static void SendProgramInfoReport(String text)
    {
        boolean [] success = new boolean [1];
        Library.SendProgramInfoReport(text,
                                            success);
        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

}
