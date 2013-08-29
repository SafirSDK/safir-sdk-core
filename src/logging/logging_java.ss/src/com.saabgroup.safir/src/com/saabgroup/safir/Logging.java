// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2013 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
package com.saabgroup.safir;

/** 
 * Class containing types and functions to be used for logging.
 */
public final class Logging
{
    //TODO: add comments about what to use when
    public enum Severity
    {
        EMERGENCY,
        ALERT,
        CRITICAL,
        ERROR,
        WARNING,
        NOTICE,
        INFORMATIONAL,
        DEBUG
    };

    /**
     * Send log messages to the system logging mechanism.
     *
     * The function takes a severity and an arbitrary string.
     * The severity levels conforms to the ones used by the well known syslog format as specified
     * in http://www.ietf.org/rfc/rfc3164.txt.
     *
     * @param severity Severity according to RFC 3164.
     * @param message Log text.
     */
    public static void sendSystemLog(Severity severity,
                                     String message) {
        sendSystemLogInternal(severity.ordinal(), message);
    }

    private static native void sendSystemLogInternal(int severity,
                                                     String message);
    
    static
    {
        System.loadLibrary("logging_java_jni");
    }

}
