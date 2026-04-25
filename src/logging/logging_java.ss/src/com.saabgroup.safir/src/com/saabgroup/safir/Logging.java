// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2013, 2026 (http://safirsdkcore.com)
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
    private Logging() {}
    /**
     * Severity level according to RFC 3164 and RFC 5424. Please read the section on Safir Logging
     * in the Safir SDK Core User's Guide for some recommendations on when to use the
     * various severity levels.
     **/
    public enum Severity
    {
        /** RFC 3164 Description: System is unusable. */
        EMERGENCY,

        /** RFC 3164 Description: Action must be taken immediately. */
        ALERT,

        /** RFC 3164 Description: Critical conditions. */
        CRITICAL,

        /** RFC 3164 Description: Error conditions. */
        ERROR,

        /** RFC 3164 Description: Warning conditions. */
        WARNING,

        /** RFC 3164 Description: Normal but significant condition. */
        NOTICE,

        /** RFC 3164 Description: Informational messages. */
        INFORMATIONAL,

        /** RFC 3164 Description: Debug-level messages. */
        DEBUG
    };

    /**
     * Syslog facility codes according to RFC 3164 and RFC 5424.
     * The numeric values are the facility codes as defined in the RFCs.
     **/
    public enum Facility
    {
        /** Kernel messages. */
        KERNEL(0),

        /** User-level messages. */
        USER(1),

        /** Mail system. */
        MAIL(2),

        /** System daemons. */
        DAEMON(3),

        /** Security/authorization messages. */
        AUTH(4),

        /** Messages generated internally by syslogd. */
        SYSLOG(5),

        /** Line printer subsystem. */
        LPR(6),

        /** Network news subsystem. */
        NEWS(7),

        /** UUCP subsystem. */
        UUCP(8),

        /** Clock daemon. */
        CRON(9),

        /** Security/authorization messages (private). */
        AUTHPRIV(10),

        /** FTP daemon. */
        FTP(11),

        /** Local use 0. */
        LOCAL0(16),

        /** Local use 1. */
        LOCAL1(17),

        /** Local use 2. */
        LOCAL2(18),

        /** Local use 3. */
        LOCAL3(19),

        /** Local use 4. */
        LOCAL4(20),

        /** Local use 5. */
        LOCAL5(21),

        /** Local use 6. */
        LOCAL6(22),

        /** Local use 7. */
        LOCAL7(23);

        private final int value;

        Facility(int value) {
            this.value = value;
        }

        public int getValue() {
            return value;
        }
    };

    /**
     * Send log messages to the system logging mechanism.
     *
     * The function takes a severity and an arbitrary string.
     * The severity levels conform to the syslog format as specified
     * in RFC 3164 and RFC 5424.
     *
     * Uses the LOCAL0 facility by default.
     *
     * @param severity Severity according to RFC 3164/5424.
     * @param message Log text.
     */
    public static void sendSystemLog(Severity severity,
                                     String message) {
        sendSystemLog(severity, Facility.LOCAL0, message);
    }

    /**
     * Send log messages to the system logging mechanism.
     *
     * The function takes a severity, facility, and an arbitrary string.
     * The severity and facility levels conform to the syslog format as specified
     * in RFC 3164 and RFC 5424.
     *
     * @param severity Severity according to RFC 3164/5424.
     * @param facility Facility according to RFC 3164/5424.
     * @param message Log text.
     */
    public static void sendSystemLog(Severity severity,
                                     Facility facility,
                                     String message) {
        sendSystemLogInternal(severity.ordinal(), facility.getValue(), message);
    }

    /**
     * Send a system log with severity Emergency.
     *
     * @param message Log text.
     */
    public static void sendEmergency(String message) {
        sendSystemLog(Severity.EMERGENCY, message);
    }

    /**
     * Send a system log with severity Alert.
     *
     * @param message Log text.
     */
    public static void sendAlert(String message) {
        sendSystemLog(Severity.ALERT, message);
    }

    /**
     * Send a system log with severity Critical.
     *
     * @param message Log text.
     */
    public static void sendCritical(String message) {
        sendSystemLog(Severity.CRITICAL, message);
    }

    /**
     * Send a system log with severity Error.
     *
     * @param message Log text.
     */
    public static void sendError(String message) {
        sendSystemLog(Severity.ERROR, message);
    }

    /**
     * Send a system log with severity Warning.
     *
     * @param message Log text.
     */
    public static void sendWarning(String message) {
        sendSystemLog(Severity.WARNING, message);
    }

    /**
     * Send a system log with severity Notice.
     *
     * @param message Log text.
     */
    public static void sendNotice(String message) {
        sendSystemLog(Severity.NOTICE, message);
    }

    /**
     * Send a system log with severity Informational.
     *
     * @param message Log text.
     */
    public static void sendInformational(String message) {
        sendSystemLog(Severity.INFORMATIONAL, message);
    }

    /**
     * Send a system log with severity Debug.
     *
     * @param message Log text.
     */
    public static void sendDebug(String message) {
        sendSystemLog(Severity.DEBUG, message);
    }

    private static native void sendSystemLogInternal(int severity,
                                                     int facility,
                                                     String message);

    static
    {
        System.loadLibrary("logging_java_jni");
    }

}
