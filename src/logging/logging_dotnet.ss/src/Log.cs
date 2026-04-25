/* ****************************************************************************
*
* Copyright Saab AB, 2007-2013, 2026 (http://safirsdkcore.com)
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
using System;
using System.Text;
using System.Runtime.InteropServices;

namespace Safir
{
    /// <summary>
    /// Class containing types and functions to be used for logging.
    /// </summary>
    public sealed class Logging
    {
        /// <summary>
        /// Severity level according to RFC 3164 and RFC 5424. Please read the section on Safir Logging
        /// in the Safir SDK Core User's Guide for some recommendations on when to use the
        /// various severity levels.
        /// </summary>
        public enum Severity
        {
            /// <summary>RFC 3164 Description: System is unusable.</summary>
            Emergency,

            /// <summary>RFC 3164 Description: Action must be taken immediately.</summary>
            Alert,

            /// <summary>RFC 3164 Description: Critical conditions.</summary>
            Critical,

            /// <summary>RFC 3164 Description: Error conditions.</summary>
            Error,

            /// <summary>RFC 3164 Description: Warning conditions.</summary>
            Warning,

            /// <summary>RFC 3164 Description: Normal but significant condition.</summary>
            Notice,

            /// <summary>RFC 3164 Description: Informational messages.</summary>
            Informational,

            /// <summary>RFC 3164 Description: Debug-level messages.</summary>
            Debug
        };

        /// <summary>
        /// Syslog facility codes according to RFC 3164 and RFC 5424.
        /// The numeric values are the facility codes as defined in the RFCs.
        /// </summary>
        public enum Facility
        {
            /// <summary>Kernel messages.</summary>
            Kernel = 0,

            /// <summary>User-level messages.</summary>
            User = 1,

            /// <summary>Mail system.</summary>
            Mail = 2,

            /// <summary>System daemons.</summary>
            Daemon = 3,

            /// <summary>Security/authorization messages.</summary>
            Auth = 4,

            /// <summary>Messages generated internally by syslogd.</summary>
            Syslog = 5,

            /// <summary>Line printer subsystem.</summary>
            Lpr = 6,

            /// <summary>Network news subsystem.</summary>
            News = 7,

            /// <summary>UUCP subsystem.</summary>
            Uucp = 8,

            /// <summary>Clock daemon.</summary>
            Cron = 9,

            /// <summary>Security/authorization messages (private).</summary>
            Authpriv = 10,

            /// <summary>FTP daemon.</summary>
            Ftp = 11,

            /// <summary>Local use 0.</summary>
            Local0 = 16,

            /// <summary>Local use 1.</summary>
            Local1 = 17,

            /// <summary>Local use 2.</summary>
            Local2 = 18,

            /// <summary>Local use 3.</summary>
            Local3 = 19,

            /// <summary>Local use 4.</summary>
            Local4 = 20,

            /// <summary>Local use 5.</summary>
            Local5 = 21,

            /// <summary>Local use 6.</summary>
            Local6 = 22,

            /// <summary>Local use 7.</summary>
            Local7 = 23
        };

        ///<summary>
        /// Send log messages to the system logging mechanism.
        /// <para/>
        /// The function takes a severity and an arbitrary string.
        /// The severity levels conform to the syslog format as specified
        /// in RFC 3164 and RFC 5424.
        /// <para/>
        /// Uses the Local0 facility by default.
        /// </summary>
        /// <param name="severity">Severity according to RFC 3164/5424.</param>
        /// <param name="message">Log text.</param>
        public static void SendSystemLog(Severity severity,
                                         String message)
        {
            SendSystemLog(severity, Facility.Local0, message);
        }

        ///<summary>
        /// Send log messages to the system logging mechanism.
        /// <para/>
        /// The function takes a severity, facility, and an arbitrary string.
        /// The severity and facility levels conform to the syslog format as specified
        /// in RFC 3164 and RFC 5424.
        /// </summary>
        /// <param name="severity">Severity according to RFC 3164/5424.</param>
        /// <param name="facility">Facility according to RFC 3164/5424.</param>
        /// <param name="message">Log text.</param>
        public static void SendSystemLog(Severity severity,
                                         Facility facility,
                                         String message)
        {
            LoggingC_SendSystemLog((System.Int32)severity,
                                   (System.Int32)facility,
                                   Encoding.UTF8.GetBytes(message + char.MinValue));
        }

        ///<summary>
        /// Send a system log with severity Emergency.
        /// </summary>
        /// <param name="message">Log text.</param>
        public static void SendEmergency(String message)
        {
            SendSystemLog(Severity.Emergency, message);
        }

        ///<summary>
        /// Send a system log with severity Alert.
        /// </summary>
        /// <param name="message">Log text.</param>
        public static void SendAlert(String message)
        {
            SendSystemLog(Severity.Alert, message);
        }

        ///<summary>
        /// Send a system log with severity Critical.
        /// </summary>
        /// <param name="message">Log text.</param>
        public static void SendCritical(String message)
        {
            SendSystemLog(Severity.Critical, message);
        }

        ///<summary>
        /// Send a system log with severity Error.
        /// </summary>
        /// <param name="message">Log text.</param>
        public static void SendError(String message)
        {
            SendSystemLog(Severity.Error, message);
        }

        ///<summary>
        /// Send a system log with severity Warning.
        /// </summary>
        /// <param name="message">Log text.</param>
        public static void SendWarning(String message)
        {
            SendSystemLog(Severity.Warning, message);
        }

        ///<summary>
        /// Send a system log with severity Notice.
        /// </summary>
        /// <param name="message">Log text.</param>
        public static void SendNotice(String message)
        {
            SendSystemLog(Severity.Notice, message);
        }

        ///<summary>
        /// Send a system log with severity Informational.
        /// </summary>
        /// <param name="message">Log text.</param>
        public static void SendInformational(String message)
        {
            SendSystemLog(Severity.Informational, message);
        }

        ///<summary>
        /// Send a system log with severity Debug.
        /// </summary>
        /// <param name="message">Log text.</param>
        public static void SendDebug(String message)
        {
            SendSystemLog(Severity.Debug, message);
        }

        [DllImport("logging_library", CallingConvention = CallingConvention.Cdecl, EntryPoint = "LoggingC_SendSystemLog")]
        private static extern void LoggingC_SendSystemLog(System.Int32 severity,
                                                          System.Int32 facility,
                                                          byte [] logMsg);
    }
}
