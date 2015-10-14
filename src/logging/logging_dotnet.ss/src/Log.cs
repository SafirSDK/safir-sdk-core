/* ****************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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
        /// Severity level according to RFC 3164. Please read the section on Safir Logging 
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

        ///<summary>
        /// Send log messages to the system logging mechanism.
        /// <para/>
        /// The function takes a severity and an arbitrary string.
        /// The severity levels conforms to the ones used by the well known syslog format as specified
        /// in http://www.ietf.org/rfc/rfc3164.txt.
        /// </summary>
        /// <param name="severity">Severity according to RFC 3164.</param>
        /// <param name="message">Log text.</param>
        public static void SendSystemLog(Severity severity,
                                         String message)
        {
            LoggingC_SendSystemLog((System.Int32)severity,
                                   Encoding.UTF8.GetBytes(message + char.MinValue));
        }


        [DllImport("logging_library", CallingConvention = CallingConvention.Cdecl, EntryPoint = "LoggingC_SendSystemLog")]
        private static extern void LoggingC_SendSystemLog(System.Int32 severity,
                                                          byte [] logMsg);
    }
}
