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
using System;
using System.Collections.Generic;
using System.Text;

namespace Safir.SwReports
{
    /// <summary>
    /// <para>Provides static methods for sending sofware reports.</para>
    ///
    /// <para>All methods are MT safe</para>
    ///
    /// <para>There are five predefined report types:</para>
    /// <list type="bullet">
    /// <item></item>
    /// <item>
    ///     <description>
    ///         Fatal Error Report
    ///     </description>
    /// </item>
    /// <item>
    ///     <description>
    ///         Error Report
    ///     </description>
    /// </item>
    /// <item>
    ///     <description>
    ///         Resource Report
    ///     </description>
    /// </item>
    /// <item>
    ///     <description>
    ///         Program Info Report
    ///     </description>
    /// </item>
    /// <item>
    ///     <description>
    ///         programming Error Report
    ///     </description>
    /// </item>
    /// </list>
    ///
    /// <para>See the corresponding method for a description of the intended usage.</para>
    ///
    /// <para>
    ///         Some methods have both an error code and a text parameter (both of type string).
    ///         The intended usage is that the error code parameter should be a short mnemonic string
    ///         and the text parameter should be a more elaborated description. The mnemonic strings
    ///         are preferably defined as Dob parameters.
    /// </para>
    ///
    /// <para>
    ///         From the location parameter string it should be easy to identify the exact code location
    ///         where the report is generated.
    /// </para>
    ///
    /// <para>All methods are thread safe.</para>
    /// <para>All methods in this namespace are deprecated. Use Safir.Logging.SendSystemLog(...) instead.</para>
    /// </summary>
    /// TODO: [Obsolete("All methods in this class are obsolete! Use Safir.Logging.SendSystemLog(...) instead.")]
    public sealed class SwReport
    {
        /// <summary>
        /// Sends a Fatal Error report.
        /// Use it to report static conditions that must be fulfilled to be able to start/continue
        /// executing the program, for example missing static resources or invalid configuration.
        /// Normally the program should not continue to execute.
        /// </summary>
        /// <param name="errorCode">Application defined error code (mnemonic)</param>
        /// <param name="location">Source code location</param>
        /// <param name="text">Application defined text</param>
        public static void SendFatalErrorReport(string errorCode,
                                                string location,
                                                string text)
        {
            byte success;
            Library.SwreC_SendFatalErrorReport(Encoding.UTF8.GetBytes(errorCode + char.MinValue),
                                               Encoding.UTF8.GetBytes(location + char.MinValue),
                                               Encoding.UTF8.GetBytes(text + char.MinValue),
                                               out success);
            if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Sends an Error report.
        /// Use it to report detected runtime errors, for example a message from an external system
        /// in an invalid format. Normally the program continues to execute, possibly in a degraded state.
        /// </summary>
        /// <param name="errorCode">Application defined error code (mnemonic)</param>
        /// <param name="location">Source code location</param>
        /// <param name="text">Application defined text</param>
        public static void SendErrorReport(string errorCode,
                                           string location,
                                           string text)
        {
            byte success;
            Library.SwreC_SendErrorReport(Encoding.UTF8.GetBytes(errorCode + char.MinValue),
                                          Encoding.UTF8.GetBytes(location + char.MinValue),
                                          Encoding.UTF8.GetBytes(text + char.MinValue),
                                          out success);
            if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }

        }

        /// <summary>
        /// Sends a Resource report.
        /// Use it to report a missing/acquired dynamic resource. Note that it is ok
        /// for dynamic resource to be temporary missing which means that a Resource Report
        /// should be sent only after a reasonably number of retries to acquire it.
        /// </summary>
        /// <param name="resourceId">Application defined resource id (mnemonic)</param>
        /// <param name="allocated">True if the resource is allocated, otherwise false</param>
        /// <param name="text">Application defined text</param>
        public static void SendResourceReport(string resourceId,
                                              bool   allocated,
                                              string text)
        {
            byte success;
            Library.SwreC_SendResourceReport(Encoding.UTF8.GetBytes(resourceId + char.MinValue),
                                             Safir.Dob.Typesystem.Internal.InternalOperations.ByteOf(allocated),
                                             Encoding.UTF8.GetBytes(text + char.MinValue),
                                             out success);
            if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }

        }

        /// <summary>
        /// Sends a Programming Error report.
        /// Use it to report programming errors, that is, errors of assert-type.
        /// Normally the program should not continue to execute, in that way enabling
        /// a redundant program instance to start.
        /// </summary>
        /// <param name="errorCode">Application defined error code (mnemonic)</param>
        /// <param name="location">Source code location</param>
        /// <param name="text">Application defined text</param>
        public static void SendProgrammingErrorReport(string errorCode,
                                                      string location,
                                                      string text)
        {
            byte success;
            Library.SwreC_SendProgrammingErrorReport(Encoding.UTF8.GetBytes(errorCode + char.MinValue),
                                                     Encoding.UTF8.GetBytes(location + char.MinValue),
                                                     Encoding.UTF8.GetBytes(text + char.MinValue),
                                                     out success);
            if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Sends a Programming Info report.
        /// Use it to report internal program information for debugging purposes.
        /// Normally the sending of this report type is controlled by internal status variables
        /// that are set by sending backdoor commands to the program.
        /// </summary>
        /// <param name="text">Application defined text</param>
        public static void SendProgramInfoReport(string text)
        {
            byte success;
            Library.SwreC_SendProgramInfoReport(Encoding.UTF8.GetBytes(text + char.MinValue),
                                                out success);
            if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }
    }
}
