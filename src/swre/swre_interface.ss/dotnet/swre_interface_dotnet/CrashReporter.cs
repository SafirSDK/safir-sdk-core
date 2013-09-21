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
using System;

namespace Safir.Application
{
    /// <summary>
    /// Provides methods to start and stop the crash reporting functionality.
    /// </summary>
    public class CrashReporter
    {
        /// <summary>
        /// Start crash reporting.
        /// <para/>
        /// Calling this function will cause google breakpad to be enabled for the current process.
        /// This function should be called as early as is humanly possible!
        /// Note that Stop() must be called before the process exits.
        /// <summary>
        public static void Start()
        {
            byte success;
            Safir.SwReports.Library.SwreC_StartCrashReporting(out success);
            if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Stop crash reporting
        /// <para/>
        /// This needs to be called before exiting an application to stop crash reporting if
        /// it has been started.
        /// </summary>
        public static void Stop()
        {
            Safir.SwReports.Library.SwreC_StopCrashReporting();
        }
    }
}
