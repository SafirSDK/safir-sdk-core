/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / stawi
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
#ifndef __SAFIR_CRASH_REPORTER_H__
#define __SAFIR_CRASH_REPORTER_H__

#include <Safir/Application/Internal/SwReportExportDefs.h>

namespace Safir
{
namespace Application
{
    /**
     * Provides methods to start and stop the crash reporting functionality.
     */
    class SWRE_INTERFACE_CPP_API CrashReporter
    {
    public:
        /**
         * Start crash reporter.
         *
         * Calling this function will cause google breakpad to be enabled for the current process.
         * This function should be called as early as is humanly possible!
         * Note that Stop() must be called before the process exits.
         *
         * You can also use the ScopedCrashReporter RAII class below to get the CrashReporter::Start
         * and CrashReporter::Stop functions to be called automatically, even in case of exceptions.
         */
        static void Start();
        
        /**
         * Stop crash reporting
         *
         * This needs to be called before exiting an application to stop crash reporter if
         * it has been started.
         *
         * You can also use the ScopedCrashReporter RAII class below to get the CrashReporter::Start
         * and CrashReporter::Stop functions to be called automatically, even in case of exceptions.
         */
        static void Stop();
    };

    /**
     * RAII class to call StartCrashReporter and StopCrashReporter automatically.
     *
     * Use this class at "program scope" in order to start the crash reporter functionality
     * as early as possible in your program.
     */
    class ScopedCrashReporter
    {
    public:

        ScopedCrashReporter()
        {
            CrashReporter::Start();
        }

        ~ScopedCrashReporter()
        {
            CrashReporter::Stop();
        }
    };

}
}

#endif
