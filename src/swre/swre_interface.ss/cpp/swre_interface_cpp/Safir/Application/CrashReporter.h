/******************************************************************************
*
* Copyright Saab AB, 2013 (http://www.safirsdk.com)
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
#ifndef __CRASH_REPORTER_H
#define __CRASH_REPORTER_H

#include <Safir/Application/Internal/SwReportExportDefs.h>


namespace Safir
{

/**
Provides methods to start and stop the crash reporting functionality.
*/
namespace Application
{
    /**
     * Start crash reporting.
     *
     * Calling this function will cause google breakpad to be enabled for the current process.
     * This function should be called as early as is humanly possible!
     * Note that StopCrashReporting() must be called before the process exits.
     *
     * You can also use the SwReportStarter RAII class below to get the StartCrashReporting
     * and StopCrashReporting functions to be called automatically.
     */
    SWRE_API void StartCrashReporting();

    /**
     * Stop crash reporting
     *
     * This needs to be called before exiting an application to stop crash reporting if
     * it has been started.
     *
     * You can also use the SwReportStarter RAII class below to get the EnableCrashReporting
     * and Stop functions to be called automatically.
     */
    SWRE_API void StopCrashReporting();


    /**
     * RAII class to call StartCrashReporting and StopCrashReporting automatically.
     *
     * Use this class at "program scope" in order to start the crash reporting functionality
     * as early as possible in your program.
     */
    class ScopedCrashReporting
    {
    public:

        ScopedCrashReporting()
        {
            StartCrashReporting();
        }

        ~ScopedCrashReporting()
        {
            StopCrashReporting();
        }
    };

}
}

#endif
