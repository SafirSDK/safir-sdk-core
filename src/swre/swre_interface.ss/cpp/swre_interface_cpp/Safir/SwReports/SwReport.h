/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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
#ifndef __SW_REPORT_H
#define __SW_REPORT_H

#include <Safir/Application/Internal/SwReportExportDefs.h>
#include <string>

namespace Safir
{

/**
Provides methods for sending sofware reports.

There are five predefined report types:

    - Fatal Error Report
    - Error Report
    - Resource Report
    - Program Info report
    - Programming Error Report

See the corresponding method for a description of the intended usage.

Some methods have both an error code and a text parameter (both of type string).
The intended usage is that the error code parameter should be a short mnemonic string
and the text parameter should be a more elaborated description. The mnemonic strings
are preferably defined as Dob parameters.

From the location parameter string it should be easy to identify the exact code location
where the report is generated.

All methods are thread safe.
*/
namespace SwReports
{
    /**
     * Clean up SwReport resources.
     * 
     * This needs to be called before exiting an application to let SwReports stop
     * its background thread if it has been started and to stop crash reporting if
     * it has been enabled. Failure to do this may cause problems if the
     * thread is currently using its dob connection when it gets killed.
     *
     * You can also use the SwReportStarter RAII class below to get the EnableCrashReporting 
     * and Stop functions to be called automatically.
     */
    SWRE_API void Stop();

    /**
     * Enable crash reporting.
     *
     * Calling this function will cause google breakpad to be enabled for the current process.
     * This function should be called as early as is humanly possible!
     * Note that Stop() must be called before the process exits.
     *
     * You can also use the SwReportStarter RAII class below to get the EnableCrashReporting
     * and Stop functions to be called automatically.
     */
    SWRE_API void EnableCrashReporting();

    /** RAII class to call EnableCrashReporting and Stop automatically. */
    class SwReportStarter
    {
    public:
        /** 
         * If crashReporting is true the EnableCrashReporting function will be called.
         * Otherwise nothing will happen...
         */
        explicit SwReportStarter(const bool crashReporting = true)
        {
            if (crashReporting)
            {
                EnableCrashReporting();
            }
        }

        /** See Stop(). */
        ~SwReportStarter()
        {
            Stop();
        }
    };

    /**
     * Sends a Fatal Error report.
     *
     * Use it to report static conditions that must be fulfilled to be able to start/continue
     * executing the program, for example missing static resources or invalid configuration.
     * Normally the program should not continue to execute.
     *
     * @param [in] errorCode Application defined error code (mnemonic).
     * @param [in] location Source code location.
     * @param [in] text Application defined text.
     */
    SWRE_API void SendFatalErrorReport(const std::wstring&   errorCode,
                                       const std::wstring&   location,
                                       const std::wstring&   text);

    /**
     * Sends an Error report.
     *
     * Use it to report detected runtime errors, for example a message from an external system
     * in an invalid format. Normally the program continues to execute, possibly in a degraded state.
     *
     * @param [in] errorCode Application defined error code (mnemonic).
     * @param [in] location Source code location.
     * @param [in] text Application defined text.
     */
    SWRE_API void SendErrorReport(const std::wstring&   errorCode,
                                  const std::wstring&   location,
                                  const std::wstring&   text);

    /**
     * Sends a Resource report.
     *
     * Use it to report a missing/acquired dynamic resource. Note that it is ok
     * for dynamic resource to be temporary missing which means that a Resource Report
     * should be sent only after a reasonably number of retries to acquire it.
     *
     * @param [in] resourceId Application defined resource id (mnemonic).
     * @param [in] allocated True if the resource is allocated, otherwise false.
     * @param [in] text Application defined text.
     */
    SWRE_API void SendResourceReport(const std::wstring&   resourceId,
                                     bool                  allocated,
                                     const std::wstring&   text);

    /**
     * Sends a Programming Error report.
     *
     * Use it to report programming errors, that is, errors of assert-type.
     * Normally the program should not continue to execute, in that way enabling
     * a redundant program instance to start.
     *
     * @param [in] errorCode Application defined error code (mnemonic).
     * @param [in] location Source code location.
     * @param [in] text Application defined text.
     */
    SWRE_API void SendProgrammingErrorReport(const std::wstring&   errorCode,
                                             const std::wstring&   location,
                                             const std::wstring&   text);

    /**
     * Sends a Programming Info report.
     *
     * Use it to report internal program information for debugging purposes.
     * Normally the sending of this report type is controlled by internal status variables
     * that are set by sending backdoor commands to the program.
     *
     * @param [in] text Application defined text.
     */
    SWRE_API void SendProgramInfoReport(const std::wstring&   text);

};
};

#endif
