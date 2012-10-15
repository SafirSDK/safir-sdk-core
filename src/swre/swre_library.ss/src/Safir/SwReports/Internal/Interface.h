/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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
#ifndef __SWRE_LIBRARY_H__
#define __SWRE_LIBRARY_H__

#include <Safir/Dob/Typesystem/Internal/KernelDefs.h>

#if defined _MSC_VER
    #ifdef SWRE_LIBRARY_EXPORTS
        #define SWRE_LIBRARY_API __declspec(dllexport)
    #else
        #define SWRE_LIBRARY_API __declspec(dllimport)
        #pragma comment( lib, "swre_library.lib" )
    #endif
#elif defined __GNUC__
    #define SWRE_LIBRARY_API
    #define __cdecl
#endif

extern "C"
{
    /**
     * Initialize the SWRE library with the program name.
     *
     * The SWRE library needs the program name of the executable
     * that it runs in.
     * If this method is not called SWRE will not be able to use a correct connection
     * name for its DOB connection ("Program name not set" will be used instead).
     * Everything else will still function correctly, though.
     *
     * @param programName [in] - The name of the program. In C++ and C# this will likely be argv[0]
     *                           and in Java it will be the name of the main class. This parameter
     *                           is used as the connection name of the SWRE library connection.
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void SwreC_SetProgramName(const char * const programName,
                                               bool & success);

    /**
     * Enable crash reporting.
     *
     * Calling this function will cause google breakpad to be enabled for the current process.
     * This function should be called as early as is humanly possible!
     * Note that SwreC_Stop must be called before the process exits.
     *
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void SwreC_EnableCrashReporting(bool & success);

    /**
     * Stop the swre library.
     */
    SWRE_LIBRARY_API void SwreC_Stop();

    /**
     * @name Trace logging buffer manipulation routines.
     */
    /** @{ */

#if 0 //not implemented
    /**
     * Add characters to trace buffer.
     *
     * The string will be added to the output buffer.
     *
     * @param str [in] - The string to append to the buffer.
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void SwreC_TraceAppendString(const char * const str,
                                                  bool & success);
#endif

    /**
     * Add characters to trace buffer.
     *
     * The string will be added to the output buffer.
     *
     * @param prefixId [in] - Identity of the prefix to put at the start of the line.
     * @param str [in] - The string to append to the buffer.
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void
      SwreC_TraceAppendStringPrefix(const DotsC_Int64 prefixId,
                                    const char * const str,
                                    bool & success);
#if 0 //not implemented
    /**
     * Add character to trace buffer.
     *
     * The character will be added to the output buffer.
     *
     * @param ch [in] - The character to append to the buffer.
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void SwreC_TraceAppendChar(const char ch,
                                                bool & success);
#endif

    /**
     * Add character to trace buffer.
     *
     * The character will be added to the output buffer.
     *
     * @param prefixId [in] - Identity of the prefix to put at the start of the line.
     * @param ch [in] - The character to append to the buffer.
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void
      SwreC_TraceAppendCharPrefix(const DotsC_Int64 prefixId,
                                  const char ch,
                                  bool & success);

    /**
     * Add character to trace buffer.
     *
     * The character will be added to the output buffer.
     *
     * @param prefixId [in] - Identity of the prefix to put at the start of the line.
     * @param ch [in] - The character to append to the buffer.
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void
      SwreC_TraceAppendWcharPrefix(const DotsC_Int64 prefixId,
                                   const wchar_t ch,
                                   bool & success);

    /**
     * Flush the buffer to output if more than 0.5 seconds have passed since buffer
     * became not empty.
     *
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void SwreC_TraceSyncBuffer(bool & success);

    /**
     * Flush the buffer to output.
     *
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void SwreC_TraceFlushBuffer(bool & success);

    /** @} */

    /**
     * @name Trace logging prefix handling.
     */
    /** @{ */

    /**
     * Add a trace logging prefix.
     *
     * If the prefix already exists the id of the existing prefix is returned.
     *
     * @param prefix [in] - The prefix to add.
     * @param prefixId [out] - The identity of the prefix.
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void
      SwreC_TracePrefixAdd(const char * const prefix,
                           DotsC_Int64 & prefixId,
                           bool & success);

    /**
     * Set whether a prefix is enabled or not.
     *
     * @param id [in] - The id of the prefix to modify.
     * @param enabled [in] - The state to set the prefix to
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void
      SwreC_TracePrefixSetEnabled(const DotsC_Int64 id,
                                  const bool enabled,
                                  bool & success);

    /**
     * Check the state of a prefix.
     *
     * @param id [in] - The id of the prefix to check.
     * @return The state of the prefix.
     */
    SWRE_LIBRARY_API bool
      SwreC_TracePrefixIsEnabled(const DotsC_Int64 id);

    /**
     * Get a direct pointer to the state of a prefix.
     *
     * This can be used to avoid a function call for each check of a prefix.
     *
     * @param id [in] - The id of the prefix to check.
     * @return A pointer to the boolean that describes the state of the prefix.
     */
    SWRE_LIBRARY_API volatile bool *
      SwreC_TracePrefixGetIsEnabledPointer(const DotsC_Int64 id);

    /** @} */

    /**
     * Send a Fatal Error software report.
     *
     * @param errorCode [in] - Application defined error code.
     * @param location [in] - Source code location.
     * @param text [in] - Application defined text.
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void SwreC_SendFatalErrorReport(const char * const errorCode,
                                                     const char * const location,
                                                     const char * const text,
                                                     bool & success);

    /**
     * Send an Error software report.
     *
     * @param errorCode [in] - Application defined error code.
     * @param location [in] - Source code location.
     * @param text [in] - Application defined text.
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void SwreC_SendErrorReport(const char * const errorCode,
                                                const char * const location,
                                                const char * const text,
                                                bool & success);

    /**
     * Send a Resource software report.
     *
     * @param resourceId [in] - Application defined error code
     * @param allocated [in] - True if the resource is allocated, otherwise false.
     * @param text [in] - Application defined text.
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void SwreC_SendResourceReport(const char * const resourceId,
                                                   bool               allocated,
                                                   const char * const text,
                                                   bool & success);

    /**
     * Send a Programming Error software report.
     *
     * @param errorCode [in] - Application defined error code.
     * @param location [in] - Source code location.
     * @param text [in] - Application defined text.
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void SwreC_SendProgrammingErrorReport(const char * const errorCode,
                                                           const char * const location,
                                                           const char * const text,
                                                           bool & success);

    /**
     * Send a Program Info software report.
     *
     * @param text [in] - Application defined text.
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void SwreC_SendProgramInfoReport(const char * const text,
                                                      bool & success);

}

#endif

