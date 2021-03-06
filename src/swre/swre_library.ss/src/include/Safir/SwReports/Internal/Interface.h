/******************************************************************************
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
#ifndef __SWRE_LIBRARY_H__
#define __SWRE_LIBRARY_H__

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef swre_library_EXPORTS
#  define SWRE_LIBRARY_API SAFIR_HELPER_DLL_EXPORT
#else
#  define SWRE_LIBRARY_API SAFIR_HELPER_DLL_IMPORT
#endif
#define SWRE_LIBRARY_LOCAL SAFIR_HELPER_DLL_LOCAL

#include <Safir/Dob/Typesystem/LanguageInterfaceDefs.h>

extern "C"
{
    /**
     * Start reception of trace on/off commands
     *
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void SwreC_StartTraceBackdoor(const char * const connectionNameCommonPart,
                                                   const char * const connectionNameInstancePart,
                                                   bool &             success);

    /**
     * Stop reception of trace on/off commands
     */
    SWRE_LIBRARY_API void SwreC_StopTraceBackdoor();

    /**
     * Start crash reporting.
     *
     * Calling this function will cause google breakpad to be enabled for the current process.
     * This function should be called as early as is humanly possible!
     * Note that SwreC_StopCrashReporting must be called before the process exits.
     *
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void SwreC_StartCrashReporting(bool & success);

    /**
     * Stop crash reporting.
     *
     * Must be called before the process exits.
     */
    SWRE_LIBRARY_API void SwreC_StopCrashReporting();

    /**
     * @name Trace logging buffer manipulation routines.
     */
    /** @{ */

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
    SwreC_TraceAppendString(const DotsC_Int64 prefixId,
                            const char * const str,
                            bool & success);

    /**
     * Add characters to trace buffer.
     *
     * The characters str[offset] to str[offset + length - 1] will be added to the output buffer.
     *
     * @param prefixId [in] - Identity of the prefix to put at the start of the line.
     * @param str [in] - The string to append to the buffer.
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void
    SwreC_TraceAppendSubstring(const DotsC_Int64 prefixId,
                               const char * const str,
                               const DotsC_Int32 offset,
                               const DotsC_Int32 length,
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
    SwreC_TraceAppendChar(const DotsC_Int64 prefixId,
                          const char ch,
                          bool & success);

    /**
     * Add wide character to trace buffer.
     *
     * The character will be added to the output buffer.
     *
     * @param prefixId [in] - Identity of the prefix to put at the start of the line.
     * @param ch [in] - The character to append to the buffer.
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void
    SwreC_TraceAppendWChar(const DotsC_Int64 prefixId,
                           const wchar_t ch,
                           bool & success);
    
    /**
     * Flush the buffer to output.
     *
     * @param success [out] - True if an exception has occurred. Call LibraryExceptions.Throw if it was true!
     */
    SWRE_LIBRARY_API void SwreC_TraceFlush(bool & success);

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

