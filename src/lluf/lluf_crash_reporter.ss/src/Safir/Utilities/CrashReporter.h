/******************************************************************************
*
* Copyright Saab Systems AB, 2012 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#ifndef __LLUF_CRASH_REPORTER_H__
#define __LLUF_CRASH_REPORTER_H__

#if defined (_WIN32) 
  #if defined(lluf_crash_reporter_EXPORTS)
    #define  LLUF_CRASH_REPORTER_EXPORT __declspec(dllexport)
  #else
    #define LLUF_CRASH_REPORTER_EXPORT __declspec(dllimport)
    #pragma comment(lib , "lluf_crash_reporter.lib")
  #endif 
#else 
 #define LLUF_CRASH_REPORTER_EXPORT
#endif


namespace Safir
{
namespace Utilities
{
class LLUF_CRASH_REPORTER_EXPORT CrashReporter
{
public:
    /** 
     * Initiate the crash reporting functionality.
     *
     * After this has been called crashes will generate a dump under runtime/data/crash_reports.
     * For more info on the dump format, see google breakpad wiki/docs.
     */
    static void Start();

    /**
     * Unregister the crash reporting functionality and clean up
     * resources. Should be called before the application exits.
     *
     * Must NOT be called from within the callback! Deadlock will ensue!
     */
    static void Stop();

    /** dumpPath is ascii only! */
    typedef void (*DumpCallback)(const char* const dumpPath);

    /** 
     * Register a callback that will be called when a dump has occurred. 
     * This can be used to report that a dump has occurred, or maybe to clean up state.
     * Be aware that the application may be in an undefined state at this point...
     */
    static void RegisterCallback(const DumpCallback callback);

    /** 
     * Write a dump immediately
     *
     * This can be used to capture the execution state independently of a crash.
     * Note that the registered dump callbacks will be invoked.
     *
     * Will return false if Start has not been called.
     */
    static bool Dump();
};
}
}
#endif

