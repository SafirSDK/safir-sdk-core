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
    void Start();
    void Stop();

    typedef void (*CrashCallback)();

    void RegisterCallback(const CrashCallback callback);
};
}
}
#endif

