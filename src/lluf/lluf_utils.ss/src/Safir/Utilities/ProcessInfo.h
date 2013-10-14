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

#ifndef __PROCESSINFO_H__
#define __PROCESSINFO_H__

#include <Safir/Utilities/Internal/UtilsExportDefs.h>
#include <string>
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>

//Get hold of pid_t

#if defined(linux) || defined(__linux) || defined(__linux__)
#  include <sys/types.h>
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
   typedef int pid_t;
#else
#  error You need to get hold of pid_t for this platform
#endif

#ifdef _MSC_VER
#pragma warning(push) 
#pragma warning(disable: 4275)
#endif

namespace Safir
{
namespace Utilities
{
    class LLUF_UTILS_API ProcessInfo:
        private boost::noncopyable //we probably do not want this to be copyable if it is to be extended in the future.
    {
    public:
        /** Create a ProcessInfo object for a specific process. */
        ProcessInfo(const pid_t pid);

        /** Destructor. */
        ~ProcessInfo();

        /** Returns the pid of the current process. */
        static pid_t GetPid();

        /** This method will probably return argv[0] of the process.
         * At the very least it will return the pid as a string.
         * Be sure not to use this as a unique identifier for processes.
         * For java processes it attempts to find the name of the running jar,
         * and for mono processes it attempts to find the name of the running exe.
         * No guarantees at all!
         */
        const std::string GetProcessName() const;

        /** This method will probably return the command line used to start 
         * the current process.
         * At the very least it will return the pid as a string.
         */
        static const std::string GetProcessDescription();
    private:
        const pid_t m_pid;
    };

}
}

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#endif


