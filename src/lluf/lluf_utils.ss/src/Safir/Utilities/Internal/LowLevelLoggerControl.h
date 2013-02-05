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
#ifndef __LLUF_LOW_LEVEL_LOGGER_CONTROL_H__
#define __LLUF_LOW_LEVEL_LOGGER_CONTROL_H__

#include <Safir/Utilities/Internal/UtilsExportDefs.h>
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/filesystem/path.hpp>

namespace Safir
{
namespace Utilities
{
namespace Internal
{
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4275)
#pragma warning (disable: 4251)
#endif

    class LLUF_UTILS_API LowLevelLoggerControl:
        private boost::noncopyable
    {
    public:
        LowLevelLoggerControl(const bool openOnly, bool readWrite);

        static const boost::filesystem::path GetLogDirectory();
        
        const int* GetLogLevelPointer() const;

        int LogLevel() const;
        void LogLevel(const int level);
        
        bool UseTimestamps() const;
        void UseTimestamps(const bool enabled);
        
        bool LogToStdout() const;
        void LogToStdout(const bool enabled);

        bool LogToFile() const;
        void LogToFile(const bool enabled);

        bool IgnoreFlush() const;
        void IgnoreFlush(const bool enabled);

        static void WriteIniFile(const int level,
                                 const bool useTimestamps,
                                 const bool toStdout,
                                 const bool toFile,
                                 const bool ignoreFlush);
        static void RemoveIniFile();
    private:
        class Impl;
        boost::shared_ptr<Impl> m_impl;
    };

#ifdef _MSC_VER
#pragma warning (pop)
#endif

}
}
}

#endif

