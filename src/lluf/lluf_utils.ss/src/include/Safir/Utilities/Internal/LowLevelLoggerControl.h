/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safirsdkcore.com)
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
#pragma once

#include <Safir/Utilities/Internal/UtilsExportDefs.h>
#include <boost/noncopyable.hpp>
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
        LowLevelLoggerControl();

        //returns nullptr if LogDirectory is not set
        const int* GetLogLevelPointer() const;

        const boost::filesystem::path LogDirectory() const
        {return m_logDirectory;}

        //returns 0 if LogDirectory is not set
        int LogLevel() const;

        bool UseTimestamps() const {return m_timestamps;}
        bool IgnoreFlush() const {return m_ignoreFlush;}
    private:
        int m_logLevel;
        bool m_timestamps;
        bool m_ignoreFlush;
        std::string m_logDirectory;
    };

#ifdef _MSC_VER
#pragma warning (pop)
#endif

}
}
}

