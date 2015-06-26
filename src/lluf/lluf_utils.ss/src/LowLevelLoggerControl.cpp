/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
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
#include <Safir/Utilities/Internal/LowLevelLoggerControl.h>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/fstream.hpp>
#include <iostream>


namespace Safir
{
namespace Utilities
{
namespace Internal
{
    LowLevelLoggerControl::LowLevelLoggerControl() :
		m_logLevel(0),
        m_timestamps(true),
        m_ignoreFlush(false)
    {
        const auto configReader = Safir::Utilities::Internal::ConfigReader();
        m_logLevel = configReader.Logging().get<int>("LowLevelLog.log_level");
        m_timestamps = configReader.Logging().get<bool>("LowLevelLog.show_timestamps");
        m_ignoreFlush = configReader.Logging().get<bool>("LowLevelLog.ignore_flush");
        m_logDirectory = configReader.Logging().get<std::string>("LowLevelLog.log_directory");
    }

    const int* LowLevelLoggerControl::GetLogLevelPointer() const
    {
        if (m_logDirectory.empty())
        {
            return nullptr;
        }
        else
        {
            return &m_logLevel;
        }
    }

    int LowLevelLoggerControl::LogLevel() const
    {
        if (m_logDirectory.empty())
        {
            return 0;
        }
        else
        {
            return m_logLevel;
        }
    }
}
}
}
