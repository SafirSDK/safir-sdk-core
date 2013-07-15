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
#include "LogCreator.h"
#include <Safir/Time/TimeProvider.h>
#include <Safir/Utilities/Internal/ConfigReader.h>

namespace Safir
{
namespace SwReports
{
namespace Internal
{


std::wstring LogCreator::GetPrefix() const
{
    std::wstring prefix;

    if (m_includeTimestamp)
    {
        prefix = L"[" + boost::posix_time::to_iso_extended_wstring
                 (Safir::Time::TimeProvider::ToPtime(Safir::Time::TimeProvider::GetUtcTime())) + L"]";
    }
    return prefix;
}

//-----------------------------------------------------------------------------
LogCreator::LogCreator()
    : m_configReader(new Safir::Utilities::Internal::ConfigReader),
      m_includeTimestamp(m_configReader->Logging().get<bool>("SYSTEM-LOG.include-timestamp"))
{
}

LogCreator::~LogCreator()
{
}

//-----------------------------------------------------------------------------
std::wstring
LogCreator::CreateSystemLog(const std::wstring& logMsg) const
{
    return GetPrefix() + logMsg;
}

//-----------------------------------------------------------------------------
std::wstring
LogCreator::CreateFatalErrorLog(const std::wstring& errorCode,
                                const std::wstring& location,
                                const std::wstring& text) const
{
    return GetPrefix() + L"FatalError " + errorCode + L"|" + location + L"|" + text;
}

//-----------------------------------------------------------------------------
std::wstring
LogCreator::CreateErrorLog(const std::wstring& errorCode,
                           const std::wstring& location,
                           const std::wstring& text) const
{
    return GetPrefix() + L"Error " + errorCode + L"|" + location + L"|" + text;
}

//-----------------------------------------------------------------------------
std::wstring
LogCreator::CreateResourceLog(const std::wstring& resourceId,
                              bool                allocated,
                              const std::wstring& text) const
{
    std::wstring s = (allocated) ? L"" : L"not ";
    return GetPrefix() + L"Resource" + resourceId + L" is " + s + L"allocated" + L"|" + text;
}

//-----------------------------------------------------------------------------
std::wstring
LogCreator::CreateProgrammingErrorLog(const std::wstring& errorCode,
                                      const std::wstring& location,
                                      const std::wstring& text) const
{
    return GetPrefix() + L"ProgrammingError " + errorCode + L"|" + location + L"|" + text;
}

//-----------------------------------------------------------------------------
std::wstring
LogCreator::CreateProgramInfoLog(const std::wstring& text) const
{
    return GetPrefix() + text;
}

}
}
}
