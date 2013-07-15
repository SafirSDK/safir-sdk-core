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
#ifndef __SWRE_LOG_CREATOR_H__
#define __SWRE_LOG_CREATOR_H__

#include <boost/shared_ptr.hpp>
#include <string>

// forward declaration
namespace Safir{namespace Utilities{namespace Internal{class ConfigReader;}}}

namespace Safir
{
namespace SwReports
{
namespace Internal
{

class LogCreator
{
public:

    LogCreator();
    ~LogCreator();

    std::wstring CreateSystemLog(const std::wstring& logMsg) const;



    // The following methods are related to the now deprecated swre reports

    std::wstring CreateFatalErrorLog(const std::wstring& errorCode,
                                     const std::wstring& location,
                                     const std::wstring& text) const;


    std::wstring CreateErrorLog(const std::wstring& errorCode,
                                const std::wstring& location,
                                const std::wstring& text) const;

    std::wstring CreateResourceLog(const std::wstring& resourceId,
                                   const bool          allocated,
                                   const std::wstring& text) const;

    std::wstring CreateProgrammingErrorLog(const std::wstring& errorCode,
                                           const std::wstring& location,
                                           const std::wstring& text) const;

    std::wstring CreateProgramInfoLog(const std::wstring& text) const;

private:

    std::wstring GetPrefix() const;

    boost::shared_ptr<Safir::Utilities::Internal::ConfigReader> m_configReader;

    bool m_includeTimestamp;

};


}
}
}
#endif

