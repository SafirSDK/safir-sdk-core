/******************************************************************************
*
* Copyright Saab AB, 2013, 2026 (http://safirsdkcore.com)
*
* Created by: Anders Widén
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
#include <Safir/Logging/Log.h>
#include <Safir/Logging/Internal/Interface.h>
#include <Safir/Utilities/Internal/StringEncoding.h>
#include <cstdint>

namespace Safir
{
namespace Logging
{

    using Safir::Utilities::Internal::ToUtf8;
    using Safir::Utilities::Internal::ToUtf16;

    void SendSystemLog(const Severity       severity,
                       const std::wstring&  message)
    {
        SendSystemLog(severity, Local0, message);
    }

    void SendSystemLog(const Severity       severity,
                       const Facility       facility,
                       const std::wstring&  message)
    {
        LoggingC_SendSystemLog(static_cast<std::int32_t>(severity),
                               static_cast<std::int32_t>(facility),
                               ToUtf8(message).c_str());
    }

    namespace Internal
    {
        LOGGING_CPP_API void LogFormattingException(const Severity severity, const std::wstring& fmt, const std::exception& e)
        {
            SendSystemLog(severity,
                          L"Exception while formatting the log message '"
                          + fmt
                          + L": "
                          + ToUtf16(e.what()));

        }
    }
}
}
