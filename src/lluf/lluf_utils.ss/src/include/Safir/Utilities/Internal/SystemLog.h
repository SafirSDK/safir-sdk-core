/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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
#ifndef __LLUF_SYSTEMLOG_H__
#define __LLUF_SYSTEMLOG_H__

#include <Safir/Utilities/Internal/UtilsExportDefs.h>
#include <boost/shared_ptr.hpp>
#include <string>
#include <sstream>


#define SEND_SYSTEM_LOG(severity, comment) \
    {std::wostringstream ostr; ostr comment; Safir::Utilities::Internal::Log::Send(Safir::Utilities::Internal::Log::severity, ostr.str());}

namespace Safir
{
namespace Utilities
{
namespace Internal
{
namespace Log
{
    enum Severity
    {
        Emergency = 0,
        Alert,
        Critical,
        Error,
        Warning,
        Notice,
        Informational,
        Debug
    };

    /**
    * Service for sending log messages to the native system logging mechanism.
    *
    * The service takes a severity and an arbitrary string.
    * The severity levels conforms to the ones used by the well known syslog format as specified
    * in http://www.ietf.org/rfc/rfc3164.txt.
    *
    * @param [in] severity Severity according to RFC 3164.
    * @param [in] text Log text.
    *
    */
    LLUF_UTILS_API void Send(const Severity severity, const std::wstring& text) noexcept;

}
}
}
}

#endif

