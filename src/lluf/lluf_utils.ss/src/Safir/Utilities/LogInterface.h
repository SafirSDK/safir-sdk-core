/******************************************************************************
*
* Copyright Saab AB, 2013 (http://www.safirsdk.com)
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
#ifndef __LLUF_LOGINTERFACE_H__
#define __LLUF_LOGINTERFACE_H__

#include <Safir/Utilities/Internal/UtilsExportDefs.h>
#include <string>

namespace Safir
{
namespace Utilities
{

/**
 * Services for sending log messages to the native system logging mechanism.
 *
 * There is a simple service for sending a log message given a severity and an arbitrary string.
 * The severity levels conforms to the ones used by the well known syslog format as specified
 * in http://www.ietf.org/rfc/rfc3164.txt.
 *
 * Also, there are services to generate log messages based on the information from the legacy
 * mechanism used in Safir SDK Core.
 *
 */
class LLUF_UTILS_API LogInterface
{
public:

    enum Severity
    {
        Emergency,
        Alert,
        Critical,
        Error,
        Warning,
        Notice,
        Informational,
        Debug
    };

    /**
     * Generate log message.
     *
     * @param [in] severity Severity according to RFC 3164.
     * @param [in] text Log text.
     */
    static void Log(const Severity severity, const std::string& text);


private:
    LogInterface(); //not instantiable
};
}
}

#endif

