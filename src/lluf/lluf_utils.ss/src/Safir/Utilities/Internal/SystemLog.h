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
#ifndef __LLUF_SYSTEMLOG_H__
#define __LLUF_SYSTEMLOG_H__

#include <Safir/Utilities/Internal/UtilsExportDefs.h>
#include <boost/shared_ptr.hpp>
#include <string>

namespace Safir
{
namespace Utilities
{
namespace Internal
{

/** Forward declaration for pimpl idiom */
class SystemLogImpl;

class LLUF_UTILS_API SystemLog
{
public:

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

    SystemLog();
    ~SystemLog();


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
    void Send(const Severity severity, const std::wstring& text);

private:

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251)
#endif

        boost::shared_ptr<SystemLogImpl> m_impl;

#ifdef _MSC_VER
#pragma warning (pop)
#endif


};


}
}
}

#endif

