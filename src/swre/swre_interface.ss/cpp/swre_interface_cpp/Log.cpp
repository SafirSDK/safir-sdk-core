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
#include <Safir/Logging/Log.h>

#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/SwReports/Internal/Interface.h>

namespace Safir 
{ 
namespace Logging
{

    using Safir::Dob::Typesystem::Utilities::ToUtf8;

    void SendSystemLog(const Safir::Logging::Severity::Enumeration severity,
                       const std::wstring&                         logMsg)
    {
        bool success;
        SwreC_SendSystemLog(severity, ToUtf8(logMsg).c_str(), success);
        if (!success)
        {
            Safir::Dob::Typesystem::LibraryExceptions::Instance().Throw();
        }
    }
}
}
