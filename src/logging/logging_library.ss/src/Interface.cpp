/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safirsdkcore.com)
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
#include <Safir/Logging/Internal/Interface.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/StringEncoding.h>
#include <iostream>

void LoggingC_SendSystemLog(const std::int32_t severity,
                            const char * const   message)
{
    try
    {
        Safir::Utilities::Internal::Log::Send
            (static_cast<Safir::Utilities::Internal::Log::Severity>(severity),
             Safir::Utilities::Internal::ToUtf16(message));
    }
    catch (...)
    {
        std::wcerr << "Got an unexpected exception when generating a log. "
                   << "Please tell your nearest Safir SDK Core developer" << std::endl;
    }
}










