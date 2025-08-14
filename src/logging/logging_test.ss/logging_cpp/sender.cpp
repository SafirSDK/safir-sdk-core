/******************************************************************************
*
* Copyright Saab AB, 2013, 2025 (http://safirsdkcore.com)
*
* Created by: Anders Widén <anders.widen@consoden.se>
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
#include <iostream>

//disable stupid incorrect microsoft warning.
#ifdef _MSC_VER
#pragma warning (disable : 4428)
#endif

int main()
{
    try
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Emergency, L"This is an emergency log. Bryn\u00e4s \u00e4r b\u00e4st!\u2620");
        Safir::Logging::SendSystemLog(Safir::Logging::Alert, L"This is an alert log");
        Safir::Logging::SendSystemLog(Safir::Logging::Critical, L"This is a critical log");
        Safir::Logging::SendSystemLog(Safir::Logging::Error, L"This is an error log");
        Safir::Logging::SendSystemLog(Safir::Logging::Warning, L"This is a warning log");
        Safir::Logging::SendSystemLog(Safir::Logging::Notice, L"This is a notice log");
        Safir::Logging::SendSystemLog(Safir::Logging::Informational, L"This is an informational log");
        Safir::Logging::SendSystemLog(Safir::Logging::Debug, L"This is a debug log");

#if __cpp_lib_format
        Safir::Logging::SendEmergency(L"This is another emergency log. Parameters: {} and {}", 10, L"bertil");
        Safir::Logging::SendAlert(L"This is another alert log. Parameters: {} and {}", 10, L"bertil");
        Safir::Logging::SendCritical(L"This is another critical log. Parameters: {} and {}", 10, L"bertil");
        Safir::Logging::SendError(L"This is another error log. Parameters: {} and {}", 10, L"bertil");
        Safir::Logging::SendWarning(L"This is another warning log. Parameters: {} and {}", 10, L"bertil");
        Safir::Logging::SendNotice(L"This is another notice log. Parameters: {} and {}", 10, L"bertil");
        Safir::Logging::SendInformational(L"This is another informational log. Parameters: {} and {}", 10, L"bertil");
        Safir::Logging::SendDebug(L"This is another debug log. Parameters: {} and {}", 10, L"bertil");
#else
        Safir::Logging::SendSystemLog(Safir::Logging::Informational, L"No c++20 format support");
#endif
    }
    catch (const std::exception& e)
    {
        std::wcout << "caught exception: " << e.what() << std::endl;
        return 1;
    }
    catch (...)
    {
        std::wcout << "caught exception" << std::endl;
        return 1;
    }

    return 0;
}


