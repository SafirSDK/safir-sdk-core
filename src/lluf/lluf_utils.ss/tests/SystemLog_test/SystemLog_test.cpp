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
#include <Safir/Utilities/Internal/SystemLog.h>
#include <iostream>

int main()
{
    //using namespace Safir::Utilities::Internal;

    try
    {
        Safir::Utilities::Internal::SystemLog logger;
        logger.Send(Safir::Utilities::Internal::SystemLog::Emergency, L"This is an emergency log");
        logger.Send(Safir::Utilities::Internal::SystemLog::Alert, L"This is an alert log");
        logger.Send(Safir::Utilities::Internal::SystemLog::Critical, L"This is a critical log with \n newline and \t tab");
        logger.Send(Safir::Utilities::Internal::SystemLog::Error, L"This is an error log");
        logger.Send(Safir::Utilities::Internal::SystemLog::Warning, L"This is a warning log with \n newline and \t tab");
        logger.Send(Safir::Utilities::Internal::SystemLog::Notice, L"This is a notice log");
        logger.Send(Safir::Utilities::Internal::SystemLog::Informational, L"This is an informational log with \n newline and \t tab");
        logger.Send(Safir::Utilities::Internal::SystemLog::Debug, L"This is a debug log with \n newline and \t tab");

        SEND_SYSTEM_LOG(Error, << L"This is another error log");
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


