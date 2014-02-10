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

class Singleton
{
public:
    static Singleton& Instance()
    {
        static Singleton inst;
        return inst;
    }

private:
    Singleton()
    {
        Safir::Utilities::Internal::Log::Send(Safir::Utilities::Internal::Log::Alert, L"This is a log from a singleton constructor");
    }

    ~Singleton()
    {
        Safir::Utilities::Internal::Log::Send(Safir::Utilities::Internal::Log::Alert, L"This is a log from a singleton destructor");
    }
};

int main()
{
    try
    {
        Singleton::Instance();

        Safir::Utilities::Internal::Log::Send(Safir::Utilities::Internal::Log::Emergency, L"This is an emergency log");

        Safir::Utilities::Internal::Log::Close();
        // The following logs should still be sent since a Send implicitly opens the log

        Safir::Utilities::Internal::Log::Send(Safir::Utilities::Internal::Log::Alert, L"This is an alert log");
        Safir::Utilities::Internal::Log::Send(Safir::Utilities::Internal::Log::Critical, L"This is a critical log with \n newline and \t tab");
        Safir::Utilities::Internal::Log::Send(Safir::Utilities::Internal::Log::Error, L"This is an error log");
        Safir::Utilities::Internal::Log::Send(Safir::Utilities::Internal::Log::Warning, L"This is a warning log with \n newline and \t tab");
        Safir::Utilities::Internal::Log::Send(Safir::Utilities::Internal::Log::Notice, L"This is a notice log");
        Safir::Utilities::Internal::Log::Send(Safir::Utilities::Internal::Log::Informational, L"This is an informational log with \n newline and \t tab");
        Safir::Utilities::Internal::Log::Send(Safir::Utilities::Internal::Log::Debug, L"This is a debug log with \n newline and \t tab");

        Safir::Utilities::Internal::Log::Close();
        Safir::Utilities::Internal::Log::Open();

        SEND_SYSTEM_LOG(Error, << L"This is another error log");

        Safir::Utilities::Internal::Log::Close();
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


