/******************************************************************************
*
* Copyright Saab Systems AB, 2012 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#include <Safir/Utilities/CrashReporter.h>
#include <iostream>
#include <stdlib.h>
#include <string>

#ifdef _MSC_VER
#pragma warning (disable: 4723)
#endif


void callback(const char* const dumpPath)
{
    std::wcout << "callback with dumpPath = '" << dumpPath << "'" << std::endl;
}


int main(int argc, char* argv[])
{

    if (argc != 2)
    {
        std::wcout << "Need one argument" << std::endl;
        return 1;
    }

    const std::string arg(argv[1]);
    Safir::Utilities::CrashReporter::RegisterCallback(callback);
    Safir::Utilities::CrashReporter::Start();

    if (arg == "SIGSEGV")
    {
        int* foo = NULL;
        *foo = 10;
        std::wcout << "Program did not crash! Error" << std::endl;
        return 1;
    }
    else if (arg == "SIGFPE")
    {
        int i = 0;
        std::wcout << 10/i << std::endl;
        std::wcout << "Program did not crash! Error!" << std::endl;
        return 1;
    }
    else if (arg == "SIGILL")
    {
        typedef void(*FUNC)(void);
        unsigned char insn[4] = { 0xff, 0xff, 0xff, 0xff };
        FUNC function = reinterpret_cast<FUNC>((char*)insn);
        function();
        std::wcout << "Program did not crash! Error!" << std::endl;
        return 1;
    }
    else if (arg == "SIGABRT")
    {
        abort();
        std::wcout << "Program did not crash! Error!" << std::endl;
        return 1;
    }

    Safir::Utilities::CrashReporter::Stop();
    std::wcout << "unrecognized argument" << std::endl;
    return 1;
}


