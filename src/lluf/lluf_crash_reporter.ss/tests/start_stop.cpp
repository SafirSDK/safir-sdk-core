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

void callback(const char* const dumpPath)
{
    std::wcout << "Callback! dumpPath = " << dumpPath << std::endl;
}

int main()
{
    try 
    {
        //dump should fail when not started.
        if (Safir::Utilities::CrashReporter::Dump())
        {
            return 1;
        }

        Safir::Utilities::CrashReporter::RegisterCallback(callback);
        Safir::Utilities::CrashReporter::Start();

        //expect register after start to fail
        try
        {
            Safir::Utilities::CrashReporter::RegisterCallback(callback);
            return 1;
        }
        catch (...)
        {
        }

        //multiple starts ok
        Safir::Utilities::CrashReporter::Start();

        Safir::Utilities::CrashReporter::Stop();

        //multiple stop ok
        Safir::Utilities::CrashReporter::Stop();

        //no restart
        try
        {
            Safir::Utilities::CrashReporter::Start();
            return 1;
        }
        catch (...)
        {
        }

        return 0;
    }
    catch (...)
    {
        return 1;
    }
}


