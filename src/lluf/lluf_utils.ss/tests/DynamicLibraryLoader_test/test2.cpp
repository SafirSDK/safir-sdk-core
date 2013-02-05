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
#include <Safir/Utilities/DynamicLibraryLoader.h>
#include <iostream>
#include <csignal>

//We want the process to exit with a known exit code when dying by SIGSEGV
#if defined (_WIN32) || defined (_WIN64)
void accessviol(int)
{
    _exit(1234);
}
#endif

int main()
{
    std::wcout << "Starting" << std::endl;
#if defined (_WIN32) || defined (_WIN64)
    std::wcout << "Registering windows signal handler" << std::endl;
    ::signal(SIGSEGV, &accessviol);
    std::wcout << "Signal handler loaded" << std::endl;
#endif

    try
    {
        boost::function<double(int,long,float,double)> fun;
        {
            std::wcout << "About to load library" << std::endl;
            Safir::Utilities::DynamicLibraryLoader lib;
            lib.Load("test_library",true);
            fun = lib.GetFunction<double(int,long,float,double)>("TestFunction");
            
            if (fun(1,2L,3.1F,4.5) != 1 + 2L + 3.1F + 4.5)
            {
                std::wcout << "The loaded function seems to work incorrectly" << std::endl;
                return 1;
            }
        }
        std::wcout << "Library should be unloaded, trying to call function again (should cause segfault)" << std::endl;
        //this should cause segfault
        fun(1,2L,3.1F,4.5);
    }
    catch(const std::exception& e)
    {
        std::wcout << "Caught an exception:\n" << e.what() << std::endl;
        return 1;
    }
    std::wcout << "No segfault was generated! Error!!!" << std::endl;
    return 1;
}

