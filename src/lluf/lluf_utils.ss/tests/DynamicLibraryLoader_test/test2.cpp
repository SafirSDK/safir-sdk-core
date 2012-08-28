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

#if defined (_WIN32)
void accessviol(int)
{
    _exit(-11);
}
#endif

int main()
{
#if defined (_WIN32)
    ::signal(SIGSEGV, &accessviol);
#endif

    try
    {
        boost::function<double(int,long,float,double)> fun;
        {
            Safir::Utilities::DynamicLibraryLoader lib;
            lib.Load("test_library",true);
            fun = lib.GetFunction<double(int,long,float,double)>("TestFunction");
            
            if (fun(1,2L,3.1F,4.5) != 1 + 2L + 3.1F + 4.5)
            {
                std::wcout << "The loaded function seems to work incorrectly" << std::endl;
                return 1;
            }
        }
        //this should cause segfault
        fun(1,2L,3.1F,4.5);
    }
    catch(const std::exception& e)
    {
        std::wcout << "Caught an exception:\n" << e.what() << std::endl;
        return 1;
    }
    return 1;
}

