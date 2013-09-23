/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
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
#include <boost/function.hpp>

//load some system library and call a function in it.
int main()
{
    try
    {
        Safir::Utilities::DynamicLibraryLoader lib;
#if defined(linux) || defined(__linux) || defined(__linux__)
        lib.Load("m",true); //math library
        double result = lib.GetFunction<double(double)>("cos")(0.0);
            
        if (result != 1.0)
        {
            std::wcout << "The loaded function seems to work incorrectly" << std::endl;
            return 1;
        }
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        lib.Load("Ws2_32",true); //winsock library
       
        const unsigned short result = 
            lib.GetFunction<unsigned short __stdcall (unsigned short)>("htons")(0xff00);
            
        if (result != 0xff)
        {
            std::wcout << "The loaded function seems to work incorrectly" << std::endl;
            return 1;
        }
#else
#  error You need to implement a DynamicLibraryLoader_test for this platform!
#endif

        
    }
    catch(const std::exception& e)
    {
        std::wcout << "Caught an exception:\n" << e.what() << std::endl;
        return 1;
    }
    return 0;
}

