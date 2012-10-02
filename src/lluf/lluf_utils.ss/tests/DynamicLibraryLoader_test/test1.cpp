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
#include <boost/static_assert.hpp>
#include <iostream>
#include <cmath>

#if defined _MSC_VER
#  if defined (_WIN64)
     // No other cc on win64 available, I think...
#    define ANOTHER_CC 
     const char * fun2_name = "TestFunction2";
#  else //32bit windows
#    define ANOTHER_CC __stdcall
     //stdcall name decoration is a bit messy (22 is the size of all arguments... plus 2 bytes padding)
     const char * fun2_name = "_TestFunction2@24";
     namespace
     {
         BOOST_STATIC_ASSERT(sizeof(short) + 2 + sizeof(int) + sizeof(long) + sizeof(float) + sizeof(double) == 24);
     }
#  endif

#elif defined __GNUC__
#  if defined (__i386)
#    define ANOTHER_CC __attribute__((stdcall))
#  elif defined (__x86_64)
#    define ANOTHER_CC __attribute__((ms_abi))
#  elif defined (__arm__)
#    define ANOTHER_CC
#  endif
const char * fun2_name = "TestFunction2";
#endif


int main()
{
    try
    {
        Safir::Utilities::DynamicLibraryLoader lib;
        lib.Load("test_library",true);
        
        const double res1 = lib.GetFunction<double (int,long,float,double) >("TestFunction")(1,2L,3.1F,4.5);
        if (res1 != 1 + 2L + 3.1F + 4.5)
        {
            std::wcout << "The loaded function seems to work incorrectly" << std::endl;
            return 1;
        }
        
        const double res2 = lib.GetFunction<double ANOTHER_CC (short,int,long,float,double) >(fun2_name)(1,2,3L,4.1F,5.5);
        if (fabs(res2 - (1 + 2 + 3L + 4.1F + 5.5)) > 1e-6)
        {
            std::wcout << "The loaded function 2 seems to work incorrectly" << std::endl;
            return 1;
        }
    }
    catch(const std::exception& e)
    {
        std::wcout << "Caught an exception:\n" << e.what() << std::endl;
        return 1;
    }
    return 0;
}

