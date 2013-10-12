/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
* 
* Created by: Lars Hagström / stlrha
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

#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <iostream>
#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4702)
#endif
int main(int /*argc*/, char* /*argv*/[])
{
    std::wcout<<"Checking configuration..."<<std::endl;

    try
    {
        DotsC_NumberOfTypeIds();
    }
    catch (const std::exception & exc)
    {

        std::wcout << "Failed with exception description: " << exc.what() << std::endl;
        return 1;
    }
    catch (...)
    {
        std::wcout << "Failed with ... exception." << std::endl;
        return 1;
    }

    std::wcout<<"Success!"<<std::endl;

    return 0;
}

