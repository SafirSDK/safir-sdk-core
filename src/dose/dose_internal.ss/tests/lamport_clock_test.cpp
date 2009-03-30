/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#include <Safir/Dob/Internal/LamportClocks.h>
#include <Safir/Dob/Internal/Atomic.h>
#include <iostream>

using namespace Safir::Dob::Internal;

int main()
{
    LamportClock clock;

    for (int i = 0; i < 10; ++i)
    {
        std::wcout << "GetNewTimestamp: " << clock.GetNewTimestamp() << std::endl;
    }

    const LamportTimestamp a = clock.GetNewTimestamp();
    std::wcout << "a = " << a << std::endl;

    for (unsigned long i = 0; i <0x0fffffff;++i)
    {
        clock.GetNewTimestamp();
    }
    const LamportTimestamp b = clock.GetNewTimestamp();
    std::wcout << "b = " << b << std::endl;


    for (unsigned long i = 0; i <0x7ffffff0;++i)
    {
        clock.GetNewTimestamp();
    }

    const LamportTimestamp c = clock.GetNewTimestamp();
    std::wcout << "c = " << c << std::endl;

    for (unsigned long i = 0; i <0x7fffff00;++i)
    {
        clock.GetNewTimestamp();
    }

    const LamportTimestamp d = clock.GetNewTimestamp();
    std::wcout << "d = " << d << std::endl;

    std::wcout << std::boolalpha;
    std::wcout << "a < b (expect true): " << (a < b) << std::endl;
    std::wcout << "b < a (expect false): " << (b < a) << std::endl;

    std::wcout << "a < c (expect false): " << (a < c) << std::endl;
    std::wcout << "c < a (expect true): " << (c < a) << std::endl;

    std::wcout << "b < c (expect true): " << (b < c) << std::endl;
    std::wcout << "c < b (expect false): " << (c < b) << std::endl;

    std::wcout << "a < d (expect true): " << (a < d) << std::endl;
    std::wcout << "d < a (expect false): " << (d < a) << std::endl;

    std::wcout << "c < d (expect true): " << (c < d) << std::endl;
    std::wcout << "d < c (expect false): " << (d < c) << std::endl;

    return 0;
}

