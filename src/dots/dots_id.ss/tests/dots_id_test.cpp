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

#include <Safir/Dob/Typesystem/Internal/Id.h>
#include <set>
#include <iostream>
#include <iomanip>
int main()
{
    bool result =
        DotsId_Generate64("Object") == 5955188366590963785LL &&
        DotsId_Generate64("Safir.Dob.Typesystem.Exception") == -2177307456017217044LL &&
        DotsId_Generate64("Safir.Dob.Typesystem.FundamentalException") == 6297553511903368764LL &&
        DotsId_Generate64("Safir.Dob.Typesystem.SoftwareViolationException") == -2318636033853590373LL;

    std::set<boost::int64_t> numbers;
    boost::int64_t ored = 0;
    for (int i = 0; i < 100000; ++i)
    {
        const boost::int64_t num = DotsId_GenerateRandom64();

        std::wcout.fill('0');
        std::wcout << "0x" << std::setw(16) << std::hex << num << std::endl;
        result &=
            num != 0 &&
            num != 100000 &&
            (num & 0xffffffff00000000LL) != 0 &&
            numbers.insert(num).second;
        ored |= num;
    }

    //all bits must have been set at least once!
    result &= (ored == 0xffffffffffffffffLL);
    std::wcout << "Bits that were set at least once in all the random numbers: 0x" << std::setw(16) << std::hex << ored << std::endl;

    if (result)
    {
        std::wcout << "All tests were successful" << std::endl;
        return 0;
    }
    else
    {
        std::wcout << "At least one test failed" << std::endl;
        return 1;
    }
}
