/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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

#include <Safir/Utilities/Internal/Id.h>
#include <set>
#include <iostream>
#include <iomanip>
int main()
{
    bool result =
        LlufId_Generate64("Object") == 5955188366590963785LL &&
        LlufId_Generate64("Safir.Dob.Typesystem.Exception") == -2177307456017217044LL &&
        LlufId_Generate64("Safir.Dob.Typesystem.FundamentalException") == 6297553511903368764LL &&
        LlufId_Generate64("Safir.Dob.Typesystem.SoftwareViolationException") == -2318636033853590373LL;

    boost::int64_t ored = 0;
    for (int i = 0; i < 100000; ++i)
    {
        const boost::int64_t num = LlufId_GenerateRandom64();

        //never generate 0, 1 or -1
        result &=
            num != 0 &&
            num != -1 &&
            num != 1;

        ored |= num;
    }

    //all bits must have been set at least once!
    result &= (ored == (boost::int64_t)0xffffffffffffffffLL);
    std::wcout << "Bits that were set at least once in all the random numbers: 0x"
               << std::setw(16) << std::hex << ored << std::endl;

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
