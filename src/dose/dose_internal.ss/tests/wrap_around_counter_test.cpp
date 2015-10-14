/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include <Safir/Dob/Internal/WrapAroundCounter.h>
#include <boost/lexical_cast.hpp>

int failures = 0;

#define Check(expr) CheckInternal(expr, __LINE__)

void CheckInternal(bool expr, int line)
{
    if (!expr)
    {
        std::wcout << "Failed check on line " << line << std::endl;
        ++failures;
    }
}

int main()
{
    using namespace Safir::Dob::Internal;
    using namespace Safir::Dob::Typesystem;

    {
        const WrapAroundCounter c0(0);
        const WrapAroundCounter c1(1);
        
        Check(c0.GetCounter() == 0);
        Check(c1.GetCounter() == 1);

        Check(c0 != c1);
        Check(!(c0 == c1));

        const WrapAroundCounter c0x(0);
        Check(c0 == c0x);
        Check(!(c0 != c0x));
    }
    
    {
        const WrapAroundCounter c0;
        WrapAroundCounter c1; ++c1;

        Check(c0.GetCounter() + 1 == c1.GetCounter());
        
        Check(c0 != c1);
        Check(!(c0 == c1));

        WrapAroundCounter c0x;
        Check(c0 == c0x);
        Check(!(c0 != c0x));
        c1.Reset();
        Check(c0.GetCounter() == c1.GetCounter());
    }

    {
        WrapAroundCounter c;
        Check(c.GetCounter() == std::numeric_limits<Int32>::min());
        ++c;
        Check(c.GetCounter() == std::numeric_limits<Int32>::min() + 1);
        --c;
        Check(c.GetCounter() == std::numeric_limits<Int32>::min());
        const WrapAroundCounter ret = c++;
        Check(c.GetCounter() == std::numeric_limits<Int32>::min() + 1);
        Check(ret.GetCounter() == std::numeric_limits<Int32>::min());
        --c;
        --c;
        Check(c.GetCounter() == std::numeric_limits<Int32>::max());
    }

    {
        WrapAroundCounter c(-100);
        Check(c.GetCounter() == -100);
        for(int i = 0; i< 200; ++i)
        {
            ++c;
        }
        Check(c.GetCounter() == 100);
    }

    {
        WrapAroundCounter c(std::numeric_limits<Int32>::max() - 100);
        for(int i = 0; i< 200; ++i)
        {
            ++c;
        }
        Check(c.GetCounter() == std::numeric_limits<Int32>::min() + 99);
    }

    {
        WrapAroundCounter c;
        std::wostringstream ostr;
        ostr << c;
        Check(ostr.str() == boost::lexical_cast<std::wstring>(std::numeric_limits<Int32>::min()));
        ostr.str(L"");
        --c;
        ostr << c;
        Check(ostr.str() == boost::lexical_cast<std::wstring>(std::numeric_limits<Int32>::max()));
    }

    if (failures != 0)
    {
        std::wcout << "Some tests failed!"<< std::endl;
        return 1;
    }

    return 0;
}


