/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
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
#include <Safir/Time/TimeProvider.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <iostream>

int main()
{
    try
    {
        Safir::Time::TimeProvider::GetUtcTime();
        std::wcout << "GetUtcTime did not throw!" << std::endl;
        return 1;
    }
    catch (const Safir::Dob::Typesystem::ConfigurationErrorException&)
    {
        // expected
    }

    try
    {
        Safir::Time::TimeProvider::ToLocalTime(0.0);
        std::wcout << "ToLocalTime did not throw!" << std::endl;
        return 1;
    }
    catch (const Safir::Dob::Typesystem::ConfigurationErrorException&)
    {
        // expected
    }

    std::wcout << "Success" << std::endl;
    return 0;
}
