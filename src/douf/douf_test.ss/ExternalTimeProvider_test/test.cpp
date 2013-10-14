/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
*
* Created by: Erik Adolfsson / sterad
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
#include <boost/date_time/posix_time/posix_time.hpp>
#include <iostream>

int main()
{
    const Safir::Dob::Typesystem::Si64::Second now = Safir::Time::TimeProvider::GetUtcTime();
    if (now != 10101.0)
    {
        std::wcout << "Safir::Time::TimeProvider::GetUtcTime() returned incorrect value\n"
                   << "Got " << now << " expected " << 10101.0 << std::endl;
        return 1;
    }
    
    const boost::posix_time::ptime as_ptime = Safir::Time::TimeProvider::ToPtime(now);
    const boost::posix_time::ptime local = Safir::Time::TimeProvider::ToLocalTime(now);
    const boost::posix_time::time_duration diff = local - as_ptime;

    if (diff.total_seconds() != 5300 && !diff.is_negative() )
    {
        std::wcout << "Safir::Time::TimeProvider::ToLocalTime() returned incorrect value\n"
                   << "Got " << diff.total_seconds() << " expected " << 5300.0 << "\n"
                   << "is_negative = " << std::boolalpha << diff.is_negative() << " expected false" << std::endl;
        return 1;
    }

    std::wcout << "Success" << std::endl;
    return 0;
}

