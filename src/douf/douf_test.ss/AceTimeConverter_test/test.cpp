/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safirsdkcore.com)
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

#include <Safir/Time/AceTimeConverter.h>
#include <Safir/Time/TimeProvider.h>
#include <ace/Time_Value.h>
#include <boost/date_time/gregorian/greg_date.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <iostream>

int main(int, char*[])
{
    const boost::posix_time::ptime a_ptime(boost::gregorian::date(2010,boost::gregorian::Feb,24), 
                                           boost::posix_time::hours(1)+boost::posix_time::minutes(5));

    const double as_double = Safir::Time::TimeProvider::ToDouble(a_ptime);

    //correct value calculated with online utc converter.
    if (as_double != 1266973500.0)
    {
        std::wcout << "Safir::Time::TimeProvider::ToDouble(ptime) returned incorrect value" << std::endl;
        return 1;
    }

    const ACE_Time_Value from_double = Safir::Time::AceTimeConverter::ToAceTime(as_double);
    const ACE_Time_Value from_ptime = Safir::Time::AceTimeConverter::ToAceTime(a_ptime);

    if (from_double != from_ptime)
    {
        std::wcout << "Safir::Time::AceTimeConverter::ToAceTime(double) and Safir::Time::AceTimeConverter::ToAceTime(ptime) returned different values" << std::endl;
        return 1;
    }

    const double back_to_double = Safir::Time::AceTimeConverter::ToDouble(from_ptime);
    if (back_to_double != as_double)
    {
        std::wcout << "Safir::Time::AceTimeConverter::ToDouble(ACE_Time_Value) returned an incorrect value" << std::endl;
        return 1;
    }

    const boost::posix_time::ptime back_to_ptime = Safir::Time::AceTimeConverter::ToPtime(from_double);
    if (back_to_ptime != a_ptime)
    {
        std::wcout << "Safir::Time::AceTimeConverter::ToPtime(ACE_Time_Value) returned an incorrect value" << std::endl;
        return 1;
    }


    const boost::posix_time::time_duration a_duration = boost::posix_time::millisec(123);
    const ACE_Time_Value ace_duration = Safir::Time::AceTimeConverter::ToAceTime(a_duration);
    if (a_duration.total_microseconds() != ace_duration.usec())
    {
        std::wcout << "Safir::Time::AceTimeConverter::ToAceTime(posix_time::duration) returned an incorrect value" << std::endl;
        return 1;
    }
    return 0;
}

