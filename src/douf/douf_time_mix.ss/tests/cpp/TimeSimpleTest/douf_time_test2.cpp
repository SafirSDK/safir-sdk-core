/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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

#include <ace/Time_Value.h>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <iostream>
#include <Safir/Time/TimeProvider.h>
#include <Safir/Time/AceTimeConverter.h>
#include <boost/date_time/gregorian/greg_date.hpp>

using namespace boost::date_time;

int main(int argc, char* argv[])
{
    double time;

    printf( "Current local time      - ");

    boost::posix_time::ptime t(boost::posix_time::microsec_clock::local_time());
    std::cout << t << std::endl;
//    std::cout << boost::posix_time::to_simple_string(t) << std::endl;

    printf( "Converted to double     - ");

    time = Safir::Time::TimeProvider::ToDouble(t);
    std::cout << Safir::Time::TimeProvider::ToPtime(time) << std::endl;

    printf( "Current UTC time        - ");

    time = Safir::Time::TimeProvider::GetUtcTime();
    std::cout << Safir::Time::TimeProvider::ToPtime(time) << std::endl;

    printf( "To local time           - ");

    t = Safir::Time::TimeProvider::ToLocalTime(time);
    std::cout << t << std::endl;

    printf( "To ACE                  - ");

    ACE_Time_Value ace_time = Safir::Time::AceTimeConverter::ToAceTime(t);
    time = Safir::Time::AceTimeConverter::ToDouble(ace_time);
    ace_time = Safir::Time::AceTimeConverter::ToAceTime(time);
    std::string s = boost::posix_time::to_simple_string(Safir::Time::AceTimeConverter::ToPtime(ace_time));
    std::cout << s << std::endl;

    printf( "To Utc time             - ");

    time = Safir::Time::TimeProvider::ToUtcTime(t);
    std::cout << Safir::Time::TimeProvider::ToPtime(time) << std::endl;

    printf( "1 Jan 1970              - ");

    const boost::posix_time::ptime _1_JAN_1970 (boost::gregorian::date(1970,Jan,1));
    time = Safir::Time::TimeProvider::ToDouble(_1_JAN_1970);
    std::cout << Safir::Time::TimeProvider::ToPtime(time) << std::endl;

    printf( "1 Feb 1868              - ");

    const boost::posix_time::ptime _1_FEB_1868 (boost::gregorian::date(1868,Feb,1));
    time = Safir::Time::TimeProvider::ToDouble(_1_FEB_1868);
    s = boost::posix_time::to_simple_string(Safir::Time::TimeProvider::ToPtime(time));
    std::cout << s << std::endl;

    printf( "1 Feb 2039              - ");

    const boost::posix_time::ptime _1_FEB_2039 (boost::gregorian::date(2039,Feb,1));
    time = Safir::Time::TimeProvider::ToDouble(_1_FEB_2039);
    s = boost::posix_time::to_simple_string(Safir::Time::TimeProvider::ToPtime(time));
    std::cout << s << std::endl;

    printf( "Boost duration to ACE   - ");

    boost::posix_time::time_duration duration = boost::posix_time::millisec(123);
    ace_time = Safir::Time::AceTimeConverter::ToAceTime(duration);
    if (duration.total_microseconds() == ace_time.usec())
    {
        std::cout << "OK" << std::endl;
    }
    else
    {
        std::cout << "NOK" << std::endl;
    }

    return 0;
}

