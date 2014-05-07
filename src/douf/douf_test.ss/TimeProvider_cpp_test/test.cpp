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

#include <boost/date_time/posix_time/posix_time.hpp>
#include <iostream>
#include <Safir/Time/TimeProvider.h>
#include <boost/date_time/gregorian/greg_date.hpp>
#include <boost/date_time/c_local_time_adjustor.hpp>
#include <iomanip>
#include <time.h>
#include <boost/math/special_functions/round.hpp>

void SetTZ(const std::string& value)
{
#ifdef _WIN32
    _putenv((std::string("TZ=") + value).c_str());
    _tzset();
#else
    setenv("TZ",value.c_str(),1);
    tzset();
#endif
}


boost::posix_time::time_duration get_utc_offset() 
{
    for(;;)
    {
        //do this a rather stupid way, but we want to do it differently from what the douf_time_library does it...
        const boost::posix_time::time_duration offset = 
            boost::posix_time::microsec_clock::local_time() -
            boost::posix_time::microsec_clock::universal_time();
        
        const double seconds = offset.total_microseconds() / 1.0e6;
        const long rounded = static_cast<long>(boost::math::round(seconds));

        //        std::wcout << "test: "<< offset << " (" << rounded << ")" << std::endl;

        //try until we get an even number of minutes
        //(the problem is that the two calls to get the time above
        //might get different times...)
        if (rounded % 60 == 0)
        {
            //round off to seconds
            return boost::posix_time::seconds(rounded);
        }
    }
}

const boost::posix_time::ptime a_ptime(boost::gregorian::date(2010,boost::gregorian::Feb,24), 
                                       boost::posix_time::hours(1)+boost::posix_time::minutes(5));

const Safir::Dob::Typesystem::Si64::Second as_double = Safir::Time::TimeProvider::ToDouble(a_ptime);

bool check_timezone(const std::string& tz, 
                    const boost::posix_time::time_duration& expectedOffset1,
                    const boost::posix_time::time_duration& expectedOffset2)
{
    SetTZ(tz);

    const boost::posix_time::time_duration offset = get_utc_offset();

    const boost::posix_time::ptime as_localtime = Safir::Time::TimeProvider::ToLocalTime(as_double);
    
    if (as_localtime - a_ptime != offset)
    {
        std::wcout << "Safir::Time::TimeProvider::ToLocalTime(double) for timezone " << tz.c_str() << " returned incorrect value" << std::endl;
        std::wcout << "diff: " << as_localtime - a_ptime << std::endl;
        return false;
    }

    if (expectedOffset1 != offset && expectedOffset2 != offset)
    {
        std::wcout << "Unexpected offset for tz " << tz.c_str() << "!" << std::endl;
        std::wcout << "Expected " << expectedOffset1 
                   << " or " << expectedOffset2 
                   << ", but got " << offset << std::endl;
        return false;
    }

    return true;
}

int main()
{

    //correct value calculated with online utc converter.
    if (as_double != 1266973500.0)
    {
        std::wcout << "Safir::Time::TimeProvider::ToDouble(ptime) returned incorrect value" << std::endl;
        return 1;
    }

    const boost::posix_time::ptime back_to_ptime = Safir::Time::TimeProvider::ToPtime(as_double);
    if (back_to_ptime != a_ptime)
    {
        std::wcout << "Safir::Time::TimeProvider::ToPtime(double) returned incorrect value" << std::endl;
        return 1;
    }
    
    const boost::posix_time::ptime as_localtime = Safir::Time::TimeProvider::ToLocalTime(as_double);

    if (as_localtime - a_ptime != get_utc_offset())
    {
        std::wcout << "Safir::Time::TimeProvider::ToLocalTime(double) returned incorrect value" << std::endl;
        std::wcout << "diff: " << as_localtime - a_ptime << std::endl;
        return 1;
    }

    const Safir::Dob::Typesystem::Si64::Second back_to_utc_ptime = Safir::Time::TimeProvider::ToUtcTime(as_localtime);
    if (back_to_utc_ptime != as_double)
    {
        std::wcout << "Safir::Time::TimeProvider::ToUtcTime(ptime) returned incorrect value" << std::endl;
        return 1;
    }

    const Safir::Dob::Typesystem::Si64::Second now_double = Safir::Time::TimeProvider::GetUtcTime();
    const boost::posix_time::ptime now_ptime = boost::posix_time::microsec_clock::universal_time();
    const boost::posix_time::ptime epoch(boost::gregorian::date(1970,1,1));
    const double diff = fabs(now_double - (now_ptime - epoch).total_microseconds()/1.0e6);
    //check that it matches to within 0.1 seconds
    if (diff > 0.1)
    {
        std::wcout << "Safir::Time::TimeProvider::GetUtcTime() returned incorrect value" << std::endl;
        std::wcout << std::setprecision(20) << "Got " << now_double << ", expected " << (now_ptime - epoch).total_microseconds()/1.0e6 << std::endl;
        std::wcout << "Diff = " << diff << std::endl;
        return 1;
    }

    if (!check_timezone("EST5EDT", boost::posix_time::hours(-5), boost::posix_time::hours(-4)) ||
        !check_timezone("PST8PDT", boost::posix_time::hours(-8), boost::posix_time::hours(-7)) ||
        !check_timezone("WAUST-8", boost::posix_time::hours(8), boost::posix_time::hours(8)) ||
        !check_timezone("NZST-12NZDT", boost::posix_time::hours(12),boost::posix_time::hours(13)))
    {
        return 1;
    }

    return 0;
}

