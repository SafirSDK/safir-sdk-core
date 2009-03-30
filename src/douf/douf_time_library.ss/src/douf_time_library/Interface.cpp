/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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
#include <string>
#include <ace/DLL.h>
#include <boost/date_time/gregorian/greg_date.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time/time_duration.hpp>
#include <boost/date_time/time_zone_base.hpp>
#include <Safir/Dob/Typesystem/Utilities.h>

#include <Safir/Time/LibraryParameters.h>
#include "Safir/Time/Internal/Interface.h"

const boost::posix_time::ptime _1_JAN_1970 (boost::gregorian::date(1970,boost::date_time::Jan,1));

typedef double (*TIMEUTC)();
typedef int (*TIMEOFFSET)();

TIMEUTC GetTimeUtc;
TIMEOFFSET GetLocalTimeOffset;

boost::shared_ptr<ACE_DLL> timeProviderLibrary(new ACE_DLL);
bool isLibraryLoaded = false;

// **************************************************************
// Loads the library and gets handles to time provider functions
// **************************************************************
void GetLibrary()
{

    // Load the library
    if (0 == timeProviderLibrary->open(Safir::Dob::Typesystem::Utilities::ToUtf8(Safir::Time::LibraryParameters::LibraryName()).c_str()))
    {
        // Get handles to the functions
        GetTimeUtc = reinterpret_cast<TIMEUTC>(timeProviderLibrary->symbol(
            Safir::Dob::Typesystem::Utilities::ToUtf8(Safir::Time::LibraryParameters::UtcTimeFunctionName()).c_str()));
        GetLocalTimeOffset = reinterpret_cast<TIMEOFFSET>(timeProviderLibrary->symbol(
            Safir::Dob::Typesystem::Utilities::ToUtf8(Safir::Time::LibraryParameters::LocalTimeOffsetFunctionName()).c_str()));
    }

    isLibraryLoaded = true;

}

// **************************************************************
// Return current UTC time
// **************************************************************
void DoufTimeC_GetUtcTime(Safir::Dob::Typesystem::Si64::Second & utcTime)
{
    if (!isLibraryLoaded)
    {
        GetLibrary();
    }

    // Get current UTC time from the library, return boost clock if the function not is provided
    if (GetTimeUtc != 0)
    {
        utcTime = GetTimeUtc();
    }
    else
    {
        boost::posix_time::time_duration d = boost::posix_time::microsec_clock::universal_time() - _1_JAN_1970;
        utcTime = (double)d.ticks() / d.ticks_per_second();
    }
}

// **************************************************************
// Return local offset from GMT
// **************************************************************
void DoufTimeC_GetLocalTimeOffset(Safir::Dob::Typesystem::Int32& offset)
{
    if (!isLibraryLoaded)
    {
        GetLibrary();
    }

    // Get local time offset from the library, return a time offset calulated from boost if the function not is provided
    if (GetTimeUtc != 0)
    {
        offset = GetLocalTimeOffset();
    }
    else
    {
        offset = (boost::posix_time::microsec_clock::local_time() - boost::posix_time::microsec_clock::universal_time()).total_seconds();
    }
}
