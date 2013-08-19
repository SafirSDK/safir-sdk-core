/******************************************************************************
*
* Copyright Saab AB, 2008-2012 (http://www.safirsdk.com)
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

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4244)
#endif

#include <boost/date_time/gregorian/greg_date.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time/time_duration.hpp>
#include <boost/date_time/time_zone_base.hpp>
#include <boost/date_time/c_local_time_adjustor.hpp>
#include <boost/thread/once.hpp>
#include <boost/bind.hpp>
#include <boost/function.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Time/Internal/Interface.h>
#include <Safir/Time/LibraryParameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/DynamicLibraryLoader.h>

namespace 
{
    const boost::posix_time::ptime _1_JAN_1970 (boost::gregorian::date(1970,boost::date_time::Jan,1));

    boost::once_flag g_onceFlag = BOOST_ONCE_INIT;

    boost::function<double()> GetUtcTime;
    boost::function<int()> GetLocalTimeOffset;

    bool g_loadFailed = true;
}

// **************************************************************
// Loads the library and gets handles to time provider functions
// **************************************************************
void LoadLibrary()
{
    const std::string libraryName = Safir::Dob::Typesystem::Utilities::ToUtf8(Safir::Time::LibraryParameters::LibraryName());
    const std::string utcTimeFcnName = Safir::Dob::Typesystem::Utilities::ToUtf8(Safir::Time::LibraryParameters::UtcTimeFunctionName());
    const std::string localTimeOffsetFcnName = Safir::Dob::Typesystem::Utilities::ToUtf8(Safir::Time::LibraryParameters::LocalTimeOffsetFunctionName());

    if (libraryName.empty())
    {
        lllout << "No external time provider specified, will use boost::posix_time instead" << std::endl;
        g_loadFailed = false;
        return;
    }

    lllout << "Attempting to load external time provider library " << libraryName.c_str() << std::endl;

    Safir::Utilities::DynamicLibraryLoader lib;

    try 
    {
        lib.Load(libraryName,false);
        //we don't want to loose the functions when the object gets destroyed, so 
        //we pass false to the Load fcn.
    }
    catch(const std::logic_error&)
    {
        lllerr << "Failed to load external time provider library '" 
               << libraryName.c_str() << std::endl;
        return;
    }
    
    lllout << "Library load successful, attempting to load symbols " 
           << utcTimeFcnName.c_str() 
           << " and " 
           << localTimeOffsetFcnName.c_str() << std::endl;
    
    try 
    {
        GetUtcTime = lib.GetFunction<double()>(utcTimeFcnName);
        GetLocalTimeOffset = lib.GetFunction<int()>(localTimeOffsetFcnName);
    }
    catch (const std::logic_error& e)
    {
        lllerr << "Failed to load functions in external time provider library: " << e.what() << std::endl;
        GetUtcTime = NULL;
        GetLocalTimeOffset = NULL;
        return;
    }

    g_loadFailed = false;
}

// **************************************************************
// Return current UTC time
// **************************************************************
void DoufTimeC_GetUtcTime(Safir::Dob::Typesystem::Si64::Second & utcTime, bool& success)
{
    boost::call_once(g_onceFlag,boost::bind(LoadLibrary));
    
    if (g_loadFailed)
    {
        success = false;
        return;
    }

    // Get current UTC time from the library, return boost clock if the function not is provided
    if (GetUtcTime != NULL)
    {
        utcTime = GetUtcTime();
    }
    else
    {
        const boost::posix_time::time_duration d = boost::posix_time::microsec_clock::universal_time() - _1_JAN_1970;
        utcTime = static_cast<double>(d.ticks()) / d.ticks_per_second();
    }

    success = true;
}

// **************************************************************
// Return local offset from GMT
// **************************************************************
void DoufTimeC_GetLocalTimeOffset(Safir::Dob::Typesystem::Int32& offset, bool& success)
{
    boost::call_once(g_onceFlag,boost::bind(LoadLibrary));

    if (g_loadFailed)
    {
        success = false;
        return;
    }

    // Get local time offset from the library, return a time offset calulated from boost if the function not is provided
    if (GetUtcTime != NULL)
    {
        offset = GetLocalTimeOffset();
    }
    else
    {
        using namespace boost::posix_time;

        // boost::date_time::c_local_adjustor uses the C-API to adjust a
        // moment given in utc to the same moment in the local time zone.
        typedef boost::date_time::c_local_adjustor<ptime> local_adj;
        
        const ptime utc_now = second_clock::universal_time();
        const ptime now = local_adj::utc_to_local(utc_now);
        const time_duration diff = now - utc_now;
        offset = diff.total_seconds();

        //total_seconds loses the sign, so we fix that
        if (diff.is_negative())
        {
            offset *= -1;
        }
    }

    success = true;
}
