/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
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
#pragma once

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef douf_time_cpp_EXPORTS
#  define DOUF_TIME_CPP_API SAFIR_HELPER_DLL_EXPORT
#else
#  define DOUF_TIME_CPP_API SAFIR_HELPER_DLL_IMPORT
#  define SAFIR_LIBRARY_NAME "douf_time_cpp"
#  include <Safir/Utilities/Internal/AutoLink.h>
#endif
#define DOUF_TIME_CPP_LOCAL SAFIR_HELPER_DLL_LOCAL

#include <Safir/Dob/Typesystem/Defs.h>

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4127)
#endif

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time/gregorian/greg_date.hpp>
#include <boost/date_time/time_duration.hpp>

//and enable the warnings again
#if defined _MSC_VER
  #pragma warning (pop)
#endif

namespace Safir
{
namespace Time
{
    /**
     * The Time class contains functions to operate on time.
     */
    class TimeProvider
    {
    public:

        /**
         * Get current UTC time.
         *
         * @return Seconds and fraction since jan 1 1970 00:00
         */
        static DOUF_TIME_CPP_API Safir::Dob::Typesystem::Si64::Second GetUtcTime();

        /**
         * Convert from UTC to local time
         *
         * @param [in] utcTime - The time in UTC
         * @return The local time
         */
        static boost::posix_time::ptime ToLocalTime(const Safir::Dob::Typesystem::Si64::Second utcTime)
        {
            return ToPtime(utcTime + GetLocalTimeOffset());
        }


        /**
         * Convert local time to UTC time
         *
         * @param [in] localTime - The local time
         * @return Seconds and fraction since jan 1 1970 00:00
         */
        static Safir::Dob::Typesystem::Si64::Second ToUtcTime(const boost::posix_time::ptime & localTime)
        {
            boost::posix_time::time_duration duration = localTime - Epoch();
            Dob::Typesystem::Int64 seconds = duration.total_seconds();
            Dob::Typesystem::Float64 fraction = duration.fractional_seconds() / pow(10.0,duration.num_fractional_digits());

            return seconds + fraction - GetLocalTimeOffset();
        }

        /**
         * Get specified UTC time in boost::posix_time::ptime representation
         *
         * @param [in] utcTime - The UTC time
         * @return The UTC time stored in a boost::posix_time::ptime object
         */
        static boost::posix_time::ptime ToPtime(const Safir::Dob::Typesystem::Si64::Second utcTime)
        {
            Dob::Typesystem::Int64 sec;
            Dob::Typesystem::Float64 fraction;

            sec = (Dob::Typesystem::Int64)utcTime;
            fraction = utcTime - sec;

            long long hour    = sec/3600;
            long long minutes = (sec - hour*3600)/60;
            long long seconds = sec - hour*3600 - minutes*60;

            // Return the duration since 1970-Jan-01
            boost::posix_time::time_duration duration((long)hour, (long)minutes, (long)seconds,
                                                      Dob::Typesystem::Int64(fraction * pow(10.0,boost::posix_time::time_duration::num_fractional_digits()) + 0.5));

            return Epoch() + duration;
        }

        /**
         * Convert specified UTC time to a Double
         *
         * @param [in] utcTime - The UTC time
         * @return Seconds and fraction since jan 1 1970 00:00
         */
        static Safir::Dob::Typesystem::Si64::Second ToDouble(const boost::posix_time::ptime & utcTime)
        {
            boost::posix_time::time_duration d = utcTime - Epoch();
            return(double)d.ticks() / d.ticks_per_second();
        }

    private:
        /**
         * Get the epoch for all the calculations, in boost::posix_time format.
         *
         * This is not a class constant, since there is no easy way to initialize a static
         * constant in a thread-safe way.
         */
        static inline boost::posix_time::ptime Epoch()
        {
            return boost::posix_time::ptime(boost::gregorian::date(1970,boost::date_time::Jan,1));
        }

        static DOUF_TIME_CPP_API Safir::Dob::Typesystem::Int32 GetLocalTimeOffset();
    };

} // namespace Time
} // namespace Safir


