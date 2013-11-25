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
#ifndef __DOUF_ACE_TIME_CONVERTER_H
#define __DOUF_ACE_TIME_CONVERTER_H

#include <ace/Time_Value.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Time/TimeProvider.h>

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4127)
#endif

#include <boost/date_time/posix_time/posix_time.hpp>

//and enable the warnings again
#if defined _MSC_VER
  #pragma warning (pop)
#endif

namespace Safir
{
namespace Time
{

    /**
     * The AceTimeConverter class provides functions to convert to/from ACE time.
     */
    class AceTimeConverter
    {
    public:

        /**
         * Get specified time in ACE Time representation
         *
         * @param [in] time - The time
         * @return The time in ACE_Time_Value
         */
        static ACE_Time_Value ToAceTime(const Safir::Dob::Typesystem::Si64::Second time);

        /**
         * Get specified boost time duration in ACE Time representation
         *
         * @param [in] duration - The time duration
         * @return The time in ACE_Time_Value
         */
        static ACE_Time_Value ToAceTime(const boost::posix_time::time_duration & duration);

        /**
         * Get specified boost ptime in ACE Time representation
         *
         * @param [in] time - The time
         * @return The time in ACE_Time_Value
         */
        static ACE_Time_Value ToAceTime(const boost::posix_time::ptime & time);

        /**
         * Convert specified ACE time to a Double
         *
         * @param [in] time - The ACE time
         * @return Seconds and fraction since jan 1 1970 00:00
         */
        static Safir::Dob::Typesystem::Si64::Second ToDouble(const ACE_Time_Value & time);

        /**
         * Get specified ACE time in boost::posix_time::ptime representation
         *
         * @param [in] utcTime - The UTC time
         * @return The UTC time stored in a boost::posix_time::ptime object
         */
        static boost::posix_time::ptime ToPtime(const ACE_Time_Value & utcTime);

    };

    inline ACE_Time_Value AceTimeConverter::ToAceTime(const boost::posix_time::time_duration & duration)
    {
        if (boost::posix_time::time_duration::num_fractional_digits() == 6)
        {
            return ACE_Time_Value(static_cast<time_t>(duration.total_seconds()),
                                  static_cast<suseconds_t>(duration.fractional_seconds()));
        }
        else
        {
            return ACE_Time_Value(static_cast<time_t>(duration.total_seconds()),
                                  static_cast<suseconds_t>(duration.fractional_seconds()
                                  * pow(1.0,  boost::posix_time::time_duration::num_fractional_digits() -6)));
        }
    }

    inline ACE_Time_Value AceTimeConverter::ToAceTime(const Safir::Dob::Typesystem::Si64::Second time)
    {
        return ACE_Time_Value(static_cast<suseconds_t>(time),
            static_cast<suseconds_t>((time - Safir::Dob::Typesystem::Int64(time)) * pow (10.0,6)));
    }

    inline ACE_Time_Value AceTimeConverter::ToAceTime(const boost::posix_time::ptime & time)
    {
        return ToAceTime(TimeProvider::ToDouble(time));
    }

    inline Safir::Dob::Typesystem::Si64::Second AceTimeConverter::ToDouble(const ACE_Time_Value & time)
    {
        return time.sec() + (time.usec() / pow (10.0,6));
    }

    inline boost::posix_time::ptime AceTimeConverter::ToPtime(const ACE_Time_Value & time)
    {
        return TimeProvider::ToPtime(ToDouble(time));
    }


}; // namespace Time
}; // namespace Safir

#endif //__DOUF_ACE_TIME_CONVERTER_H
