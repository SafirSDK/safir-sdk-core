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
#ifndef __DOUF_ACE_TIME_CONVERTER_H
#define __DOUF_ACE_TIME_CONVERTER_H

#include <ace/Time_Value.h>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <Safir/Dob/Typesystem/Defs.h>

#if defined _MSC_VER
#  ifdef DOUF_TIME_CPP_EXPORTS
#    define DOUF_TIME_CPP_API __declspec(dllexport)
#  else
#    define DOUF_TIME_CPP_API __declspec(dllimport)
#    ifdef _DEBUG
#      pragma comment( lib, "douf_time_cppd.lib" )
#    else
#      pragma comment( lib, "douf_time_cpp.lib" )
#    endif
#  endif
#elif defined __GNUC__
#  define DOUF_TIME_CPP_API
#endif

namespace Safir
{
namespace Time
{

    /**
     * The AceTimeConverter class provides functions to convert to/from ACE time.
     */
    class DOUF_TIME_CPP_API AceTimeConverter
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

}; // namespace Time
}; // namespace Safir

#endif //__DOUF_ACE_TIME_CONVERTER_H
