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
#ifndef __DOUF_TIME_H
#define __DOUF_TIME_H

#include <Safir/Dob/Typesystem/Defs.h>

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

#if defined _MSC_VER
#  ifdef DOUF_TIME_CPP_EXPORTS
#    define DOUF_TIME_CPP_API __declspec(dllexport)
#  else
#    define DOUF_TIME_CPP_API __declspec(dllimport)
#    ifdef NDEBUG
#      pragma comment( lib, "douf_time_cpp.lib" )
#    else
#      pragma comment( lib, "douf_time_cppd.lib" )
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
     * The Time class entertains functions to operate on time.
     */
    class DOUF_TIME_CPP_API TimeProvider
    {
    public:

        /**
         * Get current UTC time.
         *
         * @return Seconds and fraction since jan 1 1970 00:00
         */
        static Safir::Dob::Typesystem::Si64::Second GetUtcTime();

        /**
         * Convert from UTC to local time
         *
         * @param [in] utcTime - The time in UTC
         * @return The local time
         */
        static boost::posix_time::ptime ToLocalTime(const Safir::Dob::Typesystem::Si64::Second utcTime);

        /**
         * Convert local time to UTC time
         *
         * @param [in] localTime - The local time
         * @return Seconds and fraction since jan 1 1970 00:00
         */
        static Safir::Dob::Typesystem::Si64::Second ToUtcTime(const boost::posix_time::ptime & localTime);

        /**
         * Get specified UTC time in boost::posix_time::ptime representation
         *
         * @param [in] utcTime - The UTC time
         * @return The UTC time stored in a boost::posix_time::ptime object
         */
        static boost::posix_time::ptime ToPtime(const Safir::Dob::Typesystem::Si64::Second utcTime);

        /**
         * Convert specified UTC time to a Double
         *
         * @param [in] utcTime - The UTC time
         * @return Seconds and fraction since jan 1 1970 00:00
         */
        static Safir::Dob::Typesystem::Si64::Second ToDouble(const boost::posix_time::ptime & utcTime);
    };

}; // namespace Time
}; // namespace Safir

#endif //__DOUF_TIME_H
