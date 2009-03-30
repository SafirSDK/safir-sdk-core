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
#include <Safir/Time/AceTimeConverter.h>
#include <Safir/Time/TimeProvider.h>

namespace Safir
{
namespace Time
{
    ACE_Time_Value AceTimeConverter::ToAceTime(const boost::posix_time::time_duration & duration)
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

    ACE_Time_Value AceTimeConverter::ToAceTime(const Safir::Dob::Typesystem::Si64::Second time)
    {
        return ACE_Time_Value(static_cast<suseconds_t>(time),
            static_cast<suseconds_t>((time - Safir::Dob::Typesystem::Int64(time)) * pow (10.0,6)));
    }

    ACE_Time_Value AceTimeConverter::ToAceTime(const boost::posix_time::ptime & time)
    {
        return ToAceTime(TimeProvider::ToDouble(time));
    }

    Safir::Dob::Typesystem::Si64::Second AceTimeConverter::ToDouble(const ACE_Time_Value & time)
    {
        return time.sec() + (time.usec() / pow (10.0,6));
    }

    boost::posix_time::ptime AceTimeConverter::ToPtime(const ACE_Time_Value & time)
    {
        return TimeProvider::ToPtime(ToDouble(time));
    }

}; // namespace Douf
}; // namespace Safir
