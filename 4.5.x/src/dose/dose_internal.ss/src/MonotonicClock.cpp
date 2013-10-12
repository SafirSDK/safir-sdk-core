/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / stlrha
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
#include <Safir/Dob/Internal/MonotonicClock.h>

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable: 4244)
#endif

#include <boost/date_time/posix_time/posix_time_types.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
#endif


namespace Safir
{
namespace Dob
{
namespace Internal
{
    static const double fraction_multiplicator = pow(10.0,-boost::posix_time::time_duration::num_fractional_digits());
    const boost::posix_time::ptime epoch(boost::gregorian::date(2008,1,1));

    Typesystem::Int64 MonotonicClock::Get(const Typesystem::Int64 oldTime)
    {
        using namespace boost::posix_time;
        const ptime now = microsec_clock::universal_time();
        const time_duration diff = now - epoch;
        const Typesystem::Int64 newTime = diff.total_microseconds();
        return std::max(newTime,oldTime+1);
    }
}
}
}
