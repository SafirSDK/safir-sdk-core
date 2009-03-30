/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#include <Safir/Dob/Typesystem/Internal/Id.h>
#include "md5.h"
#include <boost/random/ranlux.hpp>
#include <boost/limits.hpp>
#include <ctime>
#include <ace/Thread_Mutex.h>
#include <ace/Guard_T.h>
#include <boost/date_time/posix_time/posix_time_types.hpp>


boost::int64_t __cdecl DotsId_Generate64(const char* str)
{
    md5_state_s md5;
    md5_init(&md5);
    md5_append(&md5,reinterpret_cast<const unsigned char*>(str),static_cast<int>(strlen(str)));
    md5_byte_t digest [16];
    md5_finish(&md5,digest);
    return *reinterpret_cast<const boost::int64_t*>(digest);
}


union conglomerate
{
    boost::int64_t i64;
    struct
    {
        boost::int32_t p1;
        boost::int32_t p2;
    } parts;

    //This function is never meant to be called, it should only contain BOOST_STATIC_ASSERTs,
    //that get executed at *compiletime*!
    static void CheckSize()
    {
        BOOST_STATIC_ASSERT(sizeof(conglomerate) == sizeof(boost::int64_t));
    }
};

/** Have tried using
 *     ranlux64_4 straight, but it appears to only generate 48 bits.
 *     ecuyer1988 appears to only generate 31 bits
 *     rand48 generates 31 bits.
 */

boost::ranlux64_4 CreateGenerator()
{
    boost::ranlux64_4 random_generator;

    return random_generator;
}




boost::int64_t __cdecl DotsId_GenerateRandom64()
{
    static boost::ranlux64_4 * random_generator = NULL;
    if (random_generator == NULL)
    {
        static ACE_Thread_Mutex instantiation_lock;
        ACE_Guard<ACE_Thread_Mutex> lck(instantiation_lock);
        if (random_generator == NULL)
        {
            random_generator = new boost::ranlux64_4();

            using namespace boost::posix_time;
            const boost::posix_time::ptime epoch(boost::gregorian::date(2008,1,1));
            const ptime now = microsec_clock::universal_time();
            const time_duration diff = now - epoch;
            random_generator->seed(static_cast<boost::uint32_t>(diff.total_microseconds() % std::numeric_limits<boost::uint32_t>::max()));
        }
    }
    static ACE_Thread_Mutex use_lock;
    ACE_Guard<ACE_Thread_Mutex> lck(use_lock);

    conglomerate num;
    num.parts.p1 = static_cast<boost::int32_t>(0x00000000ffffffffLL & (*random_generator)());
    num.parts.p2 = static_cast<boost::int32_t>(0x00000000ffffffffLL & (*random_generator)());
    return num.i64;
}
