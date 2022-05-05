/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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

#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Utilities/ProcessInfo.h>
#include "md5.h"
#include <boost/limits.hpp>
#include <ctime>
#include <string.h>
#include <limits>
#include <mutex>

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4244)
  #pragma warning (disable : 4127)
  #pragma warning (disable : 4267)
  #pragma warning (disable : 4458)
#endif

#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/random/ranlux.hpp>

//and enable the warnings again
#if defined _MSC_VER
  #pragma warning (pop)
#endif


std::int64_t LlufId_Generate64(const char* str)
{
    md5_state_s md5;
    md5_init(&md5);
    md5_append(&md5,reinterpret_cast<const unsigned char*>(str),static_cast<int>(strlen(str)));
    union  {
      md5_byte_t digest [16];
      struct {
        std::int64_t first64bits;
        std::int64_t second64bits;
      } ints;
    } digest_converter;

    md5_finish(&md5,digest_converter.digest);
    return digest_converter.ints.first64bits;
}


union conglomerate
{
    std::int64_t i64;
    struct
    {
        std::int32_t p1;
        std::int32_t p2;
    } parts;

    //This function is never meant to be called, it should only contain BOOST_STATIC_ASSERTs,
    //that get executed at *compiletime*!
    static void CheckSize()
    {
        BOOST_STATIC_ASSERT(sizeof(conglomerate) == sizeof(std::int64_t));
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


class RandomGenerator
{
public:
    static RandomGenerator& Instance()
    {
        std::call_once(SingletonHelper::m_onceFlag,[]{SingletonHelper::Instance();});
        return SingletonHelper::Instance();
    }

    std::int64_t Generate()
    {
        std::lock_guard<std::mutex> lck(m_lock);

        conglomerate num;
        num.parts.p1 = static_cast<std::int32_t>(0x00000000ffffffffLL & (m_randomGenerator)());
        num.parts.p2 = static_cast<std::int32_t>(0x00000000ffffffffLL & (m_randomGenerator)());
        return num.i64;
    }

private:
    RandomGenerator(const RandomGenerator&) = delete;
    const RandomGenerator& operator=(const RandomGenerator&) = delete;

    RandomGenerator()
    {
        using namespace boost::posix_time;
        const ptime epoch(boost::gregorian::date(2008,1,1));
        const ptime now = microsec_clock::universal_time();
        const time_duration diff = now - epoch;
        const std::uint32_t my_seed =
            (static_cast<std::uint32_t>(diff.total_microseconds()) * Safir::Utilities::ProcessInfo::GetPid())
            % std::numeric_limits<std::uint32_t>::max();
        m_randomGenerator.seed(my_seed);
    }

    ~RandomGenerator()
    {

    }

    boost::ranlux64_4 m_randomGenerator;
    std::mutex m_lock;
    /**
     * This class is here to ensure that only the Instance method can get at the
     * instance, so as to be sure that call_once is used correctly.
     * Also makes it easier to grep for singletons in the code, if all
     * singletons use the same construction and helper-name.
     */
    struct SingletonHelper
    {
    private:
        friend RandomGenerator& RandomGenerator::Instance();

        static RandomGenerator& Instance()
        {
            static RandomGenerator instance;
            return instance;
        }
        static std::once_flag m_onceFlag;
    };

};

std::once_flag RandomGenerator::SingletonHelper::m_onceFlag;


std::int64_t LlufId_GenerateRandom64()
{
    //0, 1, -1, max and min are "reserved" so we loop until we have a really random number...
    std::int64_t result;
    do
    {
       result = RandomGenerator::Instance().Generate();
    }
    while (result == 0 || result == -1 || result == 1
               || result == std::numeric_limits<std::int64_t>::max()
               || result == std::numeric_limits<std::int64_t>::min());

    return result;
}
