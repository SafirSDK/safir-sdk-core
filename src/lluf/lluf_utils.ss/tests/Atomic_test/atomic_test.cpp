/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include <Safir/Utilities/Internal/Atomic.h>
#include <stdlib.h>
#include <boost/static_assert.hpp>
#include <boost/type_traits.hpp>
#include <atomic>


namespace
{
    BOOST_STATIC_ASSERT(std::atomic<boost::uint32_t>::is_always_lock_free);

    BOOST_STATIC_ASSERT(boost::is_same<boost::uint32_t, std::uint32_t>::value);

    //these checks may have to be adjusted when/if we port to a platform with different sizes.
}

void check(const bool expr)
{
    if (!expr)
    {
        exit(1);
    }
}

int main()
{
    {
        Safir::Utilities::Internal::AtomicUint32 atomic;
        check(atomic.value() == 0);

        atomic = 10;
        check(atomic.value() == 10);

        check(10 == atomic++);
        check(atomic.value() == 11);

        check(11 == atomic--);
        check(atomic.value() == 10);

        //exchange a 10 with 20
        check(atomic.compare_exchange(20,10) == 10);
        check(atomic.value() == 20);

        //exchange a 10 with 100 (should fail)
        check(atomic.compare_exchange(100,10) == 20);
        check(atomic.value() == 20);

        check(atomic == 20);
        check(atomic != 10);
    }

    {
        Safir::Utilities::Internal::AtomicUint32 atomic2(100);
        check(atomic2.value() == 100);
        atomic2 = 10;
        check(atomic2.value() == 10);
    }

    return 0;
}
