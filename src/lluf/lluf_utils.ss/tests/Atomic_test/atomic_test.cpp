/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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
#include "../../src/Safir/Utilities/Internal/Atomic.h"
#include <stdlib.h>
#include <boost/static_assert.hpp>
#include <boost/type_traits.hpp>


namespace
{
    //Check that std::atomic<[u]int64_t> and std::atomic<[u]int32_t> are lock free
    BOOST_STATIC_ASSERT(ATOMIC_INT_LOCK_FREE == 2); //int and unsigned int are always lock free
    BOOST_STATIC_ASSERT(ATOMIC_LONG_LOCK_FREE == 2); //long and unsigned long are always lock free

    //now we need to check that boost::uint32_t actually maps to either of the above types
    BOOST_STATIC_ASSERT((boost::is_same<boost::uint32_t, unsigned int>::value || boost::is_same<boost::uint32_t, unsigned long>::value));

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


