/******************************************************************************
*
* Copyright Saab AB, 2007-2013,2015 (http://safirsdkcore.com)
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
#include <Safir/Dob/Internal/LamportClocks.h>

#define BOOST_TEST_MODULE LamportClockTest
#include <boost/test/unit_test.hpp>

using namespace Safir::Dob::Internal;

BOOST_AUTO_TEST_CASE(simple_comparison)
{
    LamportClock clock(1000);
    {
        LamportTimestamp last = clock.GetNewTimestamp();
        for (int i = 0; i < 10; ++i)
        {
            const LamportTimestamp next = clock.GetNewTimestamp();
            BOOST_CHECK(last < next);
            BOOST_CHECK(!(next < last));
            BOOST_CHECK(last != next);
            BOOST_CHECK(next != last);
            BOOST_CHECK(!(next != next));
        }
    }
}


BOOST_AUTO_TEST_CASE(different_nodes)
{
    LamportClock clock1(1000);
    LamportClock clock2(2000);

    const LamportTimestamp t1 = clock1.GetNewTimestamp();
    const LamportTimestamp t2 = clock2.GetNewTimestamp();
    BOOST_CHECK(t1 < t2);
    BOOST_CHECK(!(t2 < t1));
    BOOST_CHECK(t1 != t2);
    BOOST_CHECK(t2 != t1);

    const LamportTimestamp t3 = clock1.GetNewTimestamp();
    BOOST_CHECK(t2 < t3);
    BOOST_CHECK(!(t3 < t2));
    BOOST_CHECK(t2 != t3);
    BOOST_CHECK(t3 != t2);

    BOOST_CHECK(!(t1 != t1));
    BOOST_CHECK(!(t2 != t2));
    BOOST_CHECK(!(t3 != t3));
}


BOOST_AUTO_TEST_CASE(update_current)
{
    LamportClock clock1(2000);
    LamportClock clock2(1000);

    auto t1 = clock1.GetNewTimestamp(); //1
    t1 = clock1.GetNewTimestamp(); //2

    const auto t2 = clock2.GetNewTimestamp(); //1
    clock2.UpdateCurrentTimestamp(t1); //set clock2 to 2

    const auto t3 = clock2.GetNewTimestamp();
    BOOST_CHECK(t2 < t3);
    BOOST_CHECK(t1 < t3);
}

BOOST_AUTO_TEST_CASE(wrap_around)
{
    //This case verifies the wrap-aware LamportTimestamp comparison at four raw
    //clock positions: the start, the beginning of the first half, the beginning
    //of the second half, and a value reached after the clock has wrapped past
    //0xffffffff. These are exactly the counter values that GetNewTimestamp would
    //land on if the clock were advanced there step by step (0x0fffffff, then
    //0x7ffffff0, then 0x7fffff00 increments), but we construct them directly to
    //avoid running the clock through billions of iterations.
    const int64_t nodeId = 1000;
    const LamportTimestamp first              = LamportTimestamp::MakeTimestamp(0x00000001, nodeId);
    const LamportTimestamp beginningOfFirstHalf  = LamportTimestamp::MakeTimestamp(0x10000001, nodeId);
    const LamportTimestamp beginningOfSecondHalf = LamportTimestamp::MakeTimestamp(0x8ffffff2, nodeId);
    const LamportTimestamp wrapped             = LamportTimestamp::MakeTimestamp(0x0ffffef3, nodeId);

    BOOST_CHECK(first < beginningOfFirstHalf);
    BOOST_CHECK(beginningOfFirstHalf < beginningOfSecondHalf);
    BOOST_CHECK(beginningOfSecondHalf < first);
    BOOST_CHECK(first < wrapped);
    BOOST_CHECK(beginningOfSecondHalf < wrapped);
    BOOST_CHECK(wrapped < beginningOfFirstHalf);
}
