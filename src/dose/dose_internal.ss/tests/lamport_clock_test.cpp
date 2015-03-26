/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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
            BOOST_CHECK(last != next);
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
    BOOST_CHECK(t1 != t2);

    const LamportTimestamp t3 = clock1.GetNewTimestamp();
    BOOST_CHECK(t2 < t3);
    BOOST_CHECK(t2 != t3);
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
