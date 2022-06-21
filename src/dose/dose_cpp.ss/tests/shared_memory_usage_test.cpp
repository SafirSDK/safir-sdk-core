/******************************************************************************
*
* Copyright Saab AB, 2022 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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

#define BOOST_TEST_MODULE GetSharedMemorySizeTest
#include <boost/test/unit_test.hpp>

#include <Safir/Dob/Connection.h>
#include <Safir/Dob/ConnectionAspectMisc.h>

using namespace Safir::Dob;


BOOST_AUTO_TEST_CASE( usage_test )
{
    Connection conn;
    ConnectionAspectMisc misc(conn);
    //expect at least 50 bytes allocated from the shared memory
    BOOST_CHECK_GE(misc.GetSharedMemoryUsage(), 50);
}
