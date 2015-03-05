/******************************************************************************
*
* Copyright Consoden AB, 2015 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / anders.widen@consoden.se
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
#include "../../src/Distribution.h"

#define BOOST_TEST_MODULE DistributionTests
#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_CASE( first_test )
{
    boost::asio::io_service ioService;

    Safir::Dob::Internal::Distribution d(ioService,
                                         "Pelle",
                                         6565,
                                         878787,
                                         "127.0.0.1");

    d.SetDataReceiver([](int64_t fromNodeId,
                      int64_t fromNodeType,
                      const char* data,
                      size_t size)
                      {

                      },
                      12345,
                      [](size_t size){return new char[size];});

    bool ok = true;
    BOOST_CHECK(ok);
}

