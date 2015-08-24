/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safir.sourceforge.net)
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

#define BOOST_TEST_MODULE IncarnationBlackListHandlerTest
#include <boost/test/unit_test.hpp>
#include "../../src/IncarnationBlackListHandler.h"


using namespace Safir::Dob::Internal::Control;


BOOST_AUTO_TEST_CASE( blacklisthandlertests )
{

    //check that we detect if a directory and no filename is given
    BOOST_CHECK_THROW(IncarnationBlacklistHandler("."), std::runtime_error);


    //check that we create a file if it does not exist
    boost::filesystem::path file("test_incarnation_blacklistfile.txt");

    if(boost::filesystem::exists(file))
        boost::filesystem::remove(file);

    BOOST_CHECK_NO_THROW(IncarnationBlacklistHandler testHandler("test_incarnation_blacklistfile.txt"));

    BOOST_CHECK(boost::filesystem::exists(file) == true);


    IncarnationBlacklistHandler testHandler("test_incarnation_blacklistfile.txt");

    //make sure we don't have troubles with an empty file
    BOOST_CHECK(testHandler.validateIncarnationId(2) == true);

    //add a incarnation ids to the blacklist
    BOOST_CHECK_NO_THROW(testHandler.addIncarnationId(1));
    BOOST_CHECK_NO_THROW(testHandler.addIncarnationId(3));

    //check that it's blacklisted and that others are not
    BOOST_CHECK(testHandler.validateIncarnationId(1) == false);
    BOOST_CHECK(testHandler.validateIncarnationId(3) == false);
    BOOST_CHECK(testHandler.validateIncarnationId(2) == true);

    //check that we can add the same id again
    BOOST_CHECK_NO_THROW(testHandler.addIncarnationId(1));

    //check that they are still blacklisted
    BOOST_CHECK(testHandler.validateIncarnationId(1) == false);
    BOOST_CHECK(testHandler.validateIncarnationId(3) == false);
}

