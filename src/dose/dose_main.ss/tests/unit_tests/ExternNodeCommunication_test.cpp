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
#include "../../src/ExternNodeCommunication.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#define BOOST_TEST_MODULE ExternNodeCommunication
#include <boost/test/unit_test.hpp>

void DataCb(const Safir::Dob::Internal::DistributionData& data, const bool isAckedData)
{

}

void StartPoolDistributionCb()
{

}

BOOST_AUTO_TEST_CASE( first_test )
{
    boost::asio::io_service ioService;
    boost::asio::strand strand(ioService);


    Safir::Dob::Internal::ExternNodeCommunication ecom(strand, DataCb, StartPoolDistributionCb);

    ecom.SetOwnNode("Kalle", 123, 54321, "192.168.1.0");

    ecom.InjectNode("Olle", 789, 5656, "192.168.1.2");

    bool ok = true;
    BOOST_CHECK(ok);
}

