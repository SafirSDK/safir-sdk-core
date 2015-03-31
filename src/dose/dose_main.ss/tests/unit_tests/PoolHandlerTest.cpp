/******************************************************************************
*
* Copyright Consoden AB, 2015 (http://www.consoden.se)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#include "../../src/PoolDistribution.h"
#include <Safir/Dob/Internal/Communication.h>
#include <boost/make_shared.hpp>
#include <boost/thread.hpp>
#include <set>


#define BOOST_TEST_MODULE PoolHandlerTest
#include <boost/test/unit_test.hpp>


//using namespace Safir::Dob::Internal;

//class Communication
//{
//public:
//    bool Send(int64_t nodeId,
//              int64_t /*nodeTypeId*/,
//              const boost::shared_ptr<const char[]>& /*data*/,
//              size_t /*size*/,
//              int64_t /*dataTypeIdentifier*/,
//              bool /*deliveryGuarantee*/)
//    {
//        requests.insert(nodeId);
//        return true;
//    }

//    std::set<int64_t> requests;
//};

BOOST_AUTO_TEST_CASE( first_test )
{
//    boost::asio::io_service io;
//    auto work=boost::make_shared<boost::asio::io_service::work>(io);

//    boost::thread_group threads;
//    for (int i = 0; i < 2; ++i)
//    {
//        threads.create_thread([&]{io.run();});
//    }

//    Communication com;
//    PoolDistributionRequestor<Communication> pdr(io, com);

//    pdr.RequestPoolFrom(1, 1);
//    pdr.RequestPoolFrom(2, 1);
//    pdr.RequestPoolFrom(3, 1);

//    bool pdComplete=false;
//    pdr.Start([&]{pdComplete=true;});

//    pdr.ReceivedPoolDistributionCompleteFrom(1);
//    pdr.ReceivedPoolDistributionCompleteFrom(2);
//    pdr.ReceivedPoolDistributionCompleteFrom(3);

//    pdr.m_strand.post([&]
//    {
//        BOOST_CHECK(com.requests.find(1)!=com.requests.end());
//        BOOST_CHECK(com.requests.find(2)!=com.requests.end());
//        BOOST_CHECK(com.requests.find(3)!=com.requests.end());
//        BOOST_CHECK(pdComplete);
//    });

//    work.reset();
//    threads.join_all();
}
