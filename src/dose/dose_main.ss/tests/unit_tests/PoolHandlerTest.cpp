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
#include <set>
#include <boost/make_shared.hpp>
#include <boost/thread.hpp>
#include <boost/asio.hpp>
#include "../../src/PoolDistributionRequestSender.h"
#include "../../src/PoolDistributionHandler.h"

#define BOOST_TEST_MODULE PoolHandlerTest
#include <boost/test/unit_test.hpp>

using namespace Safir::Dob::Internal;

class Communication
{
public:
    bool Send(int64_t nodeId,
              int64_t /*nodeTypeId*/,
              const boost::shared_ptr<const char[]>& /*data*/,
              size_t /*size*/,
              int64_t /*dataTypeIdentifier*/,
              bool /*deliveryGuarantee*/)
    {
        requests.insert(nodeId);
        return true;
    }

    std::set<int64_t> requests;
};

BOOST_AUTO_TEST_CASE( PoolDistributionRequestSenderTest )
{
    boost::asio::io_service io;
    auto work=boost::make_shared<boost::asio::io_service::work>(io);

    boost::thread_group threads;
    for (int i = 0; i < 2; ++i)
    {
        threads.create_thread([&]{io.run();});
    }

    Communication com;
    PoolDistributionRequestSender<Communication> pdr(io, com);

    pdr.RequestPoolFrom(1, 1);
    pdr.RequestPoolFrom(2, 1);
    pdr.RequestPoolFrom(3, 1);

    bool pdComplete=false;
    pdr.Start([&]{pdComplete=true;});

    pdr.ReceivedPoolDistributionCompleteFrom(1);
    pdr.ReceivedPoolDistributionCompleteFrom(2);
    pdr.ReceivedPoolDistributionCompleteFrom(3);

    pdr.m_strand.post([&]
    {
        BOOST_CHECK(com.requests.find(1)!=com.requests.end());
        BOOST_CHECK(com.requests.find(2)!=com.requests.end());
        BOOST_CHECK(com.requests.find(3)!=com.requests.end());
        BOOST_CHECK(pdComplete);
    });

    work.reset();
    threads.join_all();
}

//----------------------------------------------------------------------------

class Pd
{
public:
    Pd(int64_t nodeId, int64_t,
       boost::asio::io_service::strand&,
       Communication&,
       const std::function<void(int64_t)>& completionHandler)
        :m_nodeId(nodeId)
    {
        Pd::PoolDistributions[nodeId]=std::make_pair(false, completionHandler);
    }

    void Run()
    {
        auto it=Pd::PoolDistributions.find(m_nodeId);
        if (it!=Pd::PoolDistributions.end())
            it->second.first=true;
    }

    int64_t m_nodeId;

    static std::map<int64_t, std::pair<bool, std::function<void(int64_t)> > > PoolDistributions;
};
std::map<int64_t, std::pair<bool, std::function<void(int64_t)> > > Pd::PoolDistributions;

BOOST_AUTO_TEST_CASE( PoolDistributionHandlerTest )
{
    auto dump=[]
    {
        std::wcout<<L"--- outbound pool distributions ---"<<std::endl;
        for (auto& vt : Pd::PoolDistributions)
        {
            std::wcout<<L"Node "<<vt.first<<L" started: "<<std::boolalpha<<vt.second.first<<std::endl;
        }
    };

    auto complete=[](int64_t id)
    {
        std::wcout<<L"call completionHandler "<<id<<std::endl;
        Pd::PoolDistributions[id].second(id);
    };

    boost::asio::io_service io;
    auto work=boost::make_shared<boost::asio::io_service::work>(io);

    boost::thread_group threads;
    for (int i = 0; i < 2; ++i)
    {
        threads.create_thread([&]{io.run();});
    }

    Communication com;
    PoolDistributionHandler<Communication, Pd> pdh(io, com);
    pdh.Start();

    pdh.AddPoolDistribution(1, 1);
    pdh.AddPoolDistribution(2, 1);

    pdh.m_strand.post([&]
    {
        dump();
        BOOST_CHECK(Pd::PoolDistributions[1].first==true);
        BOOST_CHECK(Pd::PoolDistributions[2].first==false);

        BOOST_CHECK_EQUAL(pdh.m_pendingPoolDistributions.size(), 2);

        complete(1);
    });

    pdh.m_strand.post([&]
    {
        BOOST_CHECK_EQUAL(pdh.m_pendingPoolDistributions.size(), 1);
        dump();
        BOOST_CHECK(Pd::PoolDistributions[1].first==true);
        BOOST_CHECK(Pd::PoolDistributions[2].first==true);

        complete(2);
    });

    pdh.m_strand.post([&]
    {
        BOOST_CHECK_EQUAL(pdh.m_pendingPoolDistributions.size(), 0);
        pdh.AddPoolDistribution(3, 1);
    });

    pdh.m_strand.post([&]
    {
        dump();
        BOOST_CHECK(Pd::PoolDistributions[3].first==true);
        BOOST_CHECK_EQUAL(pdh.m_pendingPoolDistributions.size(), 1);

        complete(3);
    });

    pdh.m_strand.post([&]
    {
        dump();
        BOOST_CHECK_EQUAL(pdh.m_pendingPoolDistributions.size(), 0);
        BOOST_CHECK(Pd::PoolDistributions[1].first==true);
        BOOST_CHECK(Pd::PoolDistributions[2].first==true);
        BOOST_CHECK(Pd::PoolDistributions[3].first==true);
    });

    work.reset();
    threads.join_all();
}
