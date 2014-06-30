/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
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
#include "../src/Coordinator.h"
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <boost/thread.hpp>
#include <boost/thread/mutex.hpp>
#include <memory>


#define BOOST_TEST_MODULE CoordinatorHandlerTest
#include <boost/test/unit_test.hpp>


//We need thread safe variants for some of the tests that use multiple threads in the ioService.
boost::mutex testMtx;
#define SAFE_BOOST_CHECK(p) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_CHECK(p);}
#define SAFE_BOOST_FAIL(s) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_FAIL(s);}
#define SAFE_BOOST_CHECK_NE(L, R) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_CHECK_NE(L, R);}
#define SAFE_BOOST_CHECK_EQUAL(L, R) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_CHECK_EQUAL(L, R);}
#define SAFE_BOOST_TEST_MESSAGE(m) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_TEST_MESSAGE(m);}

using namespace Safir::Dob::Internal::SP;

typedef std::function<void(const RawStatistics& statistics)> StatisticsCallback;


class Communication
{
public:
    void ExcludeNode(int64_t nodeId)
    {

    }

};

class RawHandler
{
public:
    void SetElectionId(const int64_t nodeId, const int64_t electionId)
    {

    }

    void AddNodesChangedCallback(const StatisticsCallback& callback)
    {

    }

    void AddRawChangedCallback(const StatisticsCallback& callback)
    {

    }

    void SetDeadNode(int64_t nodeId)
    {

    }
    
};


class ElectionHandler
{
public:
    ElectionHandler(boost::asio::io_service& ioService,
                    Communication& communication,
                    const int64_t id,
                    const std::map<int64_t, NodeType>& nodeTypes,
                    const char* const receiverId,
                    const std::function<void(const int64_t nodeId, 
                                             const int64_t electionId)>& electionCompleteCallback)
    {

    }

    bool IsElected() const
    {
        return false;
    }

    void NodesChanged(RawStatistics statistics)
    {
        
    }

    void Stop()
    {

    }
};

struct Fixture
{
    Fixture()
        : coordinator(ioService,
                      comm,
                      "asdf",
                      10,
                      10,
                      "klopp",
                      "flupp",
                      {},
                      "snoop",
                      rh)
    {
        SAFE_BOOST_TEST_MESSAGE("setup fixture");
    }
    
    ~Fixture()
    {
        SAFE_BOOST_TEST_MESSAGE("teardown fixture");
    }


    boost::asio::io_service ioService;
    ::Communication comm;
    ::RawHandler rh;

    CoordinatorBasic<::Communication, ::RawHandler, ::ElectionHandler> coordinator;
};

BOOST_FIXTURE_TEST_SUITE( s, Fixture )

BOOST_AUTO_TEST_CASE( start_stop )
{
    coordinator.Stop();
    ioService.run();
    // BOOST_CHECK(nodes[0]->comm.receiveDataCb != nullptr);
    // BOOST_CHECK(!nodes[0]->eh.IsElected());

    // //this is kind of an implementation detail...
    // BOOST_CHECK(nodes[0]->eh.IsElected(std::numeric_limits<int64_t>::min()));

    // nodes[0]->eh.Stop();
    // RunIoService();

    // //The stop call should ensure that the timer never elapses, so we don't get an "alone" election
    // BOOST_CHECK(!nodes[0]->eh.IsElected());
}


BOOST_AUTO_TEST_SUITE_END()

