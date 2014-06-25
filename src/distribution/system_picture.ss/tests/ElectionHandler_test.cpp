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
#include "../src/ElectionHandler.h"
//#include <boost/thread.hpp>
//#include <set>

#define BOOST_TEST_MODULE ElectionHandlerTest
#include <boost/test/unit_test.hpp>

class Communication
{
public:

   //Callbacks functions used in Communications public interface.
    typedef std::function<void(const std::string& name, int64_t nodeId, int64_t nodeTypeId, const std::string& controlAddress, const std::string& dataAddress)> NewNode;
    typedef std::function<void(int64_t fromNodeId)> GotReceiveFrom;
    typedef std::function<void(int64_t toNodeId)> RetransmitTo;
    typedef std::function<void(int64_t fromNodeId, int64_t fromNodeType, const boost::shared_ptr<char[]>& data, size_t size)> ReceiveData;

    void SetDataReceiver(const ReceiveData& callback, const int64_t /*dataTypeIdentifier*/)
    {
        receiveDataCb = callback;
    }


    bool SendToNode(const int64_t /*nodeId*/, 
                    const int64_t /*nodeTypeId*/, 
                    const boost::shared_ptr<char[]>& /*data*/,
                    const size_t /*size*/, 
                    const int64_t /*dataTypeIdentifier*/,
                    const bool ack)
    {
        BOOST_CHECK(ack);
        return true;
    }

    bool SendToNodeType(const int64_t /*nodeTypeId*/,
                        const boost::shared_ptr<char[]>& /*data*/, 
                        const size_t /*size*/, 
                        const int64_t /*dataTypeIdentifier*/, 
                        const bool ack)
    {
        BOOST_CHECK(ack);
        return true;
    }

    ReceiveData receiveDataCb;
};

using namespace Safir::Dob::Internal::SP;

struct Fixture
{
    Fixture():
        eh(ioService,
           comm,
           10,
           GetNodeTypes(),
           "stropp",
           [this](const int64_t nodeId, 
                  const int64_t electionId)
           {
               ElectionComplete(nodeId,electionId);
           })
    {
        BOOST_TEST_MESSAGE( "setup fixture" );
    }
    
    ~Fixture()
    {
        BOOST_TEST_MESSAGE( "teardown fixture" );
    }

    void ElectionComplete(const int64_t /*nodeId*/,
                          const int64_t /*electionId*/)
    {
        
    }

    static std::map<int64_t, NodeType> GetNodeTypes()
    {
        std::map<int64_t, NodeType> nodeTypes;
        nodeTypes.insert(std::make_pair(10, NodeType(10,"mupp",false,boost::chrono::milliseconds(1),10)));
        nodeTypes.insert(std::make_pair(20, NodeType(20,"tupp",true,boost::chrono::seconds(1),22)));
        return nodeTypes;
    }


    Communication comm;
    boost::asio::io_service ioService;

    ElectionHandlerBasic<Communication> eh;
};

BOOST_FIXTURE_TEST_SUITE( s, Fixture )

BOOST_AUTO_TEST_CASE( start_stop )
{
    BOOST_CHECK(comm.receiveDataCb != nullptr);
    BOOST_CHECK(!eh.IsElected());

    //this is kind of an implementation detail...
    BOOST_CHECK(eh.IsElected(std::numeric_limits<int64_t>::min()));

    eh.Stop();
    ioService.run();
}


BOOST_AUTO_TEST_SUITE_END()
