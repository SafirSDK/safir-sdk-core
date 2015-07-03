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
#include <Safir/Utilities/Internal/MakeUnique.h>
#include "../../src/StatePublisherRemote.h"

#define BOOST_TEST_MODULE StatePublisherRemoteTest
#include <boost/test/unit_test.hpp>


using namespace Safir::Dob::Internal::SP;

boost::asio::io_service ioService;


int numPerform = 0;
int numSend10 = 0;
int numSend20 = 0;
size_t gsize = 0;
class Handler
{
public:
    void PerformOnStateMessage(const boost::function<void(std::unique_ptr<char []> data, 
                                                        const size_t size)> & fn,
                               const bool onlyOwnState) const
    {
        BOOST_CHECK(onlyOwnState == true);
        std::wcout << "Perform" << std::endl;
        const size_t size = 10;
        auto data = std::unique_ptr<char[]>(new char[size]);
        strcpy(data.get(),"123456789");
        gsize = size;
        fn(std::move(data), size);

        ++numPerform;
        if (numPerform == 10)
        {
            stopCall();
        }
    }

    boost::function<void()> stopCall;
};

class Communication
{
public:
    bool Send(int64_t nodeId,
              int64_t nodeTypeId, 
              const boost::shared_ptr<char[]>& data, 
              size_t size, 
              int64_t /*dataTypeIdentifier*/,
              bool acked)
    {
        BOOST_CHECK(nodeId == 0);
        BOOST_CHECK(acked);
        BOOST_CHECK(size == gsize);
        BOOST_CHECK(0 == strcmp(data.get(), "123456789"));
        std::wcout << "Send" << std::endl;
        BOOST_CHECK(nodeTypeId == 10 || nodeTypeId == 20);
        if (nodeTypeId == 10)
        {
            ++numSend10;
            return true;
        }
        else
        {
            ++numSend20;
            return numSend20 % 2 == 0; //cause some overflows
        }

    }
};

BOOST_AUTO_TEST_CASE( send_ten )
{
    Handler h;
    Communication communication;

    std::map<int64_t, NodeType> nodeTypes;
    nodeTypes.insert(std::make_pair(10, NodeType(10,"mupp",false,boost::chrono::seconds(1),10,boost::chrono::seconds(1))));
    nodeTypes.insert(std::make_pair(20, NodeType(20,"tupp",true,boost::chrono::seconds(1),22,boost::chrono::seconds(1))));
    
    StatePublisherRemoteBasic<::Handler, ::Communication> publisher
        (ioService,communication,nodeTypes,"foo",h,boost::chrono::milliseconds(10));
     
    h.stopCall = [&]{publisher.Stop();};
    ioService.run();
    

    BOOST_CHECK(numPerform == 10);
    BOOST_CHECK(numSend10 == 10);
    BOOST_CHECK(numSend20 == 10);
}


