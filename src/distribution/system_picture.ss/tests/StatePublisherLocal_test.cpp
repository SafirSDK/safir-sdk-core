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
#include "../src/StatePublisherLocal.h"

#define BOOST_TEST_MODULE StatePublisherLocalTest
#include <boost/test/unit_test.hpp>


using namespace Safir::Dob::Internal::SP;

boost::asio::io_service ioService;


int numPerform = 0;
int numSend = 0;
class Handler
{
public:
    void PerformOnStateMessage(const std::function<void(std::unique_ptr<char []> data, 
                                                        const size_t size)> & fn,
                               const size_t extraSpace,
                               const bool onlyOwnState) const
    {
        BOOST_CHECK(onlyOwnState == false);
        std::wcout << "Perform" << std::endl;
        const size_t size = 10 + extraSpace;
        auto data = std::unique_ptr<char[]>(new char[size]);
        strcpy(data.get(),"123456789");
        fn(std::move(data), size);

        ++numPerform;
        if (numPerform == 10)
        {
            stopCall();
        }
    }

    std::function<void()> stopCall;
};

class Publisher
{
public:
    Publisher(boost::asio::io_service&, const std::string&)
    {

    }

    void Start() {}
    void Stop() {}
    void Send(std::unique_ptr<char []> data, const size_t size)
    {
        std::wcout << "Send" << std::endl;
        ++numSend;
    }
};

BOOST_AUTO_TEST_CASE( send_ten )
{
    Handler h;

    StatePublisherLocalBasic<::Handler, ::Publisher> publisher(ioService,h,"foo",boost::chrono::milliseconds(10));
     
    h.stopCall = [&]{publisher.Stop();};
    ioService.run();
    

    BOOST_CHECK(numPerform == 10);
    BOOST_CHECK(numSend == 10);
}


