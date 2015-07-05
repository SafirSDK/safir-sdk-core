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
#include "../../src/RawPublisherLocal.h"

#define BOOST_TEST_MODULE RawPublisherLocalTest
#include <boost/test/unit_test.hpp>


using namespace Safir::Dob::Internal::SP;

boost::asio::io_service ioService;


int numAllPerform = 0;
int numMyPerform = 0;
int numSend = 0;
size_t gsize = 0;
class Handler
{
public:
    void PerformOnAllStatisticsMessage(const workaround::function<void(std::unique_ptr<char []> data,
                                                                const size_t size)> & fn) const
    {
        std::wcout << "AllPerform" << std::endl;
        const size_t size = 10;
        auto data = std::unique_ptr<char[]>(new char[size]);
        strcpy(data.get(),"123456789");
        gsize = size;
        fn(std::move(data), size);

        ++numAllPerform;
        if (numAllPerform == 10)
        {
            stopCall();
        }
    }

    void PerformOnMyStatisticsMessage(const workaround::function<void(std::unique_ptr<char []> data,
                                                                const size_t size)> & fn) const
    {
        std::wcout << "MyPerform" << std::endl;
        const size_t size = 10;
        auto data = std::unique_ptr<char[]>(new char[size]);
        strcpy(data.get(),"123456789");
        gsize = size;
        fn(std::move(data), size);

        ++numMyPerform;
        if (numMyPerform == 10)
        {
            stopCall();
        }
    }

    boost::function<void()> stopCall;
};

class Publisher
{
public:
    Publisher(boost::asio::io_service&, const std::string&, void*, void*)
    {

    }

    void Start() {}
    void Stop() {}
    void Send(std::unique_ptr<char []> data, const size_t size)
    {
        BOOST_CHECK(size == gsize);
        BOOST_CHECK(0 == strcmp(data.get(), "123456789"));
        std::wcout << "Send" << std::endl;
        ++numSend;
    }
};

BOOST_AUTO_TEST_CASE( send_ten_all )
{
    Handler h;

    RawPublisherLocalBasic<::Handler, ::Publisher> publisher(ioService,h,"foo",boost::chrono::milliseconds(10),true);

    h.stopCall = [&]{publisher.Stop();};
    ioService.run();


    BOOST_CHECK(numAllPerform == 10);
    BOOST_CHECK(numMyPerform == 0);
    BOOST_CHECK(numSend == 10);
}

BOOST_AUTO_TEST_CASE( send_ten_my )
{
    Handler h;

    RawPublisherLocalBasic<::Handler, ::Publisher> publisher(ioService,h,"foo",boost::chrono::milliseconds(10),false);

    h.stopCall = [&]{publisher.Stop();};
    ioService.reset();
    ioService.run();


    BOOST_CHECK_EQUAL(numAllPerform, 10);
    BOOST_CHECK_EQUAL(numMyPerform, 10);
    BOOST_CHECK_EQUAL(numSend, 20);
}
