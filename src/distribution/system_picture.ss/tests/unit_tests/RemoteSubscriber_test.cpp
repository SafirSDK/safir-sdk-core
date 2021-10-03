/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safirsdkcore.com)
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
#include <functional>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include "../../src/RemoteSubscriber.h"



#define BOOST_TEST_MODULE StateSubscriberRemoteTest
#include <boost/test/unit_test.hpp>

using namespace Safir::Dob::Internal::SP;

std::function<void(int64_t fromNodeId,
                   int64_t fromNodeType,
                   const char* const data,
                   size_t size)> gDataCallback;

std::function<char*(size_t)> allocator;
std::function<void(const char* data)> deallocator;

class Com
{
public:
    void SetDataReceiver(const std::function<void(int64_t fromNodeId,
                                                  int64_t fromNodeType,
                                                  const char* const data,
                                                  size_t size)>& callback,
                         int64_t /*dataTypeIdentifier*/,
                         const std::function<char*(size_t)>& alloc,
                         const std::function<void(const char* data)>& dealloc)
    {
        gDataCallback = callback;
        allocator = alloc;
        deallocator = dealloc;
    }
};

int updates = 0;

class Handler
{
public:
    void NewRemoteStatistics(const int64_t from,
                             const std::shared_ptr<const char[]>& data,
                             const size_t size)
    {
        ++updates;
        BOOST_CHECK(from == 1);
        BOOST_CHECK(0==strcmp(data.get(), "123456789"));
        BOOST_CHECK(size == 10);
    }

};

BOOST_AUTO_TEST_CASE( send_one )
{
    Com c;
    Handler h;
    RemoteSubscriber<::Com, ::Handler> subscriber(c, "foo", h);

    const size_t size = 10;
    auto data = std::shared_ptr<char[]>(new char[size]);
    strcpy(data.get(), "123456789");
    char* dataCopy = allocator(size);
    memcpy(dataCopy,data.get(),size);

    gDataCallback(1,2002,dataCopy,size);

    BOOST_CHECK(updates == 1);
}
