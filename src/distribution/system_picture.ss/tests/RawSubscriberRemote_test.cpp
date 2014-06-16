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

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4244)
#pragma warning (disable: 4127)
#endif

#include "NodeStatisticsMessage.pb.h"

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#include "../src/RawSubscriberRemote.h"
#include <Safir/Utilities/Internal/MakeUnique.h>


#define BOOST_TEST_MODULE RawSubscriberRemoteTest
#include <boost/test/unit_test.hpp>

using namespace Safir::Dob::Internal::SP;

/*
std::unique_ptr<NodeStatisticsMessage> GetProtobuf()
{
    auto msg = Safir::make_unique<NodeStatisticsMessage>();

    msg->set_name("foo");
    msg->set_id(10);
    msg->set_node_type_id(190);
    msg->set_control_address("asdfasdf");
    msg->set_data_address("foobar");
    msg->set_election_id(91);
    
    return std::move(msg);
}
*/
/*
std::function<void(const char* const data, const size_t size)> dataCallback;

int connect_calls = 0;
int disconnect_calls = 0;

class Subscriber
{
public:
    Subscriber(boost::asio::io_service&, 
               const std::string& name,
               const std::function<void(const char* const data, const size_t size)>& callback)
    {
        dataCallback = callback;
        BOOST_CHECK(name == "foo");
    }
    void Connect() {++connect_calls;}
    void Disconnect() {++disconnect_calls;}
    
};
*/

std::function<void(int64_t fromNodeId, int64_t fromNodeType, const boost::shared_ptr<char[]>& data, size_t size)> dataCallback;

class Com
{
public:
    void SetDataReceiver(const std::function<void(int64_t fromNodeId, int64_t fromNodeType, const boost::shared_ptr<char[]>& data, size_t size)>& callback, 
                         int64_t dataTypeIdentifier)
    {
        dataCallback = callback;
    }
};

int updates = 0;

class Handler
{
public:
    void UpdateRemoteStatistics(const int64_t from, 
                                const boost::shared_ptr<char[]>& data,
                                const size_t size)
    {
        ++updates;
        BOOST_CHECK(from == 1);
        BOOST_CHECK(0==strcmp(data.get(), "123456789"));
    }

};

BOOST_AUTO_TEST_CASE( send_one )
{
    Com c;
    Handler h;
    RawSubscriberRemoteBasic<::Com, ::Handler> subscriber(c, "foo", h);

#ifdef CHECK_CRC
    const int crcBytes = sizeof(int);
#else
    const int crcBytes = 0;
#endif
    const size_t size = crcBytes + 10;
    auto data = boost::shared_ptr<char[]>(new char[size]);
    strcpy(data.get(), "123456789");
#ifdef CHECK_CRC
    const int crc = GetCrc32(data.get(), size - crcBytes);
    memcpy(data.get() + size - crcBytes, &crc, sizeof(int));
#endif

    dataCallback(1,2002,data,size);
    
    BOOST_CHECK(updates == 1);
}


