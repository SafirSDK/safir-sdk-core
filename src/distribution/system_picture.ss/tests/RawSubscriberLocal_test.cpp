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
#include "NodeStatisticsMessage.pb.h"
#include "../src/RawSubscriberLocal.h"
#include <Safir/Utilities/Internal/MakeUnique.h>

#define BOOST_TEST_MODULE RawSubscriberLocalTest
#include <boost/test/unit_test.hpp>

using namespace Safir::Dob::Internal::SP;


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

BOOST_AUTO_TEST_CASE( send_ten )
{
    RawSubscriberLocalBasic<::Subscriber> subscriber("foo");
    
    //we don't actually use the ioservice, but it is needed to be passed along to the tested classes.
    boost::asio::io_service ioService;

    int dataReceived = 0;
    subscriber.Start(ioService,
                     [&](const RawStatistics& r)
                     {
                         ++dataReceived;

                         BOOST_CHECK(r.Valid());
                         BOOST_CHECK(r.Name() == "foo");
                         BOOST_CHECK(r.Id() == 10);
                         BOOST_CHECK(r.NodeTypeId() == 190);
                         BOOST_CHECK(r.ControlAddress() == "asdfasdf");
                         BOOST_CHECK(r.DataAddress() == "foobar");
                         BOOST_CHECK(r.ElectionId() == 91);
                     });


#ifdef CHECK_CRC
    const int crcBytes = sizeof(int);
#else
    const int crcBytes = 0;
#endif
    const auto pbuf = GetProtobuf();
    const size_t size = pbuf->ByteSize() + crcBytes;
    auto data = std::unique_ptr<char[]>(new char[size]);
    pbuf->SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
#ifdef CHECK_CRC
    const int crc = GetCrc32(data.get(), size - crcBytes);
    memcpy(data.get() + size - crcBytes, &crc, sizeof(int));
#endif

    dataCallback(data.get(),size);

    subscriber.Stop();

    BOOST_CHECK(connect_calls == 1);
    BOOST_CHECK(disconnect_calls == 1);
    BOOST_CHECK(dataReceived == 1);

}


