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

#include "SystemStateMessage.pb.h"

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#include "../src/StateSubscriberLocal.h"
#include <Safir/Utilities/Internal/MakeUnique.h>


#define BOOST_TEST_MODULE StateSubscriberLocalTest
#include <boost/test/unit_test.hpp>

using namespace Safir::Dob::Internal::SP;


std::unique_ptr<SystemStateMessage> GetProtobuf()
{
    auto msg = Safir::make_unique<SystemStateMessage>();

    msg->set_elected_id(100);
    msg->set_election_id(1909);

    auto node = msg->add_node_info();
    
    node->set_name("asdf");
    node->set_id(1);
    node->set_node_type_id(10023);
    node->set_control_address("flipp");
    node->set_data_address(":flopp");
    node->set_is_dead(false);
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

BOOST_AUTO_TEST_CASE( send_one )
{
    StateSubscriberLocalBasic<::Subscriber> subscriber("foo");
    
    //we don't actually use the ioservice, but it is needed to be passed along to the tested classes.
    boost::asio::io_service ioService;

    int dataReceived = 0;
    subscriber.Start(ioService,
                     [&](const SystemState& r)
                     {
                         ++dataReceived;

                         BOOST_CHECK(r.ElectedId() == 100);
                         BOOST_CHECK(r.ElectionId() == 1909);
                         BOOST_CHECK(r.Size() == 1);
                         BOOST_CHECK(r.Name(0) == "asdf");
                         BOOST_CHECK(r.Id(0) == 1);
                         BOOST_CHECK(r.NodeTypeId(0) == 10023);
                         BOOST_CHECK(r.ControlAddress(0) == "flipp");
                         BOOST_CHECK(r.DataAddress(0) == ":flopp");
                         BOOST_CHECK(!r.IsDead(0));
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


