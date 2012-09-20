/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
*
* Created by: Anders Wid�n / stawi
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
#include "ActionSender.h"
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/DistributionChannelParameters.h>
#include <DoseTest/Parameters.h>

#include <iostream>

ActionSender::ActionSender(boost::asio::io_service& ioService)
    : m_ioService(ioService)
    , m_socket(ioService)
    , m_seqNbr(0)

{
    // Port and address must correspond to what is used by the partners
    const std::wstring multicastAddr = DoseTest::Parameters::TestMulticastAddress();
    
    //Set up address
    const boost::asio::ip::address addr = 
        boost::asio::ip::address::from_string(Safir::Dob::Typesystem::Utilities::ToUtf8
                                              (multicastAddr));
    const unsigned short port = 31789;
    const boost::asio::ip::udp::endpoint endpoint(addr, port);
    
    m_socket.open(endpoint.protocol());
    
    const int ttl = Safir::Dob::NodeParameters::RoutingHops();
    m_socket.set_option(boost::asio::ip::multicast::hops(ttl));
    
    std::wcout << "Opening socket for multicast send. Multicast address " << multicastAddr << ", port " << port << "." << std::endl;
    //if (multicastNic.empty())
    {
        std::wcout << "NIC is not set which means that the system default interface will be used" << std::endl;
    }
    /*else
    {
        std::wcout << "Used NIC: " << Safir::Dob::Typesystem::Utilities::ToWstring(multicastNic) << std::endl;
        }
    
    if (!multicastNic.empty())
    {
        boost::asio::ip::address_v4 local_interface =
            boost::asio::ip::address_v4::from_string(multicastNic);
        boost::asio::ip::multicast::outbound_interface option(local_interface);
        m_socket.set_option(option);
        }*/
    m_socket.connect(endpoint);
}

ActionSender::~ActionSender()
{
}

void ActionSender::Send(const DoseTest::ActionPtr& msg)
{
    ++m_seqNbr;
    msg->SeqNbr().SetVal(m_seqNbr);

    Safir::Dob::Typesystem::BinarySerialization binary;
    
    Safir::Dob::Typesystem::Serialization::ToBinary(msg, binary);
    
    if (binary.size() > 63000)
    {
        std::wcout << "Can't send messages that don't fit in a UDP datagram via the socket!" << std::endl;
    }
    else
    {
        m_socket.send(boost::asio::buffer(binary));
    }
}
