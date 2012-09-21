/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
*
* Created by: Anders Wid�n/ stawi
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

#ifndef __ACTIONSENDER_H__
#define __ACTIONSENDER_H__

#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/SecondaryConnection.h>
#include <DoseTest/Action.h>
#include <boost/asio.hpp>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/DistributionChannelParameters.h>
#include <DoseTest/Parameters.h>

#include <iostream>


class ActionSender
{
    typedef boost::shared_ptr<boost::asio::ip::tcp::socket> SocketPtr;
public:
    explicit ActionSender(boost::asio::io_service& ioService)
        : m_ioService(ioService)
        , m_seqNbr(0)
          
    {
        for (int i = 0; i < 3; ++i)
        {
            m_sockets.push_back(SocketPtr(new boost::asio::ip::tcp::socket(ioService)));
        }

#if 0
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
#endif
    }

    void Open(const std::string& address0, const short port0,
              const std::string& address1, const short port1,
              const std::string& address2, const short port2)
    {
        ConnectSocket(m_sockets[0],address0,port0);
        ConnectSocket(m_sockets[1],address1,port1);
        ConnectSocket(m_sockets[2],address2,port2);
    }

    void Close()
    {
        m_sockets[0]->close();
        m_sockets[1]->close();
        m_sockets[2]->close();
    }

    void Send(const DoseTest::ActionPtr& msg, const int which)
    {
        Safir::Dob::Typesystem::BinarySerialization binary;
        Safir::Dob::Typesystem::Serialization::ToBinary(msg, binary);
        SendInternal(binary,which);
    }

    void Send(const DoseTest::ActionPtr& msg)
    {
        Safir::Dob::Typesystem::BinarySerialization binary;
        Safir::Dob::Typesystem::Serialization::ToBinary(msg, binary);
        SendInternal(binary,0);
        SendInternal(binary,1);
        SendInternal(binary,2);
    }

private:

    static void ConnectSocket(const SocketPtr& socket, const std::string& address, const short port)
    {
        std::wcout << "Connecting to " << address.c_str() << ":" << port << std::endl;
        //Set up address
        const boost::asio::ip::address addr = 
            boost::asio::ip::address::from_string(address);

        const boost::asio::ip::tcp::endpoint endpoint(addr, port);

        socket->connect(endpoint);

    }

    void SendInternal(const Safir::Dob::Typesystem::BinarySerialization& binary,
                      const int which)
    {
        boost::asio::write(*m_sockets[which], boost::asio::buffer(&binary[0], binary.size()));
    }

    boost::asio::io_service& m_ioService;
    std::vector<SocketPtr > m_sockets;

    Safir::Dob::Typesystem::Int32 m_seqNbr;
};

#endif


