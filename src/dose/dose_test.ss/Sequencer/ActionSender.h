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
#include <boost/thread.hpp>

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
        if (msg->Partner().IsNull())
        {
            SendInternal(binary,0);
            SendInternal(binary,1);
            SendInternal(binary,2);
        }
        else
        {
            SendInternal(binary,msg->Partner().GetVal().GetRawValue());
        }
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

        try
        {
            //        std::wcout << "Sent action to " << which << ", waiting for ok" << std::endl;
            char reply[3];
            boost::asio::read(*m_sockets[which],
                              boost::asio::buffer(reply, 3));
            if (reply != std::string("ok"))
            {
                std::wcout << "Got unexpected reply: '" << std::wstring(reply,reply+3) << "'" << std::endl;
                throw std::logic_error("Got unexpected reply!");
            }
        }
        catch (const boost::system::system_error&)
        {
            std::wcout << "reading failed" << std::endl;
        }

        boost::this_thread::sleep(boost::posix_time::milliseconds(50));
    }

    boost::asio::io_service& m_ioService;
    std::vector<SocketPtr > m_sockets;

    Safir::Dob::Typesystem::Int32 m_seqNbr;
};

#endif


