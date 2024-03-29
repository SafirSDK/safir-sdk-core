/******************************************************************************
*
* Copyright Saab AB, 2011-2013 (http://safirsdkcore.com)
*
* Created by: Anders Widén/ stawi
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

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#pragma warning (disable: 4100)
#endif

#include <boost/asio.hpp>
#include <boost/thread.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#ifdef SendMessage
#undef SendMessage
#endif

#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/SecondaryConnection.h>
#include <DoseTest/Action.h>
#include <boost/noncopyable.hpp>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/NodeParameters.h>
#include <iostream>

class ActionSender
    : private boost::noncopyable
{
    typedef boost::shared_ptr<boost::asio::ip::tcp::socket> SocketPtr;
public:
    explicit ActionSender(boost::asio::io_service& ioService)
    {
        for (int i = 0; i < 5; ++i)
        {
            m_sockets.push_back(SocketPtr(new boost::asio::ip::tcp::socket(ioService)));
        }
    }

    void Open(const std::string& address0, const short port0,
              const std::string& address1, const short port1,
              const std::string& address2, const short port2,
              const std::string& address3, const short port3,
              const std::string& address4, const short port4)
    {
        ConnectSocket(m_sockets[0],address0,port0);
        ConnectSocket(m_sockets[1],address1,port1);
        ConnectSocket(m_sockets[2],address2,port2);
        ConnectSocket(m_sockets[3],address3,port3);
        ConnectSocket(m_sockets[4],address4,port4);
    }

    void Close()
    {
        m_sockets[0]->close();
        m_sockets[1]->close();
        m_sockets[2]->close();
        m_sockets[3]->close();
        m_sockets[4]->close();
    }

    void Send(const DoseTest::ActionPtr& msg, const int which)
    {
        Safir::Dob::Typesystem::BinarySerialization binary;
        Safir::Dob::Typesystem::Serialization::ToBinary(msg, binary);
        SendInternal(binary,which);
        SleepyTime(msg->ActionKind());
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
            SendInternal(binary,3);
            SendInternal(binary,4);
        }
        else
        {
            SendInternal(binary,static_cast<int>(msg->Partner().GetVal().GetRawValue()));
        }

        SleepyTime(msg->ActionKind());
    }

private:

    static void ConnectSocket(const SocketPtr& socket, const std::string& address, const short port)
    {
        int tries = 0;

        for(;;)
        {
            ++tries;
            try
            {
                std::wcout << "Connecting to " << address.c_str() << ":" << port << std::endl;
                //Set up address
                const boost::asio::ip::address addr =
                    boost::asio::ip::address::from_string(address);

                const boost::asio::ip::tcp::endpoint endpoint(addr, port);
                socket->connect(endpoint);

                return;
            }
            catch (const boost::system::system_error& e)
            {
                std::wcout << "Failed to Connect: " << e.what() << std::endl;
                boost::this_thread::sleep_for(boost::chrono::seconds(3));
                if (tries > 3)
                {
                    throw;
                }
            }
        }
    }

    static void Timeout(const int which)
    {
        try
        {
            boost::this_thread::sleep_for(boost::chrono::minutes(10));
            std::wcout << "Read from partner " << which << " timed out!" << std::endl;
            exit(31);
        }
        catch (const boost::thread_interrupted&)
        {
            //ok, read was successful, we just let the thread go away.
        }
    }

    void SendInternal(const Safir::Dob::Typesystem::BinarySerialization& binary,
                      const int which)
    {
        boost::asio::write(*m_sockets[which], boost::asio::buffer(&binary[0], binary.size()));

        boost::thread timeout([which]{Timeout(which);});

        try
        {
            //        std::wcout << "Sent action to " << which << ", waiting for ok" << std::endl;
            char reply[3];
            boost::asio::read(*m_sockets[which],
                              boost::asio::buffer(reply, 3));

            if (reply != std::string("ok"))
            {
                std::wcout << "Got unexpected reply: '" << std::wstring(reply,reply+3) << "' from " << which << std::endl;
                timeout.interrupt();
                timeout.join();
                throw std::logic_error("Got unexpected reply!");
            }
        }
        catch (const boost::system::system_error&)
        {
            std::wcout << "reading failed" << std::endl;
        }

        timeout.interrupt();
        timeout.join();
    }

    void SleepyTime(const DoseTest::ActionEnum::Enumeration actionKind)
    {
        //ARM tends to be slow, so we give tests there some extra time
#ifdef __arm__
        const int multiplier = 4;
#else
        const int multiplier = 1;
#endif
        switch (actionKind)
        {
        case DoseTest::ActionEnum::CreateRequest:
        case DoseTest::ActionEnum::Delete:
        case DoseTest::ActionEnum::DeleteAllInstances:
        case DoseTest::ActionEnum::DeleteRequest:
        case DoseTest::ActionEnum::InitialSet:
        case DoseTest::ActionEnum::InjectChanges:
        case DoseTest::ActionEnum::InjectDelete:
        case DoseTest::ActionEnum::RegisterEntityHandler:
        case DoseTest::ActionEnum::RegisterEntityHandlerInjection:
        case DoseTest::ActionEnum::RegisterEntityHandlerPending:
        case DoseTest::ActionEnum::RegisterServiceHandler:
        case DoseTest::ActionEnum::RegisterServiceHandlerPending:
        case DoseTest::ActionEnum::SendMessage:
        case DoseTest::ActionEnum::SendResponse:
        case DoseTest::ActionEnum::ServiceRequest:
        case DoseTest::ActionEnum::SetAll:
        case DoseTest::ActionEnum::SetChanges:
        case DoseTest::ActionEnum::UnregisterHandler:
        case DoseTest::ActionEnum::UpdateRequest:
        case DoseTest::ActionEnum::ResumePostponed:
            boost::this_thread::sleep_for(boost::chrono::milliseconds(140 * multiplier));
            break;
        default:
            boost::this_thread::sleep_for(boost::chrono::milliseconds(40 * multiplier));
            break;
        }
    }

    std::vector<SocketPtr > m_sockets;
};

#endif
