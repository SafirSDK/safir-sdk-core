/******************************************************************************
*
* Copyright Saab AB, 2022 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@gmail.com
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
#pragma once

#include <iostream>
#include <string>
#include <chrono>
#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>
#include <Safir/Dob/Internal/Communication.h>

#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100)
#endif

#include <boost/thread.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

namespace Com = Safir::Dob::Internal::Com;

class Sender
{
public:

    Sender(int64_t nodeId)
        :m_io()
        ,m_work(boost::asio::make_work_guard(m_io))
        ,m_sendTimer(m_io)
        ,m_com(Com::controlModeTag,
               m_io,
               std::string("Sender_")+std::to_string(nodeId),
               nodeId,
               m_nodeTypeId,
               Com::ResolvedAddress(std::string("127.0.0.1:1000")+std::to_string(nodeId)),
               Com::ResolvedAddress(std::string("127.0.0.1:1100")+std::to_string(nodeId)),
    {Com::NodeTypeDefinition(m_nodeTypeId, "nt10", "", "", false, 1000, 10, 20, 10, {100})},
               1450)
        ,m_threads()
    {
        for (unsigned int i=0; i<3; ++i)
        {
            m_threads.create_thread([&]{m_io.run();});
        }

        m_com.SetNewNodeCallback([this](const std::string& name, int64_t nodeId, int64_t nodeTypeId, const std::string& controlAddress, const std::string& dataAddress, bool multicast){
            OnNewNode(name, nodeId, nodeTypeId, controlAddress, dataAddress, multicast);
        });

        m_com.SetDataReceiver([this](int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t size){OnReceiveData(fromNodeId, fromNodeType, data, size);},
        0, [](size_t s){return new char[s];}, [](const char * data){delete[] data;});

        m_com.SetGotReceiveFromCallback([this](int64_t fromNodeId, bool isMulticast, bool isDuplicate){OnGotReceiveFrom(fromNodeId, isMulticast, isDuplicate);});

        m_com.SetRetransmitToCallback([this](int64_t toNodeId, size_t tc){OnRetransmitTo(toNodeId, tc);});

        m_com.SetQueueNotFullCallback([this](int64_t nodeTypeId){OnQueueNotFull(nodeTypeId);}, m_nodeTypeId);
    }

    void Start()
    {
        std::cout << m_com.Name() << ": started" << std::endl;
        m_com.Start();
        m_sendTimer.expires_after(boost::chrono::milliseconds(25));
        m_sendTimer.async_wait([this](const boost::system::error_code& ec){OnSendTimer(ec);});
    }

    void Stop()
    {
        std::cout << m_com.Name() << ": stopping...";
        m_sendTimer.cancel();
        m_com.Stop();
        m_work.reset();
        m_io.restart();
        m_threads.join_all();
        std::cout << m_com.Name() << " done!" << std::endl;
    }

private:
    const int64_t m_nodeTypeId = 10;
    boost::asio::io_context m_io;
    boost::asio::executor_work_guard<boost::asio::io_context::executor_type> m_work;
    boost::asio::steady_timer m_sendTimer;
    Com::Communication m_com;
    boost::thread_group m_threads;
    uint64_t m_sendCounter = 0;

    void OnNewNode(const std::string& name, int64_t nodeId, int64_t /*nodeTypeId*/, const std::string& /*controlAddress*/, const std::string& /*dataAddress*/, bool /*multicast*/)
    {
        std::cout << m_com.Name() << ": OnNewNode " << name << std::endl;
        m_com.IncludeNode(nodeId);
    }

    void OnReceiveData(int64_t /*fromNodeId*/, int64_t /*fromNodeType*/, const char* /*data*/, size_t /*size*/)
    {
    }

    void OnGotReceiveFrom(int64_t /*fromNodeId*/, bool /*isMulticast*/, bool /*isDuplicate*/)
    {
    }

    void OnRetransmitTo(int64_t /*toNodeId*/, size_t /*tc*/)
    {
    }

    void OnQueueNotFull(int64_t /*nodeTypeId*/)
    {
    }

    void OnSendTimer(const boost::system::error_code& ec)
    {
        if (ec == boost::asio::error::operation_aborted) // this is needed to handle the timer.cancel
        {
            return;
        }

        if (m_com.NumberOfQueuedMessages(m_nodeTypeId) < m_com.SendQueueCapacity(m_nodeTypeId))
        {
            size_t size = 20;
            auto data=Safir::Utilities::Internal::MakeSharedArray(size);
            (*reinterpret_cast<uint64_t*>(data.get()))=m_sendCounter++;
            m_com.Send(0, m_nodeTypeId, data, size, 0, true);
        }

        m_sendTimer.expires_after(boost::chrono::milliseconds(10));
        m_sendTimer.async_wait([this](const boost::system::error_code& ec){OnSendTimer(ec);});
    }

};
