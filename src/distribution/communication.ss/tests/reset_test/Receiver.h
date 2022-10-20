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
#include <atomic>
#include <map>
#include <boost/asio.hpp>
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

class Receiver
{
public:

    Receiver(int64_t nodeId, int64_t nodeTypeId, const std::vector<Com::NodeTypeDefinition>& nodeTypes)
        :m_io()
        ,m_nodeTypeId(nodeTypeId)
        ,m_work(boost::asio::make_work_guard(m_io))
        ,m_com(Com::controlModeTag,
               m_io,
               std::string("Receiver_")+std::to_string(nodeId),
               nodeId,
               m_nodeTypeId,
               Com::ResolvedAddress(std::string("127.0.0.1:1000")+std::to_string(nodeId)),
               Com::ResolvedAddress(std::string("127.0.0.1:1100")+std::to_string(nodeId)),
               nodeTypes,
               1450)
    {
        for (unsigned int i=0; i<3; ++i)
        {
            m_threads.create_thread([&]{m_io.run();});
        }

        m_com.SetNewNodeCallback([this](const std::string& name, int64_t nodeId, int64_t nodeTypeId, const std::string& controlAddress, const std::string& dataAddress, bool multicast){
            OnNewNode(name, nodeId, nodeTypeId, controlAddress, dataAddress, multicast);
        });

        m_com.SetDataReceiver([this](int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t size){OnReceiveData(fromNodeId, fromNodeType, data, size);},
        123, [](size_t s){return new char[s];}, [](const char * data){delete[] data;});

        m_com.SetGotReceiveFromCallback([this](int64_t fromNodeId, bool isMulticast, bool isDuplicate){OnGotReceiveFrom(fromNodeId, isMulticast, isDuplicate);});

        m_com.SetRetransmitToCallback([this](int64_t toNodeId, size_t tc){OnRetransmitTo(toNodeId, tc);});

        m_com.SetQueueNotFullCallback([this](int64_t nodeTypeId){OnQueueNotFull(nodeTypeId);}, m_nodeTypeId);
    }

    void Start(const std::string& seed, int64_t expectedSender)
    {
        m_numRecvFrom[expectedSender] = 0;
        m_expectedSenderId = expectedSender;
        m_com.InjectSeeds({seed});
        m_com.Start();
    }

    void Stop()
    {
        m_com.Stop();
        m_work.reset();
        m_io.restart();
        m_threads.join_all();
    }

    void Reset(const std::string& newSeed, int64_t newExpectedSender)
    {
        std::cout << "Reset" << std::endl;
        m_numRecvFrom[newExpectedSender] = 0;
        m_com.Reset();
        m_expectedSenderId = newExpectedSender;
        m_com.InjectSeeds({ newSeed });
        {
            boost::mutex::scoped_lock lock(m_mutex);
            for (auto& kv : m_numRecvFrom)
            {
                kv.second = 0;
            }
        }
    }

    unsigned int GetRecvCount(int64_t fromNodeId)
    {
        boost::mutex::scoped_lock lock(m_mutex);
        return m_numRecvFrom[fromNodeId];
    }

private:

    boost::thread_group m_threads;
    boost::mutex m_mutex;
    int64_t m_expectedSenderId = 0;
    std::map<int64_t, unsigned int> m_numRecvFrom;



    // initialized in ctor
    boost::asio::io_context m_io;
    int64_t m_nodeTypeId;
    boost::asio::executor_work_guard<boost::asio::io_context::executor_type> m_work;
    Com::Communication m_com;

    void OnNewNode(const std::string& name, int64_t nodeId, int64_t /*nodeTypeId*/, const std::string& /*controlAddress*/, const std::string& /*dataAddress*/, bool /*multicast*/)
    {
        if (nodeId != m_expectedSenderId)
        {
            std::cout << m_com.Name() << ": OnNewNode " << name << " but expected node " << m_expectedSenderId << std::endl;
            return;
        }
        std::cout << m_com.Name() << ": OnNewNode " << name << std::endl;
        m_com.IncludeNode(nodeId);
    }

    void OnReceiveData(int64_t fromNodeId, int64_t /*fromNodeType*/, const char* data, size_t /*size*/)
    {
        auto value = *reinterpret_cast<const uint64_t*>(data);
        delete[] data;
        if (fromNodeId != m_expectedSenderId)
        {
            std::cout << m_com.Name() << ": received data from " << fromNodeId << " but expected to receive from " << m_expectedSenderId << ", val=" << value <<std::endl;
        }

        UpdateRecvCount(fromNodeId);
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

    void UpdateRecvCount(int64_t nodeId)
    {
        boost::mutex::scoped_lock lock(m_mutex);
        m_numRecvFrom[nodeId]++;
    }
};
