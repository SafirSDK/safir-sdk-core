/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2014 (http://www.consoden.se)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#include "Receiver.h"

Receiver::Receiver(Com::ControlModeTag tag, boost::asio::io_service& ioService, int64_t nodeId, int64_t nodeType)
    :m_timerInclude(ioService)
    ,m_strand(ioService)
    ,m_com(tag,
           ioService,
           std::string("Node_")+boost::lexical_cast<std::string>(nodeId),
           nodeId,
           nodeType,
           std::string("127.0.0.1:1000")+boost::lexical_cast<std::string>(nodeId),
           std::string("127.0.0.1:1100")+boost::lexical_cast<std::string>(nodeId),
           {{0, "nt0", "", "", 1000, 1000}, {1, "nt1", "224.90.90.241:12000", "224.90.90.241:13000", 1000, 1000}})
{
    m_timerInclude.expires_from_now(boost::chrono::milliseconds(1000));
    m_timerInclude.async_wait(m_strand.wrap([=](const boost::system::error_code& error){if (!error) IncludeNode();}));

    m_com.SetDataReceiver([this](int64_t fromNodeId, int64_t fromNodeType, const boost::shared_ptr<char[]>& data, size_t size){ReceiveData(fromNodeId, fromNodeType, data, size);}, 0);
    m_com.SetGotReceiveFromCallback([this](int64_t id){GotReceiveFrom(id);});
    m_com.SetNewNodeCallback([this](const std::string& n, int64_t id, int64_t nt, const std::string& ca, const std::string& da){NewNode(n, id, nt, ca, da);});
    m_com.SetQueueNotFullCallback([this](int64_t id){QueueNotFull(id);}, 100);
    m_com.SetRetransmitToCallback([this](int64_t id){RetransmitTo(id);});

    m_com.Start();
}

Receiver::Receiver(Com::DataModeTag tag, boost::asio::io_service& ioService, int64_t nodeId, int64_t nodeType)
    :m_timerInclude(ioService)
    ,m_strand(ioService)
    ,m_com(tag,
           ioService,
           std::string("Node_")+boost::lexical_cast<std::string>(nodeId),
           nodeId,
           nodeType,
           std::string("127.0.0.1:1100")+boost::lexical_cast<std::string>(nodeId),
           {
             {0, "nt0", "", "", 1000, 1000}
             ,{1, "nt1", "224.90.90.241:12000", "224.90.90.241:13000", 1000, 1000}
           })
{
    m_timerInclude.expires_from_now(boost::chrono::milliseconds(1000));
    m_timerInclude.async_wait(m_strand.wrap([=](const boost::system::error_code& error){if (!error) IncludeNode();}));

    m_com.SetDataReceiver([this](int64_t fromNodeId, int64_t fromNodeType, const boost::shared_ptr<char[]>& data, size_t size){ReceiveData(fromNodeId, fromNodeType, data, size);}, 0);
    m_com.SetGotReceiveFromCallback([this](int64_t id){GotReceiveFrom(id);});
    m_com.SetNewNodeCallback([this](const std::string& n, int64_t id, int64_t nt, const std::string& ca, const std::string& da){NewNode(n, id, nt, ca, da);});
    m_com.SetQueueNotFullCallback([this](int64_t id){QueueNotFull(id);}, 100);
    m_com.SetRetransmitToCallback([this](int64_t id){RetransmitTo(id);});

    m_com.Start();
}

void Receiver::InjectNode(int64_t nodeId, int64_t nodeType)
{
    std::string name=std::string("Node_")+boost::lexical_cast<std::string>(nodeId);
    std::string address=std::string("127.0.0.1:1100")+boost::lexical_cast<std::string>(nodeId);
    m_com.InjectNode(name, nodeId, nodeType, address);
}

void Receiver::Seed(int64_t nodeId)
{
    m_com.InjectSeeds({std::string("127.0.0.1:1000")+boost::lexical_cast<std::string>(nodeId)});
}

void Receiver::Stop()
{
    m_timerInclude.cancel();
    m_com.Stop();
}

void Receiver::Print() const
{
    std::cout<<"--- "<<m_com.Name()<<" ---"<<std::endl;
    std::cout<<"Recv:"<<std::endl;
    for (auto& vt : m_recvCount)
    {
        std::cout<<"  ["<<vt.first<<"] => recvCount: "<<vt.second.first<<", lastRecvData: "<<vt.second.second<<std::endl;
    }
}

void Receiver::NewNode(const std::string& /*name*/, int64_t nodeId, int64_t /*nodeTypeId*/, const std::string& /*controlAddress*/, const std::string& /*dataAddress*/)
{    
    if (m_com.Id()>2)
    {
        std::wcout<<m_com.Id()<<L" includes "<<nodeId<<L", "<<Utilities::TimeString().c_str()<<std::endl;
        m_recvCount[nodeId]=std::make_pair(0, 0);
        m_com.IncludeNode(nodeId);
    }
    else //sender
    {
        m_strand.dispatch([=]{m_newNodes.push(nodeId);});
    }
}

void Receiver::IncludeNode()
{
    if (!m_newNodes.empty())
    {
        int64_t nodeId=m_newNodes.front();
        m_newNodes.pop();
        std::wcout<<m_com.Id()<<L" includes "<<nodeId<<L", "<<Utilities::TimeString().c_str()<<std::endl;
        m_recvCount[nodeId]=std::make_pair(0, 0);
        m_com.IncludeNode(nodeId);
    }

    m_timerInclude.expires_from_now(boost::chrono::milliseconds(4000));
    m_timerInclude.async_wait(m_strand.wrap([=](const boost::system::error_code& error)
    {
        if (!error)
            IncludeNode();
        else
            std::cout<<error.message()<<std::endl;
    }));
}

void Receiver::ReceiveData(int64_t fromNodeId, int64_t fromNodeType, const boost::shared_ptr<char[]>& data, size_t size)
{
    auto it=m_recvCount.find(fromNodeId);
    if (it==m_recvCount.end())
    {
        std::ostringstream os;
        os<<m_com.Name()<<" - Got data from node that is not included yet. NodeId: "<<fromNodeId<<", nodeType "<<fromNodeType;
        std::logic_error(os.str());
    }

    auto val=Utilities::GetValue(data);
    auto exp=it->second.second+1;

    if (!Utilities::ValidCRC(data, size))
    {
        std::ostringstream os;
        os<<m_com.Name()<<" - Bad CRC from "<<fromNodeId<<" nodeType "<<fromNodeType<<", data="<<val<<", expected="<<exp;
        std::logic_error(os.str());
    }

    if (exp>1 && exp!=val)
    {
        std::ostringstream os;
        std::cout<<m_com.Name()<<" - Received unexpected data content from "<<fromNodeId<<" nodeType "<<fromNodeType<<", data="<<val<<", expected="<<exp;
        exit(1);
    }

    ++(it->second.first);
    it->second.second=val;
}

void Receiver::GotReceiveFrom(int64_t /*fromNodeId*/)
{
}

void Receiver::RetransmitTo(int64_t /*toNodeId*/)
{
}

void Receiver::QueueNotFull(int64_t /*nodeTypeId*/)
{
}
