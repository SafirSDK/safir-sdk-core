/*************************************************************0*****************
*
* Copyright Saab AB, 2014 (http://safirsdkcore.com)
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

Receiver::Receiver(Com::ControlModeTag tag, boost::asio::io_context& ioContext, int64_t nodeId, int64_t nodeType)
    :m_timerInclude(ioContext)
    ,m_strand(ioContext)
    ,m_com(tag,
           ioContext,
           std::string("Node_")+boost::lexical_cast<std::string>(nodeId),
           nodeId,
           nodeType,
           Com::ResolvedAddress(std::string("127.0.0.1:1000")+boost::lexical_cast<std::string>(nodeId)),
           Com::ResolvedAddress(std::string("127.0.0.1:1100")+boost::lexical_cast<std::string>(nodeId)),
           []() -> std::vector<Com::NodeTypeDefinition>  //This lambda just generates a vector of nodetypes. It used to be an initializer list
           {  //but VS2012 doesnt have initializer lists :-(
               std::vector<int> retryTimeout;
               retryTimeout.push_back(10);
               std::vector<Com::NodeTypeDefinition> n;
               n.push_back(Com::NodeTypeDefinition(10, "nt10", "", "", false, 1000, 10, 20, 10, retryTimeout));
               n.push_back(Com::NodeTypeDefinition(11, "nt11", "224.90.90.241:12000", "224.90.90.241:13000", false, 1000, 10, 20, 10, retryTimeout));
               return n;
           }(),
           1450)
    ,m_running(true)
{
    m_timerInclude.expires_after(std::chrono::milliseconds(1000));
    m_timerInclude.async_wait(boost::asio::bind_executor(m_strand, [this](const boost::system::error_code& /*error*/)
                                              {if (m_running) IncludeNode();}));

    m_com.SetDataReceiver([this](int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t size)
                           {ReceiveData(fromNodeId,fromNodeType,data,size);},
                          0,
                          [](size_t s){return new char[s];},
                          [](const char * data){delete[] data;});
    m_com.SetGotReceiveFromCallback([this](int64_t fromNodeId, bool isMulticast, bool isDuplicate)
                                     {GotReceiveFrom(fromNodeId,isMulticast,isDuplicate);});
    m_com.SetNewNodeCallback([this](const std::string& name, int64_t nodeId, int64_t nodeTypeId, const std::string& controlAddress, const std::string& dataAddress, bool multicast)
    {NewNode(name, nodeId, nodeTypeId, controlAddress, dataAddress, multicast);});
    m_com.SetQueueNotFullCallback([this](int64_t nodeTypeId){QueueNotFull(nodeTypeId);},0);
    m_com.SetRetransmitToCallback([this](int64_t toNodeId, size_t tc){RetransmitTo(toNodeId,tc);});

    m_com.Start();
}

Receiver::Receiver(Com::DataModeTag tag, boost::asio::io_context& ioContext, int64_t nodeId, int64_t nodeType)
    :m_timerInclude(ioContext)
    ,m_strand(ioContext)
    ,m_com(tag,
           ioContext,
           std::string("Node_")+boost::lexical_cast<std::string>(nodeId),
           nodeId,
           nodeType,
           Com::ResolvedAddress(std::string("127.0.0.1:1100")+boost::lexical_cast<std::string>(nodeId)),
           []() ->  std::vector<Com::NodeTypeDefinition> //This lambda just generates a vector of nodetypes. It used to be an initializer list
           {  //but VS2012 doesnt have initializer lists :-(
                std::vector<int> retryTimeout;
                retryTimeout.push_back(10);
                std::vector<Com::NodeTypeDefinition> n;
                n.push_back(Com::NodeTypeDefinition(10, "nt10", "", "", false, 1000, 10, 20, 10, retryTimeout));
                n.push_back(Com::NodeTypeDefinition(11, "nt11", "224.90.90.241:12000", "224.90.90.241:13000", false, 1000, 10, 20, 10, retryTimeout));
                return n;
           }(),
           1450)
    ,m_running(true)
{
    m_timerInclude.expires_after(std::chrono::milliseconds(1000));
    m_timerInclude.async_wait(boost::asio::bind_executor(m_strand, [this](const boost::system::error_code& error){if (!error) IncludeNode();}));

    m_com.SetDataReceiver([this](int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t size)
                           {ReceiveData(fromNodeId,fromNodeType,data,size);},
                          0,
                          [](size_t s){return new char[s];},
                          [](const char * data){delete[] data;});
    m_com.SetGotReceiveFromCallback([this](int64_t fromNodeId, bool isMulticast, bool isDuplicate)
                                     {GotReceiveFrom(fromNodeId,isMulticast,isDuplicate);});
    m_com.SetNewNodeCallback([this](const std::string& name, int64_t nodeId, int64_t nodeTypeId, const std::string& controlAddress, const std::string& dataAddress, bool multicast)
                                {NewNode(name, nodeId, nodeTypeId, controlAddress, dataAddress, multicast);});
    m_com.SetQueueNotFullCallback([this](int64_t nodeTypeId){QueueNotFull(nodeTypeId);},0);
    m_com.SetRetransmitToCallback([this](int64_t toNodeId, size_t tc){RetransmitTo(toNodeId,tc);});

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
    std::vector<std::string> seeds;
    seeds.push_back(std::string("127.0.0.1:1000")+boost::lexical_cast<std::string>(nodeId));

    m_com.InjectSeeds(seeds);
}

void Receiver::Stop()
{
    m_running=false;
    m_timerInclude.cancel();
    m_com.Stop();
}

void Receiver::Print() const
{
    std::cout<<"--- "<<m_com.Name()<<" ---"<<std::endl;
    std::cout<<"Recv:"<<std::endl;
    for (auto vt = m_recvCount.cbegin(); vt != m_recvCount.cend(); ++vt)
    {
        std::cout<<"  ["<<vt->first<<"] => recvCount: "<<vt->second.first<<", lastRecvData: "<<vt->second.second<<std::endl;
    }
}

void Receiver::NewNode(const std::string& /*name*/, int64_t nodeId, int64_t /*nodeTypeId*/, const std::string& /*controlAddress*/, const std::string& /*dataAddress*/, bool /*multicast*/)
{    
    if (m_com.Id()>2)
    {
        std::wcout<<m_com.Id()<<L" includes "<<nodeId<<L", "<<Utilities::TimeString().c_str()<<std::endl;
        m_recvCount[nodeId]=std::make_pair(0, 0);
        m_com.IncludeNode(nodeId);
    }
    else //sender
    {
        boost::asio::dispatch(m_strand, [this,nodeId]{m_newNodes.push(nodeId);});
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

    m_timerInclude.expires_after(std::chrono::milliseconds(4000));
    m_timerInclude.async_wait(boost::asio::bind_executor(m_strand, [this](const boost::system::error_code& error)
    {
        if (!error)
            IncludeNode();
        else
            std::cout<<error.message()<<std::endl;
    }));
}

void Receiver::ReceiveData(int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t size)
{
    auto val=Utilities::GetValue(data);
    auto validCrc=Utilities::ValidCRC(data, size);
    delete[] data;

    auto it=m_recvCount.find(fromNodeId);
    if (it==m_recvCount.end())
    {
        std::ostringstream os;
        os<<m_com.Name()<<" - Got data from node that is not included yet. NodeId: "<<fromNodeId<<", nodeType "<<fromNodeType;
        std::logic_error(os.str());
    }

    auto exp=it->second.second+1;

    if (!validCrc)
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

void Receiver::GotReceiveFrom(int64_t /*fromNodeId*/, bool /*isMulticast*/, bool /*isDuplicate*/)
{
}

void Receiver::RetransmitTo(int64_t /*toNodeId*/, size_t /*tc*/)
{
}

void Receiver::QueueNotFull(int64_t /*nodeTypeId*/)
{
}
