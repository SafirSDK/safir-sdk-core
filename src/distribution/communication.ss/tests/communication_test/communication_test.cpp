/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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
#include <iostream>
#include <limits>
#include <chrono>
#include <boost/function.hpp>
#include <boost/thread.hpp>
#include <boost/program_options.hpp>
#include <boost/make_shared.hpp>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Dob/Internal/Communication.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#pragma warning (disable: 4244)
#pragma warning (disable: 4245)
#endif

#include <boost/asio.hpp>
#include <boost/crc.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#ifdef _MSC_VER

#  pragma comment (lib, "winmm.lib")
#  pragma warning (push)
#  pragma warning (disable: 4201)
#  include <MMSystem.h>
#  pragma warning (pop)

namespace
{
    // Windows stuff to enable better timer resolution on windows plattforms.
    void EnableWindowsMultimediaTimers()
    {
        // Set best possible timer resolution on windows

        TIMECAPS    tc;
        UINT        wTimerRes;
        const UINT  wantedResolution = 1;  // ms

        if (timeGetDevCaps(&tc, sizeof(TIMECAPS)) != TIMERR_NOERROR)
        {
            throw std::logic_error("Could not enabled WindowsMultimediaTimers");
        }

        wTimerRes = static_cast<UINT>(std::min(std::max(tc.wPeriodMin, wantedResolution), tc.wPeriodMax));
        timeBeginPeriod(wTimerRes);
    }
}
#endif

class Cmd
{
public:
    Cmd(int argc, char * argv[])
        :unicastAddress()
        ,nodeType("nt0")
        ,await(0)
        ,seeds()
        ,nsend(0)
        ,nrecv(0)
        ,accumulatedRecv(false)
        ,messageSize(1000)
        ,threadCount(2)
        ,acked(true)
    {
        boost::program_options::options_description desc("Command line options");
        desc.add_options()
                ("help,h", "Produce help message")
                ("addr,a", boost::program_options::value<std::string>(), "Unicast address on format 'address:port'")
                ("node-type,t", boost::program_options::value<std::string>(), "Node type nt0(unicast only), nt1 or nt2, defaults to nt0'.")
                ("await,w", boost::program_options::value<int>(), "Wait for specified number of other nodes before start sending data")
                ("seed,s", boost::program_options::value< std::vector<std::string> >()->multitoken(), "Seed addresses on format 'address:port'")
                ("nmsg,n", boost::program_options::value<uint64_t>(), "Number of messages to send and receive to and from each other node.")
                ("nsend", boost::program_options::value<uint64_t>(), "Number of messages to send to every other node, default unlimited")
                ("nrecv", boost::program_options::value<uint64_t>(), "Number of messages to receive from all otherl nodes (accumulated), default unlimited")
                ("size", boost::program_options::value<size_t>(), "Size of data packets, default is 1000 bytes")
                ("thread-count", boost::program_options::value<unsigned int>(), "Number of threads to run io_service, default is 2 threads")
                ("unacked", "Send unacked messages");
        boost::program_options::variables_map vm;
        boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
        boost::program_options::notify(vm);
        if (vm.count("help"))
        {
            std::cout<<desc<<std::endl;
            return;
        }

        if (vm.count("addr"))
        {
            unicastAddress=vm["addr"].as<std::string>();
        }
        if (vm.count("node-type"))
        {
            nodeType=vm["node-type"].as<std::string>();
        }
        if (vm.count("await"))
        {
            await=vm["await"].as<int>();
        }
        if (vm.count("seed"))
        {
            seeds=vm["seed"].as< std::vector<std::string> >();
        }
        if (vm.count("nmsg"))
        {
            nsend=vm["nmsg"].as<uint64_t>();
            nrecv=nsend;
        }
        if (vm.count("nsend"))
        {
            nsend=vm["nsend"].as<uint64_t>();
        }
        if (vm.count("nrecv"))
        {
            nrecv=vm["nrecv"].as<uint64_t>();
            accumulatedRecv=true;
        }
        if (vm.count("size"))
        {
            messageSize=vm["size"].as<size_t>();
        }
        if (vm.count("thread-count"))
        {
            threadCount=vm["thread-count"].as<unsigned int>();
        }
        if (nsend==0 && nrecv==0)
        {
            nsend=std::numeric_limits<int>::max();
            nrecv=std::numeric_limits<int>::max();
        }
        if (vm.count("unacked"))
        {
            acked=false;
        }
    }

    std::string unicastAddress;
    std::string nodeType;
    int await;
    std::vector<std::string> seeds;
    uint64_t nsend;
    uint64_t nrecv;
    bool accumulatedRecv;
    size_t messageSize;
    unsigned int threadCount;
    bool acked;
};

class NodeTypes
{
public:
    static const int NumberOfNodeTypes = 3;

    NodeTypes()
    {
        for (int i=0; i<NumberOfNodeTypes; ++i)
        {
            Safir::Dob::Internal::Com::NodeTypeDefinition n;
            n.name="nt"+boost::lexical_cast<std::string>(i);
            n.id=LlufId_Generate64(n.name.c_str());
            n.heartbeatInterval=1000+500*i;
            n.maxLostHeartbeats=10;
            n.retryTimeout=40;
            if (i>0)
            {
                n.controlMulticastAddress=std::string("224.90.90.241:")+boost::lexical_cast<std::string>(11000+i);
                n.dataMulticastAddress=std::string("224.90.90.241:")+boost::lexical_cast<std::string>(12000+i);
            }

            m_nodeTypes.insert(std::make_pair(n.id, n));
        }
    }

    const Safir::Dob::Internal::Com::NodeTypeDefinition& Get(int64_t nodeTypeId) const
    {
        return m_nodeTypes.find(nodeTypeId)->second;
    }

    const Safir::Dob::Internal::Com::NodeTypeDefinition& Get(const std::string& name) const
    {
        return m_nodeTypes.find(LlufId_Generate64(name.c_str()))->second;
    }

    const std::map<int64_t, Safir::Dob::Internal::Com::NodeTypeDefinition>& Map() const
    {
        return m_nodeTypes;
    }

    std::vector<Safir::Dob::Internal::Com::NodeTypeDefinition> ToVector() const
    {
        std::vector<Safir::Dob::Internal::Com::NodeTypeDefinition> v;
        for (auto& n : m_nodeTypes)
        {
            v.push_back(n.second);
        }
        return v;
    }

private:
    std::map<int64_t, Safir::Dob::Internal::Com::NodeTypeDefinition> m_nodeTypes;

};

class Semaphore
{
public:
    Semaphore(int count, bool autoReset)
        :m_initialValue(count)
        ,m_count(m_initialValue)
        ,m_reset(autoReset)
        ,m_mutex()
    {
    }

    void Wait()
    {
        boost::unique_lock<boost::mutex> lock(m_mutex);
        while (m_count>0)
        {
            m_cond.wait(lock);
        }

        if (m_reset)
        {
            m_count=m_initialValue;
        }
    }

    void Notify()
    {
        boost::unique_lock<boost::mutex> lock(m_mutex);
        --m_count;
        m_cond.notify_one();
    }

private:
    const int m_initialValue;
    int m_count;
    bool m_reset;
    boost::mutex m_mutex;
    boost::condition_variable m_cond;
};

inline void SetCRC(char* ptr, size_t size)
{
    boost::crc_32_type crc;
    crc.process_bytes(static_cast<const void*>(ptr), size-4);
    uint32_t* val=reinterpret_cast<uint32_t*>(ptr+size-4);
    *val=crc.checksum();
}

inline bool ValidCRC(const char* ptr, size_t size)
{
    boost::crc_32_type crc;
    crc.process_bytes(static_cast<const void*>(ptr), size-4);
    uint32_t checksum=*reinterpret_cast<const uint32_t*>(ptr+size-4);
    return checksum==crc.checksum();
}

inline boost::shared_ptr<char[]> CreateMessage(uint64_t val, size_t size)
{
    boost::shared_ptr<char[]> msg=boost::make_shared<char[]>(size);
    memset(msg.get(), 0, size);
    (*reinterpret_cast<uint64_t*>(msg.get()))=val;
    SetCRC(msg.get(), size);
    return msg;
}

class Sp : private boost::noncopyable
{
public:

    Sp(uint64_t numRecv, bool accumulated, bool acked,
       const std::function< void(int64_t) >& includeNode,
       const std::function< void(int64_t) >& reportNodeFinished)
        :m_nodeTypes()
        ,m_nodeNames()
        ,m_recvCount()
        ,m_totalRecvCount(0)
        ,m_retransmitCount(0)
        ,m_numRecv(numRecv)
        ,m_accumulated(accumulated)
        ,m_acked(acked)
        ,m_includeNode(includeNode)
        ,m_reportNodeFinished(reportNodeFinished)
    {        
    }

    void NewNode(const std::string& name, int64_t nodeId, int64_t nodeTypeId, const std::string& address)
    {
        m_nodeNames[nodeId]=name;

        std::cout<<"SP: NewNode: "<<name<<" ["<<nodeId<<"], nodeType: '"<<m_nodeTypes.Get(nodeTypeId).name<<"', "<<address<<std::endl;

        if (m_recvCount.find(nodeId)!=m_recvCount.cend())
        {
            std::cout<<"New node on already reported node!"<<std::endl;
            exit(1);
        }

        m_recvCount[nodeId]=0;
        m_includeNode(nodeId);

        if (m_numRecv==0)
        {
            std::cout<<"No messages to be received from node"<<m_nodeNames[nodeId]<<std::endl;
            m_reportNodeFinished(nodeId);
        }
    }

    void GotReceive(int64_t /*id*/)
    {
        //std::cout<<"SP: GotRecv from "<<id<<std::endl;
    }

    void GotNack(int64_t /*id*/)
    {        

    }

    void Retransmit(int64_t /*id*/)
    {
        ++m_retransmitCount;
    }

    void OnRecv(int64_t id, const char* msg, size_t size)
    {
        uint64_t sendCount=*reinterpret_cast<const uint64_t*>(msg);
        auto validCrc=ValidCRC(msg, size);
        delete[] msg;

        if (!validCrc)
        {
            std::cout<<"ComTest: Bad CRC! size="<<size<<std::endl;
        }

        ++m_totalRecvCount;
        uint64_t& rc=m_recvCount[id];


        if (rc>0)
        {
            ++rc;
        }
        else
        {
            //first message we receive, accept anything and set receive count to the received value.
            //this is to handle package loss of first message.
            rc=sendCount;
        }

        if (m_acked)
        {
            if (rc!=sendCount)
            {
                std::cout<<"Recveived "<<sendCount<<", expected to get "<<rc<<std::endl;
                exit(1);
            }

            if (rc==m_numRecv)
            {
                std::cout<<"Received all messages from node "<<m_nodeNames[id]<<std::endl;
                m_reportNodeFinished(id);
            }

            if (m_accumulated && m_totalRecvCount>=m_numRecv)
            {
                std::cout<<"Total number of messages received"<<std::endl;
                m_reportNodeFinished(id);
            }
        }
    }

    void PrintRecvCount()
    {
        for (std::map<int64_t, uint64_t>::const_iterator it=m_recvCount.begin(); it!=m_recvCount.end(); ++it)
        {
            std::cout<<"Has got "<<it->second<<" from node "<<m_nodeNames[it->first]<<std::endl;
        }
    }

    uint64_t RetransmitCount() const {return m_retransmitCount;}

    int64_t GetNodeIdByName(const std::string& name) const
    {
        for (auto& vt : m_nodeNames)
        {
            if (vt.second==name)
                return vt.first;
        }
        return 0;
    }

private:
    NodeTypes m_nodeTypes;
    std::map<int64_t, std::string> m_nodeNames;
    std::map<int64_t, uint64_t> m_recvCount;
    uint64_t m_totalRecvCount;
    uint64_t m_retransmitCount;
    uint64_t m_numRecv;
    bool m_accumulated;
    bool m_acked;
    std::function< void(int64_t) > m_includeNode;
    std::function< void(int64_t) > m_reportNodeFinished;
};

char* Allocate(size_t size) {return new char[size];}

int main(int argc, char * argv[])
{
#ifdef _MSC_VER
    EnableWindowsMultimediaTimers();
#endif
    Cmd cmd(argc, argv);
    if (cmd.unicastAddress.empty())
    {
        std::cout<<"Invalid arguments. Use option --help for more information."<<std::endl;
        return 0;
    }

    std::ostringstream name;
    name<<"Test_"<<cmd.unicastAddress;
    boost::asio::io_service ioService;
    auto work=boost::make_shared<boost::asio::io_service::work>(ioService);
    int numberOfDiscoveredNodes=0;
    boost::barrier startBarrier(2);
    Semaphore stopCondition(cmd.accumulatedRecv ? 1 : cmd.await, false); //if accumulated only one node will report finished, the one posting the last message.
    Semaphore queueFullSem(1, true);

    std::set<int64_t> nodes;
    boost::shared_ptr<Safir::Dob::Internal::Com::Communication> com;
    boost::shared_ptr<Sp> sp(new Sp(cmd.nrecv, cmd.accumulatedRecv, cmd.acked,
    [&](int64_t id)
    {
        nodes.insert(id);
        com->IncludeNode(id);
        ++numberOfDiscoveredNodes;
        if (numberOfDiscoveredNodes==cmd.await)
        {
            startBarrier.wait();
        }
     },
    [&](int64_t id)
    {
        std::cout<<"nofify stop cond id "<<id<<std::endl;
        stopCondition.Notify();
    }));

    NodeTypes nodeTypes;
    int64_t myId=LlufId_GenerateRandom64();
    int64_t myNodeTypeId=nodeTypes.Get(cmd.nodeType).id;
    com.reset(new Safir::Dob::Internal::Com::Communication(Safir::Dob::Internal::Com::controlModeTag,
                                                           ioService,
                                                           name.str(),
                                                           myId,
                                                           myNodeTypeId,
                                                           cmd.unicastAddress,
                                                           cmd.unicastAddress,
                                                           nodeTypes.ToVector()));

    std::cout<<"----------------------------------------------------------------------------"<<std::endl;
    std::cout<<"-- Name:      "<<com->Name()<<std::endl;
    std::cout<<"-- Id:        "<<com->Id()<<std::endl;
    std::cout<<"-- CtrlAddr:  "<<com->ControlAddress()<<std::endl;
    std::cout<<"-- DataAddr:  "<<com->DataAddress()<<std::endl;
    std::cout<<"-- Node type: "<<cmd.nodeType<<std::endl;
    std::cout<<"----------------------------------------------------------------------------"<<std::endl;

    com->SetDataReceiver([=](int64_t fromNode, int64_t /*fromNodeType*/, const char* msg, size_t size){sp->OnRecv(fromNode, msg, size);}, 123, Allocate);
    com->SetGotReceiveFromCallback([=](int64_t id){sp->GotReceive(id);});
    com->SetRetransmitToCallback([=](int64_t id){sp->Retransmit(id);});
    com->SetNewNodeCallback([=](const std::string& name, int64_t nodeId, int64_t nodeTypeId, const std::string& ca, const std::string& /*da*/)
                            {sp->NewNode(name, nodeId, nodeTypeId, ca);});

    for (const auto& vt : nodeTypes.Map())
    {
        com->SetQueueNotFullCallback([&](int64_t){queueFullSem.Notify();}, vt.first);
    }

    boost::thread_group threads;
    for (unsigned int i=0; i<cmd.threadCount; ++i)
    {
        threads.create_thread([&]{ioService.run();});
    }

    if (cmd.seeds.size()>0)
    {
        com->InjectSeeds(cmd.seeds);
    }

    com->Start();

    if (cmd.await>0)
    {
        std::cout<<"Waiting for "<<cmd.await<<" other nodes before start sending data..."<<std::endl;
        startBarrier.wait(); //wait for Sp to signal that all nodes are discovered
        boost::this_thread::sleep_for(boost::chrono::milliseconds(1000)); //wait an extrea second to let all counterpars receivers start too
    }

    std::cout<<"Start sending "<<cmd.nsend<<" messages!"<<std::endl;

    auto startTime=std::chrono::high_resolution_clock::now();
    uint64_t numberOfOverflows=0;
    uint64_t sendCounter=0;
    while (sendCounter<cmd.nsend)
    {
        ++sendCounter;

        auto data=CreateMessage(sendCounter, cmd.messageSize);

        for (auto& nt : nodeTypes.Map())
        {
            if (cmd.acked)
            {
                while (!com->Send(0, nt.second.id, data, cmd.messageSize, 123, true))
                {
                    ++numberOfOverflows;
                    queueFullSem.Wait();
                }
            }
            else
            {
                com->Send(0, nt.second.id, data, cmd.messageSize, 123, false);
            }
        }

        if (sendCounter%50==0)
        {
            boost::this_thread::yield();
        }

        if (sendCounter%10000==0)
        {
            std::cout<<"sent "<<sendCounter<<std::endl;
        }


//        //--------TEST EXCLUDE-------------
//        static bool seedExcluded=false;
//        if (!seedExcluded)
//        {
//            auto soFar=std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now()-startTime);
//            if (soFar.count()>10000)
//            {
//                seedExcluded=true;
//                auto seedId=sp->GetNodeIdByName("Test_127.0.0.1:10000");
//                if (seedId!=0)
//                {
//                    std::cout<<"Exclude node: "<<seedId<<std::endl;
//                    com->ExcludeNode(seedId);
//                }
//            }
//        }
//        //-------------------------------------
    }

    std::cout<<"All messages posted to communication. Waiting for all to get delivered..."<<std::endl;
    if (cmd.acked)
    {
        stopCondition.Wait();

        for (auto& nt : nodeTypes.Map())
        {
            while (com->NumberOfQueuedMessages(nt.second.id)>0)
            {
                boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
            }
        }
    }

    auto elapsed=std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now()-startTime);

    std::cout<<"---------------------------------------------------------"<<std::endl;
    std::cout<<"Finished after "<<static_cast<double>(elapsed.count())/1000.0<<" sec"<<std::endl;
    std::cout<<"Sent: "<<sendCounter<<std::endl;
    std::cout<<"Overflows: "<<numberOfOverflows<<std::endl;
    std::cout<<"Retransmits: "<<sp->RetransmitCount()<<std::endl;
    sp->PrintRecvCount();
    std::cout<<"---------------------------------------------------------"<<std::endl;
    boost::this_thread::sleep_for(boost::chrono::milliseconds(2000)); //allow 1 sec for retransmissions
    ioService.stop();
    threads.join_all();

    return 0;
}

