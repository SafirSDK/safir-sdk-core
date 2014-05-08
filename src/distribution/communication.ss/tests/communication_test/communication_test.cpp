#include <iostream>
#include <limits>
#include <boost/asio.hpp>
#include <boost/function.hpp>
#include <boost/thread.hpp>
#include <boost/program_options.hpp>
#include <boost/timer.hpp>
#include <boost/make_shared.hpp>
#include <boost/crc.hpp>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Dob/Internal/Communication.h>

// ./communication_test -a 127.0.0.1:10001 -m 224.90.90.241:10000 -w 2 -n 10000 -s 127.0.0.1:10000

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
        ,messageSize(1000)
    {
        boost::program_options::options_description desc("Command line options");
        desc.add_options()
                ("help,h", "Produce help message")
                ("addr,a", boost::program_options::value<std::string>(), "Unicast address on format 'address:port'")
                ("node-type,t", boost::program_options::value<std::string>(), "Node type nt0, nt1 or nt2, defaults to nt0'")
                ("await,w", boost::program_options::value<int>(), "Wait for specified number of other nodes before start sending data")
                ("seed,s", boost::program_options::value< std::vector<std::string> >()->multitoken(), "Seed addresses on format 'address:port'")
                ("nmsg,n", boost::program_options::value<unsigned int>(), "Number of messages to send and receive. Equal to set both nsend and nsend to the same value")
                ("nsend", boost::program_options::value<unsigned int>(), "Number of messages to send, default unlimited")
                ("nrecv", boost::program_options::value<unsigned int>(), "Number of messages to receive from a singel node, default unlimited")
                ("size", boost::program_options::value<size_t>(), "Size of data packets, default is 1000 bytes");
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
            nsend=vm["nmsg"].as<unsigned int>();
            nrecv=nsend;
        }
        if (vm.count("nsend"))
        {
            nsend=vm["nsend"].as<unsigned int>();
        }
        if (vm.count("nrecv"))
        {
            nrecv=vm["nrecv"].as<unsigned int>();
        }
        if (vm.count("size"))
        {
            messageSize=vm["size"].as<size_t>();
        }

        if (nsend==0 && nrecv==0)
        {
            nsend=std::numeric_limits<int>::max();
            nrecv=std::numeric_limits<int>::max();
        }
    }

    std::string unicastAddress;
    std::string nodeType;
    int await;
    std::vector<std::string> seeds;
    unsigned int nsend;
    unsigned int nrecv;
    size_t messageSize;
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
            n.retryTimeout=50;
            if (i>0)
            {
                n.controlMulticastAddress=std::string("224.90.90.241:")+boost::lexical_cast<std::string>(11000+i);
                n.dataMulticastAddress=std::string("224.90.90.241:")+boost::lexical_cast<std::string>(12000+i);
            }

            m_nodeTypes.insert(std::make_pair(n.id, n));
        }
    }

    const Safir::Dob::Internal::Com::NodeTypeDefinition& Get(boost::int64_t nodeTypeId) const
    {
        return m_nodeTypes.find(nodeTypeId)->second;
    }

    const Safir::Dob::Internal::Com::NodeTypeDefinition& Get(const std::string& name) const
    {
        return m_nodeTypes.find(LlufId_Generate64(name.c_str()))->second;
    }

    const std::map<boost::int64_t, Safir::Dob::Internal::Com::NodeTypeDefinition> Map() const
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
    std::map<boost::int64_t, Safir::Dob::Internal::Com::NodeTypeDefinition> m_nodeTypes;

};

class Semaphore
{
public:
    Semaphore(int count)
        :m_initialValue(count)
        ,m_count(m_initialValue)
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
        m_count=m_initialValue;
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
    boost::mutex m_mutex;
    boost::condition_variable m_cond;
};

void SetCRC(const boost::shared_ptr<char[]>& ptr, size_t size)
{
    boost::crc_32_type crc;
    crc.process_bytes(static_cast<const void*>(ptr.get()), size-4);
    boost::uint32_t* val=reinterpret_cast<boost::uint32_t*>(ptr.get()+size-4);
    *val=crc.checksum();
}

bool ValidCRC(const boost::shared_ptr<char[]>& ptr, size_t size)
{
    boost::crc_32_type crc;
    crc.process_bytes(static_cast<const void*>(ptr.get()), size-4);
    boost::uint32_t checksum=*reinterpret_cast<boost::uint32_t*>(ptr.get()+size-4);
    return checksum==crc.checksum();
}

class Sp : private boost::noncopyable
{
public:

    Sp(unsigned int numRecv,
       const boost::function< void(boost::int64_t) >& includeNode,
       const boost::function< void(boost::int64_t) >& reportNodeFinished)
        :m_nodeTypes()
        ,m_nodeNames()
        ,m_recvCount()
        ,m_retransmitCount(0)
        ,m_numRecv(numRecv)
        ,m_includeNode(includeNode)
        ,m_reportNodeFinished(reportNodeFinished)
    {        
    }


    void NewNode(const std::string& name, boost::int64_t nodeId, boost::int64_t nodeTypeId, const std::string& address)
    {
        m_nodeNames[nodeId]=name;

        std::cout<<"SP: NewNode: "<<name<<" ["<<nodeId<<"], nodeType: '"<<m_nodeTypes.Get(nodeTypeId).name<<"'', "<<address<<std::endl;

        if (m_recvCount.find(nodeId)!=m_recvCount.cend())
        {
            std::cout<<"New node on already reported node!"<<std::endl;
            exit(1);
        }

        m_recvCount[nodeId]=0;
        m_includeNode(nodeId);

        if (m_numRecv==0)
        {
            std::cout<<"Received all messages from node "<<m_nodeNames[nodeId]<<std::endl;
            m_reportNodeFinished(nodeId);
        }
    }

    void GotReceive(boost::int64_t /*id*/)
    {
        //std::cout<<"SP: GotRecv from "<<id<<std::endl;

    }

    void GotNack(boost::int64_t /*id*/)
    {        

    }


    void Retransmit(boost::int64_t id)
    {
        ++m_retransmitCount;
        if (m_retransmitCount%25==0)
        {
            std::cout<<"Retransmits to "<<m_nodeNames[id]<<": "<<m_retransmitCount<<std::endl;
        }
    }

    void OnRecv(boost::int64_t id, const boost::shared_ptr<char[]>& msg, size_t size)
    {
        if (!ValidCRC(msg, size))
        {
            std::cout<<"Bad CRC! size="<<size<<std::endl;
        }

        unsigned int rc=++m_recvCount[id];

        if (rc%10000==0)
        {
            std::cout<<"Recv from "<<m_nodeNames[id]<<", count="<<m_recvCount[id]<<std::endl;
        }
        if (rc==m_numRecv)
        {
            std::cout<<"Received all messages from node "<<m_nodeNames[id]<<std::endl;
            m_reportNodeFinished(id);
        }
    }

    void PrintRecvCount()
    {
        for (std::map<boost::int64_t, unsigned int>::const_iterator it=m_recvCount.begin(); it!=m_recvCount.end(); ++it)
        {
            std::cout<<"Has got "<<it->second<<" from node "<<m_nodeNames[it->first]<<std::endl;
        }
    }

    unsigned int RetransmitCount() const {return m_retransmitCount;}

private:
    NodeTypes m_nodeTypes;
    std::map<boost::int64_t, std::string> m_nodeNames;
    std::map<boost::int64_t, unsigned int> m_recvCount;
    unsigned int m_retransmitCount;
    unsigned int m_numRecv;
    boost::function< void(boost::int64_t) > m_includeNode;
    boost::function< void(boost::int64_t) > m_reportNodeFinished;
};

int main(int argc, char * argv[])
{
    Cmd cmd(argc, argv);
    if (cmd.unicastAddress.empty())
    {
        std::cout<<"Invalid arguments. Use option --help for more information."<<std::endl;
        return 0;
    }

    std::ostringstream name;
    name<<"Test_"<<cmd.unicastAddress;
    boost::shared_ptr<boost::asio::io_service> ioService(new boost::asio::io_service());
    auto work=boost::make_shared<boost::asio::io_service::work>(*ioService);
    int numberOfDiscoveredNodes=0;
    boost::barrier startBarrier(2);
    Semaphore stopCondition(cmd.await);
    Semaphore queueFullSem(1);

    boost::shared_ptr<Safir::Dob::Internal::Com::Communication> com;
    boost::shared_ptr<Sp> sp(new Sp(cmd.nrecv,
    [&](boost::int64_t id)
    {
        com->IncludeNode(id);
        ++numberOfDiscoveredNodes;
        if (numberOfDiscoveredNodes==cmd.await)
        {
            startBarrier.wait();
        }
     },
    [&](boost::int64_t id)
    {
        stopCondition.Notify();
    }));

    NodeTypes nodeTypes;
    boost::int64_t myId=LlufId_GenerateRandom64();
    boost::int64_t myNodeTypeId=nodeTypes.Get(cmd.nodeType).id;
    std::cout<<"----------------------------------------------------------------------------"<<std::endl;
    std::cout<<"-- Name:      "<<name.str()<<std::endl;
    std::cout<<"-- Id:        "<<myId<<std::endl;
    std::cout<<"-- Address:   "<<cmd.unicastAddress<<std::endl;
    std::cout<<"-- Node type: "<<cmd.nodeType<<std::endl;
    std::cout<<"----------------------------------------------------------------------------"<<std::endl;
    com.reset(new Safir::Dob::Internal::Com::Communication(Safir::Dob::Internal::Com::controlModeTag,
                                                           ioService,
                                                           name.str(),
                                                           myId,
                                                           myNodeTypeId,
                                                           cmd.unicastAddress,
                                                           cmd.unicastAddress,
                                                           nodeTypes.ToVector()));

    com->SetDataReceiver([=](boost::int64_t fromNode, boost::int64_t fromNodeType, const boost::shared_ptr<char[]>& msg, size_t size){sp->OnRecv(fromNode, msg, size);}, 0);
    com->SetGotReceiveFromCallback([=](boost::int64_t id){sp->GotReceive(id);});
    com->SetRetransmitToCallback([=](boost::int64_t id){sp->Retransmit(id);});
    com->SetNewNodeCallback([=](const std::string& name, boost::int64_t nodeId, boost::int64_t nodeTypeId, const std::string& ca, const std::string& da)
                            {sp->NewNode(name, nodeId, nodeTypeId, ca);});

    com->SetQueueNotFullCallback([&](boost::int64_t){queueFullSem.Notify();}, 100); //50% free queue space before we get notification

    boost::thread_group threads;
    for (int i = 0; i < 9; ++i)
    {
        threads.create_thread(boost::bind(&boost::asio::io_service::run, ioService));
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

    boost::timer timer;
    unsigned int numberOfOverflows=0;
    unsigned int sendCounter=0;    
    while (sendCounter<cmd.nsend)
    {
        std::string tmp="communication_test_"+boost::lexical_cast<std::string>(sendCounter);
        boost::shared_ptr<char[]> data=boost::make_shared<char[]>(cmd.messageSize);
        strncpy(data.get(), tmp.c_str(), cmd.messageSize);
        SetCRC(data, cmd.messageSize);
        for (auto& nt : nodeTypes.Map())
        {
            while (!com->SendToNodeType(nt.second.id, data, cmd.messageSize, 0))
            {
                ++numberOfOverflows;
                queueFullSem.Wait();
            }
        }
        ++sendCounter;

        if (sendCounter%10000==0)
        {
            std::cout<<"sent "<<sendCounter<<std::endl;
        }
    }

    std::cout<<"All messages posted to communication. Waiting for all to get delivered..."<<std::endl;
    stopCondition.Wait();

    for (auto& nt : nodeTypes.Map())
    {
        while (com->NumberOfQueuedMessages(nt.second.id)>0)
        {
            boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
        }
    }

    std::cout<<"---------------------------------------------------------"<<std::endl;
    std::cout<<"Finished after "<<timer.elapsed()<<" sec"<<std::endl;
    std::cout<<"Sent: "<<sendCounter<<std::endl;
    std::cout<<"Overflows: "<<numberOfOverflows<<std::endl;
    std::cout<<"Retransmits: "<<sp->RetransmitCount()<<std::endl;
    sp->PrintRecvCount();
    std::cout<<"---------------------------------------------------------"<<std::endl;
    boost::this_thread::sleep_for(boost::chrono::milliseconds(1000)); //allow 1 sec for retransmissions
    ioService->stop();
    threads.join_all();

    return 0;
}

