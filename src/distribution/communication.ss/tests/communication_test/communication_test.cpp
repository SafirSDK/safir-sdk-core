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
        ,multicastAddress()
        ,await(0)
        ,seeds()
        ,nsend(std::numeric_limits<int>::max())
        ,nrecv(std::numeric_limits<int>::max())
        ,messageSize(1000)
    {
        boost::program_options::options_description desc("Command line options");
        desc.add_options()
                ("help,h", "Produce help message")
                ("uaddr,a", boost::program_options::value<std::string>(), "Unicast address on format 'address:port'")
                ("maddr,m", boost::program_options::value<std::string>(), "Multicast address on format 'address:port'")
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

        if (vm.count("uaddr"))
        {
            unicastAddress=vm["uaddr"].as<std::string>();
        }
        if (vm.count("maddr"))
        {
            multicastAddress=vm["maddr"].as<std::string>();
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
    }

    std::string unicastAddress;
    std::string multicastAddress;
    int await;
    std::vector<std::string> seeds;
    unsigned int nsend;
    unsigned int nrecv;
    size_t messageSize;
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
        :m_nodeNames()
        ,m_recvCount()
        ,m_retransmitCount(0)
        ,m_numRecv(numRecv)
        ,m_includeNode(includeNode)
        ,m_reportNodeFinished(reportNodeFinished)
    {        
    }


    void NewNode(const std::string& name,
                 boost::int64_t id,
                 const std::string& address,
                 bool multicastEnabled)
    {
        m_nodeNames[id]=name;

        std::cout<<"SP: NewNode: "<<name<<" ["<<id<<"], "<<address;
        if (multicastEnabled)
        {
            std::cout<<" MulticastEnabled";
        }
        std::cout<<std::endl;
        if (m_recvCount.find(id)!=m_recvCount.cend())
        {
            std::cout<<"New node on already reported node!"<<std::endl;
            exit(1);
        }

        m_recvCount[id]=0;
        m_includeNode(id);

        if (m_numRecv==0)
        {
            std::cout<<"Received all messages from node "<<m_nodeNames[id]<<std::endl;
            m_reportNodeFinished(id);
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

    boost::int64_t myId=LlufId_GenerateRandom64();
    std::cout<<"----------------------------------------------------------------------------"<<std::endl;
    std::cout<<"-- Name:      "<<name.str()<<std::endl;
    std::cout<<"-- Id:        "<<myId<<std::endl;
    std::cout<<"-- Unicast:   "<<cmd.unicastAddress<<std::endl;
    std::cout<<"-- Multicast: "<<(cmd.multicastAddress.empty() ? "NOT_ENABLED" :  cmd.multicastAddress)<<std::endl;
    std::cout<<"----------------------------------------------------------------------------"<<std::endl;
    com.reset(new Safir::Dob::Internal::Com::Communication(ioService, name.str(),
                                                           myId,
                                                           cmd.unicastAddress,
                                                           cmd.multicastAddress));

    com->SetDataReceiver([=](boost::int64_t id, const boost::shared_ptr<char[]>& msg, size_t size){sp->OnRecv(id, msg, size);}, 0);
    com->SetGotReceiveFromCallback([=](boost::int64_t id){sp->GotReceive(id);});
    com->SetRetransmitToCallback([=](boost::int64_t id){sp->Retransmit(id);});
    com->SetNewNodeCallback([=](const std::string& name,
                            boost::int64_t id,
                            const std::string& address,
                            bool multicastEnabled){sp->NewNode(name, id, address, multicastEnabled);});

    com->SetQueueNotFullCallback([&]{queueFullSem.Notify();}, 100); //50% free queue space before we get notification

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
        while (!com->SendAll(data, cmd.messageSize, 0))
        {
            ++numberOfOverflows;
            queueFullSem.Wait();
        }
        ++sendCounter;

        if (sendCounter%10000==0)
        {
            std::cout<<"sent "<<sendCounter<<std::endl;
        }
    }

    std::cout<<"All messages posted to communication. Waiting for all to get delivered..."<<std::endl;
    stopCondition.Wait();
    while (com->NumberOfQueuedMessages()>0)
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
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

