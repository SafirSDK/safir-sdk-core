/******************************************************************************
*
* Copyright Saab AB, 2012 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Dob/Internal/SystemPicture.h>
#include <Safir/Utilities/CrashReporter.h>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <boost/atomic.hpp>
#include <boost/lexical_cast.hpp>
#include <fstream>
#include <iostream>
#include <map>
#include <boost/thread/recursive_mutex.hpp>

//disable warnings in boost
#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100 4267 4505)
#endif

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/program_options.hpp>
#include <boost/thread.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
//and disable a warning that happens on instantiation
#  pragma warning (disable : 4505)
#endif

std::string nowString()
{
    using namespace boost::posix_time;
    using namespace boost;
    const time_duration td = microsec_clock::universal_time().time_of_day();
    return to_simple_string(td);
}

//This is a magic unlocker that has the strange behaviour of having a bool
//operator that always returns false. It is for the use in preprocessor
//magic only.
class Magic
{
public:
    operator bool() const
    {
        return false;
    }
    static const Magic Lock() {wcout_lock.lock(); return Magic(wcout_lock);}
private:
    explicit Magic(boost::recursive_mutex& mutex):
        m_lock(&mutex,boost::mem_fn(&boost::recursive_mutex::unlock))
    {
    }

    boost::shared_ptr<boost::recursive_mutex> m_lock;
    static boost::recursive_mutex wcout_lock;
};
boost::recursive_mutex Magic::wcout_lock; //static instance

#define log if(Magic lck_asdf = Magic::Lock()) ; else std::wcout << "[" << nowString()<< "] "

std::int64_t GenerateId()
{
    for(;;)
    {
        auto id = LlufId_GenerateRandom64();
        if (id > 0)
        {
            return id;
        }
    }
}

std::wostream& operator<<(std::wostream& out, const std::string& str)
{
    return out << str.c_str();
}

std::wostream& operator<<(std::wostream& out, const boost::program_options::options_description& opt)
{
    std::ostringstream ostr;
    ostr << opt;
    return out << ostr.str().c_str();
}


class ProgramOptions
{
public:
    ProgramOptions(int argc, char* argv[])
        : parseOk(false)
    {
        using namespace boost::program_options;
        options_description options("Options");
        options.add_options()
            ("help,h", "show help message")
            ("master",
             "Is the node a master. If not specified the node will be a slave")
            ("only-control",
             "Don't start the dose_main part of the node")
            ("address",
             value<std::string>(&address)->default_value("127.0.0.1"),
             "Address and port of the control channel")
            ("seed",
             value<std::string>(&seed),
             "Seed address. Just ip address, no port.")
            ("number",
             value<int>(&number),
             "On slave nodes: the number identifying the slave. On master node: means nothing")
            ("number-of-nodes",
             value<int>(&numberOfNodes),
             "number of nodes in test run. Mandatory")
            ("revolutions",
             value<int>(&revolutions)->default_value(1),
             "Number of revolutions to perform. Only valid on master")
            ("node-type",
             value<int64_t>(&nodeType),
             "Node type to use. 1 or 2 are valid values. 1 is without multicast and 2 is with multicast.");

        variables_map vm;

        try
        {
            store(command_line_parser(argc, argv).
                  options(options).run(), vm);
            notify(vm);
        }
        catch (const std::exception& exc)
        {
            std::wcerr << "Error parsing command line: " << exc.what() << "\n" << std::endl;
            ShowHelp(options);
            return;
        }

        if (vm.count("help"))
        {
            ShowHelp(options);
            return;
        }

        master = vm.count("master") != 0;
        start_main = vm.count("only-control") == 0;

        if (vm.count("number-of-nodes") == 0 ||
            vm.count("node-type") == 0)
        {
            ShowHelp(options);
            return;
        }


        if (master)
        {
            name = "master";
            controlAddress = address + ":33999";
            dataAddress = address + ":43999";
        }
        else
        {
            std::ostringstream ss;
            ss << std::setw(3) << std::setfill('0') << number;
            name = "slave_" + ss.str();
            controlAddress = address + ":33" + ss.str();
            dataAddress = address + ":43" + ss.str();
        }
        parseOk = true;
    }
    bool parseOk;

    std::string name;
    std::string controlAddress;
    std::string dataAddress;
    bool master;
    bool start_main;
    std::string seed;
    int number;
    int numberOfNodes;
    int revolutions;
    //bool checkIncarnation;
    int64_t nodeType;
private:
    std::string address;

    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        log << std::boolalpha
                   << "Usage: control_stub [OPTIONS]\n"
                   << desc << "\n"
                   << std::endl;
    }

};




class Common
{
public:
    Common(const std::int64_t id)
        : m_success(true)
        , m_work(new boost::asio::io_service::work(m_ioService))
        , m_id(id)
    {
        m_commNodeTypes.push_back(Safir::Dob::Internal::Com::NodeTypeDefinition
                                  (1,
                                   "NodeTypeA",
                                   "", //no multicast
                                   "", //no multicast
                                   1000,
                                   20,
                                   15));

        m_spNodeTypes.insert(std::make_pair(1,
                                            Safir::Dob::Internal::SP::NodeType
                                            (1,
                                             "NodeTypeA",
                                             false,
                                             boost::chrono::milliseconds(1000),
                                             15,
                                             boost::chrono::milliseconds(20))));

        m_commNodeTypes.push_back(Safir::Dob::Internal::Com::NodeTypeDefinition
                                  (2,
                                   "NodeTypeB",
                                   "224.123.45.67:10000",
                                   "224.123.45.67:10001",
                                   2000,
                                   50,
                                   8));

        m_spNodeTypes.insert(std::make_pair(2,
                                            Safir::Dob::Internal::SP::NodeType
                                            (2,
                                             "NodeTypeB",
                                             false,
                                             boost::chrono::milliseconds(2000),
                                             8,
                                             boost::chrono::milliseconds(50))));
    }

    void Start()
    {
        if (m_communication == nullptr)
        {
            return;
        }

        m_communication->Start();

        for (int i = 0; i < 2; ++i)
        {
            m_threads.create_thread([this]
            {
                try
                {
                    m_ioService.run();
                    return;
                }
                catch (const std::exception & exc)
                {
                    log << "Caught 'std::exception' exception from io_service.run(): "
                               << "  '" << exc.what() << "'." << std::endl;
                }
                m_success.exchange(false);
                log << "Thread exiting due to failure" << std::endl;
            });
        }
    }


protected:
    void StopCommon()
    {
        m_systemPicture->Stop();
        m_communication->Stop();
        m_work.reset();
        m_threads.join_all();
    }

    bool SuccessCommon() const
    {
        return m_work == nullptr && m_success;
    }

    virtual ~Common() {}
    boost::atomic<bool> m_success;
    boost::thread_group m_threads;

    boost::asio::io_service m_ioService;
    //make some work to stop io_service from exiting.
    std::unique_ptr<boost::asio::io_service::work> m_work;

    const std::int64_t m_id;
    std::vector<Safir::Dob::Internal::Com::NodeTypeDefinition> m_commNodeTypes;
    std::map<int64_t, Safir::Dob::Internal::SP::NodeType> m_spNodeTypes;


    std::unique_ptr<Safir::Dob::Internal::Com::Communication> m_communication;
    std::unique_ptr<Safir::Dob::Internal::SP::SystemPicture> m_systemPicture;
};



class Control: public Common
{
public:
    explicit Control(const ProgramOptions& options,
                     const std::int64_t id,
                     const std::function<void(bool restart)>& stopFcn)
        : Common(id)
        , m_strand(m_ioService)
        , m_options(options)
        , m_stopFcn(stopFcn)
        , m_joinCalls(0)
    {
        m_communication.reset(new Safir::Dob::Internal::Com::Communication
                              (Safir::Dob::Internal::Com::controlModeTag,
                               m_ioService,
                               options.name,
                               m_id,
                               options.nodeType,
                               Safir::Dob::Internal::Com::ResolvedAddress(options.controlAddress),
                               Safir::Dob::Internal::Com::ResolvedAddress(options.dataAddress),
                               m_commNodeTypes));

        std::vector<std::string> seeds;
        seeds.push_back(options.seed + ":33999");
        m_communication->InjectSeeds(seeds);

        m_systemPicture.reset(new Safir::Dob::Internal::SP::SystemPicture
                              (Safir::Dob::Internal::SP::master_tag,
                               m_ioService,
                               *m_communication,
                               options.name,
                               m_id,
                               options.nodeType,
                               m_spNodeTypes,
                               [this](const int64_t id)
                               {return CheckJoinSystem(id);},
                               [this](const int64_t id)
                               {return CheckFormSystem(id);}));
    }

    void Stop()
    {
        StopCommon();
    }
    virtual bool Success() const = 0;
protected:

    bool CheckJoinSystem(const int64_t id)
    {
        ++m_joinCalls;
        if (m_joinCalls > 1)
        {
            throw std::logic_error("Expect only one call to CheckJoinSystem");
        }

        log << "Checking incarnation id " << id << std::endl;

        if (m_lastIncarnation == 0)
        {
            log << "Have no previous incarnation, setting last and returning true" << std::endl;
            m_lastIncarnation = id;
            return true;
        }
        else if (m_lastIncarnation == id)
        {
            return true;
        }
        else
        {
            throw std::logic_error("Incarnation mismatch! Expected "
                                   + boost::lexical_cast<std::string>(m_lastIncarnation)
                                   + ", got "
                                   + boost::lexical_cast<std::string>(id));
        }
    }

    bool CheckFormSystem(const int64_t /*incarnationId*/)
    {
        return true;
    }

    boost::asio::io_service::strand m_strand;

    const ProgramOptions m_options;
    const std::function<void(bool restart)> m_stopFcn;
    static int64_t m_lastIncarnation;
    int m_joinCalls;
};
int64_t Control::m_lastIncarnation = 0;

class ControlMaster: public Control
{
public:
    explicit ControlMaster(const ProgramOptions& options,
                           const std::int64_t id,
                           const std::function<void(bool restart)>& stopFcn)
        : Control(options, id, stopFcn)
        , m_revolutions(0)
        , m_waitAWhile(0)
        , m_allSlavesStarted(false)
        , m_nextToDie(0)
        , m_waitForExit(false)
        , m_waitToDie(0)
    {
        // Start subscription to system state changes from SP
        m_systemPicture->StartStateSubscription(m_strand.wrap([this](const Safir::Dob::Internal::SP::SystemState& data)
                                                              {NewState(data);}));

    }

    bool Success() const
    {
        return SuccessCommon();
    }
private:
    void NewState(const Safir::Dob::Internal::SP::SystemState& data)
    {
        log << "Got new state:\n" << data << std::endl;

        if (m_waitAWhile > 0)
        {
            --m_waitAWhile;
            return;
        }
        int nodes = 0;
        for (int i = 0; i < data.Size(); ++i)
        {
            //don't count dead nodes or ourselves
            if (!data.IsDead(i) && data.Id(i) != m_id)
            {
                ++nodes;
            }
        }
        if (nodes == m_options.numberOfNodes && !m_allSlavesStarted)
        {
            m_allSlavesStarted = true;
            log << "All slaves are up"  << std::endl;
            m_waitAWhile = 10;
            return;
        }
        if (m_waitForExit)
        {
            if (nodes == 0)
            {
                log << "All slaves have died, as expected" << std::endl;
                m_stopFcn(false);
            }
            return;
        }
        if (m_allSlavesStarted && m_waitToDie == 0)
        {
            if (m_revolutions >= m_options.revolutions && !m_waitForExit)
            {
                log << "Finished our revolutions, will tell all slaves to exit"  << std::endl;
                Send("exit");
                m_waitForExit = true;
            }
            else
            {
                log << "Will tell slave " << m_nextToDie << " to restart"  << std::endl;
                Send("restart " + boost::lexical_cast<std::string>(m_nextToDie));
                std::ostringstream ss;
                ss << "slave_"<<std::setw(3) << std::setfill('0') << m_nextToDie;

                for (int i = 0; i < data.Size(); ++i)
                {
                    //find id of node to wait for
                    if (!data.IsDead(i) && data.Name(i) == ss.str())
                    {
                        m_waitToDie = data.Id(i);
                    }
                }
            }
        }
        if (m_waitToDie != 0)
        {
            for (int i = 0; i < data.Size(); ++i)
            {
                if (data.IsDead(i) && data.Id(i) == m_waitToDie)
                {
                    log << "Slave " << m_nextToDie
                        << " died, just as expected, will wait for it to come back" << std::endl;
                    m_allSlavesStarted = false;
                    m_waitToDie = 0;
                    ++m_nextToDie;
                    if (m_nextToDie >= m_options.numberOfNodes)
                    {
                        m_nextToDie = 0;
                        ++m_revolutions;
                    }
                }
            }
        }
    }
    void Send(const std::string& message)
    {
        const size_t size = message.size();
        const boost::shared_ptr<char[]> data(new char[size]);
        memcpy(data.get(), &message[0], size);
        //send the data to both node types.

        while(!m_communication->Send(0,1,data,size,1000100444,true))
        {
            //retry
        }

        while(!m_communication->Send(0,2,data,size,1000100444,true))
        {
            //retry
        }
    }
    int m_revolutions;
    int m_waitAWhile;
    bool m_allSlavesStarted;
    int m_nextToDie;
    bool m_waitForExit;
    std::int64_t m_waitToDie;
};

class ControlSlave: public Control
{
public:
    explicit ControlSlave(const ProgramOptions& options,
                          const std::int64_t id,
                          const std::function<void(bool restart)>& stopFcn)
        : Control(options, id, stopFcn)
        , m_stop(false)
    {
        // Start subscription to system state changes from SP
        m_systemPicture->StartStateSubscription(m_strand.wrap([this](const Safir::Dob::Internal::SP::SystemState& data)
        {
            log << "Got new state:\n" << data << std::endl;
            m_lastState = data;
            if (m_stop)
            {
                Stop(m_restart);
            }
        }));

        m_communication->SetDataReceiver(m_strand.wrap([this](const int64_t fromNodeId,
                                                              const int64_t fromNodeType,
                                                              const char* data,
                                                              const size_t size)
                                                       {DataReceived(fromNodeId,fromNodeType,data,size);}),
                                         1000100444,
                                         [](size_t size){return new char[size];},
                                         [](const char * data){delete[] data;});


    }

    bool Success() const
    {
        //including the master and excluding ourselves there should be 10 nodes.
        int nodes = 0;
        for (int i = 0; i < m_lastState.Size(); ++i)
        {
            //don't count dead nodes or ourselves
            if (!m_lastState.IsDead(i) && m_lastState.Id(i) != m_id)
            {
                ++nodes;
            }
        }
        if (nodes != m_options.numberOfNodes)
        {
            log << "Slave did not see all nodes before exiting" << std::endl;
            return false;
        }
        return SuccessCommon();
    }
private:
    void Stop(bool restart)
    {
        //naive size check
        if (m_lastState.Size() >= m_options.numberOfNodes)
        {
            m_stopFcn(restart);
        }
        else
        {
            log << "Delaying stop, since we've not seen enough nodes yet" << std::endl;
            m_stop = true;
            m_restart = restart;
        }
    }

    void DataReceived(const int64_t /*fromNodeId*/,
                      const int64_t /*fromNodeType*/,
                      const char* data,
                      const size_t size)
    {
        const std::string message(data, data+size);
        log << "Got message: " << message << std::endl;
        if (message == "restart " + boost::lexical_cast<std::string>(m_options.number))
        {
            log << "Oooh, that means me!" << std::endl;
            Stop(true);
        }
        else if (message == "exit")
        {
            log << "Time to die!" << std::endl;
            Stop(false);
        }
    }

    Safir::Dob::Internal::SP::SystemState m_lastState;
    int m_stop;
    bool m_restart;
};

class Main: public Common
{
public:
    explicit Main(const ProgramOptions& options, const std::int64_t id)
        : Common(id)
        , m_strand(m_ioService)
        , m_sendTimer(m_ioService)
        , m_timerStopped(false)
        , m_receivedBytes(0)
    {
        if (!options.start_main)
        {
            return;
        }
        m_injectedNodes.insert(id);//consider ourselves already injected

        m_communication.reset(new Safir::Dob::Internal::Com::Communication
                              (Safir::Dob::Internal::Com::dataModeTag,
                               m_ioService,
                               options.name,
                               m_id,
                               options.nodeType,
                               Safir::Dob::Internal::Com::ResolvedAddress(options.dataAddress),
                               m_commNodeTypes));


        m_systemPicture.reset(new Safir::Dob::Internal::SP::SystemPicture
                              (Safir::Dob::Internal::SP::slave_tag,
                               m_ioService,
                               *m_communication,
                               options.name,
                               m_id,
                               options.nodeType,
                               m_spNodeTypes));

        m_communication->SetDataReceiver(m_strand.wrap([this](const int64_t /*fromNodeId*/,
                                            const int64_t /*fromNodeType*/,
                                            const char* data_,
                                            const size_t size)
        {
            const boost::shared_ptr<const char[]> data(data_);
            if (size != 10000)
            {
                throw std::logic_error("Received incorrectly sized data!");
            }
            for (size_t i = 0; i < size; ++i)
            {
                if (data[i] != 3)
                {
                    throw std::logic_error("Received corrupt data!");
                }
            }
            m_receivedBytes += size;
        }),
                                         1000100222,
                                         [](size_t size){return new char[size];},
                                         [](const char * data){delete[] data;});


        m_sendTimer.expires_from_now(boost::chrono::milliseconds(1000));
        m_sendTimer.async_wait([this](const boost::system::error_code& error){Send(error);});

        m_systemPicture->StartStateSubscription(m_strand.wrap([this](const Safir::Dob::Internal::SP::SystemState& data)
        {
            InjectNodes(data);
        }));

    }

    void Stop()
    {
        if (m_communication == nullptr)
        {
            return;
        }
        m_sendTimer.cancel();
        m_timerStopped = true;
        StopCommon();

        if (m_receivedBytes == 0)
        {
            log << "Main has received no data!" << std::endl;
        }
    }

    bool Success() const
    {
        if (m_communication == nullptr)
        {
            return true;
        }
        return SuccessCommon() && m_receivedBytes != 0;
    }
private:
    void Send(const boost::system::error_code& error)
    {
        if (error || m_timerStopped)
        {
            return;
        }

        const size_t size = 10000;
        const boost::shared_ptr<char[]> data(new char[size]);
        memset(data.get(), 3, size);
        //send the data to both node types.
        m_communication->Send(0,1,data,size,1000100222,true);
        m_communication->Send(0,2,data,size,1000100222,true);
        m_sendTimer.expires_from_now(boost::chrono::milliseconds(1000));
        m_sendTimer.async_wait([this](const boost::system::error_code& error){Send(error);});
    }

    void InjectNodes(const Safir::Dob::Internal::SP::SystemState& data)
    {
        for (int i = 0; i < data.Size(); ++i)
        {
            //don't inject dead or already injected nodes
            if (data.IsDead(i) || m_injectedNodes.find(data.Id(i)) != m_injectedNodes.end())
            {
                continue;
            }

            log << "Injecting node " << data.Name(i) << "(" << data.Id(i)
                     << ") of type " << data.NodeTypeId(i)
                     << " with address " << data.DataAddress(i) << std::endl;

            m_communication->InjectNode(data.Name(i),
                                        data.Id(i),
                                        data.NodeTypeId(i),
                                        data.DataAddress(i));

            m_injectedNodes.insert(data.Id(i));
        }
    }

    boost::asio::io_service::strand m_strand;
    std::set<int64_t> m_injectedNodes;

    boost::asio::steady_timer m_sendTimer;
    boost::atomic<bool> m_timerStopped;
    uint64_t m_receivedBytes;
};


int main(int argc, char * argv[])
{
    log << "Pid: " << Safir::Utilities::ProcessInfo::GetPid() << std::endl;

    try
    {
        //ensure call to CrashReporter::Stop at application exit
        boost::shared_ptr<void> crGuard(static_cast<void*>(0),
                                        [](void*){Safir::Utilities::CrashReporter::Stop();});
        Safir::Utilities::CrashReporter::Start();

        const ProgramOptions options(argc, argv);
        if (!options.parseOk)
        {
            return 1;
        }

        boost::asio::io_service ioService;
        boost::asio::io_service::strand strand(ioService);
        auto work = Safir::make_unique<boost::asio::io_service::work>(ioService);
        boost::asio::signal_set signalSet(ioService);

#if defined (_WIN32)
        signalSet.add(SIGABRT);
        signalSet.add(SIGBREAK);
        signalSet.add(SIGINT);
        signalSet.add(SIGTERM);
#else
        signalSet.add(SIGQUIT);
        signalSet.add(SIGINT);
        signalSet.add(SIGTERM);
#endif

        signalSet.async_wait([&work](const boost::system::error_code& error,
                                     const int /*signal_number*/)
                             {
                                 if (!!error && work != nullptr) //fix for ws2012 warning
                                 {
                                     log << "Got a signals error: " << error << std::endl;
                                 }
                                 work.reset();
                             });


        std::unique_ptr<Control> control;
        std::unique_ptr<Main> main;

        boost::function<void(bool restart)> stopFunc;
        stopFunc = [&](const bool restart)
        {
            if (control == nullptr || main == nullptr)
            {
                return;
            }

            control->Stop();
            main->Stop();

            if (!control->Success())
            {
                log << "Control failed!" << std::endl;
            }
            if (!main->Success())
            {
                log << "Main failed!" << std::endl;
            }

            if (!control->Success() || !main->Success())
            {
                throw std::runtime_error("Control or main failed");
            }
            control.reset();
            main.reset();

            if (restart)
            {
                const auto id = GenerateId();

                if (options.master)
                {
                    control.reset(new ControlMaster(options, id, strand.wrap(stopFunc)));
                }
                else
                {
                    control.reset(new ControlSlave(options, id, strand.wrap(stopFunc)));
                }
                main.reset(new Main(options, id));

                control->Start();
                main->Start();
            }
            else
            {
                work.reset();
                signalSet.cancel();
            }
        };

        const auto id = GenerateId();

        if (options.master)
        {
            control.reset(new ControlMaster(options,id, strand.wrap(stopFunc)));
        }
        else
        {
            control.reset(new ControlSlave(options, id, strand.wrap(stopFunc)));
        }
        main.reset(new Main(options, id));

        log << "Starting Control and Main" << std::endl;

        control->Start();
        main->Start();


        ioService.run();

        if (control != nullptr)
        {
            control->Stop();
            control.reset();
        }
        if (main != nullptr)
        {
            main->Stop();
            main.reset();
        }
    }
    catch(std::exception& e)
    {
        log << "Caught exception: " << e.what() << std::endl;
        return 1;
    }
    log << "Exiting..." << std::endl;
    return 0;
}
