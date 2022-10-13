/******************************************************************************
*
* Copyright Saab AB, 2012,2014,2022 (http://safirsdkcore.com)
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
#include <atomic>
#include <boost/lexical_cast.hpp>
#include <fstream>
#include <iostream>
#include <map>

//disable warnings in boost
#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100 4267 4505)
#endif

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/program_options.hpp>
#include <boost/thread.hpp>
#include <boost/thread/recursive_mutex.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
//and disable a warning that happens on instantiation
#  pragma warning (disable : 4505)
#endif

const size_t DATA_SIZE = 3200;

Safir::Utilities::Internal::SharedCharArray createData()
{
    const Safir::Utilities::Internal::SharedCharArray data(new char[DATA_SIZE]);
    for (size_t i = 0; i < DATA_SIZE; ++i)
    {
        data[i] = static_cast<char>(i % 10);
    }
    return data;
}


Safir::Utilities::Internal::SharedCharArray DATA = createData();

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

    std::shared_ptr<boost::recursive_mutex> m_lock;
    static boost::recursive_mutex wcout_lock;
};
boost::recursive_mutex Magic::wcout_lock; //static instance

#define logout if(Magic lck_asdf = Magic::Lock()) ; else std::cout
#define logerr if(Magic lck_asdf = Magic::Lock()) ; else std::cerr

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

class ProgramOptions
{
public:
    ProgramOptions(int argc, char* argv[])
        : parseOk(false)
    {
        bool useMulticast = false;

        using namespace boost::program_options;
        options_description options("Options");
        options.add_options()
            ("help,h", "show help message")
            ("id",
             value<int64_t>(&nodeId)->required(),
             "nodeId for this node. Between 1 and 999")
            ("seed",
             value<int>(&seed)->default_value(-1),
             "Seed node.")
            ("use-multicast",
             bool_switch(&useMulticast),
             "Whether to use multicast communication or not")
            ("light",
             bool_switch(&isLightNode),
             "Whether to be a lightnode or not");

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

        if (nodeId < 1 || nodeId > 999)
        {
            ShowHelp(options);
            return;
        }

        nodeType = (useMulticast ? 2 : 1) + (isLightNode ? 10 : 0); //

        std::ostringstream ss;
        ss << std::setw(3) << std::setfill('0') << nodeId;
        name = "node_" + ss.str();
        address = "127.0.0.1";
        controlAddress = address + ":33" + ss.str();
        dataAddress = address + ":43" + ss.str();

        parseOk = true;
    }
    bool parseOk;

    int64_t nodeId;
    std::string name;
    std::string address;
    std::string controlAddress;
    std::string dataAddress;
    bool isLightNode = false;
    int seed;
    int revolutions;
    int64_t nodeType;
private:

    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        logerr << std::boolalpha
                   << "Usage: control_stub [OPTIONS]\n"
                   << desc << "\n"
                   << std::endl;
    }

};

std::vector<boost::chrono::steady_clock::duration> ToChronoDurations(const std::vector<int>& ms)
{
    std::vector<boost::chrono::steady_clock::duration> ch;
    for(auto it = ms.cbegin(); it != ms.cend(); ++it)
    {
        ch.push_back(boost::chrono::milliseconds(*it));
    }
    return ch;
}

class Common
{
public:
    Common(const std::int64_t id, const std::string& logPrefix)
        : m_success(true)
        , m_work(new boost::asio::io_service::work(m_ioService))
        , m_id(id)
        , m_logPrefix(logPrefix + ": ")
    {
        std::vector<int> retryTimeout20;
        retryTimeout20.push_back(20);
        retryTimeout20.push_back(200);
        std::vector<int> retryTimeout50;
        retryTimeout50.push_back(50);
        retryTimeout50.push_back(250);

        m_commNodeTypes.push_back(Safir::Dob::Internal::Com::NodeTypeDefinition
                                  (1,
                                   "NormalUnicast",
                                   "", //no multicast
                                   "", //no multicast
                                   false,
                                   500, //heartbeatInterval
                                   10, //maxLostHeartbeats
                                   20, //slidingwindowsize
                                   10, //ackrequestThreshold
                                   retryTimeout20));

        m_spNodeTypes.insert(std::make_pair(1,
                                            Safir::Dob::Internal::SP::NodeType
                                            (1,
                                             "NormalUnicast",
                                             false,
                                             boost::chrono::milliseconds(1000),
                                             10,
                                             ToChronoDurations(retryTimeout20))));

        m_commNodeTypes.push_back(Safir::Dob::Internal::Com::NodeTypeDefinition
                                  (2,
                                   "NormalMulticast",
                                   "224.123.45.67:10000",
                                   "224.123.45.67:10001",
                                   false,
                                   500,
                                   10,
                                   20,
                                   10,
                                   retryTimeout50));

        m_spNodeTypes.insert(std::make_pair(2,
                                            Safir::Dob::Internal::SP::NodeType
                                            (2,
                                             "NormalMulticast",
                                             false,
                                             boost::chrono::milliseconds(2000),
                                             10,
                                             ToChronoDurations(retryTimeout20))));

        m_commNodeTypes.push_back(Safir::Dob::Internal::Com::NodeTypeDefinition
                                  (11,
                                   "LightUnicast",
                                   "", //no multicast
                                   "", //no multicast
                                   true,
                                   500,
                                   10,
                                   20,
                                   10,
                                   retryTimeout20));

        m_spNodeTypes.insert(std::make_pair(11,
                                            Safir::Dob::Internal::SP::NodeType
                                            (11,
                                             "LightUnicast",
                                             true,
                                             boost::chrono::milliseconds(1000),
                                             10,
                                             ToChronoDurations(retryTimeout20))));

        m_commNodeTypes.push_back(Safir::Dob::Internal::Com::NodeTypeDefinition
                                  (12,
                                   "LightMulticast",
                                   "224.123.45.67:10002",
                                   "224.123.45.67:10003",
                                   true,
                                   500,
                                   10,
                                   20,
                                   10,
                                   retryTimeout50));

        m_spNodeTypes.insert(std::make_pair(12,
                                            Safir::Dob::Internal::SP::NodeType
                                            (12,
                                             "LightMulticast",
                                             true,
                                             boost::chrono::milliseconds(2000),
                                             8,
                                             ToChronoDurations(retryTimeout20))));
    }

    void Start()
    {
        if (m_communication == nullptr)
        {
            return;
        }

        m_communication->Start();

        //run ioservice in four threads, to increase likelyhood of detecting threading bugs
        for (int i = 0; i < 4; ++i)
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
                    {
                        logerr << m_logPrefix << "Caught 'std::exception' exception from io_service.run(): "
                               << "  '" << exc.what() << "'." << std::endl;
                    }
                    exit(1);
                }
                m_success.exchange(false);
                logerr << m_logPrefix << "Thread exiting due to failure" << std::endl;
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

    virtual ~Common() = default;
    std::atomic<bool> m_success;
    boost::thread_group m_threads;

    boost::asio::io_service m_ioService;
    //make some work to stop io_service from exiting.
    std::unique_ptr<boost::asio::io_service::work> m_work;

    const std::int64_t m_id;
    const std::string m_logPrefix;
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
                     const std::function<void()>& stopFcn)
        : Common(id, "Control")
        , m_strand(m_ioService)
        , m_stopFcn(stopFcn)
    {
        m_communication.reset(new Safir::Dob::Internal::Com::Communication
                              (Safir::Dob::Internal::Com::controlModeTag,
                               m_ioService,
                               options.name,
                               m_id,
                               options.nodeType,
                               Safir::Dob::Internal::Com::ResolvedAddress(options.controlAddress),
                               Safir::Dob::Internal::Com::ResolvedAddress(options.dataAddress),
                               m_commNodeTypes,
                               1450));

        if (options.seed != -1)
        {
            std::ostringstream ss;
            ss << options.address << ":33" << std::setw(3) << std::setfill('0') << options.seed;
            m_communication->InjectSeeds({ss.str()});
        }

        m_systemPicture.reset(new Safir::Dob::Internal::SP::SystemPicture
                              (Safir::Dob::Internal::SP::master_tag,
                               m_ioService,
                               *m_communication,
                               options.name,
                               m_id,
                               options.nodeType,
                               m_spNodeTypes,
                               boost::chrono::seconds(10), //don't use auto aloneTimeout
                               [this](const int64_t id)
                               {return CheckJoinSystem(id);},
                               [this](const int64_t id)
                               {return CheckFormSystem(id);}));

        // Start subscription to system state changes from SP
        m_systemPicture->StartStateSubscription(m_strand.wrap([this](const Safir::Dob::Internal::SP::SystemState& data)
                                                              {NewState(data);}));

    }

    void Stop()
    {
        StopCommon();
    }
    bool Success() const
    {
        return SuccessCommon();
    }
protected:

    bool CheckJoinSystem(const int64_t incarnationId)
    {
        logout << "{\"join\": " << incarnationId << "}" << std::endl;
        return true;
    }

    bool CheckFormSystem(const int64_t incarnationId)
    {
        logout << "{\"form\": " << incarnationId << "}" << std::endl;
        return true;
    }

    void NewState(const Safir::Dob::Internal::SP::SystemState& data)
    {
        logout << data.ToJson() << std::endl;
    }

    boost::asio::io_service::strand m_strand;

    const std::function<void()> m_stopFcn;
};


class Main: public Common
{
public:
    explicit Main(const ProgramOptions& options, const std::int64_t id)
        : Common(id, "Main")
        , m_isLightNode(options.isLightNode)
        , m_strand(m_ioService)
        , m_sendTimer(m_ioService)
        , m_timerStopped(false)
    {
        m_communication.reset(new Safir::Dob::Internal::Com::Communication
                              (Safir::Dob::Internal::Com::dataModeTag,
                               m_ioService,
                               options.name,
                               m_id,
                               options.nodeType,
                               Safir::Dob::Internal::Com::ResolvedAddress(options.dataAddress),
                               m_commNodeTypes,
                               1450));


        m_systemPicture.reset(new Safir::Dob::Internal::SP::SystemPicture
                              (Safir::Dob::Internal::SP::slave_tag,
                               m_ioService,
                               *m_communication,
                               options.name,
                               m_id,
                               options.nodeType,
                               m_spNodeTypes));

        m_communication->SetDataReceiver(m_strand.wrap([this](const int64_t fromNodeId,
                                                              const int64_t /*fromNodeType*/,
                                                              const char* data,
                                                              const size_t size)
        {
            if (size != DATA_SIZE)
            {
                throw std::logic_error("Received incorrectly sized data!");
            }
            for (size_t i = 0; i < size; ++i)
            {
                if (data[i] != static_cast<char>(i % 10))
                {
                    logerr << "Incorrect data in packet from node " << fromNodeId
                           <<". Byte "<< i <<" had unexpected value 0x" << std::hex
                           << static_cast<int>(data[i])
                           << " instead of 0x" << i % 10 << std::endl;
                    std::ostringstream o1;
                    std::ostringstream o2;
                    for (size_t j = std::max(0,static_cast<int>(i)-8); j < std::min(size, i+8); ++j)
                    {
                        o1 << " 0x" << std::setfill('0') << std::setw(2) << static_cast<int>(data[j]);
                        o2 << " 0x" << std::setfill('0') << std::setw(2) << static_cast<int>(j %10);
                    }
                    logerr << "Expected " << o2.str() << std::endl;
                    logerr << "Got      " << o1.str() << std::endl;
                    
                    throw std::logic_error("Received corrupt data!");
                }
            }
            m_expectDataFrom.erase(fromNodeId);
        }),
                                         1000100222,
                                         [](size_t size){return new char[size];},
                                         [](const char * data){delete[] data;});


        m_sendTimer.expires_from_now(boost::chrono::milliseconds(20));
        m_sendTimer.async_wait([this](const boost::system::error_code& error){Send(error);});

        m_systemPicture->StartStateSubscription(m_strand.wrap([this](const Safir::Dob::Internal::SP::SystemState& data)
        {
            HandleState(data);
        }));

    }

    void Stop()
    {
        m_sendTimer.cancel();
        m_timerStopped = true;
        StopCommon();

        CheckNoExpectedData();
    }

    bool Success() const
    {
        return SuccessCommon() && m_timerStopped && m_expectDataFrom.empty();
    }
private:
    void CheckNoExpectedData()
    {
        if (m_expectDataFrom.empty())
        {
            return;
        }
        for (const auto node: m_expectDataFrom)
        {
            logerr << "Expected data from " << node << std::endl;
        }
        throw std::logic_error("Not all expected data was received");
    }

    void Send(const boost::system::error_code& error)
    {
        if (error || m_timerStopped)
        {
            return;
        }

        //send the data to all node types.
        m_communication->Send(0, 1, DATA, DATA_SIZE, 1000100222, true);
        m_communication->Send(0, 2, DATA, DATA_SIZE, 1000100222, true);
        m_communication->Send(0, 11, DATA, DATA_SIZE, 1000100222, true);
        m_communication->Send(0, 12, DATA, DATA_SIZE, 1000100222, true);
        m_sendTimer.expires_from_now(boost::chrono::milliseconds(20));
        m_sendTimer.async_wait([this](const boost::system::error_code& error){Send(error);});
    }

    void HandleState(const Safir::Dob::Internal::SP::SystemState& state)
    {
        logerr << m_logPrefix << state.ToJson() << std::endl;

        if (m_lastState.IsValid() && !m_lastState.IsDetached() && state.IsDetached())
        {
            logerr << m_logPrefix << "Looks like I became detached. Excluding all nodes that were in my last state!" << std::endl;
            for (int i = 0; i < m_lastState.Size(); ++i)
            {
                //skip self, though
                if (m_lastState.Id(i) == m_id)
                {
                    continue;
                }
                logerr << m_logPrefix << " Calling ExcludeNode for node " << m_lastState.Id(i) << std::endl;
                m_systemPicture->ExcludeNode(m_lastState.Id(i));
            }

            m_expectDataFrom.clear();
            m_injectedNodes.clear();
        }

        std::set<int64_t> deadInLastState;
        for (int i = 0; i < m_lastState.Size(); ++i)
        {
            if (m_lastState.IsDead(i))
            {
                deadInLastState.insert(m_lastState.Id(i));
            }
        }

        for (int i = 0; i < state.Size(); ++i)
        {
            const auto id = state.Id(i);
            if (id == m_id)
            {
                continue;
            }

            if (m_isLightNode && (state.NodeTypeId(i) == 11 || state.NodeTypeId(i) == 12))
            {
                logerr << m_logPrefix << "Ignoring lightnode " << id << " since I am a lightnode" << std::endl;
                continue;
            }

            const bool injected = m_injectedNodes.find(id) != m_injectedNodes.end();
            const bool wasDead = deadInLastState.find(id) != deadInLastState.end();

            if (!state.IsDead(i) && !injected)
            {
                logerr << m_logPrefix << "Injecting node " << state.Name(i) << "(" << state.Id(i)
                       << ") of type " << state.NodeTypeId(i)
                       << " with address " << state.DataAddress(i) << std::endl;

                m_expectDataFrom.insert(id);
                m_communication->InjectNode(state.Name(i),
                                            id,
                                            state.NodeTypeId(i),
                                            state.DataAddress(i));

                m_injectedNodes.insert(id);
            }
            else if (state.IsDead(i) && injected && !wasDead)
            {
                logerr << m_logPrefix << "Excluding node " << state.Name(i) << "(" << id << ")" << std::endl;
                m_systemPicture->ExcludeNode(id);
                CheckNoExpectedData();
            }
            else if (!state.IsDead(i) && injected && wasDead)
            {
                logerr << "Main: Resurrecting node " << state.Name(i) << "(" << id << ")" << std::endl;
                m_expectDataFrom.insert(id);
                m_systemPicture->ResurrectNode(id);
            }
        }

        m_lastState = state;
    }

    const bool m_isLightNode;
    boost::asio::io_service::strand m_strand;
    std::set<int64_t> m_injectedNodes;
    boost::asio::steady_timer m_sendTimer;
    std::atomic<bool> m_timerStopped;
    std::set<int64_t> m_expectDataFrom;
    Safir::Dob::Internal::SP::SystemState m_lastState;
};

int main(int argc, char * argv[])
{
    logerr << "Pid: " << Safir::Utilities::ProcessInfo::GetPid() << std::endl;

    try
    {
        //ensure call to CrashReporter::Stop at application exit
        std::shared_ptr<void> crGuard(static_cast<void*>(0),
                                        [](void*){Safir::Utilities::CrashReporter::Stop();});
        Safir::Utilities::CrashReporter::Start();

        const ProgramOptions options(argc, argv);
        if (!options.parseOk)
        {
            return 1;
        }

        logerr << "Starting node " << options.name << " as node type " << options.nodeType << std::endl;

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
                                 if (error && work != nullptr)
                                 {
                                     logerr << "Got a signals error: " << error << std::endl;
                                 }
                                 work.reset();
                             });


        std::unique_ptr<Control> control;
        std::unique_ptr<Main> main;

        std::function<void()> stopFunc;
        stopFunc = [&]()
        {
            if (control == nullptr || main == nullptr)
            {
                return;
            }

            control->Stop();
            main->Stop();

            if (!control->Success())
            {
                logerr << "Control failed!" << std::endl;
            }
            if (!main->Success())
            {
                logerr << "Main failed!" << std::endl;
            }

            if (!control->Success() || !main->Success())
            {
                throw std::runtime_error("Control or main failed");
            }
            control.reset();
            main.reset();

            logerr << "Going to reset work and cancel the signalSet" << std::endl;
            work.reset();
            signalSet.cancel();
            logerr << "Done. ioService should stop running now" << std::endl;
        };

        const auto id = options.nodeId;

        control.reset(new Control(options, id, strand.wrap(stopFunc)));

        main.reset(new Main(options, id));

        logerr << "Starting Control and Main" << std::endl;

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
        logerr << "Caught exception: " << e.what() << std::endl;
        return 1;
    }
    logerr << "Exiting..." << std::endl;
    return 0;
}
