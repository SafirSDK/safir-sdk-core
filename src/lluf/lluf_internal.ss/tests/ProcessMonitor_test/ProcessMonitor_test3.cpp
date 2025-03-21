/******************************************************************************
*
* Copyright Saab AB, 2014,2023 (http://safirsdkcore.com)
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
#include <Safir/Utilities/ProcessMonitor.h>
#include <boost/lexical_cast.hpp>
#include <vector>
#include <set>
#include <thread>

#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4244)
#  pragma warning (disable : 4267)
#  pragma warning (disable : 4100)
#endif

#include <boost/thread.hpp>
#include <boost/asio.hpp>

#if defined _MSC_VER
#  pragma warning (pop)

//Disable the warning
//  'this' : used in base member initializer list
//that  we will get in the Fixture below.
#  pragma warning (disable : 4355)
#endif


#if defined(linux) || defined(__linux) || defined(__linux__)
#  define PROCMON_LINUX
#endif

#ifdef PROCMON_LINUX
#include <sys/wait.h>
#endif

#define BOOST_TEST_MODULE ProcessMonitorTest
#include <boost/test/unit_test.hpp>

struct Fixture
{
    Fixture()
        : io()
        , work(boost::asio::make_work_guard(io))
        , monitor(io,
                  [this](const pid_t pid){TerminatedCb(pid);},
                  std::chrono::milliseconds(20)) //high polling rate to speed up tests.
    {
        BOOST_TEST_MESSAGE("setup fixture");
    }

    ~Fixture()
    {
        BOOST_TEST_MESSAGE("teardown fixture");
        Stop();

#ifdef PROCMON_LINUX
        for (std::set<pid_t>::iterator it = sleepers.begin();
             it != sleepers.end(); ++it)
        {
            if (-1 == waitpid(*it, NULL,0))
            {
                perror("waitpid");
            }
        }
#else

        for (std::map<pid_t,HANDLE>::iterator it = sleepers.begin();
             it != sleepers.end(); ++it)
        {
            ::WaitForSingleObject(it->second,INFINITE);
            ::CloseHandle(it->second);
        }
#endif
    }

    void RunIoContext()
    {
        thread = boost::thread([this]{io.run();});
    }

    void Stop()
    {
        work.reset();
        monitor.Stop();
        if (thread.get_id() != boost::thread::id())
        {
            thread.join();
            thread = boost::thread();
        }
    }

    std::vector<pid_t> TerminatedPids() const
    {
        boost::lock_guard<boost::mutex> lck(mutex);
        return terminatedPids;
    }

    pid_t LaunchSleeper(const double duration)
    {
        using namespace boost::unit_test;
        BOOST_TEST_REQUIRE( framework::master_test_suite().argc == 3 );
        BOOST_TEST_REQUIRE( framework::master_test_suite().argv[1] == "--sleeper-exe" );

        const std::string sleeper = framework::master_test_suite().argv[2];

#ifdef PROCMON_LINUX
        const pid_t pid = fork();
        switch (pid)
        {
        case -1:
            throw std::logic_error(std::string("fork failed: ") + strerror(errno));
        case 0:
            execl(sleeper.c_str(),
                  "ProcessMonitorSleeper",
                  boost::lexical_cast<std::string>(duration).c_str(),
                  nullptr);
            throw std::logic_error(std::string("execl failed: ") + strerror(errno));
        default:
            sleepers.insert(pid);
            return pid;
        }
#else
        STARTUPINFOA info={sizeof(info)};
        PROCESS_INFORMATION processInfo;
        if (::CreateProcessA(NULL,
                             (LPSTR)((sleeper + " " +
                                     boost::lexical_cast<std::string>(duration)).c_str()),
                             NULL,
                             NULL,
                             TRUE, 0, NULL, NULL, &info, &processInfo))
        {
            ::CloseHandle(processInfo.hThread);
            sleepers.insert(std::make_pair(processInfo.dwProcessId,processInfo.hProcess));
            return processInfo.dwProcessId;
        }
        else
        {
            throw std::logic_error("CreateProcess failed!");
        }
#endif
    }

    void WaitSleeper(const pid_t pid)
    {
#ifdef PROCMON_LINUX
        if (-1 == waitpid(pid, NULL,0))
        {
            perror("waitpid");
        }
#else
        std::map<pid_t,HANDLE>::iterator findIt = sleepers.find(pid);
        if (findIt == sleepers.end())
        {
            throw std::logic_error("boohoo");
        }
        ::WaitForSingleObject(findIt->second, INFINITE);
        ::CloseHandle(findIt->second);
#endif
        sleepers.erase(pid);
    }

    void WaitAny()
    {
#ifdef PROCMON_LINUX
        int status;
        const pid_t pid = wait(&status);
        if (pid == -1)
        {
            perror("wait");
        }
        sleepers.erase(pid);
#else
        if (!sleepers.empty())
        {
            if (WAIT_OBJECT_0 != ::WaitForSingleObject(sleepers.begin()->second, INFINITE))
            {
                std::wcout << "Error in WaitForSingleObject" << std::endl;
            }

            ::CloseHandle(sleepers.begin()->second);
            sleepers.erase(sleepers.begin());
        }
#endif
    }
    boost::asio::io_context io;
    boost::asio::executor_work_guard<boost::asio::io_context::executor_type> work;
    boost::thread thread;

    Safir::Utilities::ProcessMonitor monitor;
private:
    void TerminatedCb(const pid_t pid)
    {
        boost::lock_guard<boost::mutex> lck(mutex);
        terminatedPids.push_back(pid);
    }

    std::vector<pid_t> terminatedPids;
    mutable boost::mutex mutex;

#ifdef PROCMON_LINUX
    std::set<pid_t> sleepers;
#else
    std::map<pid_t,HANDLE> sleepers;
#endif
};

BOOST_FIXTURE_TEST_SUITE( s, Fixture )

BOOST_AUTO_TEST_CASE(create_destroy)
{
    RunIoContext();
    Stop();
    BOOST_CHECK(TerminatedPids().empty());
}

BOOST_AUTO_TEST_CASE(stop_unknown)
{
    RunIoContext();
    monitor.StopMonitorPid(100);
    work.reset();
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    Stop();
    BOOST_CHECK(TerminatedPids().empty());
}

BOOST_AUTO_TEST_CASE(monitor_0)
{
    RunIoContext();
    monitor.StartMonitorPid(0);
    while(TerminatedPids().empty())
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
    monitor.Stop();
    std::vector<pid_t> terminated = TerminatedPids();
    BOOST_CHECK_EQUAL(terminated.size(), 1U);
    BOOST_CHECK_EQUAL(terminated.at(0), 0);
}

BOOST_AUTO_TEST_CASE(monitor_0_twice)
{
    RunIoContext();
    monitor.StartMonitorPid(0);
    monitor.StartMonitorPid(0);
    while(TerminatedPids().empty())
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
    monitor.Stop();

    std::vector<pid_t> terminated = TerminatedPids();
    BOOST_CHECK_LE(terminated.size(), 2U);
    BOOST_CHECK_EQUAL(terminated.at(0), 0);
    if (terminated.size() == 2)
    {
        BOOST_CHECK_EQUAL(terminated.at(0), 0);
    }
}

BOOST_AUTO_TEST_CASE(monitor_self)
{
    RunIoContext();
    monitor.StartMonitorPid(getpid());
    monitor.StopMonitorPid(getpid());

    Stop();

    std::vector<pid_t> terminated = TerminatedPids();
    BOOST_CHECK(terminated.empty());
}

BOOST_AUTO_TEST_CASE(monitor_self_twice)
{
    RunIoContext();
    monitor.StartMonitorPid(getpid());
    monitor.StartMonitorPid(getpid());
    monitor.StopMonitorPid(getpid());
    monitor.StopMonitorPid(getpid());

    Stop();

    std::vector<pid_t> terminated = TerminatedPids();
    BOOST_CHECK(terminated.empty());
}

BOOST_AUTO_TEST_CASE(monitor_self_and_0)
{
    RunIoContext();
    monitor.StartMonitorPid(getpid());
    monitor.StopMonitorPid(getpid());
    monitor.StartMonitorPid(0);
    while(TerminatedPids().empty())
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
    Stop();

    std::vector<pid_t> terminated = TerminatedPids();
    BOOST_CHECK_EQUAL(terminated.size(), 1U);
    BOOST_CHECK_EQUAL(terminated.at(0), 0);
}

BOOST_AUTO_TEST_CASE(monitor_sleeper)
{
    RunIoContext();
    const pid_t pid = LaunchSleeper(0.5);
    monitor.StartMonitorPid(pid);

    WaitSleeper(pid);
    while(TerminatedPids().empty())
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }

    Stop();

    std::vector<pid_t> terminated = TerminatedPids();
    BOOST_CHECK_EQUAL(terminated.size(), 1U);
    BOOST_CHECK_EQUAL(terminated.at(0), pid);
}

BOOST_AUTO_TEST_CASE(stop_monitor)
{
    RunIoContext();
    const pid_t pid = LaunchSleeper(0.5);
    monitor.StartMonitorPid(pid);
    monitor.StopMonitorPid(pid);

    WaitSleeper(pid);
    std::this_thread::sleep_for(std::chrono::milliseconds(500));

    Stop();

    std::vector<pid_t> terminated = TerminatedPids();
    BOOST_CHECK_EQUAL(terminated.size(), 0U);
}

BOOST_AUTO_TEST_CASE(many_sleepers)
{
    RunIoContext();

    std::set<pid_t> pids;
    BOOST_TEST_MESSAGE("Launching sleepers");
    for (int i = 0; i < 30; ++i)
    {
        const pid_t pid = LaunchSleeper(0.3);
        pids.insert(pid);
        monitor.StartMonitorPid(pid);
    }

    BOOST_TEST_MESSAGE("Waiting for exit");
    for (int i = 0; i < 30; ++i)
    {
        WaitAny();
    }

    BOOST_TEST_MESSAGE("Waiting for term");
    while(TerminatedPids().size() != 30)
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }

    BOOST_TEST_MESSAGE("Stopping");
    Stop();

    BOOST_TEST_MESSAGE("Checking result");
    const std::vector<pid_t> terminated = TerminatedPids();
    BOOST_CHECK_EQUAL(terminated.size(), 30U);
    for (std::vector<pid_t>::const_iterator it = terminated.begin();
         it != terminated.end(); ++it)
    {
        pids.erase(*it);
    }
    BOOST_CHECK(pids.empty());
}

BOOST_AUTO_TEST_SUITE_END()
