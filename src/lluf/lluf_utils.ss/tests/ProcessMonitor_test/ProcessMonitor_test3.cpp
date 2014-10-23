/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
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
#include "../../src/Safir/Utilities/ProcessMonitor.h"
#include <boost/bind.hpp>
#include <boost/thread.hpp>
#include <boost/lexical_cast.hpp>
#include <vector>
#include <set>

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
        : monitor(ioService,
                  boost::bind(&Fixture::TerminatedCb,this,_1),
                  boost::chrono::milliseconds(20)) //high polling rate to speed up tests.
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


    void RunIoService()
    {
        thread = boost::thread(boost::bind(&boost::asio::io_service::run, &ioService));
    }

    void Stop()
    {
        monitor.Stop();
        if (thread != boost::thread())
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
        BOOST_TEST_MESSAGE("LaunchSleeper " << duration);
#ifdef PROCMON_LINUX
        const pid_t pid = fork();
        switch (pid)
        {
        case -1:
            throw std::logic_error(std::string("fork failed: ") + strerror(errno));
        case 0:
            execl("./Sleeper", "Sleeper", boost::lexical_cast<std::string>(duration).c_str(), NULL);
            throw std::logic_error(std::string("execl failed: ") + strerror(errno));
        default:
            sleepers.insert(pid);
            return pid;
        }
#else
        STARTUPINFOA info={sizeof(info)};
        PROCESS_INFORMATION processInfo;
        if (::CreateProcessA(NULL, (LPSTR)(".\\Sleeper.exe " + boost::lexical_cast<std::string>(duration)).c_str(), NULL, NULL, TRUE, 0, NULL, NULL, &info, &processInfo))
        //if (::CreateProcessA(NULL, (LPSTR)std::string(".\\Sleeper.exe ").c_str(), NULL, NULL, TRUE, 0, NULL, NULL, &info, &processInfo))
        {
            //::CloseHandle(processInfo.hThread);
            sleepers.insert(std::make_pair(processInfo.dwProcessId,processInfo.hProcess));
            BOOST_TEST_MESSAGE("  launched " << processInfo.dwProcessId);
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
        BOOST_TEST_MESSAGE("WaitSleeper " << pid);
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
        BOOST_TEST_MESSAGE("WaitAny");
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
            ::WaitForSingleObject(sleepers.begin()->second, INFINITE);
            ::CloseHandle(sleepers.begin()->second);
            sleepers.erase(sleepers.begin());
        }
#endif
    }

    boost::asio::io_service ioService;
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
    RunIoService();
    Stop();
    BOOST_CHECK(TerminatedPids().empty());
}


BOOST_AUTO_TEST_CASE(monitor_0)
{
    RunIoService();
    monitor.StartMonitorPid(0);
    while(TerminatedPids().empty())
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
    }
    monitor.Stop();
    std::vector<pid_t> terminatedPids = TerminatedPids();
    BOOST_CHECK_EQUAL(terminatedPids.size(), 1);
    BOOST_CHECK_EQUAL(terminatedPids.at(0), 0);
}

BOOST_AUTO_TEST_CASE(monitor_0_twice)
{
    RunIoService();
    monitor.StartMonitorPid(0);
    monitor.StartMonitorPid(0);
    while(TerminatedPids().empty())
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
    }
    monitor.Stop();

    std::vector<pid_t> terminatedPids = TerminatedPids();
    BOOST_CHECK_LE(terminatedPids.size(), 2);
    BOOST_CHECK_EQUAL(terminatedPids.at(0), 0);
    if (terminatedPids.size() == 2)
    {
        BOOST_CHECK_EQUAL(terminatedPids.at(0), 0);
    }
}

BOOST_AUTO_TEST_CASE(monitor_self)
{
    RunIoService();
    monitor.StartMonitorPid(getpid());
    monitor.StopMonitorPid(getpid());

    Stop();

    std::vector<pid_t> terminatedPids = TerminatedPids();
    BOOST_CHECK(terminatedPids.empty());
}

BOOST_AUTO_TEST_CASE(monitor_self_twice)
{
    RunIoService();
    monitor.StartMonitorPid(getpid());
    monitor.StartMonitorPid(getpid());
    monitor.StopMonitorPid(getpid());
    monitor.StopMonitorPid(getpid());

    Stop();

    std::vector<pid_t> terminatedPids = TerminatedPids();
    BOOST_CHECK(terminatedPids.empty());
}

BOOST_AUTO_TEST_CASE(monitor_self_and_0)
{
    RunIoService();
    monitor.StartMonitorPid(getpid());
    monitor.StopMonitorPid(getpid());
    monitor.StartMonitorPid(0);
    while(TerminatedPids().empty())
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
    }
    Stop();

    std::vector<pid_t> terminatedPids = TerminatedPids();
    BOOST_CHECK_EQUAL(terminatedPids.size(), 1);
    BOOST_CHECK_EQUAL(terminatedPids.at(0), 0);
}

BOOST_AUTO_TEST_CASE(monitor_sleeper)
{
    RunIoService();
    const pid_t pid = LaunchSleeper(20);
    monitor.StartMonitorPid(pid);

    WaitSleeper(pid);
    while(TerminatedPids().empty())
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
    }

    Stop();

    std::vector<pid_t> terminatedPids = TerminatedPids();
    BOOST_CHECK_EQUAL(terminatedPids.size(), 1);
    BOOST_CHECK_EQUAL(terminatedPids.at(0), pid);
}

BOOST_AUTO_TEST_CASE(stop_monitor)
{
    RunIoService();
    const pid_t pid = LaunchSleeper(0.5);
    monitor.StartMonitorPid(pid);
    monitor.StopMonitorPid(pid);

    WaitSleeper(pid);
    boost::this_thread::sleep_for(boost::chrono::milliseconds(500));

    Stop();

    std::vector<pid_t> terminatedPids = TerminatedPids();
    BOOST_CHECK_EQUAL(terminatedPids.size(), 0);
}


BOOST_AUTO_TEST_CASE(many_sleepers)
{
    RunIoService();
    for (int i = 0; i < 100; ++i)
    {
        const pid_t pid = LaunchSleeper(0.3);
        monitor.StartMonitorPid(pid);
    }

    for (int i = 0; i < 100; ++i)
    {
        WaitAny();
    }

    while(TerminatedPids().size() != 100)
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
    }

    Stop();

    std::vector<pid_t> terminatedPids = TerminatedPids();
    BOOST_CHECK_EQUAL(terminatedPids.size(), 100);
}

BOOST_AUTO_TEST_SUITE_END()
