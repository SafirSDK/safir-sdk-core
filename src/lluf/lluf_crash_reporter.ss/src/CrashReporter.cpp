/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#include <Safir/Utilities/CrashReporter.h>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <string>
#include <vector>
#include <mutex>
#include <memory>

#if defined(linux) || defined(__linux) || defined(__linux__)
#define LLUF_CRASH_REPORTER_LINUX
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
#define LLUF_CRASH_REPORTER_WINDOWS
#else
#error You need to adapt CrashReporter for this platform!
#endif

#if defined(LLUF_CRASH_REPORTER_LINUX)
#  include <client/linux/handler/exception_handler.h>
#else

#  ifdef _MSC_VER
#    pragma warning (push)
#    pragma warning (disable: 4091)
#  endif

#  include <client/windows/handler/exception_handler.h>
#  include "Windows.h"

#  ifdef _MSC_VER
#    pragma warning (pop)
#  endif

#endif

#include <boost/filesystem.hpp>

namespace
{
    using namespace Safir::Utilities;

    const boost::filesystem::path GetDumpDirectory()
    {
        Internal::ConfigReader config;
        return boost::filesystem::path(config.Locations().get<std::string>("crash_dump_directory"));
    }


    /** Singleton class responsible for initiating the crash reporter thread. */
    class State
    {
    public:
        static State& Instance();

        void Start();
        void Stop();

        void RegisterCallback(const CrashReporter::DumpCallback callback);

        bool Dump();
    private:
        State();
        ~State();

        //Callback functions for writing core dump
#if defined LLUF_CRASH_REPORTER_LINUX
        static bool callback(const google_breakpad::MinidumpDescriptor& descriptor,
                             void* /*context*/,
                             bool /*succeeded*/)
        {
            Instance().HandleCallback(descriptor.path());
#else
        static bool callback(const wchar_t *dumpPath,
                             const wchar_t *id,
                             void* /*context*/,
                             EXCEPTION_POINTERS* /*exinfo*/,
                             MDRawAssertionInfo* /*assertion*/,
                             bool /*succeeded*/)
        {
            //assume that dumpPath is ascii only!
            sprintf(&Instance().m_dumpPathSpace[0],"%S\\%S.dmp",dumpPath,id);
            Instance().HandleCallback(&Instance().m_dumpPathSpace[0]);
#endif

            //Returning false will leave the crash as unhandled
            //causing breakpad to terminate the application
            return false;
        }

        void HandleCallback(const char* const dumpPath);

        std::shared_ptr<google_breakpad::ExceptionHandler> m_handler;

        std::mutex m_lock;
        bool m_started;
        bool m_stopped;

#ifdef LLUF_CRASH_REPORTER_WINDOWS
        UINT m_errormode;

        //Since memory allocation in callback is dangerous, we get some
        //memory to use later.
        std::vector<char> m_dumpPathSpace;
#endif
        typedef std::vector<CrashReporter::DumpCallback> DumpCallbackTable;
        DumpCallbackTable m_callbacks;

        /**
         * This class is here to ensure that only the Instance method can get at the
         * instance, so as to be sure that call_once is used correctly.
         * Also makes it easier to grep for singletons in the code, if all
         * singletons use the same construction and helper-name.
         */
        struct SingletonHelper
        {
        private:
            friend State& State::Instance();

            static State& Instance();
            static std::once_flag m_onceFlag;
        };
    };


    std::once_flag State::SingletonHelper::m_onceFlag;

    State & State::SingletonHelper::Instance()
    {
        static State instance;
        return instance;
    }

    State & State::Instance()
    {
        std::call_once(SingletonHelper::m_onceFlag,[]{SingletonHelper::Instance();});
        return SingletonHelper::Instance();
    }


    State::State()
        : m_started(false)
        , m_stopped(false)
#ifdef LLUF_CRASH_REPORTER_WINDOWS
        , m_dumpPathSpace(2048,0) //2K characters should be plenty...
#endif
    {
    }

    State::~State()
    {
    }

    void State::HandleCallback(const char* const dumpPath)
    {
        for (DumpCallbackTable::iterator it = m_callbacks.begin();
             it != m_callbacks.end(); ++it)
        {
            (*it)(dumpPath);
        }
    }

    void State::Start()
    {

        std::unique_lock<std::mutex> lck(m_lock);
        if (m_stopped)
        {
            throw std::logic_error("Cannot restart the CrashReporter after it has been stopped!");
        }
        if (!m_started)
        {
            try
            {
                boost::filesystem::create_directories(GetDumpDirectory());
            }
            catch (const boost::filesystem::filesystem_error&)
            {
                throw std::logic_error("Failed to create dump directory '" + GetDumpDirectory().string() + "'.");
            }


#if defined LLUF_CRASH_REPORTER_LINUX
            m_handler.reset (new google_breakpad::ExceptionHandler(google_breakpad::MinidumpDescriptor(GetDumpDirectory().string()),
                                                                   NULL,
                                                                   callback,
                                                                   NULL,
                                                                   true,
                                                                   -1));
#else
            //GetErrorMode does not exist in winxp, so we do SetErrorMode twice instead.
            m_errormode = SetErrorMode(0);
            //Set the system to not display the Windows Error Reporting dialog.
            SetErrorMode(m_errormode|SEM_NOGPFAULTERRORBOX);

            m_handler.reset (new google_breakpad::ExceptionHandler(GetDumpDirectory().wstring(),
                                                                   NULL,
                                                                   callback,
                                                                   NULL,
                                                                   google_breakpad::ExceptionHandler::HANDLER_ALL));

#endif
        }
        m_started = true;
    }

    void State::Stop()
    {
        std::unique_lock<std::mutex> lck(m_lock);
        if (m_started && !m_stopped)
        {
#ifdef LLUF_CRASH_REPORTER_WINDOWS
            SetErrorMode(m_errormode); //Restore previous value
#endif
            m_handler.reset();
        }
        m_started = false;
        m_stopped = true;
    }


    void State::RegisterCallback(const CrashReporter::DumpCallback callback)
    {
        std::unique_lock<std::mutex> lck(m_lock);
        if (m_started || m_stopped)
        {
            throw std::logic_error("CrashReporter must not be started when registering callbacks!");
        }

        m_callbacks.push_back(callback);
    }

    bool State::Dump()
    {
        std::unique_lock<std::mutex> lck(m_lock);
        if (!m_started)
        {
            return false;
        }

        m_handler->WriteMinidump();

        return true;
    }


}


namespace Safir
{
namespace Utilities
{
    void CrashReporter::Start()
    {
        State::Instance().Start();
    }

    void CrashReporter::Stop()
    {
        State::Instance().Stop();
    }

    void CrashReporter::RegisterCallback(const DumpCallback callback)
    {
        State::Instance().RegisterCallback(callback);
    }

    bool CrashReporter::Dump()
    {
        return State::Instance().Dump();
    }
}
}
