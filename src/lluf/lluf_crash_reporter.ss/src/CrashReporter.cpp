/******************************************************************************
*
* Copyright Saab Systems AB, 2012 (http://www.safirsdk.com)
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
#include <string>
#include <vector>
#include <boost/bind.hpp>
#include <boost/thread/once.hpp>
#include <boost/thread/mutex.hpp>

#if defined(linux) || defined(__linux) || defined(__linux__)
#define LLUF_CRASH_REPORTER_LINUX
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
#define LLUF_CRASH_REPORTER_WINDOWS
#else
#error You need to adapt CrashReporter for this platform!
#endif

#if defined(LLUF_CRASH_REPORTER_LINUX)
#include "client/linux/handler/exception_handler.h"
#else
#include "client/windows/handler/exception_handler.h"
#endif

#define BOOST_FILESYSTEM_NO_DEPRECATED
#define BOOST_FILESYSTEM_VERSION 3
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/convenience.hpp>


namespace
{
    using namespace Safir::Utilities;

    const boost::filesystem::path GetDumpDirectory()
    {
#if defined (LLUF_CRASH_REPORTER_LINUX)
        const char * const env = getenv("SAFIR_RUNTIME");
#else
        const wchar_t * const env = _wgetenv("SAFIR_RUNTIME");
#endif
        if (env == NULL)
        {
            throw std::logic_error("SAFIR_RUNTIME environment variable is not set");
        }
        boost::filesystem::path filename(env);

        filename /= "dump";
        filename /= "reports";
        return filename;
    }


    /** Singleton class responsible for initiating the crash reporter thread. */
    class State
    {
    public:
        static State& Instance();

        void Start();
        void Stop();

        void RegisterCallback(const CrashReporter::CrashCallback callback);
    private:
        State();
        ~State();
        
        //Callback functions for writing core dump
#if defined LLUF_CRASH_REPORTER_LINUX
        static bool callback(const char* dumpPath_,
                             const char* id,
                             void* /*context*/,
                             bool /*succeeded*/)
        {
            const std::string dumpPath = std::string(dumpPath_) + "/" + id + ".dmp";
#else
        static bool callback(const wchar_t *dumpPath_, 
                             const wchar_t *id,
                             void */*context*/, 
                             EXCEPTION_POINTERS */*exinfo*/,
                             MDRawAssertionInfo */*assertion*/,
                             bool /*succeeded*/)
        {
            //TODO fix windows!
            //assume that dumpPath_ is ascii only!
            const std::string dumpPath(dumpPath_, dumpPath+strlen(dumpPath));
#endif
            Instance().HandleCallback(dumpPath);

            //Returning false will leave the crash as unhandled
            //causing breakpad to terminate the application
            return false;
        }

        void HandleCallback(const std::string& dumpPath);
        
        boost::shared_ptr<google_breakpad::ExceptionHandler> m_handler;

        boost::mutex m_lock;
        bool m_started;
        bool m_stopped;
        
        typedef std::vector<CrashReporter::CrashCallback> CrashCallbackTable;
        CrashCallbackTable m_callbacks;        
        /**
         * This class is here to ensure that only the Instance method can get at the 
         * instance, so as to be sure that boost call_once is used correctly.
         * Also makes it easier to grep for singletons in the code, if all 
         * singletons use the same construction and helper-name.
         */
        struct SingletonHelper
        {
        private:
            friend State& State::Instance();
            
            static State& Instance();
            static boost::once_flag m_onceFlag;
        };
    };


    boost::once_flag State::SingletonHelper::m_onceFlag = BOOST_ONCE_INIT;

    State & State::SingletonHelper::Instance()
    {
        static State instance;
        return instance;
    }

    State & State::Instance()
    {
        boost::call_once(SingletonHelper::m_onceFlag,boost::bind(SingletonHelper::Instance));
        return SingletonHelper::Instance();
    }


    State::State():
        m_started(false),
        m_stopped(false)
    {
    }

    State::~State()
    {
    }

    void State::HandleCallback(const std::string& dumpPath)
    {
        CrashCallbackTable copy;
        {
            boost::lock_guard<boost::mutex> lck(m_lock);
            copy = m_callbacks;
        }

        for (CrashCallbackTable::iterator it = copy.begin();
             it != copy.end(); ++it)
        {
            (*it)(dumpPath.c_str());
        }
    }

    void State::Start()
    {
        boost::lock_guard<boost::mutex> lck(m_lock);
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
            m_handler.reset (new google_breakpad::ExceptionHandler(GetDumpDirectory().string(),
                                                                   NULL, 
                                                                   callback, 
                                                                   NULL, 
                                                                   true));
#else
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
        boost::lock_guard<boost::mutex> lck(m_lock);
        if (m_started && !m_stopped)
        {
            m_handler.reset();
        }
        m_started = false;
        m_stopped = true;
    }


    void State::RegisterCallback(const CrashReporter::CrashCallback callback)
    {
        boost::lock_guard<boost::mutex> lck(m_lock);
        if (!m_started || m_stopped)
        {
            throw std::logic_error("CrashReporter is not started");
        }

        m_callbacks.push_back(callback);
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

    void  CrashReporter::Stop()
    {
        State::Instance().Stop();
    }

    void  CrashReporter::RegisterCallback(const CrashCallback callback)
    {
        State::Instance().RegisterCallback(callback);
    }
}
}
