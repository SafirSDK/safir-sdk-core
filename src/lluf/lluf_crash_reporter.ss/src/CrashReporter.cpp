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

namespace
{
    using namespace Safir::Utilities;

    /** Singleton class responsible for initiating the crash reporter thread. */
    class State
    {
    public:
        static State& Instance();

        void RegisterCallback(const CrashReporter::CrashCallback callback);
    private:
        State();
        ~State();
        
        //Callback functions for writing core dump
#if defined LLUF_CRASH_REPORTER_LINUX
        static bool callback(const char* /*dumpPath*/,
                             const char* /*id*/,
                             void* /*context*/,
                             bool /*succeeded*/)
#else
        static bool callback(const wchar_t */*dumpPath*/, 
                             const wchar_t */*id*/,
                             void */*context*/, 
                             EXCEPTION_POINTERS */*exinfo*/,
                             MDRawAssertionInfo */*assertion*/,
                             bool /*succeeded*/)
#endif
        {
            Instance().HandleCallback();

            //Returning false will leave the crash as unhandled
            //causing breakpad to terminate the application
            return false;
        }

        void HandleCallback();
        
        google_breakpad::ExceptionHandler * m_reporter;

        typedef std::vector<CrashReporter::CrashCallback> CrashCallbackTable;
        CrashCallbackTable m_callbacks;

        boost::mutex m_callbacksLock;
        
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


    State::State()
    {
    }

    void State::HandleCallback()
    {
        CrashCallbackTable copy;
        {
            boost::lock_guard<boost::mutex> lck(m_callbacksLock);
            copy = m_callbacks;
        }

        for (CrashCallbackTable::iterator it = copy.begin();
             it != copy.end(); ++it)
        {
            (*it)();
        }
    }
    void State::RegisterCallback(const CrashReporter::CrashCallback callback)
    {
        boost::lock_guard<boost::mutex> lck(m_callbacksLock);
        m_callbacks.push_back(callback);
    }

}


namespace Safir
{
namespace Utilities
{
    void CrashReporter::Start()
    {
        State::Instance();
        
    }

    void  CrashReporter::Stop()
    {

    }

    void  CrashReporter::RegisterCallback(const CrashCallback callback)
    {

    }
}
}
