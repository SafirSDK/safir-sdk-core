/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <time.h>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/exception.hpp>
#include <iostream>
#include <ace/Guard_T.h>
#include <ace/OS_NS_unistd.h>

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4702)
  #pragma warning (disable : 4127)
  #pragma warning (disable : 4267)
#endif

#include <ace/Thread.h>
#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/date_time/posix_time/time_formatters.hpp>

//and enable the warnings again
#if defined _MSC_VER
  #pragma warning (pop)
#endif

#define OUTPUT_LOG_TIMEOUT 10

namespace Safir
{
namespace Utilities
{
namespace Internal
{
namespace Internal
{
    boost::filesystem::path GetLogDirectory()
    {
        const char * ENV_NAME = "SAFIR_RUNTIME";
        char * env = getenv(ENV_NAME);
        if (env == NULL)
        {
            return "";
        }
        boost::filesystem::path filename(env,boost::filesystem::native);

        filename /= "log";
        filename /= "Dob-LowLevelLog";
        return filename;
    }

    //also creates directories and removes files that are in the way
    boost::filesystem::path GetLogFilename()
    {
        boost::filesystem::path path = GetLogDirectory();
        if (path.empty())
        {
            return "";
        }

        try
        {
            boost::filesystem::create_directory(path);
        }
        catch (const boost::filesystem::filesystem_error &)
        {
            return "";
        }

        ProcessInfo proc(ACE_OS::getpid());
        std::string filename = proc.GetProcessName() + "-" + boost::lexical_cast<std::string>(ACE_OS::getpid()) + ".txt";

        try
        {
            boost::filesystem::path fullpath = path/filename;
            return fullpath;
        }
        catch (const boost::filesystem::filesystem_error &)
        {
            //failed to find useful name
            return std::string("strange-") + boost::lexical_cast<std::string>(rand());
        }

     }


    //check for file %SAFIR_RUNTIME%\log\Dob-LowLevelLog\logging_on
    bool IsLoggingOnByDefault()
    {
        boost::filesystem::path filename = GetLogDirectory();
        if (filename.empty())
        {
            return false;
        }
        filename /= "logging_on";
        return boost::filesystem::exists(filename);
    }

    LowLevelLogger::LowLevelLogger(bool forceFlush)
        : std::basic_ostream<wchar_t, std::char_traits<wchar_t> >(new LowLevelLoggerStreamBuf(forceFlush))
    {
    }

    LowLevelLogger::~LowLevelLogger()
    {
        delete rdbuf();
    }

    LowLevelLoggerStreamBuf::LowLevelLoggerStreamBuf(bool forceFlush)
        : std::basic_streambuf<wchar_t, std::char_traits<wchar_t> >(),
          m_bDatePending(true),
          m_ostr(),
          m_backend(LowLevelLoggerBackend::Instance()),
          m_forceFlush(forceFlush)

    {
    }

    LowLevelLoggerStreamBuf::~LowLevelLoggerStreamBuf()
    {
    }

    LowLevelLoggerStreamBuf::_Tr::int_type
    LowLevelLoggerStreamBuf::uflow()
    {
        return _Tr::eof();
    }

    LowLevelLoggerStreamBuf::_Tr::int_type
    LowLevelLoggerStreamBuf::underflow()
    {
        return _Tr::eof();
    }

    LowLevelLoggerStreamBuf::_Tr::int_type
    LowLevelLoggerStreamBuf::overflow(_Tr::int_type c)
    {
        if (c ==_Tr::eof())
        {
            return _Tr::not_eof(c);
        }

        if (m_bDatePending)
        {
            WriteDateTime();
            m_bDatePending = false;
        }

        m_ostr.put(_Tr::to_char_type(c));

        if (_Tr::to_char_type(c) == '\n')
        {
            m_bDatePending = true;
            m_ostr.flush();

            if (m_backend.OutputThreadStarted())
            {
                m_backend.CopyToInternalBuffer(m_ostr);
                if (m_forceFlush)
                {
                    m_backend.FlushBuffer();
                }
            }
            else
            {
               // In the lllerr case we can get here without logging being enabled. In this case the output thread isn't
                // running so we make the output synchronous from here
                std::wcout << m_ostr.str();
                std::wcout.flush();
            }

            m_ostr.str(std::wstring());
        }

        return _Tr::not_eof(c);
    }

    void LowLevelLoggerStreamBuf::WriteDateTime()
    {
        std::string dateTime = boost::posix_time::to_simple_string(boost::posix_time::microsec_clock::universal_time());
        m_ostr << "[" <<  dateTime.c_str() << "] ";
    }

    LowLevelLoggerBackend * volatile LowLevelLoggerBackend::m_pInstance = NULL;
    ACE_Thread_Mutex LowLevelLoggerBackend::m_InstantiationLock;

    //
    //Instance
    //
    LowLevelLoggerBackend & LowLevelLoggerBackend::Instance()
    {
        if (m_pInstance == NULL)
        {
            ACE_Guard<ACE_Thread_Mutex> lck(m_InstantiationLock);
            if (m_pInstance == NULL)
            {
                m_pInstance = new LowLevelLoggerBackend();
            }
        }
        return *m_pInstance;
    }

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4355)
    //disable using this in constructor warning
#endif

    LowLevelLoggerBackend::LowLevelLoggerBackend()
        : m_os(),
          m_outputBuf(),
          m_pLoggingEnabled(NULL),
          m_startupSynchronizer("LLUF_LLL_INITIALIZATION", this),
          m_outputThreadStarted(false)
    {
        const boost::filesystem::path filename = GetLogFilename();
        if (filename.empty())
        {
            m_reactor.close();
            //logging will be disabled (pointer is NULL)
            return;
        }

        try
        {
            //open the output file
            m_OutputFile.open(filename,std::ios::out);
            if (!m_OutputFile.good())
            {
                m_OutputFile.close();
                return;
            }
        }
        catch (const std::exception &)
        {
            m_OutputFile.close();
            return;
        }

        const std::size_t BufferInitialSize = 4000;

        m_os.reserve(BufferInitialSize);
        m_outputBuf.reserve(BufferInitialSize);

        m_startupSynchronizer.Start();

        // start output thread
        StartThread();

        m_outputThreadStarted = true;
    }

#ifdef _MSC_VER
#pragma warning(pop)
#endif

    //
    // Destructor
    //
    LowLevelLoggerBackend::~LowLevelLoggerBackend()
    {
        StopThread();
    }

    void LowLevelLoggerBackend::Create()
    {
        boost::interprocess::shared_memory_object::remove("LLUF_LLL_SHM");
        boost::interprocess::shared_memory_object shm(boost::interprocess::create_only,"LLUF_LLL_SHM", boost::interprocess::read_write);
        shm.truncate(sizeof(bool));
        boost::interprocess::mapped_region shmRegion(shm,boost::interprocess::read_write);
        *static_cast<bool*>(shmRegion.get_address()) = IsLoggingOnByDefault();
    }

    void LowLevelLoggerBackend::Use()
    {
        boost::interprocess::shared_memory_object shm(boost::interprocess::open_only,"LLUF_LLL_SHM", boost::interprocess::read_only);
        m_shm.swap(shm);
        boost::interprocess::mapped_region shmRegion(m_shm,boost::interprocess::read_only);
        m_shmRegion.swap(shmRegion);
        m_pLoggingEnabled = static_cast<bool*>(m_shmRegion.get_address());
    }

    void LowLevelLoggerBackend::Destroy()
    {
    }

    void LowLevelLoggerBackend::FlushBuffer()
    {
        if (m_outputThreadStarted)
        {
            m_reactor.notify(this);
        }
    }

    void LowLevelLoggerBackend::CopyToInternalBuffer(const std::wostringstream& ostr)
    {
        ACE_Guard<ACE_Thread_Mutex> lck(m_bufLock);

        m_os << ostr.str();

    }

    void LowLevelLoggerBackend::StartThread()
    {
        if (m_outputThreadStarted)
            return;

        ACE_thread_t threadId;
        ACE_hthread_t threadHandle;

        const int result = ACE_Thread::spawn(&LowLevelLoggerBackend::OutputThreadFunc, 
                                             this,
                                             THR_NEW_LWP | THR_JOINABLE ,
                                             &threadId,
                                             &threadHandle);

        m_outputThreadStarted = true;

    }

    void LowLevelLoggerBackend::StopThread()
    {
        if (m_outputThreadStarted)
        {
            m_reactor.end_reactor_event_loop();

            m_outputThreadStarted = false;
        }
    }

    void LowLevelLoggerBackend::Run()
    {        
        m_reactor.owner(ACE_Thread::self());
        m_reactor.schedule_timer(this, NULL, ACE_Time_Value(OUTPUT_LOG_TIMEOUT,0));

        m_reactor.run_reactor_event_loop();

    }

    int LowLevelLoggerBackend::handle_exception (ACE_HANDLE)
    {
        OutputInternalBuffer();
        return 0;
    }

    int LowLevelLoggerBackend::handle_timeout (const ACE_Time_Value &current_time, const void *act)
    {
        m_reactor.schedule_timer(this, NULL, ACE_Time_Value(OUTPUT_LOG_TIMEOUT,0));
        OutputInternalBuffer();
        return 0;
    }


    void LowLevelLoggerBackend::OutputInternalBuffer()
    {
        if (!LoggingEnabled())
            return;

        ACE_Guard<ACE_Thread_Mutex> internalLck(m_internalBufLock);

        { 
            ACE_Guard<ACE_Thread_Mutex> lck(m_bufLock);
            //std::string dateTime = boost::posix_time::to_simple_string(boost::posix_time::microsec_clock::universal_time());
            //m_os << "FLUSH : " << dateTime.c_str() << std::endl;

            // Swap the output buffer and the underlying log buffer.
            m_os.swap_vector(m_outputBuf);

        } // buffer lock released here

        for (BufferT::const_iterator it = m_outputBuf.begin(); it != m_outputBuf.end(); ++it)
        {
            m_OutputFile.put(*it);
            std::wcout.put(*it);
        }
        m_OutputFile.flush();
        std::wcout.flush();

        m_outputBuf.clear();
    }

    ACE_THR_FUNC_RETURN LowLevelLoggerBackend::OutputThreadFunc(void * _this)
    {
        LowLevelLoggerBackend* This = static_cast<LowLevelLoggerBackend*>(_this);
        try
        {
            This->Run();
        }
        catch (const std::exception & exc)
        {
            std::wostringstream ostr;
            ostr << "std::exception in LowLevelLoggerBackend::OutputThreadFunc:" << std::endl
                << exc.what();
            std::wcout << ostr.str() << std::endl;
            exit(-1);
        }
        catch (...)
        {
            std::wostringstream ostr;
            ostr << "exception thrown in LowLevelLoggerBackend::OutputThreadFunc!";
            std::wcout << ostr.str() << std::endl;
            exit(-1);
        }

        return NULL;
    }

}
}
}
}
