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
#include <Safir/Utilities/Internal/LowLevelLoggerThreaded.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <boost/filesystem/exception.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <iostream>
#include <time.h>


//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4702)
  #pragma warning (disable : 4127)
  #pragma warning (disable : 4267)
  #pragma warning (disable : 4244)
#endif

#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/date_time/posix_time/time_formatters.hpp>
#include <boost/thread.hpp>

//and enable the warnings again
#if defined _MSC_VER
  #pragma warning (pop)
#endif

namespace Safir
{
namespace Utilities
{
namespace Internal
{
namespace InternalThreaded
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

        ProcessInfo proc(ProcessInfo::GetPid());
        std::string filename = proc.GetProcessName() + "-" + boost::lexical_cast<std::string>(ProcessInfo::GetPid()) + ".txt";

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
                    m_backend.OutputInternalBuffer();
                }
            }
            else
            {
               // In the lllerr case we can get here without logging being enabled. In this case the output thread isn't
                // running so we make the output synchronous from here
                m_backend.OutputFile() << m_ostr.str();
                std::wcout << m_ostr.str();
                m_backend.OutputFile().flush();
                std::wcout.flush();
            }

            m_ostr.str(std::wstring());
        }

        return _Tr::not_eof(c);
    }

    int LowLevelLoggerStreamBuf::sync()
    {
        m_backend.OutputInternalBuffer();
        return std::basic_streambuf<wchar_t, std::char_traits<wchar_t> >::sync();
    }


    void LowLevelLoggerStreamBuf::WriteDateTime()
    {
        std::string dateTime = boost::posix_time::to_simple_string(boost::posix_time::microsec_clock::universal_time());
        m_ostr << "[" <<  dateTime.c_str() << "] ";
    }

    boost::once_flag LowLevelLoggerBackend::SingletonHelper::m_onceFlag = BOOST_ONCE_INIT;

    LowLevelLoggerBackend & LowLevelLoggerBackend::SingletonHelper::Instance()
    {
        static LowLevelLoggerBackend instance;
        return instance;
    }

    LowLevelLoggerBackend & LowLevelLoggerBackend::Instance()
    {
        boost::call_once(SingletonHelper::m_onceFlag,boost::bind(SingletonHelper::Instance));
        return SingletonHelper::Instance();
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
          m_startupSynchronizer("LLUF_LLL_INITIALIZATION"),
          m_outputThreadStarted(false)
    {
        const boost::filesystem::path filename = GetLogFilename();
        if (filename.empty())
        {
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

        m_startupSynchronizer.Start(this);

        // start output thread
        boost::thread thr(boost::bind(&LowLevelLoggerBackend::OutputThread, this));

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

    void LowLevelLoggerBackend::CopyToInternalBuffer(const std::wostringstream& ostr)
    {
        boost::lock_guard<boost::mutex> lck(m_bufLock);

        m_os << ostr.str();

    }

    void LowLevelLoggerBackend::OutputInternalBuffer()
    {
        boost::lock_guard<boost::mutex> internalLck(m_internalBufLock);

        {
            boost::lock_guard<boost::mutex> lck(m_bufLock);
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

    void LowLevelLoggerBackend::OutputThread()
    {
        try
        {
            for (;;)
            {
                boost::this_thread::sleep(boost::posix_time::seconds(10));

                this->OutputInternalBuffer();
            }
        }
        catch (const std::exception & exc)
        {
            std::wostringstream ostr;
            ostr << "std::exception in LowLevelLoggerBackend::OutputWorker:" << std::endl
                << exc.what();
            std::wcout << ostr.str() << std::endl;
            exit(1);
        }
        catch (...)
        {
            std::wostringstream ostr;
            ostr << "exception thrown in LowLevelLoggerBackend::OutputWorker!";
            std::wcout << ostr.str() << std::endl;
            exit(1);
        }
    }
}
}
}
}
