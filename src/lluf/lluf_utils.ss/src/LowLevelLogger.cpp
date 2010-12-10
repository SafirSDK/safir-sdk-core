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
#endif

#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/date_time/posix_time/time_formatters.hpp>

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
        std::string filename = proc.GetProcessName() + ".txt";

        try
        {
            boost::filesystem::path fullpath = path/filename;
            if (boost::filesystem::is_regular(fullpath) && boost::filesystem::exists(fullpath))
            {
                boost::filesystem::remove(fullpath);
            }
            return fullpath;
        }
        catch (const boost::filesystem::filesystem_error &)
        {
            //try another name below.
        }

        filename = boost::lexical_cast<std::string>(ACE_OS::getpid()) + ".txt";

        try
        {
            boost::filesystem::path fullpath = path/filename;
            if (boost::filesystem::is_regular(fullpath) && boost::filesystem::exists(fullpath))
            {
                boost::filesystem::remove(fullpath);
            }
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

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4355)
    //disable using this in constructor warning
#endif

    LowLevelLoggerStreamBuf::LowLevelLoggerStreamBuf():
        std::basic_streambuf<wchar_t, std::char_traits<wchar_t> >(),
        m_bDatePending(true),
        m_pLoggingEnabled(NULL),
        m_startupSynchronizer("LLUF_LLL_INITIALIZATION", this)
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

        m_startupSynchronizer.Start();
    }

#ifdef _MSC_VER
#pragma warning(pop)
#endif

    void LowLevelLoggerStreamBuf::Create()
    {
        boost::interprocess::shared_memory_object::remove("LLUF_LLL_SHM");
        boost::interprocess::shared_memory_object shm(boost::interprocess::create_only,"LLUF_LLL_SHM", boost::interprocess::read_write);
        shm.truncate(sizeof(bool));
        boost::interprocess::mapped_region shmRegion(shm,boost::interprocess::read_write);
        *static_cast<bool*>(shmRegion.get_address()) = IsLoggingOnByDefault();
    }

    void LowLevelLoggerStreamBuf::Use()
    {
        boost::interprocess::shared_memory_object shm(boost::interprocess::open_only,"LLUF_LLL_SHM", boost::interprocess::read_only);
        m_shm.swap(shm);
        boost::interprocess::mapped_region shmRegion(m_shm,boost::interprocess::read_only);
        m_shmRegion.swap(shmRegion);
        m_pLoggingEnabled = static_cast<bool*>(m_shmRegion.get_address());
    }

    void LowLevelLoggerStreamBuf::Destroy()
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

        m_OutputFile.put(_Tr::to_char_type(c));
        std::wcout.put(_Tr::to_char_type(c));

        if (_Tr::to_char_type(c) == '\n')
        {
            m_bDatePending = true;
            m_OutputFile.flush();
            std::wcout.flush();
        }

        return _Tr::not_eof(c);;
    }

    void LowLevelLoggerStreamBuf::WriteDateTime()
    {
        std::string dateTime = boost::posix_time::to_simple_string(boost::posix_time::microsec_clock::universal_time());
        m_OutputFile << "[" <<  dateTime.c_str() << "] ";
        std::wcout   << "[" <<  dateTime.c_str() << "] ";
    }



    LowLevelLogger * volatile LowLevelLogger::m_pInstance = NULL;
    ACE_Thread_Mutex LowLevelLogger::m_InstantiationLock;


    //
    //Instance
    //
    LowLevelLogger & LowLevelLogger::Instance()
    {
        if (m_pInstance == NULL)
        {
            ACE_Guard<ACE_Thread_Mutex> lck(m_InstantiationLock);
            if (m_pInstance == NULL)
            {
                m_pInstance = new LowLevelLogger();
            }
        }
        return *m_pInstance;
    }


    LowLevelLogger::LowLevelLogger():
        std::basic_ostream<wchar_t, std::char_traits<wchar_t> >(new LowLevelLoggerStreamBuf())
    {

    }

    //
    // Destructor
    //
    LowLevelLogger::~LowLevelLogger()
    {

    }

}
}
}
}
