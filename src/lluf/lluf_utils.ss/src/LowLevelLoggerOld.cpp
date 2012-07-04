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
#include <Safir/Utilities/Internal/LowLevelLoggerOld.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <time.h>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/exception.hpp>
#include <iostream>

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

        ProcessInfo proc(ProcessInfo::GetPid());
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

        filename = boost::lexical_cast<std::string>(ProcessInfo::GetPid()) + ".txt";

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

    LowLevelLoggerOldStreamBuf::LowLevelLoggerOldStreamBuf():
        std::basic_streambuf<wchar_t, std::char_traits<wchar_t> >(),
        m_datePending(true),
        m_pLoggingEnabled(NULL),
        m_startupSynchronizer("LLUF_LLL_INITIALIZATION")
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
            m_outputFile.open(filename,std::ios::out);
            if (!m_outputFile.good())
            {
                m_outputFile.close();
                return;
            }
        }
        catch (const std::exception &)
        {
            m_outputFile.close();
            return;
        }

        m_startupSynchronizer.Start(this);
    }

#ifdef _MSC_VER
#pragma warning(pop)
#endif

    void LowLevelLoggerOldStreamBuf::Create()
    {
        boost::interprocess::shared_memory_object::remove("LLUF_LLL_SHM");
        boost::interprocess::shared_memory_object shm(boost::interprocess::create_only,"LLUF_LLL_SHM", boost::interprocess::read_write);
        shm.truncate(sizeof(bool));
        boost::interprocess::mapped_region shmRegion(shm,boost::interprocess::read_write);
        *static_cast<bool*>(shmRegion.get_address()) = IsLoggingOnByDefault();
    }

    void LowLevelLoggerOldStreamBuf::Use()
    {
        boost::interprocess::shared_memory_object shm(boost::interprocess::open_only,"LLUF_LLL_SHM", boost::interprocess::read_only);
        m_shm.swap(shm);
        boost::interprocess::mapped_region shmRegion(m_shm,boost::interprocess::read_only);
        m_shmRegion.swap(shmRegion);
        m_pLoggingEnabled = static_cast<bool*>(m_shmRegion.get_address());
    }

    void LowLevelLoggerOldStreamBuf::Destroy()
    {

    }


    LowLevelLoggerOldStreamBuf::~LowLevelLoggerOldStreamBuf()
    {

    }

    LowLevelLoggerOldStreamBuf::_Tr::int_type
    LowLevelLoggerOldStreamBuf::uflow()
    {
        return _Tr::eof();
    }

    LowLevelLoggerOldStreamBuf::_Tr::int_type
    LowLevelLoggerOldStreamBuf::underflow()
    {
        return _Tr::eof();
    }

    LowLevelLoggerOldStreamBuf::_Tr::int_type
    LowLevelLoggerOldStreamBuf::overflow(_Tr::int_type c)
    {
        if (c ==_Tr::eof())
        {
            return sync();
        }

        if (m_datePending)
        {
            WriteDateTime();
            m_datePending = false;
        }

        m_outputFile.put(_Tr::to_char_type(c));
        std::wcout.put(_Tr::to_char_type(c));
        
        if (_Tr::to_char_type(c) == '\n')
        {
            m_datePending = true;
        }

        return _Tr::not_eof(c);
    }


    int LowLevelLoggerOldStreamBuf::sync()
    {
        m_outputFile.flush();
        std::wcout.flush();
        return std::basic_streambuf<wchar_t, std::char_traits<wchar_t> >::sync();
    }

    void LowLevelLoggerOldStreamBuf::WriteDateTime()
    {
        const std::string dateTime = 
            boost::posix_time::to_simple_string(boost::posix_time::microsec_clock::universal_time());

        m_outputFile << "[" <<  dateTime.c_str() << "] ";
        std::wcout   << "[" <<  dateTime.c_str() << "] ";
    }


    boost::once_flag LowLevelLoggerOld::SingletonHelper::m_onceFlag = BOOST_ONCE_INIT;

    LowLevelLoggerOld & LowLevelLoggerOld::SingletonHelper::Instance()
    {
        static LowLevelLoggerOld instance;
        return instance;
    }

    LowLevelLoggerOld & LowLevelLoggerOld::Instance()
    {
        boost::call_once(SingletonHelper::m_onceFlag,boost::bind(SingletonHelper::Instance));
        return SingletonHelper::Instance();
    }


    LowLevelLoggerOld::LowLevelLoggerOld():
        std::basic_ostream<wchar_t, std::char_traits<wchar_t> >(new LowLevelLoggerOldStreamBuf())
    {

    }

    //
    // Destructor
    //
    LowLevelLoggerOld::~LowLevelLoggerOld()
    {

    }

}
}
}
}
