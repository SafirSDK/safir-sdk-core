/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/LowLevelLoggerControl.h>
#include <Safir/Utilities/ProcessInfo.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable:4127)
#pragma warning (disable:4702)
#endif

#include <boost/bind.hpp>
#include <boost/filesystem/exception.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#include <iomanip>
#include <fstream>

namespace //anonymous namespace for internal functions
{
    using namespace Safir::Utilities::Internal;

    //also creates directories and removes files that are in the way
    const boost::filesystem::path GetLogFilename(const boost::filesystem::path& path)
    {
        try
        {
            boost::filesystem::create_directories(path);
        }
        catch (const boost::filesystem::filesystem_error)
        {
            throw std::logic_error("Failed to create log directory");
        }

        const pid_t pid = Safir::Utilities::ProcessInfo::GetPid();
        const Safir::Utilities::ProcessInfo proc(pid);
        const std::string filename = proc.GetProcessName() + "-" + boost::lexical_cast<std::string>(pid) + ".txt";

        try
        {
            const boost::filesystem::path fullpath = path/filename;
            if (boost::filesystem::is_regular_file(fullpath) && boost::filesystem::exists(fullpath))
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

    class TimestampingStreambuf
        : public std::basic_streambuf<wchar_t>
        , private boost::noncopyable
    {
    public:
        TimestampingStreambuf(const LowLevelLoggerControl& control)
            : m_control(control)
        {

        }

        std::wfilebuf* open(const std::string& filename, std::ios_base::openmode mode)
        {
            return m_sink.open(filename,mode);
        }
    protected:
        int_type overflow( int_type m = traits_type::eof() ) override
        {
            WriteTime();

            m_datePending = traits_type::to_char_type( m ) == L'\n';
            return m_sink.sputc(m);
        }

        std::streamsize xsputn(const char_type * s, std::streamsize n) override
        {
            const wchar_t* start = s;
            std::streamsize i = 0;
            const wchar_t* const end = s+n;

            while (start + i != end)
            {
                if (start[i] == L'\n')
                {
                    WriteTime();

                    m_sink.sputn(start,i+1);
                    start += i + 1;
                    i = 0;
                    m_datePending = true;
                }
                else
                {
                    ++i;
                }
            }

            if (end - start > 0)
            {
                WriteTime();
                m_sink.sputn(start,end-start);
            }


            return n;
        }

        int sync() override
        {
            if (m_control.IgnoreFlush())
            {
                return 0;
            }
            else
            {
                return m_sink.pubsync();
            }
        }

        /**
         * This is an optimized version of to_simple_string(time_duration) from posix_time.
         * It is here just to reduce the amount of time spent in serializing the time strings
         * in the logger.
         * The wostringstream is a member rather than a local variable since that is
         * actually faster (tested on linux with gcc).
         *
         * Note: This function is not thread safe!
         */
        inline void WriteTime()
        {
            if(m_datePending && m_control.UseTimestamps())
            {
                m_datePending = false;

                using namespace boost::posix_time;
                using namespace boost;
                const time_duration td = microsec_clock::universal_time().time_of_day();

                m_ostr << L"[" << std::setfill(L'0') << std::setw(2)
                       << date_time::absolute_value(td.hours()) << L":";
                m_ostr << std::setw(2)
                       << date_time::absolute_value(td.minutes()) << L":";
                m_ostr << std::setw(2)
                       << date_time::absolute_value(td.seconds());

                const time_duration::fractional_seconds_type frac_sec =
                    date_time::absolute_value(td.fractional_seconds());

                m_ostr << L"." << std::setw(m_fractionalDigits)
                       << frac_sec;
                m_ostr << L"] ";
            }
        }

    private:
        const LowLevelLoggerControl& m_control;
        std::basic_filebuf<wchar_t> m_sink;
        std::wostream m_ostr {&m_sink};
        bool m_datePending {true};
        const int m_fractionalDigits{boost::posix_time::time_duration::num_fractional_digits()};
    };
}


namespace Safir
{
namespace Utilities
{
namespace Internal
{
namespace Internal
{
    class LowLevelLogger::Impl
    {
    public:
        explicit Impl()
            : m_buffer(m_control)
        {
            if (m_control.LogLevel() > 0)
            {
                m_buffer.open(GetLogFilename(m_control.LogDirectory()).string(),std::ios_base::out);
            }
        }

        const LowLevelLoggerControl m_control;
        TimestampingStreambuf m_buffer;
    };

    boost::once_flag LowLevelLogger::SingletonHelper::m_onceFlag = BOOST_ONCE_INIT;

    LowLevelLogger & LowLevelLogger::SingletonHelper::Instance()
    {
        static LowLevelLogger instance;
        return instance;
    }

    LowLevelLogger & LowLevelLogger::Instance()
    {
        boost::call_once(SingletonHelper::m_onceFlag,boost::bind(SingletonHelper::Instance));
        return SingletonHelper::Instance();
    }


    LowLevelLogger::LowLevelLogger()
        : std::wostream(nullptr)
        , m_impl(new Impl())
        , m_pLogLevel(nullptr)
    {
        rdbuf(&m_impl->m_buffer);
        m_pLogLevel = m_impl->m_control.GetLogLevelPointer();
    }

    LowLevelLogger::~LowLevelLogger()
    {

    }
}
}
}
}
