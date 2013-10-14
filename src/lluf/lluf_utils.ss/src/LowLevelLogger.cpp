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
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/filtering_streambuf.hpp>
#include <boost/iostreams/stream_buffer.hpp>
#include <boost/iostreams/tee.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/locks.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#include <iomanip>
#include <iostream>

namespace //anonymous namespace for internal functions
{
    typedef boost::shared_ptr<const Safir::Utilities::Internal::LowLevelLoggerControl> ControlPtr;

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

    class DateOutputFilter
    {
    public:
        typedef wchar_t char_type;
        struct category :
            boost::iostreams::output_filter_tag,
            boost::iostreams::multichar_tag,            
            boost::iostreams::flushable_tag {};

        explicit DateOutputFilter(const ControlPtr& control)
            : m_datePending(true)
            , m_fractionalDigits(boost::posix_time::time_duration::num_fractional_digits())
            , m_ostr(new std::wostringstream())
            , m_ostrLock(new boost::mutex())
            , m_control(control)
        {
        
        }

        template <typename Sink>
        std::streamsize write( Sink& dest, const wchar_t* s, const std::streamsize n)
        {
            const wchar_t* start = s;
            std::streamsize i = 0;
            const wchar_t* const end = s+n;

            while (start + i != end)
            {
                if (start[i] == '\n')
                {
                    if (m_datePending && (m_control == NULL || m_control->UseTimestamps()))
                    {
                        const std::wstring timestring = TimeString();
                        boost::iostreams::write(dest,timestring.c_str(),timestring.size());
                        m_datePending = false;
                    }
                    boost::iostreams::write(dest,start,i+1);
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
                if (m_datePending && (m_control == NULL || m_control->UseTimestamps()))
                {
                    const std::wstring timestring = TimeString();
                    boost::iostreams::write(dest,timestring.c_str(),timestring.size());
                    m_datePending = false;
                }
                boost::iostreams::write(dest,start,end-start);
            }
                    

            return n;
        }
        
        template <typename Sink>
        bool flush(Sink& s) const 
        {
            return boost::iostreams::flush(s);
        }

        /** This copy constructor does actually not copy anything at all,
         * All it does is create new internal structures to avoid
         * allocation in TimeString.
         */
        DateOutputFilter(const DateOutputFilter& other)
            : m_datePending(true)
            , m_fractionalDigits(other.m_fractionalDigits)
            , m_ostr(new std::wostringstream())
            , m_ostrLock(new boost::mutex())
            , m_control(other.m_control)
        {

        }

    private:
        const DateOutputFilter& operator=(const DateOutputFilter& other); //no assignment op.
        
        /** 
         * This is an optimized version of to_simple_string(time_duration) from posix_time.
         * It is here just to reduce the amount of time spent in serializing the time strings
         * in the logger.
         * The ch argument is appended to the string, so that the caller only has to do 
         * a call to write, rather than a write followed by a put.
         * The wostringstream is a member with a lock rather than a local variable since
         * that is actually faster (tested on linux with gcc). 
         */
        const std::wstring TimeString()
        {
            using namespace boost::posix_time;
            using namespace boost;
            const time_duration td = microsec_clock::universal_time().time_of_day();

            boost::lock_guard<boost::mutex> lck(*m_ostrLock);
            m_ostr->str(L"");

            *m_ostr << L"[" << std::setfill(fill_char) << std::setw(2)
                    << date_time::absolute_value(td.hours()) << L":";
            *m_ostr << std::setw(2)
                    << date_time::absolute_value(td.minutes()) << L":";
            *m_ostr << std::setw(2)
                    << date_time::absolute_value(td.seconds());

            const time_duration::fractional_seconds_type frac_sec =
                date_time::absolute_value(td.fractional_seconds());

            *m_ostr << L"." << std::setw(m_fractionalDigits)
                  << frac_sec;
            *m_ostr << L"] ";
            return m_ostr->str();
        }


        bool m_datePending;

        static const wchar_t fill_char = '0';
        const unsigned short m_fractionalDigits;

        boost::shared_ptr<std::wostringstream> m_ostr;
        boost::shared_ptr<boost::mutex> m_ostrLock;
        
        const ControlPtr m_control;
    };

    /* Just adding wcout to filtering_streambuf seems to stop
     * flushing from working correctly, so we do this instead.*/
    class FlushingWcoutSink
    {
    public:
        typedef wchar_t      char_type;
        struct category :
            boost::iostreams::sink_tag,
            boost::iostreams::flushable_tag {};

        std::streamsize write(const wchar_t* s, const std::streamsize n) const
        {
            std::wcout.write(s,n);
            return n;
        }

        bool flush() const 
        {
            return boost::iostreams::flush(std::wcout);
        }
    };

    /**/
    class FilteringStreambuf:
        public boost::iostreams::filtering_wostreambuf
    {
    public:
        explicit FilteringStreambuf()
            : m_control()
        {

        }

        void SetControl(const ControlPtr& control)
        {
            m_control = control;
        }

        int sync()
        {
            //            std::wcerr << "Got sync call" << std::endl;
            if (m_control == NULL || !m_control->IgnoreFlush())
            {
                return boost::iostreams::filtering_wostreambuf::sync();
            }
            else
            {
                return 0;
            }
        }
    private:
        ControlPtr m_control;
    };

    class TeeDevice
        : public boost::iostreams::tee_device<boost::iostreams::wfile_sink, std::wostream>
    {
        typedef boost::iostreams::tee_device<boost::iostreams::wfile_sink, std::wostream> Base;
    public:
        explicit TeeDevice(boost::iostreams::wfile_sink& fileSink)
            : Base(fileSink,std::wcout)
            , m_fileSink(fileSink)
        {
        }

        std::streamsize write(const wchar_t* s, std::streamsize n)
        {
            if (m_control == NULL ||
                (m_control->LogToStdout() && m_control->LogToFile()))
            {
                return Base::write(s,n);
            }
            else if (m_control->LogToStdout())
            {
                return boost::iostreams::write(std::wcout,s,n);
            }
            else if (m_control->LogToFile())
            {
                return boost::iostreams::write(m_fileSink,s,n);
            }
            else
            {
                return n;
            }
        }

        void SetControl(const ControlPtr& control)
        {
            m_control = control;
        }

    private:
        const TeeDevice& operator=(const TeeDevice&); //no assignment

        boost::iostreams::wfile_sink& m_fileSink;
        ControlPtr m_control;
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

    };


    class LowLevelLogger::LoggingImpl
        : public LowLevelLogger::Impl
    {
    public:
        explicit LoggingImpl(LowLevelLogger& lll)
        {
            m_control.reset(new LowLevelLoggerControl(false,false));
            if (m_control->Disabled())
            {
                m_control.reset();
                throw std::runtime_error("LowLevelLogger is disabled!");
            }
            
            m_fileSink.reset(new boost::iostreams::wfile_sink(GetLogFilename(m_control->LogDirectory()).string()));
            m_tee.reset(new TeeDevice(*m_fileSink));
            
            m_buffer.SetControl(m_control);
            m_tee->SetControl(m_control);
            DateOutputFilter filter(m_control);
            m_buffer.push(filter);
            m_buffer.push(*m_tee);
            lll.rdbuf(&m_buffer);
            
            lll.m_pLogLevel = m_control->GetLogLevelPointer();
        }

    private:
        boost::shared_ptr<LowLevelLoggerControl> m_control;
        boost::shared_ptr<boost::iostreams::wfile_sink> m_fileSink;
        boost::shared_ptr<TeeDevice> m_tee;
        FilteringStreambuf m_buffer;
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
        : std::wostream(NULL)
        , m_pLogLevel(NULL)
    {
        //wcout on windows has no buffering turned on at all! Give it some buffering!
#ifdef _MSC_VER
        std::wcout.rdbuf()->pubsetbuf(NULL,8196);
#endif
        
        //this will allow wcout to coexist with cout and with printf/wprintf
        //which Ada io is based upon.
        std::ios_base::sync_with_stdio(false);

        try
        {
            m_impl.reset(new LoggingImpl(*this));
        }
        catch (const std::exception&)
        {
            m_impl.reset();
            //set it to null, just to be extra sure. Should not be needed...
            m_pLogLevel = NULL;
        }
    }

    LowLevelLogger::~LowLevelLogger()
    {

    }
}
}
}
}


