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
#include <Safir/Utilities/Internal/MakeUnique.h>

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
#include <boost/make_shared.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio.hpp>

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

    class TimeOutputFilter
    {
    public:
        typedef wchar_t char_type;
        struct category :
            boost::iostreams::output_filter_tag,
            boost::iostreams::multichar_tag,
            boost::iostreams::flushable_tag {};

        TimeOutputFilter(const ControlPtr& control, bool locking = true)
            : m_datePending(true)
            , m_fractionalDigits(boost::posix_time::time_duration::num_fractional_digits())
            , m_ostr(new std::wostringstream())
            , m_ostrLock(locking ? new boost::mutex() : nullptr)
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
                    if (m_datePending && (m_control == nullptr || m_control->UseTimestamps()))
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
                if (m_datePending && (m_control == nullptr || m_control->UseTimestamps()))
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
        TimeOutputFilter(const TimeOutputFilter& other)
            : m_datePending(true)
            , m_fractionalDigits(other.m_fractionalDigits)
            , m_ostr(new std::wostringstream())
            , m_ostrLock(other.m_ostrLock == nullptr ? nullptr : new boost::mutex())
            , m_control(other.m_control)
        {

        }

    private:
        const TimeOutputFilter& operator=(const TimeOutputFilter& other); //no assignment op.

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

            std::unique_ptr<boost::lock_guard<boost::mutex>> lck;
            if (m_ostrLock != nullptr)
            {
                lck.reset(new boost::lock_guard<boost::mutex>(*m_ostrLock));
            }

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

        std::unique_ptr<std::wostringstream> m_ostr;
        std::unique_ptr<boost::mutex> m_ostrLock;

        const ControlPtr m_control;
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
            if (m_control == nullptr || !m_control->IgnoreFlush())
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

    class Queue
    {
    public:
        explicit Queue(const std::uint64_t size)
            : m_size(size)
        {
            m_queueFront.reserve(m_size);
            m_queueBack.reserve(m_size);
        }

        void Enqueue(const wchar_t* s, const std::streamsize n)
        {
            boost::lock_guard<boost::mutex> lck(m_queueLock);
            if (m_full)
            {
                return;
            }
            if (m_queueFront.size() + n > m_size)
            {
                m_full = true;
                return;
            }
            m_queueFront.insert(m_queueFront.end(),s,s+n);
        }

        //assumes only one consumer!
        void Dequeue(const std::function<void(const wchar_t* data, const std::streamsize size, const bool wasFull)>& fn)
        {
            bool wasFull;
            {
                boost::lock_guard<boost::mutex> lck(m_queueLock);
                wasFull = m_full;
                m_full = false;
                std::swap(m_queueFront,m_queueBack);
            }
            if (!m_queueBack.empty())
            {
                fn(&m_queueBack[0],m_queueBack.size(),wasFull);
                m_queueBack.clear();
            }
        }
    private:
        const std::uint64_t m_size;

        //This queue is double buffered
        std::vector<wchar_t> m_queueFront;
        std::vector<wchar_t> m_queueBack;

        boost::mutex m_queueLock;
        bool m_full{false};

    };

    class QueueingSink
    {
    public:
        typedef wchar_t      char_type;
        struct category :
            boost::iostreams::sink_tag,
            boost::iostreams::flushable_tag {};

        QueueingSink(const ControlPtr& control, const boost::shared_ptr<Queue>& queue)
            : m_control(control)
            , m_queue(queue)
        {

        }

        std::streamsize write(const wchar_t* s, const std::streamsize n)
        {
            m_queue->Enqueue(s,n);
            return n;
        }


        bool flush()
        {
            return true;
        }

    private:
        const ControlPtr m_control;
        const boost::shared_ptr<Queue> m_queue;
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
    class LowLevelLogger::SyncFileLogger
    {
    public:
        explicit SyncFileLogger(const boost::shared_ptr<LowLevelLoggerControl>& control)
            : m_control(control)
        {
            m_fileSink.reset(new boost::iostreams::wfile_sink(GetLogFilename(m_control->LogDirectory()).string()));

            m_buffer.SetControl(m_control);
            m_buffer.push(TimeOutputFilter(m_control));
            m_buffer.push(*m_fileSink);
        }

        boost::shared_ptr<LowLevelLoggerControl> Control() const {return m_control;}
        FilteringStreambuf& Buffer() {return m_buffer;}
        boost::shared_ptr<boost::iostreams::wfile_sink> TakeFileSink()
        {
            m_buffer.set_auto_close(false);
            m_buffer.pop();
            return std::move(m_fileSink);
        }
    private:
        const boost::shared_ptr<LowLevelLoggerControl> m_control;
        boost::shared_ptr<boost::iostreams::wfile_sink> m_fileSink;
        FilteringStreambuf m_buffer;
    };


    class LowLevelLogger::AsyncLogger
    {
    public:
        AsyncLogger(const boost::shared_ptr<LowLevelLoggerControl>& control,
                    boost::asio::io_service& ioService,
                    const boost::shared_ptr<boost::iostreams::wfile_sink>& fileSink)
            : m_control(control)
            , m_queue(new Queue(control->BufferSize()))
            , m_strand(ioService)
            , m_timer(ioService)
            , m_fileSink(fileSink)
        {
            ScheduleTimer();
        }

        std::wostream* CreateAsyncStream()
        {
            assert(m_queueBuffer.get() == nullptr);
            m_queueBuffer.reset(new FilteringStreambuf());
            m_queueBuffer->SetControl(m_control);
            m_queueBuffer->push(TimeOutputFilter(m_control, false));
            m_queueBuffer->push(QueueingSink(m_control,m_queue));
            return new std::wostream(m_queueBuffer.get());
        }

        void Stop()
        {
            m_strand.dispatch([this]
                              {
                                  m_timer.cancel();
                                  FlushBuffer();
                              });
        }

    private:
        void FlushBuffer()
        {
            m_queue->Dequeue([this](const wchar_t* data,
                                    const std::streamsize size,
                                    const bool wasFull)
                             {
                                 write(data,size);
                                 if (wasFull)
                                 {
                                     write(L"WARNING: BUFFER OVERFLOW! DATA HAS BEEN DISCARDED!\n");
                                 }
                             });
        }

        void write(const wchar_t* s, std::streamsize n)
        {
            if (m_fileSink != nullptr)
            {
                boost::iostreams::write(*m_fileSink,s,n);
                boost::iostreams::flush(*m_fileSink);
            }

            if (m_control->LogToStdout())
            {
                boost::iostreams::write(std::wcout,s,n);
                boost::iostreams::flush(std::wcout);
            }
        }

        void write(const std::wstring& str)
        {
            write(str.c_str(),str.size());
        }

        void ScheduleTimer()
        {
            m_timer.expires_from_now(m_control->WritePeriod());
            m_timer.async_wait([this](const boost::system::error_code& error)
                               {
                                   if (!error)
                                   {
                                       m_strand.dispatch([this]
                                                         {
                                                             FlushBuffer();
                                                             ScheduleTimer();
                                                         });
                                   }
                               });
        }

        const boost::shared_ptr<LowLevelLoggerControl> m_control;
        boost::shared_ptr<Queue> m_queue;
        boost::asio::io_service::strand m_strand;
        boost::asio::steady_timer m_timer;

        boost::thread_specific_ptr<FilteringStreambuf> m_queueBuffer;

        boost::shared_ptr<boost::iostreams::wfile_sink> m_fileSink;
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
        : m_logLevel(nullptr)
        , m_synchronous(true)
        , m_synchronousStream(nullptr)
    {
        const auto control = boost::make_shared<LowLevelLoggerControl>(false,false);

        if (control->FileLoggingEnabled())
        {
            auto impl = Safir::make_unique<SyncFileLogger>(control);
            m_logLevel = control->GetLogLevelPointer();
            m_synchronousStream.rdbuf(&impl->Buffer());

            m_syncLogger = std::move(impl);
        }
    }

    LowLevelLogger::~LowLevelLogger()
    {

    }

    std::wostream* LowLevelLogger::CreateAsyncStream()
    {
        assert(m_asyncLogger != nullptr && m_syncLogger == nullptr);
        return m_asyncLogger->CreateAsyncStream();
    }

    void LowLevelLogger::SwitchToAsynchronousMode(boost::asio::io_service& ioService)
    {
        //wcout on windows has no buffering turned on at all! Give it some buffering!
#ifdef _MSC_VER
        std::wcout.rdbuf()->pubsetbuf(nullptr,8196);
#endif

        std::ios_base::sync_with_stdio(false);

        m_synchronous = false;

        boost::shared_ptr<LowLevelLoggerControl> control;
        boost::shared_ptr<boost::iostreams::wfile_sink> fileSink;
        if (m_syncLogger != nullptr)
        {
            control = m_syncLogger->Control();
            fileSink = m_syncLogger->TakeFileSink();
        }

        if (control == nullptr)
        {
            control = boost::make_shared<LowLevelLoggerControl>(false,false);
            m_logLevel = control->GetLogLevelPointer();
        }

        m_asyncLogger = Safir::make_unique<AsyncLogger>(control, ioService, fileSink);

        m_syncLogger.reset();

    }

    void LowLevelLogger::Stop()
    {
        assert(m_asyncLogger != nullptr);
        if (m_asyncLogger != nullptr)
        {
            m_asyncLogger->Stop();
        }
    }
}
}
}
}
