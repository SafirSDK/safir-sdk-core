/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / stlrha
*
******************************************************************************/
#include "dose_main_signal_handler.h"

#if defined (_WIN32)
#  define DOSE_WIN32_SIGNALS
#endif

#include <csignal>
#include <boost/bind.hpp>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>

#if defined(DOSE_WIN32_SIGNALS)
#  include <boost/function.hpp>
#else
#  include <unistd.h>
#  include <iostream>
#  include <cerrno>
#  include <Safir/Dob/Typesystem/Exceptions.h>
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{


#if defined(DOSE_WIN32_SIGNALS)
    namespace 
    {
        boost::function<void(int)> g_signalFunction;
    }

    class SignalHandler::Impl
        : private boost::noncopyable
    {
    public:
        explicit Impl(boost::asio::io_service& ioService);
    private:

        void HandleSignal(const int sig);

        boost::asio::io_service& m_ioService;

        static void SignalFunc(const int sig);
    };

    SignalHandler::Impl::Impl(boost::asio::io_service& ioService)
        : m_ioService(ioService)
    {
        g_signalFunction = boost::bind(&SignalHandler::Impl::HandleSignal,this,_1);
        
        ::signal(SIGABRT, &SignalFunc); //TODO: we should make breakpad handle SIGABRT on windows
        ::signal(SIGBREAK, &SignalFunc);
        ::signal(SIGINT, &SignalFunc);
        ::signal(SIGTERM, &SignalFunc);
    }

    
    void SignalHandler::Impl::SignalFunc(const int sig)
    {
        g_signalFunction(sig);
    }

    void SignalHandler::Impl::HandleSignal(const int sig)
    {
        lllout << "Got signal " << sig << ", stopping io_service" << std::endl;
        m_ioService.stop();
    }



#else
    namespace
    {
        const char * STOP_DATA = "stop";
        const ssize_t STOP_DATA_LENGTH = strlen(STOP_DATA) + 1; //include null term
    }


    class SignalHandler::Impl
        : private boost::noncopyable
    {
    public:
        explicit Impl(boost::asio::io_service& ioService);
    private:

        void HandleSignal();

        boost::asio::io_service& m_ioService;
        boost::asio::posix::stream_descriptor m_stopPipeStream;
        static volatile int m_writePipe;
        static volatile sig_atomic_t m_receivedSignal;
        static struct sigaction m_oldSigaction[NSIG];

        static void SigactionFunc(const int sig);
    };



    volatile int SignalHandler::Impl::m_writePipe = 0;
    volatile sig_atomic_t SignalHandler::Impl::m_receivedSignal = 0;
    struct sigaction SignalHandler::Impl::m_oldSigaction[NSIG];

    SignalHandler::Impl::Impl(boost::asio::io_service& ioService)
        : m_ioService(ioService)
        , m_stopPipeStream(ioService)
    {

        int pipes[2];
        const int res = ::pipe(pipes);
        if (res == -1)
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Failed to call ::pipe", __WFILE__, __LINE__);
        }
        m_writePipe = pipes[1];

        m_stopPipeStream.assign(pipes[0]);
        m_stopPipeStream.async_read_some(boost::asio::null_buffers(),
                                         boost::bind(&SignalHandler::Impl::HandleSignal,this));
        
        struct sigaction sa;
        sa.sa_handler = &SigactionFunc;
        sigemptyset(&sa.sa_mask);
        sa.sa_flags = SA_RESTART;
        ::sigaction(SIGQUIT, &sa, &m_oldSigaction[SIGQUIT]);
        ::sigaction(SIGINT, &sa, &m_oldSigaction[SIGINT]);
        ::sigaction(SIGTERM, &sa, &m_oldSigaction[SIGTERM]);
    }

    
    void SignalHandler::Impl::SigactionFunc(const int sig)
    {
        ::sigaction(SIGQUIT, &m_oldSigaction[SIGQUIT], NULL);
        ::sigaction(SIGINT, &m_oldSigaction[SIGINT], NULL);
        ::sigaction(SIGTERM, &m_oldSigaction[SIGTERM], NULL);
        
        if (m_receivedSignal == 0)
        {
            m_receivedSignal = sig;
            const ssize_t res = ::write(m_writePipe,STOP_DATA,STOP_DATA_LENGTH);
            if (res == -1)
            {
                perror("write");
                exit(1);
            }
        }
        else
        {
            SEND_SYSTEM_LOG(Critical,
                            << "Got extra signal " << sig);
        }
    }

    void SignalHandler::Impl::HandleSignal()
    {
        char data[100];
        const int length = ::read(m_stopPipeStream.native(),data,100);
        if (length == -1)
        {
            const std::string error(::strerror(errno));
            std::wostringstream ostr;
            ostr << "Failed to read from pipe in SignalHandler::Impl::HandleSignal: " << error.c_str();
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__); 
        }
        if (length != STOP_DATA_LENGTH || std::string(data) != STOP_DATA)
        {
            std::wostringstream ostr;
            ostr << "Read unexpected data from pipe in SignalHandler::Impl::HandleSignal: '"
                 << data << "' (length = " << length << ")";
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__); 
        }

        lllout << "Got signal " << m_receivedSignal << ", stopping io_service" << std::endl;
        m_ioService.stop();
    }
#endif

    SignalHandler::SignalHandler(boost::asio::io_service& ioService)
        : m_impl(new Impl(ioService))
    {

    }

}
}
}
