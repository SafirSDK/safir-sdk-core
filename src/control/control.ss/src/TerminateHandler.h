/******************************************************************************
*
* Copyright Saab AB, 2016 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#pragma once

#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/AsioStrandWrap.h>
#include <thread>

#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100 4267 4251)
#endif

#include <boost/asio.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

/**
 * This class handles signals to make safir_control stop.
 * On Windows this means handling some signals and some console events, and
 * on Linux it means handling some signals.
 */
class TerminateHandler
{
public:
    TerminateHandler(boost::asio::io_context& io,
                     std::function<void()> stopCallback,
                     const std::function<void(const std::string& str)>& logStatus)
        : m_strand(io)
        , m_signalSet(io)
    {
#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        m_signalSet.add(SIGABRT);
        m_signalSet.add(SIGBREAK);
        m_signalSet.add(SIGINT);
        m_signalSet.add(SIGTERM);

        //We install a ConsoleCtrlHandler to handle presses of the Close button
        //on the console window
        ConsoleCtrlHandlerFcn = [logStatus,stopCallback](DWORD event)
            {
                std::ostringstream ostr;
                ostr << "CTRL: Got Windows Console Event " << event << " ... stop sequence initiated.";
                logStatus(ostr.str());

                stopCallback;
            };
        ::SetConsoleCtrlHandler(ConsoleCtrlHandler,TRUE);
#elif defined(linux) || defined(__linux) || defined(__linux__)
        m_signalSet.add(SIGQUIT);
        m_signalSet.add(SIGINT);
        m_signalSet.add(SIGTERM);
#endif

        m_signalSet.async_wait(Safir::Utilities::Internal::WrapInStrand(m_strand, [stopCallback, logStatus]
                                             (const boost::system::error_code& error,
                                              const int signalNumber)
        {
            if (error)
            {
                if (error == boost::asio::error::operation_aborted)
                {
                    return;
                }
                else
                {
                    std::ostringstream os;
                    os << "Got a signals error (m_signalSet): " << error;
                    throw std::logic_error(os.str());
                }
            }
            std::ostringstream os;
            os << "CTRL: Got signal " << signalNumber << " ... stop sequence initiated.";
            logStatus(os.str());

            stopCallback();
        }));
    }

    void Stop()
    {
        boost::asio::dispatch(m_strand, [this]{m_signalSet.cancel();});
    }

private:

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    /** Function pointer to our real event handler code.*/
    static std::function<void(DWORD event)> ConsoleCtrlHandlerFcn;

    /**
     * This is the handler that Windows will call on events occurring.
     * It in turn will call the ConsoleCtrlHandlerFcn set up by us.
     * This is how things must be done, since there is no facility for passing
     * any data along to the handler in win32.
     */
    static BOOL WINAPI ConsoleCtrlHandler(DWORD event)
    {
        switch (event)
        {
        case CTRL_CLOSE_EVENT:
        case CTRL_LOGOFF_EVENT:
        case CTRL_SHUTDOWN_EVENT:
            {
                ConsoleCtrlHandlerFcn(event);

                //We could sleep forever here, since the function will be terminated when
                //our main function returns. Anyway, we only have something like
                //five seconds before the process gets killed anyway.
                //So the below code is just to ensure we sleep for a while and
                //dont generate any compiler errors...
                for(int i = 0; i < 10; ++i)
                {
                    std::this_thread::sleep_for(std::chrono::seconds(1));
                }
            }
            return TRUE;
        default:
            return FALSE;
        }
    }
#endif


    boost::asio::io_context::strand m_strand;
    boost::asio::signal_set m_signalSet;
};
