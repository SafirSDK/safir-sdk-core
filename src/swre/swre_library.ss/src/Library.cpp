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
#include "Library.h"
#include <algorithm>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Application/BackdoorCommand.h>

#if _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4702)
#endif
#include <boost/lexical_cast.hpp>
#if _MSC_VER
#pragma warning(pop)
#endif

#include "Dispatcher.h"
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/PanicLogging.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <Safir/Utilities/LogInterface.h>
#include <Safir/SwReports/Internal/FatalErrorReport.h>
#include <Safir/SwReports/Internal/ErrorReport.h>
#include <Safir/SwReports/Internal/ResourceReport.h>
#include <Safir/SwReports/Internal/ProgrammingErrorReport.h>
#include <Safir/SwReports/Internal/ProgramInfoReport.h>
#include <boost/bind.hpp>
#include <Safir/Utilities/CrashReporter.h>
#include <boost/regex.hpp>
#include <boost/tokenizer.hpp>
#include <iomanip>
#include <iostream>
#include <signal.h>

//TODO get rid of these!
#ifdef GetMessage
#undef GetMessage
#endif

namespace Safir
{
namespace SwReports
{
namespace Internal
{

    static const std::wstring nullIndStr = L"*** NOT DEFINED ***";

    static const wchar_t* pingCmd = L"ping";
    static const wchar_t* helpCmd = L"help";

    typedef boost::tokenizer<boost::char_separator<wchar_t>,
                             std::wstring::const_iterator,
                             std::wstring> wtokenizer;

    const boost::char_separator<wchar_t> separator(L" ");

    void Library::AtExitFunc()
    {
        // To late to call Stop() here. We must do that before getting here.
        
        if (Instance().m_thread != boost::thread())
        {
            lllerr << "Swre logger thread was still running when this program exited!" << std::endl
                   << "Please make sure that Safir::SwReports::Stop is called before exiting your application!" << std::endl
                   << "If you killed your program in a non-standard way you may still be getting this warning, " << std::endl
                   << "even if you're calling Stop correctly in your normal stop-code." << std::endl;
        }

        {
            boost::lock_guard<boost::mutex> lck(Instance().m_reportQueueLock);
            if (!Instance().m_reportQueue.empty())
            {
                lllerr << "It appears that you've sent one or more error reports after stopping the Swre logger thread:" << std::endl;
                for (std::deque<ReportPtr>::iterator it = Instance().m_reportQueue.begin();
                     it != Instance().m_reportQueue.end(); ++it)
                {
                    lllerr << "----- Error report --------------------------------" << std::endl
                           << Safir::Dob::Typesystem::Serialization::ToXml(*it)
                           << std::endl
                           << "----------------------------------------------------" << std::endl;
                }
                Instance().m_reportQueue.clear();
            }
        }

        {
            boost::lock_guard<boost::mutex> lock(Instance().m_traceBufferLock);
            if (!Instance().m_traceBuffer.empty())
            {
                lllerr << "It appears that you've done some trace logging after stopping the Swre logger thread:" << std::endl;
                lllerr << Instance().m_traceBuffer << std::endl;
                Instance().m_traceBuffer.clear();
            }
        }
    }

    void Library::SignalFunc(const int signal)
    {
        std::wcerr << "swre_library caught a signal! signal = " << signal << ". Will try to clean up and call exit" << std::endl;

        // Stop the thread nicely
        Instance().Stop();
        // Then try to exit and cleanup
        Instance().AtExitFunc();
        // Return that something was wrong
        _exit(200);
    }

    void Library::HandleExit()
    {
        SendReports();

        {
            boost::lock_guard<boost::mutex> lock(m_traceBufferLock);
            if (!m_traceBuffer.empty())
            {
                std::wcout << m_traceBuffer << std::flush;
                try
                {
                    //remove trailing newlines from report
                    const std::wstring::iterator lastChar = m_traceBuffer.end() - 1;
                    if (*lastChar == '\n')
                    {
                        m_traceBuffer.erase(lastChar);
                    }

                    const ReportPtr report = m_reportCreator.CreateProgramInfoReport(m_traceBuffer);
                    m_connection->Send(report, Safir::Dob::Typesystem::ChannelId(), this);
                }
                catch (const Safir::Dob::OverflowException &)
                {
                    std::wcout << "Damn, there was an overflow when sending the trace message." << std::endl
                               << "There is no way that it can be sent to the Dob now, so I'll just exit..."<< std::endl;
                }
                m_traceBuffer.clear();
            }
        }
    }

    boost::once_flag Library::SingletonHelper::m_onceFlag = BOOST_ONCE_INIT;
    boost::scoped_ptr<Library> Library::SingletonHelper::m_instance;

    void Library::SingletonHelper::Instantiate()
    {
        m_instance.reset(new Library());

        //Install cannot be done in constructor, since then the singleton will be destroyed 
        //before the atexit handler is called.
        m_instance->Install();
    }

    Library & Library::Instance()
    {
        boost::call_once(SingletonHelper::m_onceFlag,boost::bind(SingletonHelper::Instantiate));
        return *SingletonHelper::m_instance;
    }


    Library::Library():
        m_isBackdoorStarted(false),
        m_prefixPending(true),
        m_thread(),
        m_sendReportsPending(0),
        m_flushPending(0),
        m_crashed(0)
    {
        std::wstring env;
        {
            char * cenv = getenv("FORCE_LOG");
            if (cenv != NULL)
            {
                env = Safir::Dob::Typesystem::Utilities::ToWstring(cenv);
            }
        }
        try
        {
            for (std::wstring::iterator it = env.begin();
                it != env.end(); ++it)
            {
                if (*it == '\"')
                {
                    *it = ' ';
                }
            }

            wtokenizer tokenizer(env,separator);

            //Tokenize the environment variable.

            std::vector<std::wstring> arguments;
            std::copy(tokenizer.begin(),tokenizer.end(),
                std::back_inserter(arguments));

            m_arguments = arguments;
        }
        catch (const std::exception & exc)
        {
            std::wcout << "Error while parsing FORCE_LOG environment variable '" <<
                env << "'.\nException description is '"<< exc.what() << "'.\nContinuing as if it was not set" << std::endl;
        }
    }

    Library::~Library() 
    {

    }

 
    void Library::Install()
    {
#if defined (_WIN32)
        ::signal(SIGABRT, &SignalFunc);  //TODO: we should make breakpad handle SIGABRT on windows
        ::signal(SIGBREAK, &SignalFunc);
        ::signal(SIGINT, &SignalFunc);
        ::signal(SIGTERM, &SignalFunc);
#else
        struct sigaction sa;
        sa.sa_handler = &SignalFunc;
        sigemptyset(&sa.sa_mask);
        sa.sa_flags = SA_RESTART;
        ::sigaction(SIGQUIT, &sa, NULL);
        ::sigaction(SIGINT, &sa, NULL);
        ::sigaction(SIGTERM, &sa, NULL);
#endif
        //TODO: when crash reporter is not enabled we will not try to stop thread on 
        //crashes!

        //TODO: This whole class should be refactored into several smaller ones
        //with separate duties. This class is getting very messy!
        //Also make the startup and shutdown easier to understand.
        //Can we avoid using atexit?!

        std::atexit(AtExitFunc);
    }


    void
    Library::SetProgramName(const std::wstring & programName)
    {
        m_programName = programName.substr(programName.find_last_of(L"/\\")+1);
    }


    Library::PrefixId
    Library::AddPrefix(const std::wstring & prefix)
    {
        // Always start own thread so its connection is the one that will be used to
        // establish the backdoor subscription.

        StartThread();
        boost::lock_guard<boost::recursive_mutex> lck(m_prefixSearchLock);
        Prefixes::iterator findIt = std::find(m_prefixes.begin(), m_prefixes.end(), prefix);
        if (findIt != m_prefixes.end())
        {
            return ToPrefixId(*findIt);
        }
        else
        {
            bool enabled = false;
            //Check if FORCE_LOG contains all or <prefix>
            if (find(m_arguments.begin(),m_arguments.end(),L"all") != m_arguments.end() ||
                find(m_arguments.begin(),m_arguments.end(),prefix) != m_arguments.end())
            {
                enabled = true;
            }
            m_prefixes.push_back(PrefixState(prefix,enabled));
            return ToPrefixId(m_prefixes.back());
        }
    }

    volatile bool * Library::GetPrefixStatePointer(const PrefixId prefixId)
    {
        return &ToPrefix(prefixId).m_isEnabled;
    }


    bool Library::IsEnabledPrefix(const PrefixId prefixId) const
    {
        return ToPrefix(prefixId).m_isEnabled;
    }

    void Library::EnablePrefix(const PrefixId prefixId, const bool enabled)
    {
        ToPrefix(prefixId).m_isEnabled = enabled;
    }


    void
    Library::Trace(const PrefixId prefixId,
                   const wchar_t ch,
                   const bool dontLock)
    {
        StartThread();
        boost::unique_lock<boost::mutex> lock(m_traceBufferLock, boost::defer_lock);
        if (!dontLock)
        {
            lock.lock();
        }
        //if the buffer has grown too big we just discard the tracing.
        //if this occurs the truncation message will be put last in the
        //report before it is sent.
        if (m_traceBuffer.size() > static_cast<size_t>(Safir::SwReports::Internal::Report::TextMaxStringLength()))
        {
            return;
        }

        if (m_prefixPending)
        {
            m_traceBuffer.append(ToPrefix(prefixId).m_prefix);
            m_traceBuffer.append(L": ");
            m_prefixPending = false;
        }
        if (ch == '\n')
        {
            m_prefixPending = true;
        }
        m_traceBuffer.push_back(ch);
    }

    void
    Library::TraceString(const PrefixId prefixId,
                         const std::wstring & str)
    {
        boost::lock_guard<boost::mutex> lock(m_traceBufferLock);
        std::for_each(str.begin(), str.end(), boost::bind(&Library::Trace,this,prefixId,_1,true));
    }

    void
    Library::TraceSync()
    {
        if (m_flushPending == 0)
        {
            m_flushPending = 1;
            //the functor will keep a reference to this shared_ptr, so we dont need to...
            boost::shared_ptr<boost::asio::deadline_timer> syncTimer
                (new boost::asio::deadline_timer(m_ioService,boost::posix_time::milliseconds(500)));
            syncTimer->async_wait(boost::bind(&Library::HandleTimeout,this,_1,syncTimer));
        }
    }

    void
    Library::HandleTimeout(const boost::system::error_code& error, 
                           const boost::shared_ptr<boost::asio::deadline_timer>&)
    {
        if (error)
        {
            lllerr << "Got an error from boost asio timer!\n" << error << std::endl;
        }
        
        TraceFlush();
    }

    void
    Library::TraceFlush()
    {
        if (boost::this_thread::get_id() == m_threadId)
        {   
            //ok, we're in the correct thread. flush the data!
            m_flushPending = 0;

            std::wstring traceBuffer;
            {
                boost::lock_guard<boost::mutex> lock(m_traceBufferLock);
                std::swap(m_traceBuffer,traceBuffer);
            }

            if (traceBuffer.empty())
            {
                return;
            }

            // If the buffer is too big to fit in one report
            // we truncate it and put in a truncation message.
            if (traceBuffer.size() > static_cast<size_t>(Safir::SwReports::Internal::Report::TextMaxStringLength()))
            {
                const std::wstring TRUNCATION_WARNING =
                    L"\nTHE TRACE BUFFER WAS TRUNCATED BECAUSE IT HAS GROWN TOO BIG! \n"
                    L"IT IS ONLY POSSIBLE TO LOG 60K CHARACTERS PER 0.5 SECONDS\n";

                //remove end of string
                traceBuffer.erase(Safir::SwReports::Internal::Report::TextMaxStringLength() - TRUNCATION_WARNING.size());
                traceBuffer.append(TRUNCATION_WARNING);
            }

            std::wcout << traceBuffer << std::flush;

            //remove trailing newlines from report
            const std::wstring::iterator lastChar = traceBuffer.end() - 1;
            if (*lastChar == '\n')
            {
                traceBuffer.erase(lastChar);
            }
            
            ReportPtr report = m_reportCreator.CreateProgramInfoReport(traceBuffer);

            try
            {
                m_connection->Send(report, Safir::Dob::Typesystem::ChannelId(), this);
            }
            catch (const Safir::Dob::OverflowException &)
            {
                //do nothing
            }
        }
        else
        {
            if (m_thread != boost::thread())
            {
                if (m_flushPending == 0)
                {
                    m_flushPending = 1;
                    m_ioService.post(boost::bind(&Library::TraceFlush,this));
                }
            }
        }
    }


    inline void
    Library::StartThread()
    {
        if (m_thread == boost::thread())
        {
            boost::lock_guard<boost::mutex> lock(m_threadStartingLock);
            if (m_thread == boost::thread())
            {
                m_thread = boost::thread(boost::bind(&Library::Run,this));
                m_threadId = m_thread.get_id();
            }
        }
    }


    void Library::CrashFunc(const char* const dumpPath)
    {
        Instance().m_crashed = 1;
        std::ostringstream ostr;
        ostr << "An application has crashed! A dump was generated to:\n" 
             << dumpPath;
        std::wcerr << ostr.str().c_str() << std::endl;
        Safir::Utilities::Internal::PanicLogging::Log(ostr.str());

        // Stop the thread nicely
        Instance().StopInternal();

        // Then try to exit and cleanup
        Instance().AtExitFunc();
    }

    void
    Library::StartCrashReporting()
    {
        Safir::Utilities::CrashReporter::RegisterCallback(CrashFunc);
        Safir::Utilities::CrashReporter::Start();
    }

    void
    Library::StopInternal()
    {
        //We only let one call to Stop try to do the join. otherwise who knows what will happen...
        if (m_thread != boost::thread())
        {
            boost::lock_guard<boost::mutex> lock(m_threadStartingLock);
            if (m_thread != boost::thread())
            {
                m_ioService.stop();
                m_thread.join();
                m_thread = boost::thread();
                m_threadId = boost::thread::id();
            }
        }
    }

    void
    Library::Stop()
    {
        StopInternal();

        //CrashReporter gets stopped in thread, but if the thread was not running
        //we need to stop it here too.

        Safir::Utilities::CrashReporter::Stop();
    }

    //The swre library thread uses context 0 to connect to the dob. The strange looking negative number
    //is a way to indicate that this is a connection with special privileges.
    const Safir::Dob::Typesystem::Int32 SWRE_LIBRARY_THREAD_CONTEXT = -1000000;

    void
    Library::Run()
    {
        try
        {
            m_connection.reset(new Safir::Dob::Connection());
            Dispatcher dispatcher(*m_connection,m_ioService);

            std::wstring connName = m_programName;
            if (connName.empty())
            {
                Safir::Utilities::ProcessInfo proc(Safir::Utilities::ProcessInfo::GetPid());
                connName = Safir::Dob::Typesystem::Utilities::ToWstring(proc.GetProcessName());
            }
            m_connection->Open(connName,
                               boost::lexical_cast<std::wstring>(Safir::Utilities::ProcessInfo::GetPid()),
                               SWRE_LIBRARY_THREAD_CONTEXT,
                               NULL, 
                               &dispatcher);

            StartBackdoor();
            boost::asio::io_service::work keepRunning(m_ioService);
            m_ioService.run();
            HandleExit();
            m_connection->Close();
            m_connection.reset();
        }
        catch (const std::exception & exc)
        {
            std::wcout << "SwreLibrary caught an unexpected exception!" <<std::endl
                       << "Please report this error to the Safir System Kernel team!" <<std::endl
                       << exc.what() <<std::endl;
        }
        catch (...)
        {
            std::wcout << "SwreLibrary caught an unexpected ... exception!" <<std::endl
                       << "Please report this error to the Safir System Kernel team!" <<std::endl;
        }
        if (m_crashed != 0)
        {
            Safir::Utilities::CrashReporter::Stop();
        }
    }


    void
    Library::StartBackdoor()
    {
        if (m_isBackdoorStarted)
        {
            return;
        }

        // The backdoor subscription should always be done from the own thread connection. If we
        // were to use a connection controlled by the application the backdoor subscription would
        // be lost if that connection is closed. This has always been true but is even more accentuated
        // now when an application may close its connection and then open it in a different context.

        m_backdoorConnection.Attach();

        using namespace Safir::Dob::Typesystem;
        m_backdoorConnection.SubscribeMessage(Safir::Application::BackdoorCommand::ClassTypeId,
                                              Safir::Dob::Typesystem::ChannelId(),
                                              this);
        m_isBackdoorStarted = true;
    }

    void
    Library::OnMessage(const Safir::Dob::MessageProxy messageProxy)
    {
        const Safir::Dob::MessagePtr message = messageProxy.GetMessage();
        try
        {
            const boost::wregex::flag_type regExpFlags = boost::regex::perl | boost::regex::icase;

            const Safir::Application::BackdoorCommandConstPtr cmd =
                boost::static_pointer_cast<Safir::Application::BackdoorCommand>(message);

            if (!cmd->NodeName().IsNull())
            {
                if (!boost::regex_search(Safir::Dob::NodeParameters::Nodes(Safir::Dob::ThisNodeParameters::NodeNumber())->NodeName().GetVal(),
                                         boost::wregex(cmd->NodeName().GetVal(), regExpFlags)))
                {
                    // Node name doesn't match
                    return;  // *** RETURN ***
                }
            }

            if (!cmd->ConnectionName().IsNull())
            {
                Safir::Dob::SecondaryConnection conn;
                conn.Attach();
                Safir::Dob::ConnectionAspectMisc connectionAspectMisc(conn);
                if (!boost::regex_search(connectionAspectMisc.GetConnectionName(), boost::wregex(cmd->ConnectionName().GetVal(), regExpFlags)) &&
                    !boost::regex_search(m_programName, boost::wregex(cmd->ConnectionName().GetVal(), regExpFlags)))
                {
                    // Connection name doesn't match
                    return;  // *** RETURN ***
                }
            }

            if (cmd->Command().IsNull())
            {
                // No command given
                return;  // *** RETURN ***
            }

            // Ok, it seems that this PI-command is for this application

            wtokenizer tokenizer(cmd->Command().GetVal(),separator);


            //copy the tokens into a vector

            //Convert the strings to wstrings
            std::vector<std::wstring> cmdTokens;
            std::copy(tokenizer.begin(),tokenizer.end(),std::back_inserter(cmdTokens));

            if (!cmdTokens.empty())
            {
                if (cmdTokens[0] == pingCmd)
                {
                    // It's a 'ping' command.
                    std::wcout << "Ping reply" << std::endl;
                    Safir::Dob::SecondaryConnection conn;
                    conn.Attach();
                    try
                    {
                        conn.Send(m_reportCreator.CreateProgramInfoReport(L"Tracer Ping reply"), Safir::Dob::Typesystem::ChannelId(), this);
                    }
                    catch (const Safir::Dob::OverflowException & )
                    {
                        //do nothing
                    }

                    return; // *** RETURN ***
                }
                else if (cmdTokens[0] == helpCmd)
                {
                    std::wstring help = GetHelpText();
                    //TODO: should this be sent to the other outputs too?
                    std::wcout << help<<std::endl;
                    Safir::Dob::SecondaryConnection conn;
                    conn.Attach();
                    try
                    {
                        conn.Send(m_reportCreator.CreateProgramInfoReport(help), Safir::Dob::Typesystem::ChannelId(), this);
                    }
                    catch (const Safir::Dob::OverflowException & )
                    {
                        //do nothing
                    }

                    return; // *** RETURN ***
                }
            }

            // Let the subclass handle the command
            HandleCommand(cmdTokens);

        }
        catch (const boost::bad_expression& /* e*/ )
        {
            // An invalid regular expression was used, skip this command
            return;  // *** RETURN ***
        }
    }

    //TODO: where is the backdoor started?!?!?!?!
    //I've just forgotten, need to check so we dont start the thread too early!

    void
    Library::HandleCommand(const std::vector<std::wstring>& cmdTokens)
    {
        if (cmdTokens.size() != 2)
        {
            return;
        }

        boost::lock_guard<boost::recursive_mutex> lck(m_prefixSearchLock);
        for (Prefixes::iterator it = m_prefixes.begin();
             it != m_prefixes.end(); ++it)
        {
            if (cmdTokens[0] == it->m_prefix || cmdTokens[0] == L"all")
            {
                if (cmdTokens[1] == L"on")
                {
                    TraceString(ToPrefixId(*it), L": Turning logging on\n");
                    it->m_isEnabled = true;
                }
                else if (cmdTokens[1] == L"off")
                {
                    TraceString(ToPrefixId(*it), L": Turning logging off\n");
                    it->m_isEnabled = false;
                }
                else
                {
                    TraceString(ToPrefixId(*it), std::wstring(L"Got unrecognized command '") + cmdTokens[1] + L"'\n");
                }
            }
        }
        TraceSync();
    }

    std::wstring
    Library::GetHelpText()
    {
        std::wostringstream out;
        out << "Trace logger supports the following commands: " << std::endl;
        out << "  " << std::setw(10) << "all" << " on/off - Turn logging of all prefices on or off" << std::endl;
        boost::lock_guard<boost::recursive_mutex> lck(m_prefixSearchLock);
        for (Prefixes::iterator it = m_prefixes.begin();
             it != m_prefixes.end(); ++it)
        {
            out << "  " << std::setw (10) << it->m_prefix << " on/off - Turn logging of this prefix on or off. Currently "<<
                (it->m_isEnabled?"on":"off") <<std::endl;
        }

        return out.str();
    }


    Library::PrefixState &
    Library::ToPrefix(const PrefixId prefixId)
    {
        return *reinterpret_cast<PrefixState*>(prefixId);
    }

    Library::PrefixId
    Library::ToPrefixId(PrefixState & prefix)
    {
        return reinterpret_cast<PrefixId>(&prefix);
    }

    void
    Library::Send(ReportPtr report)
    {
        // For now a report is always sent as an swre report AND to platform specific logging via the interface provided by lluf.
        // We might want to make this configurable in the future.

        // Send to swre_logger
        if (!TrySend(report))
        {

            StartThread();

            {
                boost::lock_guard<boost::mutex> lck(m_reportQueueLock);
                m_reportQueue.push_back(report);
            }

            if (m_sendReportsPending == 0)
            {
                m_sendReportsPending = 1;
                m_ioService.post(boost::bind(&Library::SendReports,this));
            }

        }

        // Send to platform specific logging

        // Map the swre report type to an lluf severity
        Safir::Utilities::LogInterface::Severity severity;

        switch (report->GetTypeId())
        {
            case Safir::SwReports::Internal::FatalErrorReport::ClassTypeId:
            {
                severity = Safir::Utilities::LogInterface::Critical;
            }
            break;

            case Safir::SwReports::Internal::ErrorReport::ClassTypeId:
            {

            }
            break;

            case Safir::SwReports::Internal::ResourceReport::ClassTypeId:
            {

            }
            break;

            case Safir::SwReports::Internal::ProgrammingErrorReport::ClassTypeId:
            {

            }
            break;

            case Safir::SwReports::Internal::ProgramInfoReport::ClassTypeId:
            {

            }
            break;

        }


    }

    bool
    Library::TrySend(ReportPtr report)
    {
        Safir::Dob::SecondaryConnection conn;
        try
        {
            conn.Attach();

            Safir::Dob::ConnectionAspectMisc connAspectMisc(conn);            
            if (connAspectMisc.GetContext() != 0)
            {
                // Reports must be sent from context 0
                return false;
            }
        }
        catch (const Safir::Dob::NotOpenException &)
        {
            return false;
        }

        try
        {
            //should be ok to use the m_tryMessageSender from multiple threads at the 
            //same time, since nothing is modified, and the calback is empty.
            conn.Send(report, Safir::Dob::Typesystem::ChannelId(), &m_tryMessageSender);
        }
        catch (const Safir::Dob::OverflowException &)
        {
            return false;
        }
        return true;
    }

    void
    Library::SendFatalErrorReport(const std::wstring & errorCode,
                                  const std::wstring & location,
                                  const std::wstring & text)
    {

        ReportPtr report = m_reportCreator.CreateFatalErrorReport(errorCode, location, text);
        Send(report);
    }

    void
    Library::SendErrorReport(const std::wstring & errorCode,
                             const std::wstring & location,
                             const std::wstring & text)
    {
        ReportPtr report = m_reportCreator.CreateErrorReport(errorCode,location,text);
        Send(report);
    }

    void
    Library::SendResourceReport(const std::wstring & resourceId,
                                const bool allocated,
                                const std::wstring & text)
    {
        ReportPtr report = m_reportCreator.CreateResourceReport(resourceId,allocated,text);
        Send(report);
    }

    void
    Library::SendProgrammingErrorReport(const std::wstring & errorCode,
                                        const std::wstring & location,
                                        const std::wstring & text)
    {
        ReportPtr report = m_reportCreator.CreateProgrammingErrorReport(errorCode,location,text);
        Send(report);
    }

    void
    Library::SendProgramInfoReport(const std::wstring & text)
    {
        ReportPtr report = m_reportCreator.CreateProgramInfoReport(text);
        Send(report);
    }

    void Library::SendReports()
    {
        m_sendReportsPending = 0;
        try
        {
            boost::lock_guard<boost::mutex> lck(m_reportQueueLock);
            while (!m_reportQueue.empty())
            {
                m_reportCreator.SetConnectionInfoIfNotSet(m_reportQueue.front());
                m_connection->Send(m_reportQueue.front(), Safir::Dob::Typesystem::ChannelId(), this);
                m_reportQueue.pop_front();
            }
        }
        catch (const Safir::Dob::OverflowException &)
        {
            //do nothing
        }
    }

    void Library::OnNotMessageOverflow()
    {
        SendReports();
        TraceFlush();
    }
}
}
}
