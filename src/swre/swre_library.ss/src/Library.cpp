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

#include <ace/Thread.h>
#include <ace/Process.h>
#include <ace/SOCK_Dgram_Bcast.h>
#include <ace/OS_NS_sys_socket.h>
#include <ace/Signal.h>
#include "Dispatcher.h"
#include <iostream>
#include <boost/bind.hpp>
#include <boost/tokenizer.hpp>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <iomanip>
#include <boost/regex.hpp>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/PanicLogging.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <ace/Guard_T.h>
//AWI: Temp removal #include <Safir/Utilities/CrashReporter.h>

#ifdef GetMessage
#undef GetMessage
#endif

namespace Safir
{
namespace SwReports
{
namespace Internal
{
    static const wchar_t* pingCmd = L"ping";
    static const wchar_t* helpCmd = L"help";

    typedef boost::tokenizer<boost::char_separator<wchar_t>,
                             std::wstring::const_iterator,
                             std::wstring> wtokenizer;

    const boost::char_separator<wchar_t> separator(L" ");

    const ACE_Time_Value half_a_second = ACE_Time_Value(0,500000);
    const ACE_Time_Value ten_milliseconds = ACE_Time_Value(0,10000);
    const ACE_Time_Value ten_seconds = ACE_Time_Value(10,0);

    Library * volatile Library::m_instance = NULL;
    ACE_Mutex Library::m_instantiationLock;

    void Library::AtExitFunc()
    {
        // To late to call Stop() here. We must do that before getting here.
        
        if (Instance().m_threadStatus == Started)
        {
            lllerr << "Swre logger thread was still running when this program exited!" << std::endl
                   << "Please make sure that Safir::SwReports::Stop is called before exiting your application!" << std::endl
                   << "If you killed your program in a non-standard way you may still be getting this warning, " << std::endl
                   << "even if you're calling Stop correctly in your normal stop-code." << std::endl;
        }
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

        if (!Instance().m_traceBuffer.empty())
        {
            lllerr << "It appears that you've done some trace logging after stopping the Swre logger thread:" << std::endl;
            lllerr << Instance().m_traceBuffer << std::endl;
            Instance().m_traceBuffer.clear();
        }
    }

    void Library::SignalFunc(const int /*signal*/)
    {
        // Stop the thread nicely
        Instance().Stop();
        // Then try to exit and cleanup
        Instance().AtExitFunc();
        // Return that something was wrong
        _exit(-1);
    }

    void Library::HandleExit()
    {
        if (!m_reportQueue.empty())
        {
            std::for_each(m_reportQueue.begin(),
                          m_reportQueue.end(),
                          boost::bind(&ReportCreator::SetConnectionInfoIfNotSet,
                                      boost::ref(m_reportCreator),
                                      _1,
                                      boost::ref(*m_connection)));
            SendReports();
        }
        if (!m_traceBuffer.empty())
        {
            std::wcout << m_traceBuffer << std::endl;
            try
            {
                ReportPtr report = m_reportCreator.CreateProgramInfoReport(m_traceBuffer);
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

    Library & Library::Instance()
    {
        if (m_instance == NULL)
        {
            ACE_Guard<ACE_Mutex> lock(m_instantiationLock);

            if (m_instance == NULL)
            {
                m_instance = new Library();

                // Register signals to handle
                ACE_Sig_Set sigset;
                sigset.sig_add(SIGINT);
                sigset.sig_add(SIGQUIT);
                sigset.sig_add(SIGTERM);
                // Register callback for those signals
                ACE_Sig_Action sig(sigset, &SignalFunc);

                std::atexit(AtExitFunc);
            }
        }
        return *m_instance;
    }

    Library::Library():
        ACE_Event_Handler(&m_reactor),
        m_reactor(),
        m_isBackdoorStarted(false),
        m_prefixPending(true),
        m_threadStatus(NotStarted),
        m_threadStartingEvent(0),
        m_writeNotified(0),
        m_readNotified(0),
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
        ACE_Guard<ACE_Recursive_Thread_Mutex> lck(m_prefixSearchLock);
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
        ACE_Guard<ACE_Thread_Mutex> lock(m_traceBufferLock,0,0);
        if (!dontLock)
        {
            lock.acquire();
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
        ACE_Guard<ACE_Thread_Mutex> lock(m_traceBufferLock);
        std::for_each(str.begin(), str.end(), boost::bind(&Library::Trace,this,prefixId,_1,true));
    }

    void
    Library::TraceSync()
    {
        //Note that the bufferLock must not be locked here, since there is a lock inside the timer queue
        //and that lock is locked while inside the handle_timeout function, which locks the buffer lock.
        // ==> If the bufferLock is locked here there may be a deadlock!
        reactor()->schedule_timer(this,NULL, half_a_second);
    }

    void
    Library::TraceFlush()
    {
        if (ACE_Thread::self() == m_threadId)
        {    //ok, we're in the correct thread. flush the data!
            ACE_Guard<ACE_Thread_Mutex> lock(m_traceBufferLock);
            reactor()->cancel_timer(this); //remove any extra timers
            if (m_traceBuffer.empty())
            {
                return;
            }

            // If the buffer is too big to fit in one report
            // we truncate it and put in a truncation message.
            if (m_traceBuffer.size() > static_cast<size_t>(Safir::SwReports::Internal::Report::TextMaxStringLength()))
            {
                const std::wstring TRUNCATION_WARNING =
                    L"\nTHE TRACE BUFFER WAS TRUNCATED BECAUSE IT HAS GROWN TOO BIG! \n"
                    L"IT IS ONLY POSSIBLE TO LOG 60K CHARACTERS PER 0.5 SECONDS\n";

                //remove end of string
                m_traceBuffer.erase(Safir::SwReports::Internal::Report::TextMaxStringLength() - TRUNCATION_WARNING.size());
                m_traceBuffer.append(TRUNCATION_WARNING);
            }

            ReportPtr report = m_reportCreator.CreateProgramInfoReport(m_traceBuffer);

            try
            {
                m_connection->Send(report, Safir::Dob::Typesystem::ChannelId(), this);
                std::wcout << m_traceBuffer << std::endl;
                m_traceBuffer.clear();
            }
            catch (const Safir::Dob::OverflowException &)
            {
                //do nothing
            }
        }
        else
        {
            if (m_threadStatus == Started)
            {
                if (m_readNotified == 0)
                {
                    m_readNotified = 1;
                    reactor()->notify(this,ACE_Event_Handler::READ_MASK);
                }
            }
        }
    }


    inline void
    Library::StartThread()
    {
        if (m_threadStatus == NotStarted)
        {
            ACE_Guard<ACE_Mutex> lock(m_threadStartingLock);
            if (m_threadStatus == NotStarted)
            {
                ACE_Thread::spawn(ThreadFunc,this,THR_NEW_LWP | THR_JOINABLE,&m_threadId,&m_threadHandle);
                m_threadStartingEvent.wait();
                assert(m_threadStatus == Started);
            }
        }
    }

    ACE_THR_FUNC_RETURN
    Library::ThreadFunc(void * param)
    {
        Library * _this = static_cast<Library*>(param);
        _this->Run();
        return 0;
    }

    void Library::CrashFunc(const char* const dumpPath)
    {
        Instance().m_crashed = 1;
        std::ostringstream ostr;
        ostr << "An application has crashed! A dump was generated to:\n" 
             << dumpPath;
        Safir::Utilities::Internal::PanicLogging::Log(ostr.str());

        // Stop the thread nicely
        Instance().StopInternal();

        // Then try to exit and cleanup
        Instance().AtExitFunc();
    }

    void
    Library::StartCrashReporting()
    {
        //AWI:Temp removal of breakpad dependency
        //Safir::Utilities::CrashReporter::RegisterCallback(CrashFunc);
        //Safir::Utilities::CrashReporter::Start();
    }

    void
    Library::StopInternal()
    {
        //We only let one call to Stop try to do the join. otherwise who knows what will happen...
        if (m_threadStatus == Started)
        {
            ACE_Guard<ACE_Mutex> lock(m_threadStartingLock);
            if (m_threadStatus == Started)
            {
                reactor()->end_reactor_event_loop();
                const int joinRes = ACE_Thread::join(m_threadHandle);
                if (joinRes == -1)
                {
                    lllerr << "ACE_Thread::join gave result -1! errno = " << errno << std::endl;
                }
                if (m_threadStatus != Stopped)
                {
                    lllerr << "Swre Library::Stop: threadStatus was unexpectedly " << m_threadStatus << std::endl;
                }
            }
        }
    }

    void
    Library::Stop()
    {
        StopInternal();

        //CrashReporter gets stopped in thread, but if the thread was not running
        //we need to stop it here too.

        //AWI: Temp removal of breakpad dependency
        //Safir::Utilities::CrashReporter::Stop();
    }

    //The swre library thread uses context 0 to connect to the dob. The strange looking negative number
    //is a way to indicate that this is a connection with special privileges.
    const Safir::Dob::Typesystem::Int32 SWRE_LIBRARY_THREAD_CONTEXT = -1000000;

    void
    Library::Run()
    {
        m_threadStatus = Started;
        try
        {
            reactor()->owner(ACE_Thread::self()); //take ownership of reactor!
            m_connection.reset(new Safir::Dob::Connection());
            Dispatcher dispatcher(m_reactor,*m_connection);

            std::wstring connName = m_programName;
            if (connName.empty())
            {
                Safir::Utilities::ProcessInfo proc(ACE_OS::getpid());
                connName = Safir::Dob::Typesystem::Utilities::ToWstring(proc.GetProcessName());
            }
            m_connection->Open(connName,boost::lexical_cast<std::wstring>(ACE_OS::getpid()),SWRE_LIBRARY_THREAD_CONTEXT,NULL, &dispatcher);

            m_threadStartingEvent.signal();
            StartBackdoor();
            reactor()->run_reactor_event_loop();
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

        m_threadStatus = Stopped;
        if (!m_crashed)
        {
            //AWI: Temp removal of breakpad dependency
            //Safir::Utilities::CrashReporter::Stop();
        }
    }


    //Flush
    int Library::handle_input (ACE_HANDLE)
    {
        m_readNotified = 0;
        TraceFlush();
        return 0;
    }

    int
    Library::handle_timeout(const ACE_Time_Value & /*current_time*/, const void * /*act*/)
    {
        TraceFlush();
        return 0;
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

        ACE_Guard<ACE_Recursive_Thread_Mutex> lck(m_prefixSearchLock);
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
        ACE_Guard<ACE_Recursive_Thread_Mutex> lck(m_prefixSearchLock);
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

    class MessageSender:
        public Safir::Dob::MessageSender
    {
        virtual void OnNotMessageOverflow(){};
    };

    //TODO: remove the TrySend handling once it is possible to connect more than 55 apps to dose.
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

        static Safir::SwReports::Internal::MessageSender trySendSender;

        try
        {
            conn.Send(report, Safir::Dob::Typesystem::ChannelId(), &trySendSender);
        }
        catch (const Safir::Dob::OverflowException &)
        {
            std::wostringstream ostr;
            ostr << "Error report overflowed" << std::endl
                 << Safir::Dob::Typesystem::Serialization::ToXml(report);
            Safir::Utilities::Internal::PanicLogging::Log(Safir::Dob::Typesystem::Utilities::ToUtf8(ostr.str()));
        }
        return true;
    }

    void
    Library::SendFatalErrorReport(const std::wstring & errorCode,
                                  const std::wstring & location,
                                  const std::wstring & text)
    {
        ReportPtr report = m_reportCreator.CreateFatalErrorReport(errorCode,location,text);
        if (!TrySend(report))
        {
            StartThread();

            {
                ACE_Guard<ACE_Mutex> lck(m_reportQueueLock);
                m_reportQueue.push_back(report);
            }

            if (m_writeNotified == 0)
            {
                m_writeNotified = 1;
                reactor()->notify(this,ACE_Event_Handler::WRITE_MASK);
            }
        }
    }

    void
    Library::SendErrorReport(const std::wstring & errorCode,
                             const std::wstring & location,
                             const std::wstring & text)
    {
        ReportPtr report = m_reportCreator.CreateErrorReport(errorCode,location,text);
        if (!TrySend(report))
        {
            StartThread();

            {
                ACE_Guard<ACE_Mutex> lck(m_reportQueueLock);
                m_reportQueue.push_back(report);
            }

            if (m_writeNotified == 0)
            {
                m_writeNotified = 1;
                reactor()->notify(this,ACE_Event_Handler::WRITE_MASK);
            }
        }
    }

    void
    Library::SendResourceReport(const std::wstring & resourceId,
                                const bool allocated,
                                const std::wstring & text)
    {
        ReportPtr report = m_reportCreator.CreateResourceReport(resourceId,allocated,text);
        if (!TrySend(report))
        {
            StartThread();

            {
                ACE_Guard<ACE_Mutex> lck(m_reportQueueLock);
                m_reportQueue.push_back(report);
            }

            if (m_writeNotified == 0)
            {
                m_writeNotified = 1;
                reactor()->notify(this,ACE_Event_Handler::WRITE_MASK);
            }
        }
    }

    void
    Library::SendProgrammingErrorReport(const std::wstring & errorCode,
                                        const std::wstring & location,
                                        const std::wstring & text)
    {
        ReportPtr report = m_reportCreator.CreateProgrammingErrorReport(errorCode,location,text);
        if (!TrySend(report))
        {
            StartThread();

            {
                ACE_Guard<ACE_Mutex> lck(m_reportQueueLock);
                m_reportQueue.push_back(report);
            }

            if (m_writeNotified == 0)
            {
                m_writeNotified = 1;
                reactor()->notify(this,ACE_Event_Handler::WRITE_MASK);
            }
        }
    }

    void
    Library::SendProgramInfoReport(const std::wstring & text)
    {
        ReportPtr report = m_reportCreator.CreateProgramInfoReport(text);
        if (!TrySend(report))
        {
            StartThread();

            {
                ACE_Guard<ACE_Mutex> lck(m_reportQueueLock);
                m_reportQueue.push_back(report);
            }

            if (m_writeNotified == 0)
            {
                m_writeNotified = 1;
                reactor()->notify(this,ACE_Event_Handler::WRITE_MASK);
            }
        }
    }

    void Library::SendReports()
    {
        try
        {
            while (!m_reportQueue.empty())
            {
                ACE_Guard<ACE_Mutex> lck(m_reportQueueLock);
                m_reportCreator.SetConnectionInfoIfNotSet(m_reportQueue.back());
                m_connection->Send(m_reportQueue.front(), Safir::Dob::Typesystem::ChannelId(), this);
                m_reportQueue.pop_front();
            }
        }
        catch (const Safir::Dob::OverflowException &)
        {
            //do nothing
        }
    }

    //Send reports
    int Library::handle_output (ACE_HANDLE)
    {
        m_writeNotified = 0;
        SendReports();
        return 0;
    }

    void Library::OnNotMessageOverflow()
    {
        SendReports();
        TraceFlush();
    }
}
}
}
