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

#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/PanicLogging.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <boost/bind.hpp>
#include <Safir/Utilities/CrashReporter.h>
#include <boost/regex.hpp>
#include <boost/tokenizer.hpp>
#include <iomanip>
#include <iostream>

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
    using Safir::Dob::Typesystem::Utilities::ToUtf8;

    static const wchar_t* pingCmd = L"ping";
    static const wchar_t* helpCmd = L"help";

    typedef boost::tokenizer<boost::char_separator<wchar_t>,
                             std::wstring::const_iterator,
                             std::wstring> wtokenizer;

    const boost::char_separator<wchar_t> separator(L" ");

    boost::once_flag Library::SingletonHelper::m_onceFlag = BOOST_ONCE_INIT;
    boost::scoped_ptr<Library> Library::SingletonHelper::m_instance;

    void Library::SingletonHelper::Instantiate()
    {
        m_instance.reset(new Library());
    }

    Library & Library::Instance()
    {
        boost::call_once(SingletonHelper::m_onceFlag,boost::bind(SingletonHelper::Instantiate));
        return *SingletonHelper::m_instance;
    }


    Library::Library()
        : m_programName(),
          m_arguments(),
          m_prefixes(),
          m_prefixSearchLock(),
          m_backdoorConnection(),
          m_traceBufferLock(),
          m_traceBuffer(),
          m_prefixPending(true),
          m_logCreator(),
          m_systemLog()
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

    void
    Library::SetProgramName(const std::wstring& programName)
    {
        m_programName = programName.substr(programName.find_last_of(L"/\\")+1);
    }

    void
    Library::StartTraceBackdoor(const std::wstring& connectionNameCommonPart,
                                const std::wstring& connectionNameInstancePart)
    {
        m_backdoorConnection.Attach(connectionNameCommonPart,
                                    connectionNameInstancePart);

        m_backdoorConnection.SubscribeMessage(Safir::Application::BackdoorCommand::ClassTypeId,
                                              Safir::Dob::Typesystem::ChannelId(),
                                              this);
    }

    void
    Library::StopTraceBackdoor()
    {
        if (!m_backdoorConnection.IsOpen())
        {
            // Connection has been closed.
            return; // *** RETURN ***
        }

        m_backdoorConnection.UnsubscribeMessage(Safir::Application::BackdoorCommand::ClassTypeId,
                                                Safir::Dob::Typesystem::ChannelId(),
                                                this);
    }


    Library::PrefixId
    Library::AddPrefix(const std::wstring & prefix)
    {

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

    // Private method that assumes the buffer lock is already taken.
    void
    Library::AddToTraceBuf(const PrefixId prefixId,
                           const wchar_t  ch)
    {
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
    Library::Trace(const PrefixId prefixId,
                   const wchar_t ch)
    {
        boost::lock_guard<boost::mutex> lock(m_traceBufferLock);
        AddToTraceBuf(prefixId, ch);
    }

    void
    Library::TraceString(const PrefixId prefixId,
                         const std::wstring & str)
    {
        boost::lock_guard<boost::mutex> lock(m_traceBufferLock);
        std::for_each(str.begin(), str.end(), boost::bind(&Library::AddToTraceBuf,this,prefixId,_1));
    }

    void
    Library::TraceFlush()
    {
        boost::lock_guard<boost::mutex> lock(m_traceBufferLock);

        if (m_traceBuffer.empty())
        {
            return;
        }

        // traces are always written to std out
        std::wcout << m_traceBuffer << std::flush;

        m_systemLog.Send(Safir::Utilities::SystemLog::Debug,
                         ToUtf8(m_traceBuffer));

        m_traceBuffer.clear();
    }

    void Library::CrashFunc(const char* const dumpPath)
    {
        std::ostringstream ostr;
        ostr << "An application has crashed! A dump was generated to:\n" 
             << dumpPath;
        std::wcerr << ostr.str().c_str() << std::endl;
        Safir::Utilities::Internal::PanicLogging::Log(ostr.str());
    }

    void
    Library::StartCrashReporting()
    {
        Safir::Utilities::CrashReporter::RegisterCallback(CrashFunc);
        Safir::Utilities::CrashReporter::Start();
    }

    void
    Library::StopCrashReporting()
    {
        Safir::Utilities::CrashReporter::Stop();
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

                    std::wcout << L"Tracer Ping reply" << std::endl;

                    SendProgramInfoReport(L"Tracer Ping reply");

                    return; // *** RETURN ***
                }
                else if (cmdTokens[0] == helpCmd)
                {
                    std::wstring help = GetHelpText();

                    std::wcout << help << std::endl;

                    SendProgramInfoReport(help);

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
        TraceFlush();
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
    Library::SendSystemLog(const Safir::Utilities::SystemLog::Severity severity,
                           const std::wstring&                         logMsg)
    {
        m_systemLog.Send(severity,
                         ToUtf8(m_logCreator.CreateSystemLog(logMsg)));
    }

    void
    Library::SendFatalErrorReport(const std::wstring& errorCode,
                                  const std::wstring& location,
                                  const std::wstring& text)
    {
        m_systemLog.Send(Safir::Utilities::SystemLog::Critical,
                         ToUtf8(m_logCreator.CreateFatalErrorLog(errorCode,
                                                                 location,
                                                                 text)));
    }

    void
    Library::SendErrorReport(const std::wstring& errorCode,
                             const std::wstring& location,
                             const std::wstring& text)
    {
        m_systemLog.Send(Safir::Utilities::SystemLog::Error,
                         Safir::Dob::Typesystem::Utilities::ToUtf8
                         (m_logCreator.CreateErrorLog(errorCode,
                                                      location,
                                                      text)));
    }

    void
    Library::SendResourceReport(const std::wstring& resourceId,
                                const bool          allocated,
                                const std::wstring& text)
    {
        m_systemLog.Send(Safir::Utilities::SystemLog::Warning,
                         Safir::Dob::Typesystem::Utilities::ToUtf8
                         (m_logCreator.CreateResourceLog(resourceId,
                                                         allocated,
                                                         text)));
    }

    void
    Library::SendProgrammingErrorReport(const std::wstring & errorCode,
                                        const std::wstring & location,
                                        const std::wstring & text)
    {
        m_systemLog.Send(Safir::Utilities::SystemLog::Critical,
                         Safir::Dob::Typesystem::Utilities::ToUtf8
                         (m_logCreator.CreateProgrammingErrorLog(errorCode,
                                                                 location,
                                                                 text)));
    }

    void
    Library::SendProgramInfoReport(const std::wstring & text)
    {
        m_systemLog.Send(Safir::Utilities::SystemLog::Debug,
                         ToUtf8(m_logCreator.CreateProgramInfoLog(text)));
    }
}
}
}
