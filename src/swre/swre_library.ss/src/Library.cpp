/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
#include <Safir/Logging/Log.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <Safir/Utilities/Internal/ConfigReader.h>
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
    using Safir::Dob::Typesystem::Utilities::ToWstring;

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


    Library::Library():
          m_arguments(),
          m_prefixes(),
          m_prefixSearchLock(),
          m_backdoorConnection(),
          m_traceBufferLock(),
          m_prefixPending(true),
          m_windowsNativeLogging(false)
    {
        std::wstring env;
        {
            char * cenv = getenv("FORCE_LOG");
            if (cenv != NULL)
            {
                env = ToWstring(cenv);
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
            std::wostringstream ostr;
            ostr << "Failed to parse FORCE_LOG env (" << env << "): "<< exc.what();
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          ostr.str());
        }

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)

        Safir::Utilities::Internal::ConfigReader configReader;

        m_windowsNativeLogging = configReader.Logging().get<bool>("SystemLog.native_logging");
#endif

    }

    Library::~Library() 
    {

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
    Library::TraceInternal(const PrefixId prefixId,
                           const wchar_t ch)
    {
        if (m_prefixPending)
        {
            //no syslog when using windows native logging
            if (!m_windowsNativeLogging)
            {
                m_traceSyslogBuffer.append(ToPrefix(prefixId).m_prefix);
                m_traceSyslogBuffer.append(L": ");
            }

            m_traceStdoutBuffer.append(ToPrefix(prefixId).m_prefixAscii);
            m_traceStdoutBuffer.append(L": ");
            m_prefixPending = false;
        }

        //since we dont know the locale of wcout we strip off all non-ascii chars
        if ((ch & ~0x7F) == 0)
        {
            m_traceStdoutBuffer.push_back(ch);
        }
        else
        {
            m_traceStdoutBuffer.push_back('@');
        }
        
        //no syslog when using windows native logging
        if (!m_windowsNativeLogging)
        {
            //Syslogs are flushed on newlines instead of flushes
            if (ch == '\n')
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Debug,
                                              m_traceSyslogBuffer);
                m_traceSyslogBuffer.clear();
            }
            else
            {
                m_traceSyslogBuffer.push_back(ch);
            }
        }

        if (ch == '\n')
        {
            m_prefixPending = true;
        }
    }

    void
    Library::TraceFlush()
    {
        boost::lock_guard<boost::mutex> lock(m_traceBufferLock);

        if (!m_traceStdoutBuffer.empty())
        {
            std::wcout << m_traceStdoutBuffer << std::flush;
            m_traceStdoutBuffer.clear();
        }
    }

    void
    Library::TraceChar(const PrefixId prefixId,
                       char ch)
    {
        if ((ch & ~0x7F) != 0)
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"TraceChar got non ascii character");
            ch = '#';
        }
        TraceWChar(prefixId,ch);
    }

    void
    Library::TraceWChar(const PrefixId prefixId,
                        const wchar_t ch)
    {
        boost::lock_guard<boost::mutex> lock(m_traceBufferLock);
        TraceInternal(prefixId, ch);
    }

    void
    Library::TraceString(const PrefixId prefixId,
                         const char* str)
    {
        TraceString(prefixId, ToWstring(str));
    }

    void
    Library::TraceString(const PrefixId prefixId,
                         const char* str,
                         const size_t offset,
                         const size_t length)
    {
        TraceString(prefixId, ToWstring(std::string(str + offset, str + offset + length)));
    }

    void
    Library::TraceString(const PrefixId prefixId,
                         const std::wstring& str)
    {
        boost::lock_guard<boost::mutex> lock(m_traceBufferLock);
        std::for_each(str.begin(), str.end(), boost::bind(&Library::TraceInternal,this,prefixId,_1));
    }


    void Library::CrashFunc(const char* const dumpPath)
    {
        std::wostringstream ostr;
        ostr << "An application has crashed! A dump was generated to:\n" 
             << dumpPath;
        std::wcerr << ostr.str().c_str() << std::endl;
        Safir::Logging::SendSystemLog(Safir::Logging::Alert,
                                      ostr.str());
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
                if (!boost::regex_search(connectionAspectMisc.GetConnectionName(), boost::wregex(cmd->ConnectionName().GetVal(), regExpFlags)))
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

                    Safir::Logging::SendSystemLog(Safir::Logging::Debug,
                                                  L"Tracer Ping reply");

                    return; // *** RETURN ***
                }
                else if (cmdTokens[0] == helpCmd)
                {
                    std::wstring help = GetHelpText();

                    std::wcout << help << std::endl;

                    Safir::Logging::SendSystemLog(Safir::Logging::Debug,
                                                  help);
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

    Library::PrefixState::PrefixState(const std::wstring & prefix, const bool enabled)
        : m_prefix(prefix)
        , m_prefixAscii(prefix)
        , m_isEnabled(enabled) 
    {
        //replace non-ascii chars
        for(std::wstring::iterator it = m_prefixAscii.begin();
            it != m_prefixAscii.end(); ++it)
        {
            if ((*it & ~0x7F) != 0)
            {
                *it = '@';
            }
        }
    }

    void
    Library::SendFatalErrorReport(const std::wstring& errorCode,
                                  const std::wstring& location,
                                  const std::wstring& text)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Critical,
                         L"FatalError " + errorCode + L"|" + location + L"|" + text);
    }

    void
    Library::SendErrorReport(const std::wstring& errorCode,
                             const std::wstring& location,
                             const std::wstring& text)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                      L"Error " + errorCode + L"|" + location + L"|" + text);
    }

    void
    Library::SendResourceReport(const std::wstring& resourceId,
                                const bool          allocated,
                                const std::wstring& text)
    {
        Safir::Logging::SendSystemLog(allocated ? Safir::Logging::Informational : Safir::Logging::Error,
                                      L"Resource " + resourceId + L" is " 
                                      + ((allocated) ? L"" : L"not ") 
                                      + L"allocated" + L"|" + text);
    }

    void
    Library::SendProgrammingErrorReport(const std::wstring & errorCode,
                                        const std::wstring & location,
                                        const std::wstring & text)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Critical,
                                      L"ProgrammingError " + errorCode + L"|" + location + L"|" + text);
    }

    void
    Library::SendProgramInfoReport(const std::wstring & text)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Debug,
                                      text);
    }
}
}
}
