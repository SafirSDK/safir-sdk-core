/******************************************************************************
*
* Copyright Saab AB, 2007-2013, 2025 (http://safirsdkcore.com)
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
#include <regex>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Application/BackdoorCommand.h>
#include <Safir/Application/TracerStatus.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Logging/Log.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <Safir/Utilities/CrashReporter.h>
#include <Safir/Utilities/Internal/Id.h>
#include <boost/tokenizer.hpp>
#include <iomanip>
#include <iostream>
#include <memory>
#include <Safir/Dob/Internal/ControlInfo.h>

#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4702 4005)
#endif

#include <boost/lexical_cast.hpp>
#include <Safir/Control/Status.h>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif

#ifdef GetMessage
#  undef GetMessage
#endif

using Safir::Dob::Typesystem::Utilities::ToUtf8;
using Safir::Dob::Typesystem::Utilities::ToWstring;

static const wchar_t* pingCmd = L"ping";
static const wchar_t* helpCmd = L"help";

typedef boost::tokenizer<boost::char_separator<wchar_t>,
                         std::wstring::const_iterator,
                         std::wstring> wtokenizer;

const boost::char_separator<wchar_t> separator(L" ");

std::once_flag Library::SingletonHelper::m_onceFlag;

Library& Library::SingletonHelper::Instance()
{
    static Library instance;
    return instance;
}

Library & Library::Instance()
{
    std::call_once(SingletonHelper::m_onceFlag,[]{SingletonHelper::Instance();});
    return SingletonHelper::Instance();
}


Library::Library()
    : m_arguments()
    , m_work(boost::asio::make_work_guard(m_ioContext))
    , m_dispatcher(m_connection, m_ioContext)
    , m_prefixes(m_connection, m_ioContext)
    , m_traceBufferLock()
    , m_prefixPending(true)
    , m_windowsNativeLogging(false)
    , m_tracerDataSender(m_ioContext, LlufId_GenerateRandom64())
{
    Safir::Utilities::ProcessInfo proc(Safir::Utilities::ProcessInfo::GetPid());
    m_programName = Safir::Dob::Typesystem::Utilities::ToWstring(proc.GetProcessName());
    // Strip trailing ".exe" (case-insensitive) from program name
    m_programName = std::regex_replace(m_programName, std::wregex(L"\\.[eE][xX][eE]$"), L"");
    m_tracerDataSender.SetProgramName(ToUtf8(m_programName));
    m_tracerDataSender.SetNodeName(ToUtf8(Safir::Dob::ThisNodeParameters::Name()));
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
    StopTraceBackdoor();
}

void
Library::SetProgramName(const std::wstring& programName)
{
    m_programName = programName;
    m_tracerDataSender.SetProgramName(ToUtf8(m_programName));
}

void
Library::StartTraceBackdoor(const std::wstring& connectionNameCommonPart,
                            const std::wstring& connectionNameInstancePart)
{
    //Get the full connection name of the connection in the main app
    {
        Safir::Dob::SecondaryConnection mainConnection;
        mainConnection.Attach(connectionNameCommonPart,
                            connectionNameInstancePart);

        Safir::Dob::ConnectionAspectMisc connectionAspectMisc(mainConnection);

        m_mainConnectionName = connectionAspectMisc.GetConnectionName();
    }

    m_thread = std::thread([this]
    {
        try
        {
            const auto myPid = Safir::Utilities::ProcessInfo::GetPid();
            m_connection.Open(m_programName, std::to_wstring(myPid), 0, NULL, &m_dispatcher);

            Safir::Dob::ConnectionAspectMisc connectionAspectMisc(m_connection);
            m_backdoorConnectionName = connectionAspectMisc.GetConnectionName();

            m_connection.SubscribeMessage(Safir::Application::BackdoorCommand::ClassTypeId,
                                          Safir::Dob::Typesystem::ChannelId(),
                                          this);

            m_prefixes.StartEntityHandling(m_programName);

            // Start ControlInfoReceiver to obtain incarnationId and forward it to the tracer
            m_controlInfoReceiver = std::make_shared<Safir::Dob::Internal::Control::ControlInfoReceiver>
                (m_ioContext,
                 [this](int64_t incarnationId, int64_t /*nodeId*/)
                 {
                     m_tracerDataSender.SetIncarnationId(incarnationId);
                 });
            m_controlInfoReceiver->Start();

            m_ioContext.run();
        }
        catch (const Safir::Dob::LowMemoryException&)
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Severity::Error,
                                          L"Low memory exception occurred while starting trace logging for " + m_programName +L".");
        }
        catch (const std::exception& exc)
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Severity::Critical,
                                          L"An unexpected error occurred while starting trace logging for " +
                                          m_programName +
                                          L": " + Safir::Dob::Typesystem::Utilities::ToWstring(exc.what()));
        }
        m_connection.Close();
    });
}

void
Library::StopTraceBackdoor()
{
    if (m_thread.joinable())
    {
        boost::asio::post(m_ioContext,[this]{m_prefixes.StopEntityHandling();});
        if (m_controlInfoReceiver)
        {
            m_controlInfoReceiver->Stop();
        }

        m_work.reset();
        m_thread.join();
    }
}

PrefixId
Library::AddPrefix(std::wstring prefix)
{
    if (prefix.empty())
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Severity::Warning,
                                      L"Application tried to register an empty prefix. This is deprecated behaviour, and will become illegal in a future version of Safir SDK Core. Please use an actual string.");

        prefix = L"<empty>";
    }

    //Colons in the prefix will screw up things, since we use colons to separate
    //prefixes from log data in the output. And this is used for parsing too.
    std::replace(prefix.begin(), prefix.end(), ':', '#');

    auto const prefixId = m_prefixes.Add(prefix);

    if (find(m_arguments.begin(),m_arguments.end(),L"all") != m_arguments.end() ||
        find(m_arguments.begin(),m_arguments.end(),prefix) != m_arguments.end())
    {
        m_prefixes.Enable(prefixId, true);
    }
    return prefixId;
}

volatile bool * Library::GetPrefixStatePointer(const PrefixId prefixId)
{
    return m_prefixes.GetStatePointer(prefixId);
}


bool Library::IsEnabledPrefix(const PrefixId prefixId) const
{
    return m_prefixes.IsEnabled(prefixId);
}

void Library::EnablePrefix(const PrefixId prefixId, const bool enabled)
{
    m_prefixes.Enable(prefixId, enabled);
}

// Private method that assumes the buffer lock is already taken.
void
Library::TraceInternal(const PrefixId prefixId,
                       const wchar_t ch)
{
    if (m_prefixPending)
    {
        //no syslog when using windows native logging
        if (m_prefixes.LogToSafirLogging() && !m_windowsNativeLogging)
        {
            m_traceSyslogBuffer.append(m_prefixes.GetPrefix(prefixId));
            m_traceSyslogBuffer.append(L": ");
        }

        if (m_prefixes.LogToStdout())
        {
            m_traceStdoutBuffer.append(m_prefixes.GetPrefixAscii(prefixId));
            m_traceStdoutBuffer.append(L": ");
        }

        if (m_prefixes.LogToTracer())
        {
            m_traceUdpBuffer.append(m_prefixes.GetPrefix(prefixId));
            m_traceUdpBuffer.append(L": ");
        }

        m_prefixPending = false;
    }

    if (m_prefixes.LogToStdout())
    {
        //since we dont know the locale of wcout we strip off all non-ascii chars
        if ((ch & ~0x7F) == 0)
        {
            m_traceStdoutBuffer.push_back(ch);
        }
        else
        {
            m_traceStdoutBuffer.push_back('@');
        }
    }

    //no syslog when using windows native logging
    if (m_prefixes.LogToSafirLogging() && !m_windowsNativeLogging)
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

    if (m_prefixes.LogToTracer())
    {
        m_traceUdpBuffer.push_back(ch);
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

    if (m_prefixes.LogToStdout() && !m_traceStdoutBuffer.empty())
    {
        std::wcout << m_traceStdoutBuffer << std::flush;
        m_traceStdoutBuffer.clear();
    }

    if (m_prefixes.LogToTracer() && !m_traceUdpBuffer.empty())
    {
        m_tracerDataSender.Send(ToUtf8(m_traceUdpBuffer));
        m_traceUdpBuffer.clear();
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
    std::for_each(str.begin(), str.end(), [this, prefixId](const wchar_t c){TraceInternal(prefixId,c);});
}


void Library::CrashFunc(const char* const dumpPath)
{
    std::wostringstream ostr;
    ostr << "An application has crashed or is executing in an undefined state! A dump was generated to:\n"
         << dumpPath;
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
        const std::regex_constants::syntax_option_type regExpFlags =
            std::regex_constants::ECMAScript | std::regex_constants::icase;

        const Safir::Application::BackdoorCommandConstPtr cmd =
            std::static_pointer_cast<Safir::Application::BackdoorCommand>(message);

        if (!cmd->NodeName().IsNull())
        {
            if (!std::regex_search(Safir::Dob::ThisNodeParameters::Name(),
                                   std::wregex(cmd->NodeName().GetVal(), regExpFlags)))
            {
                // Node name doesn't match
                return;
            }
        }

        if (!cmd->ConnectionName().IsNull())
        {
            if (std::regex_search(m_mainConnectionName, std::wregex(cmd->ConnectionName().GetVal(), regExpFlags)) ||
                (!m_backdoorConnectionName.empty() &&
                 std::regex_search(m_backdoorConnectionName, std::wregex(cmd->ConnectionName().GetVal(), regExpFlags))))
            {
                //continue below if we matched either of the connection names
            }
            else
            {
                // Connection name doesn't match
                return;
            }
        }

        if (cmd->Command().IsNull())
        {
            // No command given
            return;
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
                std::wostringstream ostr;
                ostr << "<app>: Tracer Ping reply from "
                     << m_mainConnectionName
                     <<  " on node "
                     << Safir::Dob::ThisNodeParameters::Name();

                std::wcout << ostr.str() << std::endl;

                Safir::Logging::SendSystemLog(Safir::Logging::Debug,
                                              ostr.str());

                m_tracerDataSender.Send(ToUtf8(ostr.str() + L"\n"));

                return;
            }
            else if (cmdTokens[0] == helpCmd)
            {
                std::wstring help = GetHelpText();

                std::wcout << help << std::endl;

                Safir::Logging::SendSystemLog(Safir::Logging::Debug,
                                              help);

                m_tracerDataSender.Send(ToUtf8(help + L"\n"));
                return;
            }
        }

        // Let the subclass handle the command
        HandleCommand(cmdTokens);

    }
    catch (const std::regex_error& /* e*/ )
    {
        // An invalid regular expression was used, skip this command
        return;
    }
}

void
Library::HandleCommand(const std::vector<std::wstring>& cmdTokens)
{
    if (cmdTokens.size() != 2)
    {
        return;
    }

    const auto prefix = cmdTokens[0];
    const auto command = cmdTokens[1];

    std::set<PrefixId> prefixIds;
    if (prefix == L"all")
    {
        prefixIds = m_prefixes.GetAllPrefixIds();
    }
    else
    {
        const auto pf = m_prefixes.GetPrefixId(prefix);
        if (pf != 0)
        {
            prefixIds = {pf};
        }
    }
    for (const auto prefixId: prefixIds)
    {
        if (command != L"on" && command != L"off")
        {
            TraceString(prefixId, L"Got unrecognized command '" + command + L"'\n");
            continue;
        }
        TraceString(prefixId, L"Turning logging " + command + L"\n");
        m_prefixes.Enable(prefixId, command == L"on");

    }

    TraceFlush();
}

std::wstring
Library::GetHelpText()
{
    std::wostringstream out;
    out << "<app>: Trace logger supports the following commands: " << std::endl;
    out << "<app>:   " << std::setw(m_prefixes.LongestPrefixLength()) << "all" << " on/off - Turn logging of all prefices on or off" << std::endl;
    out << m_prefixes.GetHelpText();
    return out.str();
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

