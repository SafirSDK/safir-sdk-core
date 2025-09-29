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

#pragma once

#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Utilities/Internal/Atomic.h>
#include <boost/noncopyable.hpp>
#include <boost/static_assert.hpp>
#include <boost/thread/recursive_mutex.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/locks.hpp>
#include <mutex>
#include <list>
#include <queue>
#include "Prefixes.h"
#include "TracerDataSender.h"
#include <memory>
#include <Safir/Dob/Consumer.h>
#include <Safir/Control/Status.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Internal/ControlInfo.h>

class Library:
    public Safir::Dob::MessageSubscriber,
    private boost::noncopyable
{
public:

    static Library & Instance();

    void SetProgramName(const std::wstring & programName);

    void StartTraceBackdoor(const std::wstring& connectionNameCommonPart,
                            const std::wstring& connectionNameInstancePart);
    void StopTraceBackdoor();

    void StartCrashReporting();
    void StopCrashReporting();

    PrefixId AddPrefix(std::wstring prefix);
    volatile bool * GetPrefixStatePointer(const PrefixId prefixId);
    bool IsEnabledPrefix(const PrefixId prefixId) const;
    void EnablePrefix(const PrefixId prefixId, const bool enabled);

    void TraceChar(const PrefixId prefixId,
                   char ch);
    void TraceWChar(const PrefixId prefixId,
                    const wchar_t ch);
    void TraceString(const PrefixId prefixId,
                     const char* str);

    void TraceString(const PrefixId prefixId,
                     const char* str,
                     const size_t offset,
                     const size_t length);

    void TraceString(const PrefixId prefixId,
                     const std::wstring& str);

    void TraceFlush();

    void SendFatalErrorReport(const std::wstring & errorCode,
                              const std::wstring & location,
                              const std::wstring & text);


    void SendErrorReport(const std::wstring & errorCode,
                         const std::wstring & location,
                         const std::wstring & text);

    void SendResourceReport(const std::wstring & resourceId,
                            const bool allocated,
                            const std::wstring & text);

    void SendProgrammingErrorReport(const std::wstring & errorCode,
                                    const std::wstring & location,
                                    const std::wstring & text);

    void SendProgramInfoReport(const std::wstring & text);
private:
    Library();
    ~Library();

    static void CrashFunc(const char* const dumpPath);

    void GetEnv();

    void OnMessage(const Safir::Dob::MessageProxy messageProxy) override;

    void HandleCommand(const std::vector<std::wstring>& cmdTokens);
    std::wstring GetHelpText();

    std::vector<std::wstring> m_arguments;

    std::thread m_thread;
    boost::asio::io_context m_ioContext;
    boost::asio::executor_work_guard<boost::asio::io_context::executor_type> m_work;
    Safir::Utilities::AsioDispatcher m_dispatcher;
    Safir::Dob::Connection m_connection;

    Prefixes m_prefixes;

    // This is the connection name which we were given in the constructor.
    // We use this for matching names in messages, since this was the old way of
    // addressing backdoors. Now we match both this and the process name.
    std::wstring m_mainConnectionName;

    std::wstring m_backdoorConnectionName;

    //This is the name of the program, as fetched from the OS or set programattically
    std::wstring m_programName;

    void TraceInternal(const PrefixId prefixId,
                       const wchar_t ch);


    //trace buffer and the associated lock
    boost::mutex m_traceBufferLock;
    std::wstring m_traceStdoutBuffer;
    std::wstring m_traceSyslogBuffer;
    std::wstring m_traceUdpBuffer;
    bool m_prefixPending;

    bool m_windowsNativeLogging;

    TracerDataSender m_tracerDataSender;
    std::shared_ptr<Safir::Dob::Internal::Control::ControlInfoReceiver> m_controlInfoReceiver;


    /**
     * This class is here to ensure that only the Instance method can get at the
     * instance, so as to be sure that call_once is used correctly.
     * Also makes it easier to grep for singletons in the code, if all
     * singletons use the same construction and helper-name.
     */
    struct SingletonHelper
    {
    private:
        friend Library& Library::Instance();

        static Library& Instance();
        static std::once_flag m_onceFlag;
    };
};


