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

#ifndef __LIBRARY_H__
#define __LIBRARY_H__

#include <boost/noncopyable.hpp>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Connection.h>
#include "ReportCreator.h"
#include <list>
#include <boost/static_assert.hpp>
#include <ace/Mutex.h>
#include <ace/Auto_Event.h>
#include <ace/Recursive_Thread_Mutex.h>
#include <ace/Reactor.h>
#include <queue>

namespace Safir
{
namespace SwReports
{
namespace Internal
{

    class Library:
        public ACE_Event_Handler,
        public Safir::Dob::MessageSubscriber,
        public Safir::Dob::MessageSender,
        private boost::noncopyable
    {
    public:
        typedef Safir::Dob::Typesystem::Int64 PrefixId;

        static Library & Instance();

        void Stop();

        void SetProgramName(const std::wstring & programName);

        //void SetCommandLineArguments(const std::vector<std::wstring> & arguments);

        PrefixId AddPrefix(const std::wstring & prefix);
        volatile bool * GetPrefixStatePointer(const PrefixId prefixId);
        bool IsEnabledPrefix(const PrefixId prefixId) const;
        void EnablePrefix(const PrefixId prefixId, const bool enabled);

        void Trace(const PrefixId prefixId,
                   const wchar_t ch,
                   const bool dontLock = false);
        void TraceString(const PrefixId prefixId,
                         const std::wstring & str);

        void TraceSync();
        void TraceFlush();

        bool TrySend(ReportPtr report);

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
        ~Library() {}

        static void AtExitFunc();
        static void SignalFunc(const int signal);
  
        //This is called when the thread is exiting (NOT atexit)
        void HandleExit();

        void GetEnv();

        void StartThread();
        static ACE_THR_FUNC_RETURN ThreadFunc(void * param);
        void Run();

        //READ_MASK means Flush
        virtual int handle_input (ACE_HANDLE);

        //WRITE_MASK means send queued reports!
        virtual int handle_output(ACE_HANDLE);

        //Time to flush the buffer
        virtual int handle_timeout(const ACE_Time_Value & current_time, const void * act);

        void StartBackdoor();
        virtual void OnMessage(const Safir::Dob::MessageProxy messageProxy);
        void HandleCommand(const std::vector<std::wstring>& cmdTokens);
        std::wstring GetHelpText();

        void OnNotMessageOverflow();

        struct PrefixState
        {
            PrefixState():m_prefix(),m_isEnabled(false) {}
            PrefixState(const std::wstring & prefix, const bool enabled):m_prefix(prefix),m_isEnabled(enabled) {}
            //   bool operator==(const PrefixState & other) const {return m_prefix == other.m_prefix;}
            bool operator==(const std::wstring & str) const {return m_prefix == str;}

            std::wstring m_prefix;
            bool m_isEnabled;
        };
        BOOST_STATIC_ASSERT(sizeof(Library::PrefixId) >= sizeof(Library::PrefixState*));

        static PrefixState & ToPrefix(const PrefixId prefixId);
        static PrefixId ToPrefixId(PrefixState & prefix);


        void SendReports(); //only callable from the internal thread

        typedef std::list<PrefixState> Prefixes;
        typedef std::vector<std::wstring> Arguments;

        std::wstring m_programName;
        Arguments m_arguments;

        ACE_Reactor m_reactor;

        //contains all the prefixes. Pointers into this structure are returned as handles
        //the language bindings. NEVER remove anything from this list!
        Prefixes m_prefixes;
        ACE_Recursive_Thread_Mutex m_prefixSearchLock; //lock for anyone that loops through the prefixes or adds elements to it.

        //a secondary connection for the backdoor handling. This will be attached to the connection
        //of the first caller of AddPrefix, or if there is no connection in that thread the
        //main thread will be started and this connection will be attached to it.
        Safir::Dob::SecondaryConnection m_backdoorConnection;
        volatile bool m_isBackdoorStarted;
        ACE_Recursive_Thread_Mutex m_backdoorStartingLock;


        //trace buffer and the associated lock
        ACE_Thread_Mutex m_traceBufferLock;
        std::wstring m_traceBuffer;
        bool m_prefixPending;

        //reports stuff
        ReportCreator m_reportCreator;
        ACE_Mutex m_reportQueueLock;
        std::deque<ReportPtr> m_reportQueue;

        //thread stuff
        ACE_Mutex m_threadStartingLock; //make sure that only one thread starts the logger thread...
        enum ThreadStatus {NotStarted, Started, Stopped};
        ThreadStatus m_threadStatus;

        ACE_thread_t m_threadId;
        ACE_hthread_t m_threadHandle;
        boost::shared_ptr<Safir::Dob::Connection> m_connection;
        ACE_Auto_Event m_threadStartingEvent;

        volatile int m_writeNotified;
        volatile int m_readNotified;

        //Singleton stuff
        static ACE_Mutex m_instantiationLock;
        static Library * volatile m_instance;
    };

}
}
}
#endif

