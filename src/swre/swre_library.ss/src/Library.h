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

#include "ReportCreator.h"
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Internal/Atomic.h>
#include <boost/asio.hpp>
#include <boost/noncopyable.hpp>
#include <boost/static_assert.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/thread.hpp>
#include <list>
#include <queue>

namespace Safir
{
namespace SwReports
{
namespace Internal
{

    class Library:
        public Safir::Dob::MessageSubscriber,
        public Safir::Dob::MessageSender,
        private boost::noncopyable
    {
    public:
        typedef Safir::Dob::Typesystem::Int64 PrefixId;

        static Library & Instance();

        void StartCrashReporting();
        void Stop();

        void SetProgramName(const std::wstring & programName);

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

        void StopInternal();
        static void AtExitFunc();
        static void SignalFunc(const int signal);
        static void CrashFunc(const char* const dumpPath);
        
        //install signal and atexit functions
        void Install();
  
        //This is called when the thread is exiting (NOT atexit)
        void HandleExit();

        void GetEnv();

        void StartThread();
        void Run();

        void HandleTimeout(const boost::system::error_code& error, 
                           const boost::shared_ptr<boost::asio::deadline_timer>& theTimer);
        void SendQueuedReports();

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

        bool TrySend(ReportPtr report);
        class TryMessageSender:
            public Safir::Dob::MessageSender
        {
            virtual void OnNotMessageOverflow(){};
        };
        TryMessageSender m_tryMessageSender;
        
        typedef std::list<PrefixState> Prefixes;
        typedef std::vector<std::wstring> Arguments;

        std::wstring m_programName;
        Arguments m_arguments;

        boost::asio::io_service m_ioService;

        //contains all the prefixes. Pointers into this structure are returned as handles
        //the language bindings. NEVER remove anything from this list!
        Prefixes m_prefixes;
        boost::recursive_mutex m_prefixSearchLock; //lock for anyone that loops through the prefixes or adds elements to it.

        //a secondary connection for the backdoor handling. This will be attached to
        // the own thread connection.
        Safir::Dob::SecondaryConnection m_backdoorConnection;
        volatile bool m_isBackdoorStarted;

        //trace buffer and the associated lock
        boost::mutex m_traceBufferLock;
        std::wstring m_traceBuffer;
        bool m_prefixPending;

        //reports stuff
        ReportCreator m_reportCreator;
        boost::mutex m_reportQueueLock;
        std::deque<ReportPtr> m_reportQueue;

        //thread stuff
        boost::mutex m_threadStartingLock; //make sure that only one thread starts the logger thread...
        boost::shared_ptr<Safir::Dob::Connection> m_connection;

        boost::thread m_thread;
        boost::thread::id m_threadId;
        
        Safir::Dob::Internal::AtomicUint32 m_sendReportsPending;
        Safir::Dob::Internal::AtomicUint32 m_flushPending;

        Safir::Dob::Internal::AtomicUint32 m_crashed;

        /**
         * This class is here to ensure that only the Instance method can get at the 
         * instance, so as to be sure that boost call_once is used correctly.
         * Also makes it easier to grep for singletons in the code, if all 
         * singletons use the same construction and helper-name.
         */
        struct SingletonHelper
        {
        private:
            friend Library& Library::Instance();

            static void Instantiate();
            static boost::scoped_ptr<Library> m_instance;
            static boost::once_flag m_onceFlag;
        };

        //vs2008 mistakenly gives a warning about not allowing friends to be inline. But that is spurious in this case.        
#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4396)
#endif
        //lets boost::checked_delete access the destructor
        friend void boost::checked_delete<>(Library*x);

#ifdef _MSC_VER
#pragma warning (pop)
#endif

    };

}
}
}
#endif

