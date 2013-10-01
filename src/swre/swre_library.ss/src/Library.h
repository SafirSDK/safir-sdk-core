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

#ifndef __LIBRARY_H__
#define __LIBRARY_H__

#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Internal/Atomic.h>
#include <boost/noncopyable.hpp>
#include <boost/static_assert.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/thread/recursive_mutex.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/locks.hpp>
#include <boost/thread/once.hpp>
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
        private boost::noncopyable
    {
    public:
        typedef Safir::Dob::Typesystem::Int64 PrefixId;

        static Library & Instance();

        void StartTraceBackdoor(const std::wstring& connectionNameCommonPart,
                                const std::wstring& connectionNameInstancePart);
        void StopTraceBackdoor();

        void StartCrashReporting();
        void StopCrashReporting();

        PrefixId AddPrefix(const std::wstring & prefix);
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

        virtual void OnMessage(const Safir::Dob::MessageProxy messageProxy);
        void HandleCommand(const std::vector<std::wstring>& cmdTokens);
        std::wstring GetHelpText();

        struct PrefixState
        {
            PrefixState():m_prefix(),m_isEnabled(false) {}
            PrefixState(const std::wstring & prefix, const bool enabled);
            bool operator==(const std::wstring & str) const {return m_prefix == str;}

            std::wstring m_prefix;
            std::wstring m_prefixAscii;
            bool m_isEnabled;
        };
        BOOST_STATIC_ASSERT(sizeof(Library::PrefixId) >= sizeof(Library::PrefixState*));

        static PrefixState & ToPrefix(const PrefixId prefixId);
        static PrefixId ToPrefixId(PrefixState & prefix);
        
        typedef std::list<PrefixState> Prefixes;
        typedef std::vector<std::wstring> Arguments;

        Arguments m_arguments;

        //contains all the prefixes. Pointers into this structure are returned as handles
        //the language bindings. NEVER remove anything from this list!
        Prefixes m_prefixes;
        boost::recursive_mutex m_prefixSearchLock; //lock for anyone that loops through the prefixes or adds elements to it.

        // A secondary connection for the trace backdoor handling. This will be attached to
        // the connection given by the user.
        Safir::Dob::SecondaryConnection m_backdoorConnection;

        void TraceInternal(const PrefixId prefixId,
                           const wchar_t ch);

        //trace buffer and the associated lock
        boost::mutex m_traceBufferLock;
        std::wstring m_traceStdoutBuffer;
        std::wstring m_traceSyslogBuffer;
        bool m_prefixPending;

        bool m_windowsNativeLogging;

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

