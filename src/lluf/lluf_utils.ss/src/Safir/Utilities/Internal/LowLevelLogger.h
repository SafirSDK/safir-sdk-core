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
#ifndef __LLUF_LOW_LEVEL_LOGGER_H__
#define __LLUF_LOW_LEVEL_LOGGER_H__
#include <fstream>

#include <boost/interprocess/shared_memory_object.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/interprocess/streams/vectorstream.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/noncopyable.hpp>
#include <Safir/Utilities/Internal/UtilsExportDefs.h>

//#include <ace/Process_Mutex.h>
//#include <ace/Process_Semaphore.h>
#include <Safir/Utilities/StartupSynchronizer.h>
#include <ace/Thread_Mutex.h>

#include <vector>

/**
  * This is a utility for logging to file that is _only_ intended for
  * use by low level parts of the Safir system. All other applications should
  * Use some other mechanism for logging.
  *
  * just use lllout like you would any ostream
  * lllout << "hello world 1"<<std::endl;
  * lllout << 123 << std::endl;
  */

#define lllout if (!Safir::Utilities::Internal::Internal::LowLevelLoggerBackend::Instance().LoggingEnabled()) ; else *boost::scoped_ptr<Safir::Utilities::Internal::Internal::LowLevelLogger>(new Safir::Utilities::Internal::Internal::LowLevelLogger(false))
#define lllinfo *boost::scoped_ptr<Safir::Utilities::Internal::Internal::LowLevelLogger>(new Safir::Utilities::Internal::Internal::LowLevelLogger(false))
#define lllerr *boost::scoped_ptr<Safir::Utilities::Internal::Internal::LowLevelLogger>(new Safir::Utilities::Internal::Internal::LowLevelLogger(true))

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251) // warning C4251: 'Safir::Dob::Typesystem::LibraryExceptions::m_CallbackMap' : class 'stdext::hash_map<_Kty,_Ty>' needs to have dll-interface to be used by clients of class 'Safir::Dob::Typesystem::LibraryExceptions'
#pragma warning (disable: 4275) // warning C4275: non dll-interface class 'boost::noncopyable_::noncopyable' used as base for dll-interface class 'Safir::Dob::Typesystem::LibraryExceptions'
#endif

namespace Safir
{
namespace Utilities
{
namespace Internal
{
    //this is all the hidden magic implementation
    namespace Internal
    {
        class LLUF_UTILS_API LowLevelLogger :
            public std::basic_ostream<wchar_t, std::char_traits<wchar_t> >
        {
        public:
            LowLevelLogger(bool forceFlush);
            ~LowLevelLogger();
        };

        class LLUF_UTILS_API LowLevelLoggerBackend :
            public Synchronized,
            private boost::noncopyable
        {
        public:
            static LowLevelLoggerBackend & Instance();

            //inline bool LoggingEnabled() const {return static_cast<const LowLevelLoggerStreamBuf *>(rdbuf())->LoggingEnabled();}
            inline bool LoggingEnabled() const {return m_pLoggingEnabled != NULL && *m_pLoggingEnabled;}

            void CopyToInternalBuffer(const std::wostringstream& ostr);
            void OutputInternalBuffer();

            inline bool OutputThreadStarted() const {return m_outputThreadStarted;}
            inline boost::filesystem::wofstream& OutputFile() {return m_OutputFile;}

            //StartupSynchronizer stuff
            void Create();
            void Use();
            void Destroy();

        private:
            LowLevelLoggerBackend();
            ~LowLevelLoggerBackend();

            static LowLevelLoggerBackend * volatile m_pInstance;

            static ACE_Thread_Mutex m_InstantiationLock;

            boost::interprocess::shared_memory_object m_shm;
            boost::interprocess::mapped_region m_shmRegion;
            boost::filesystem::wofstream m_OutputFile;
     
            typedef std::vector<wchar_t> BufferT;
            boost::interprocess::basic_vectorstream<BufferT> m_os;
            BufferT m_outputBuf;

            bool * m_pLoggingEnabled;

            StartupSynchronizer m_startupSynchronizer;

            static ACE_THR_FUNC_RETURN OutputThreadFunc(void* _this);
            void OutputWorker();
            ACE_Thread_Mutex m_bufLock;
            ACE_Thread_Mutex m_internalBufLock;
            bool m_outputThreadStarted;
        };

        class LLUF_UTILS_API LowLevelLoggerStreamBuf :
            public std::basic_streambuf<wchar_t, std::char_traits<wchar_t> >
        {
            typedef std::char_traits<wchar_t> _Tr;

        public:
            LowLevelLoggerStreamBuf(bool forceFlush);
            virtual ~LowLevelLoggerStreamBuf();

        private:
            virtual _Tr::int_type uflow();
            virtual _Tr::int_type underflow();
            virtual _Tr::int_type overflow(_Tr::int_type c = _Tr::eof());

            void WriteDateTime();

            bool m_bDatePending;

            std::wostringstream m_ostr;

            LowLevelLoggerBackend & m_backend;
            bool m_forceFlush;
        };
    }
}
}
}

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#endif

