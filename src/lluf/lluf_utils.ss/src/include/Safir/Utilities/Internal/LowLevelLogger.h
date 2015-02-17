/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#ifndef __LLUF_LOW_LEVEL_LOGGER_NEW_H__
#define __LLUF_LOW_LEVEL_LOGGER_NEW_H__

#include <Safir/Utilities/Internal/UtilsExportDefs.h>
#include <boost/noncopyable.hpp>
#include <boost/thread/once.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/tss.hpp>
#include <boost/shared_ptr.hpp>
#include <ostream>
#include <boost/static_assert.hpp>

//forward declaration
namespace boost { namespace asio {
    class io_service;
}}

/**
  * This is a utility for logging to file that is _only_ intended for
  * use by low level parts of the Safir system. All other applications should
  * use some other mechanism for logging.
  *
  * Just use lllog like you would any ostream
  *   lllog(1) << "hello world 1"<<std::endl;
  *   lllog(5) << 123 << std::endl;
  * The argument to lllog itself is the logging level, where high means verbose.
  * Level is a value between 1 and 9 (no checks are made for this, but any other
  * values may cause unexpected behavior).
  */
#define lllog(level) if (Safir::Utilities::Internal::Internal::LowLevelLogger::Instance().LogLevel() < level) \
                         BOOST_STATIC_ASSERT(level > 0 && level <= 9); \
                     else if(Safir::Utilities::Internal::Internal::LowLevelLogger::Magic lck_fjki34 =           \
                             Safir::Utilities::Internal::Internal::LowLevelLogger::Instance().MagicLock()) ;    \
                     else Safir::Utilities::Internal::Internal::LowLevelLogger::Instance().Stream()
#define lllout lllog(8)

namespace Safir
{
namespace Utilities
{
namespace Internal
{
    class LowLevelLoggerControl;

    //this is all the hidden magic implementation
    namespace Internal
    {
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4275)
#pragma warning(disable: 4251)
#endif
        class LLUF_UTILS_API LowLevelLogger
            : private boost::noncopyable
        {
        public:
            static LowLevelLogger & Instance();

            inline int LogLevel() const
            {
                if (m_logLevel == NULL)
                {
                    return 0;
                }
                else
                {
                    return *m_logLevel;
                }
            }

            /*            inline bool Synchronous() const
            {
                return m_synchronous;
                }*/

            void SwitchToAsynchronousMode(boost::asio::io_service& ioService);
            void Stop(); //only need to call this if SwitchToAsynchronousMode has been called.

            //This is a magic unlocker that has the strange behaviour of having a bool
            //operator that always returns false. It is for the use in preprocessor
            //magic only.
            class Magic
            {
            public:
                inline Magic():m_lock(nullptr) {}

                inline explicit Magic(boost::mutex& mutex):
                    m_lock(&mutex)
                {
                }

                inline ~Magic()
                {
                    if (m_lock != nullptr)
                    {
                        m_lock->unlock();
                    }
                }

                inline operator bool() const
                {
                    return false;
                }
            private:
                boost::mutex* m_lock;
            };

            //Returns a Magic object that will unlock the lock when destroyed
            inline const Magic MagicLock()
            {
                if (LowLevelLogger::Instance().m_synchronous)
                {
                    LowLevelLogger::Instance().m_synchronousLock.lock();
                    return Magic(LowLevelLogger::Instance().m_synchronousLock);
                }
                else
                {
                    return Magic();
                }
            }

            inline std::wostream& Stream()
            {
                if (LowLevelLogger::Instance().m_synchronous)
                {
                    return m_synchronousStream;
                }
                else
                {
                    if (m_asynchronousStream.get() == nullptr)
                    {
                        m_asynchronousStream.reset(CreateAsyncStream());
                    }
                    return *m_asynchronousStream;
                }
            }
        private:
            /** Constructor*/
            explicit LowLevelLogger();

            /** Destructor */
            ~LowLevelLogger();

            std::wostream* CreateAsyncStream();

            /**
             * This class is here to ensure that only the Instance method can get at the
             * instance, so as to be sure that boost call_once is used correctly.
             * Also makes it easier to grep for singletons in the code, if all
             * singletons use the same construction and helper-name.
             */
            struct SingletonHelper
            {
            private:
                friend LowLevelLogger& LowLevelLogger::Instance();

                static LowLevelLogger& Instance();
                static boost::once_flag m_onceFlag;
            };

            boost::shared_ptr<LowLevelLoggerControl> m_control;

            class SyncFileLogger;
            class AsyncLogger;

            std::unique_ptr<SyncFileLogger> m_syncLogger;
            std::unique_ptr<AsyncLogger> m_asyncLogger;

            const int* m_logLevel;
            bool m_synchronous;

            //this lock needs to be taken before logging to the logger when in synchronous mode!
            boost::mutex m_synchronousLock;

            //This stream is used when using synchronous mode
            std::wostream m_synchronousStream;

            boost::thread_specific_ptr<std::wostream> m_asynchronousStream;
        };
#ifdef _MSC_VER
#pragma warning (pop)
#endif
    }
}
}
}

#endif
