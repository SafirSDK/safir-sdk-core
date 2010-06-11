/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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
#ifndef __DOSE_SEMAPHORE_H__
#define __DOSE_SEMAPHORE_H__
#include <string>

#if defined (_WIN32)

#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4267)
#endif

#  define DOSE_USE_ACE_PROCESS_SEMAPHORE_FOR_SIGNALS
#  include <ace/Process_Semaphore.h>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif

#elif defined(linux) || defined(__linux) || defined(__linux__)

#  include <boost/interprocess/sync/named_semaphore.hpp>
#  if !defined (BOOST_INTERPROCESS_NAMED_SEMAPHORE_USES_POSIX_SEMAPHORES)
#    error Expected boost interprocess to use posix semaphores! dont know what to do now!
#  endif

#else

#  error "Unable to work out platform"

#endif

#include <boost/interprocess/sync/interprocess_semaphore.hpp>
#include <boost/noncopyable.hpp>


namespace Safir
{
namespace Dob
{
namespace Internal
{
    class NamedSemaphore:
        private boost::noncopyable
    {
    public:
        /** defaults to an unsignalled state */
        explicit NamedSemaphore(const std::string& name);

        ~NamedSemaphore();

        void remove();

        void wait();
        bool try_wait();
        void post();

    private:
#ifdef DOSE_USE_ACE_PROCESS_SEMAPHORE_FOR_SIGNALS
        ACE_Process_Semaphore m_semaphore;
#else
        boost::interprocess::named_semaphore m_semaphore;
        const std::string m_name;
#endif
    };


#ifdef DOSE_USE_ACE_PROCESS_SEMAPHORE_FOR_SIGNALS

    inline NamedSemaphore::NamedSemaphore(const std::string& name):
        m_semaphore(0, name.c_str())
    {

    }

    inline NamedSemaphore::~NamedSemaphore()
    {

    }

    inline void NamedSemaphore::remove()
    {

    }

    inline void NamedSemaphore::wait()
    {
        m_semaphore.acquire();
    }

    inline bool NamedSemaphore::try_wait()
    {
        return 0 == m_semaphore.tryacquire();
    }

    inline void NamedSemaphore::post()
    {
        m_semaphore.release();
    }

#else
    inline NamedSemaphore::NamedSemaphore(const std::string& name):
        m_semaphore(boost::interprocess::open_or_create, name.c_str(), 0),
        m_name(name)
    {

    }

    inline NamedSemaphore::~NamedSemaphore()
    {

    }

    inline void NamedSemaphore::remove()
    {
        boost::interprocess::named_semaphore::remove(m_name.c_str());
    }

    inline void NamedSemaphore::wait()
    {
        // On Linux, even in the absence of signal handlers, certain blocking interfaces
        // can fail with the error EINTR. This includes sem_wait wich is what
        // boost::interprocess::named_semaphore will use. I (STAWI) don't know why this
        // isn't handled transparantly by boost interprocess, but since this seems not to
        // be the case it is handled at this level.
        for (;;)
        {
            try
            {
                m_semaphore.wait();
                break;
            }
            catch (const boost::interprocess::interprocess_exception& e)
            {
                if (e.get_native_error() != EINTR)
                {
                    throw;
                }

            }
        }  
    }

    inline bool NamedSemaphore::try_wait()
    {
        return m_semaphore.try_wait();
    }

    inline void NamedSemaphore::post()
    {
        m_semaphore.post();
    }
#endif

    class Semaphore:
        private boost::noncopyable
    {
    public:
        Semaphore(unsigned int initialCount) : m_semaphore(initialCount) {};

        ~Semaphore() {};

        void wait();
        bool try_wait() {m_semaphore.try_wait();};
        void post() {m_semaphore.post();};

    private:

        boost::interprocess::interprocess_semaphore m_semaphore;
    };

    inline void Semaphore::wait()
    {
        // On Linux, even in the absence of signal handlers, certain blocking interfaces
        // can fail with the error EINTR. This includes sem_wait wich is what
        // boost::interprocess::interprocess_semaphore will use. I (STAWI) don't know why this
        // isn't handled transparantly by boost interprocess, but since this seems not to
        // be the case it is handled at this level.
        for (;;)
        {
            try
            {
                m_semaphore.wait();
                break;
            }
            catch (const boost::interprocess::interprocess_exception& e)
            {
                if (e.get_native_error() != EINTR)
                {
                    throw;
                }

            }
        }  
    }
}
}
}

#endif

