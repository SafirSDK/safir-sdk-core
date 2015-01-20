/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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

#  define DOSE_SEMAPHORE_WIN32
//We take the boost.interprocess approach and define/import what we need from windows.h
//instead of including it and letting it taint the world with its define and other
//nastyness
// So there are a bunch of definitions below.

#elif defined(linux) || defined(__linux) || defined(__linux__)

#  include <boost/interprocess/sync/named_semaphore.hpp>
#  if !defined (BOOST_INTERPROCESS_NAMED_SEMAPHORE_USES_POSIX_SEMAPHORES)
#    error Expected boost interprocess to use posix semaphores! dont know what to do now!
#  endif

#else

#  error "Unable to work out platform"

#endif

#include <Safir/Utilities/Internal/ConfigReader.h>
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

        static void remove(const std::string& name);

        void wait();
        bool try_wait();
        void post();

    private:

#ifdef DOSE_SEMAPHORE_WIN32
        void* m_semaphoreHandle;
#else
        boost::interprocess::named_semaphore m_semaphore;
#endif
    };


#ifdef DOSE_SEMAPHORE_WIN32

    namespace Win32 
    {
        extern "C" __declspec(dllimport) void * __stdcall CreateSemaphoreA(void*, long, long, const char *);
        extern "C" __declspec(dllimport) int __stdcall ReleaseSemaphore(void *, long, long *);
        extern "C" __declspec(dllimport) unsigned long __stdcall WaitForSingleObject(void *, unsigned long);
        extern "C" __declspec(dllimport) int __stdcall CloseHandle(void*);
        extern "C" __declspec(dllimport) unsigned long __stdcall GetLastError();

        static const unsigned long infinite_time        = 0xFFFFFFFF;
        static const unsigned long wait_object_0        = 0;
        static const unsigned long wait_timeout         = 258L;
    }

    inline NamedSemaphore::NamedSemaphore(const std::string& name):
        m_semaphoreHandle(NULL)
    {
        m_semaphoreHandle =
                Win32::CreateSemaphoreA(NULL,
                                        0,
                                        0x7fffffff,
                                        (name + Safir::Utilities::Internal::Expansion::GetSafirInstanceSuffix()).c_str());
        if (m_semaphoreHandle == NULL) 
        {
            throw boost::interprocess::interprocess_exception(Win32::GetLastError());
        }
    }

    inline NamedSemaphore::~NamedSemaphore()
    {
        if (m_semaphoreHandle != NULL) 
        {
            Win32::CloseHandle(m_semaphoreHandle);
            m_semaphoreHandle = NULL;
        }
    }

    inline void NamedSemaphore::remove(const std::string&)
    {

    }

    inline void NamedSemaphore::wait()
    {
        const long res = Win32::WaitForSingleObject(m_semaphoreHandle, Win32::infinite_time);
        if (res != Win32::wait_object_0)
        {
            throw boost::interprocess::interprocess_exception(Win32::GetLastError());
        }
    }

    inline bool NamedSemaphore::try_wait()
    {
        const unsigned long res = Win32::WaitForSingleObject(m_semaphoreHandle, 0);
        if (res == Win32::wait_object_0)
        {
            return true;
        }
        else if (res == Win32::wait_timeout)
        {
            return false;
        }
        else
        {
            throw boost::interprocess::interprocess_exception(Win32::GetLastError());
        }
    }

    inline void NamedSemaphore::post()
    {
        const int res = Win32::ReleaseSemaphore(m_semaphoreHandle,1,NULL);
        if (!res) 
        {
            throw boost::interprocess::interprocess_exception(Win32::GetLastError());
        }
    }

#else
    inline NamedSemaphore::NamedSemaphore(const std::string& name):
        m_semaphore(boost::interprocess::open_or_create,
                    (name + Safir::Utilities::Internal::Expansion::GetSafirInstanceSuffix()).c_str(),
                    0)
    {

    }

    inline NamedSemaphore::~NamedSemaphore()
    {

    }

    inline void NamedSemaphore::remove(const std::string& name)
    {
        boost::interprocess::named_semaphore::remove((name + Safir::Utilities::Internal::Expansion::GetSafirInstanceSuffix()).c_str());
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
        Semaphore(unsigned int initialCount) : m_semaphore(initialCount) {}

        ~Semaphore() {}

        void wait();
        bool try_wait() {return m_semaphore.try_wait();}
        void post() {m_semaphore.post();}

    private:

        boost::interprocess::interprocess_semaphore m_semaphore;
    };

    inline void Semaphore::wait()
    {
#if defined(linux) || defined(__linux) || defined(__linux__)
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
#else
        m_semaphore.wait();
#endif
     }
}
}
}

#endif

