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

#include <boost/noncopyable.hpp>


namespace Safir
{
namespace Dob
{
namespace Internal
{
    class Semaphore:
        private boost::noncopyable
    {
    public:
        /** defaults to an unsignalled state */
        explicit Semaphore(const std::string& name);

        ~Semaphore();

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

    inline Semaphore::Semaphore(const std::string& name):
        m_semaphore(0, name.c_str())
    {

    }

    inline Semaphore::~Semaphore()
    {

    }

    inline void Semaphore::remove()
    {

    }

    inline void Semaphore::wait()
    {
        m_semaphore.acquire();
    }

    inline bool Semaphore::try_wait()
    {
        return 0 == m_semaphore.tryacquire();
    }

    inline void Semaphore::post()
    {
        m_semaphore.release();
    }

#else
    inline Semaphore::Semaphore(const std::string& name):
        m_semaphore(boost::interprocess::open_or_create, name.c_str(), 0),
        m_name(name)
    {

    }

    inline Semaphore::~Semaphore()
    {

    }

    inline void Semaphore::remove()
    {
        boost::interprocess::named_semaphore::remove(m_name.c_str());
    }

    inline void Semaphore::wait()
    {
        m_semaphore.wait();
    }

    inline bool Semaphore::try_wait()
    {
        return m_semaphore.try_wait();
    }

    inline void Semaphore::post()
    {
        m_semaphore.post();
    }
#endif

}
}
}

#endif

