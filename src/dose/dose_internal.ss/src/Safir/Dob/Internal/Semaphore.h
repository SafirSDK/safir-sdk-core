/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagstr�m / stlrha
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
#ifdef DOSE_SEMAPHORE_WIN32
        void* m_semaphoreHandle;
#else
        boost::interprocess::named_semaphore m_semaphore;
        const std::string m_name;
#endif
    };


    class Semaphore:
        private boost::noncopyable
    {
    public:
        Semaphore(unsigned int initialCount) : m_semaphore(initialCount) {};

        ~Semaphore() {};

        void wait();
        bool try_wait() {return m_semaphore.try_wait();};
        void post() {m_semaphore.post();};

    private:
        boost::interprocess::interprocess_semaphore m_semaphore;
    };

}
}
}

#endif

