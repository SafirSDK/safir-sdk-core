/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Anders Widén / stawi
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

#ifndef __DOSE_UPGRADEABLE_PTR_H__
#define __DOSE_UPGRADEABLE_PTR_H__

#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <boost/interprocess/sync/interprocess_recursive_mutex.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>


namespace Safir
{
namespace Dob
{
namespace Internal
{
    template <class T,
              class WeakPtrT = typename SharedMemoryObject::SmartPointers<T>::weak_ptr,
              class SharedPtrT = typename SharedMemoryObject::SmartPointers<T>::shared_ptr>
    class UpgradeablePtr : public SharedMemoryObject
    {
    public:

        typedef SharedPtrT   SharedPtr;

        UpgradeablePtr(T* rawPtr)
               : m_sharedPtr(rawPtr),
                 m_weakPtr(m_sharedPtr)
        {
            m_lockPtr = LockPtr(GetSharedMemory().template construct<boost::interprocess::interprocess_recursive_mutex>
                                (boost::interprocess::anonymous_instance)());
        }

        explicit UpgradeablePtr(const SharedPtrT sharedPtr)
                    : m_sharedPtr(sharedPtr),
                      m_weakPtr(m_sharedPtr)
        {
            m_lockPtr = LockPtr(GetSharedMemory().template construct<boost::interprocess::interprocess_recursive_mutex>
                                (boost::interprocess::anonymous_instance)());
        }

        void Downgrade()
        {
            ScopedLock lck(*m_lockPtr);

            m_sharedPtr.reset();
        }

        bool IsDowngraded() const
        {
            ScopedLock lck(*m_lockPtr);

            return m_sharedPtr == NULL;
        }

        // Might return NULL!
        // The bool component of the returned pair is true if and only if the SharedPtrT component has been upgraded (obtained from a the weak pointer).
        std::pair<SharedPtrT, bool> UpgradeAndGet()
        {
            ScopedLock lck(*m_lockPtr);

            bool upgraded = false;

            if (m_sharedPtr == NULL)
            {
                m_sharedPtr = m_weakPtr.lock();
                upgraded = true;
            }
            return std::make_pair(m_sharedPtr, upgraded);
        }

        // Might return NULL!
        SharedPtrT Get() const
        {
            ScopedLock lck(*m_lockPtr);

            return m_sharedPtr;
        }

        // The bool component of the returned pair is true if and only if the SharedPtrT component has been obtained from a the weak pointer.
        std::pair<SharedPtrT, bool> GetIncludeWeak() const
        {
            ScopedLock lck(*m_lockPtr);

            bool obtainedFromWeak = false;

            if (m_sharedPtr == NULL)
            {
                obtainedFromWeak = true;
            }
            return std::make_pair(m_weakPtr.lock(), obtainedFromWeak);
        }

    private:
        SharedPtrT  m_sharedPtr;
        WeakPtrT    m_weakPtr;

        typedef SmartPointers<boost::interprocess::interprocess_recursive_mutex>::shared_ptr LockPtr;

        LockPtr m_lockPtr;

        typedef boost::interprocess::scoped_lock<boost::interprocess::interprocess_recursive_mutex> ScopedLock;
    };
}
}
}
//#pragma warning (pop)
#endif
