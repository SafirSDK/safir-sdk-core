/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
*
* Created by: Anders Widén/ stawi
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

#ifndef __DOSE_LEVELED_LOCK_HELPER_H__
#define __DOSE_LEVELED_LOCK_HELPER_H__

#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/ProcessInfo.h>

#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100)
#endif

#include <boost/thread.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API LeveledLockHelper:
        public SharedMemoryObject,
        private boost::noncopyable
    {
    private:
        //This is to make sure that only Instance can call the constructor even though the constructor
        //itself has to be public (limitation of boost::interprocess)
        struct private_constructor_t {};
    public:
        /**
         * Get the singleton instance.
         * Note that this is not a singleton in the usual sense of the word,
         * it is the single instance of something that resides in shared memory.
         */
        static LeveledLockHelper& Instance();

        //The constructor and destructor have to be public for the boost::interprocess internals to be able to call
        //them, but we can make the constructor "fake-private" by making it require a private type as argument.
        explicit LeveledLockHelper(private_constructor_t);
        ~LeveledLockHelper();

        // Get the lowest held lock level for the calling thread. Returns 0 if no lock is taken.
        const unsigned short GetLowestHeldLevel() const;

        void AddLevel(const unsigned short level);

        void RemoveLevel(const unsigned short level);

        bool IsHeld(const unsigned short level) const;

        const unsigned int GetNumberOfHeldLocks() const;

    private:

        typedef std::pair<pid_t, boost::thread::id> Key;
        typedef PairContainers<Key, Containers<unsigned short>::multiset>::map LevelMap;
        mutable LevelMap m_levelMap;

        mutable boost::interprocess::interprocess_mutex m_levelMapLock;

        /**
         * This class is here to ensure that only the Instance method can get at the 
         * instance, so as to be sure that boost call_once is used correctly.
         * Also makes it easier to grep for singletons in the code, if all 
         * singletons use the same construction and helper-name.
         */
        struct SingletonHelper
        {
        private:
            friend LeveledLockHelper& LeveledLockHelper::Instance();

            static LeveledLockHelper& Instance();
            static boost::once_flag m_onceFlag;
        };

    };
}
}
}
#endif

