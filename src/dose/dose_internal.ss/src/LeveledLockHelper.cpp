/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
#include <Safir/Dob/Internal/LeveledLockHelper.h>
#include <Safir/Utilities/ProcessInfo.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{

    // The held levels for a thread is in the current implementation saved in shared memory. This makes it possible
    // to add methods that fetch lock-related information for any process/thread. If the fetching of such information
    // is not anticipated there should be no problem to store the data in process internal memory.

    boost::once_flag LeveledLockHelper::SingletonHelper::m_onceFlag = BOOST_ONCE_INIT;

    LeveledLockHelper & LeveledLockHelper::SingletonHelper::Instance()
    {
        static LeveledLockHelper* instance = GetSharedMemory().find_or_construct<LeveledLockHelper>("LEVELEDLOCKHELPER")(private_constructor_t());
        return *instance;
    }

    LeveledLockHelper & LeveledLockHelper::Instance()
    {
        boost::call_once(SingletonHelper::m_onceFlag,boost::bind(SingletonHelper::Instance));
        return SingletonHelper::Instance();
    }

    LeveledLockHelper::LeveledLockHelper(private_constructor_t)
    {
    }

    LeveledLockHelper::~LeveledLockHelper()
    {
    }

    const unsigned short
    LeveledLockHelper::GetLowestHeldLevel() const
    {
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_levelMapLock);

        Key key = std::make_pair(Safir::Utilities::ProcessInfo::GetPid(), boost::this_thread::get_id());

        LevelMap::const_iterator findIt = m_levelMap.find(key);

        if (findIt != m_levelMap.end())
        {
            if (findIt->second.empty())
            {
                return 0;
            }
            else
            {
                return *findIt->second.begin();
            }
        }
        else
        {
            return 0;
        }
    }

    void
    LeveledLockHelper::AddLevel(const unsigned short level)
    {
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_levelMapLock);

        Key key = std::make_pair(Safir::Utilities::ProcessInfo::GetPid(), boost::this_thread::get_id());

        LevelMap::iterator findIt = m_levelMap.find(key);

        if (findIt != m_levelMap.end())
        {
            findIt->second.insert(level);
        }
        else
        {
            Containers<unsigned short>::multiset ms;
            ms.insert(level);
            m_levelMap.insert(std::make_pair(key, ms)); 
        }
    }

    void
    LeveledLockHelper::RemoveLevel(const unsigned short level)
    {
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_levelMapLock);

        Key key = std::make_pair(Safir::Utilities::ProcessInfo::GetPid(), boost::this_thread::get_id());

        LevelMap::iterator findIt = m_levelMap.find(key);

        ENSURE(findIt != m_levelMap.end(), << "A thread that has never added a level tries to remove one!");

        findIt->second.erase(findIt->second.find(level));
    }

    bool
    LeveledLockHelper::IsHeld(const unsigned short level) const
    {
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_levelMapLock);

        Key key = std::make_pair(Safir::Utilities::ProcessInfo::GetPid(), boost::this_thread::get_id());

        LevelMap::iterator mapIt = m_levelMap.find(key);

        if (mapIt == m_levelMap.end())
        {
            return false;
        }

        return mapIt->second.find(level) != mapIt->second.end();
    }

    const unsigned int LeveledLockHelper::GetNumberOfHeldLocks() const
    {
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_levelMapLock);

        Key key = std::make_pair(Safir::Utilities::ProcessInfo::GetPid(), boost::this_thread::get_id());

        LevelMap::iterator mapIt = m_levelMap.find(key);

        if (mapIt == m_levelMap.end() || mapIt->second.empty())
        {
            return 0;
        }
        else
        {
            return static_cast<int>(mapIt->second.size());
        }
    }
}
}
}
