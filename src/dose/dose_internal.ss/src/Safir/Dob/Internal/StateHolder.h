/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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

#ifndef _dose_internal_state_holder_h
#define _dose_internal_state_holder_h

#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/LeveledLock.h>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class StateHolder
    {
    public:
        StateHolder():m_currentState(no_state_tag) {}

        explicit StateHolder(const DistributionData& state) : m_currentState(state) {}

        DistributionData GetState() const
        {
            ScopedStateHolderLock lck(m_lock);
            return m_currentState;
        }

        void SetState(const DistributionData& state)
        {
            ScopedStateHolderLock lck(m_lock);
            m_currentState = state;
        }

    private:
        DistributionData m_currentState; //most recent entity state

        //Locking Policy:
        //This class uses a non-recursive lock, since the lock is
        //only there to protect the assignment of one reference
        //counted variable.
        typedef Safir::Dob::Internal::LeveledLock<boost::interprocess::interprocess_mutex,
                                                  STATE_HOLDER_LOCK_LEVEL,
                                                  NO_MASTER_LEVEL_REQUIRED> StateHolderLock;
        mutable StateHolderLock m_lock;
        typedef boost::interprocess::scoped_lock<StateHolderLock> ScopedStateHolderLock;
    };
}
}
}

#endif

