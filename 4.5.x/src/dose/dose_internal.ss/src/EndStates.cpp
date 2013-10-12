/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#include <Safir/Dob/Internal/EndStates.h>
#include <Safir/Dob/Internal/StateContainer.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <vector>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    EndStates* EndStates::m_instance = NULL;

    EndStates& EndStates::Instance()
    {
        ENSURE(m_instance != NULL, << "EndStates::Instance was called before Initialize!!!");
        return *m_instance;
    }

    void EndStates::Initialize()
    {
        m_instance = GetSharedMemory().find_or_construct<EndStates>("ENDSTATES")(private_constructor_t());
    }


    EndStates::EndStates(private_constructor_t):
        m_lastTimestamp(0)
    {

    }

    EndStates::~EndStates()
    {

    }

    void EndStates::Add(const StateSharedPtr& state)
    {
        ScopedEndStatesLock lck(m_lock);

        const std::pair<StateTable::iterator,bool> insertResult =
            m_states.insert(std::make_pair(state,m_lastTimestamp));

        if (!insertResult.second)
        {
            lllout << "Duplicate endstate added to EndStates! Keeping the old one a little bit longer" << std::endl;
            insertResult.first->second = m_lastTimestamp;
        }
    }

    void EndStates::HandleTimeout()
    {
        std::vector<StateSharedPtr> ptrs;

        {
            ScopedEndStatesLock lck(m_lock);

            for (StateTable::iterator it = m_states.begin();
                 it != m_states.end(); )//iterator increment below
            {
                if (it->second < m_lastTimestamp)
                {
                    ptrs.push_back(it->first);
                    m_states.erase(it++);
                }
                else
                {
                    ++it;
                }
            }
        }  // here the EndStatesLock is released

        ++m_lastTimestamp;

    }  // and here we drop the shared pointers (without holding the EndStatesLock)
}
}
}
