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

#include <Safir/Dob/Internal/LamportClocks.h>
#include <iostream>
#include <boost/atomic.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{

    std::wostream & operator << (std::wostream & out, const LamportTimestamp & timestamp)
    {
        return out << timestamp.m_clock << ":" << timestamp.m_nodeId;
    }


    LamportClock::LamportClock(const int64_t nodeId):
        m_currentClock(0),
        m_nodeId(nodeId)
    {

    }

    void LamportClock::UpdateCurrentTimestamp(const LamportTimestamp& timestamp)
    {
        const auto otherClock = timestamp.GetClock();
        //algorithm:
        //While the current clock value is smaller than the other value
        //we try to swap them. The swap will fail if the current clock
        //changes between the read and swap, and then we will try again.
        for(;;)
        {
            auto currentClock = m_currentClock.load();
            if (otherClock > currentClock)
            {
                const bool res = m_currentClock.compare_exchange_strong(currentClock,otherClock);
                if (res)
                {
                    return;
                }
            }
            else
            {
                m_currentClock++;
                return;
            }
        }
    }

    const LamportTimestamp LamportClock::GetNewTimestamp()
    {
        return LamportTimestamp(++m_currentClock, m_nodeId);
    }


}
}
}

