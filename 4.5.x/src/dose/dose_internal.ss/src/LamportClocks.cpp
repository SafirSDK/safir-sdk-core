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
#include <Safir/Dob/Internal/Atomic.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <iostream>
namespace Safir
{
namespace Dob
{
namespace Internal
{
    const boost::uint64_t NODE_NUMBER = ThisNodeParameters::NodeNumber();

    LamportTimestamp::LamportTimestamp(const boost::uint32_t clock):
        m_timestamp(clock)
    {
        //shift 6 bits and put the node number there.
        m_timestamp = (static_cast<boost::uint64_t>(clock) << 6)| NODE_NUMBER;
    }

    boost::uint32_t LamportTimestamp::GetClock() const
    {
        return static_cast<boost::uint32_t>(m_timestamp >> 6);
    }


    std::wostream & operator << (std::wostream & out, const LamportTimestamp & timestamp)
    {
        return out << timestamp.GetClock() << ":" << (timestamp.m_timestamp & 0x3F);
    }



    LamportClock::LamportClock():
        m_currentClock(0)
    {

    }

    LamportClock::~LamportClock()
    {

    }


    const LamportTimestamp LamportClock::GetCurrentTimestamp() const
    {
        return LamportTimestamp(m_currentClock.value());
    }


    void LamportClock::UpdateCurrentTimestamp(const LamportTimestamp & timestamp)
    {
        boost::uint32_t otherClock = timestamp.GetClock();

        //algorithm:
        //While the current clock value is smaller than the other value
        //we try to swap them. The swap will fail if the current clock
        //changes between the read and swap, and then we will try again.
        for(;;)
        {
            const boost::uint32_t currentClock = m_currentClock.value();
            if (otherClock > currentClock)
            {
                m_currentClock.compare_exchange(otherClock,currentClock);
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
        return LamportTimestamp(m_currentClock++);
    }


}
}
}

