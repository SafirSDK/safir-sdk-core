/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#include <Safir/Dob/Internal/MessageQueue.h>
#include <Safir/Dob/Internal/StateDeleter.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Internal/TimeRegistration.h>
#include <Safir/Dob/Internal/Atomic.h>
#include <Safir/Dob/Internal/ScopeExit.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    MessageQueue::MessageQueue(const size_t capacity):
        m_capacity(static_cast<boost::uint32_t>(capacity)),
        m_size(0),
        m_noPushed(0),
        m_noOverflows(0),
        m_simulateFull(false)
    {

    }


    //Returns false if queue is full
    bool MessageQueue::push(const DistributionData & msg)
    {
        ScopedMessageQueueLock lck(m_lock);
        if ((m_size < m_capacity) && m_simulateFull == 0) //not full
        {
            m_data.push_back(msg);
            ++m_size;
            ++m_noPushed;
            return true;
        }
        else
        {
            ++m_noOverflows;
            return false;
        }
    }

    void MessageQueue::FinishDispatch(QueueData& queue, const size_t& numDispatched, bool& isNoLongerFull)
    {
        ScopedMessageQueueLock lck(m_lock);

        if (m_size == m_capacity && numDispatched > 0)
        {
            isNoLongerFull = true;
        }

        m_size -= numDispatched;

        if (!queue.empty())
        {
            m_data.splice(m_data.begin(),queue);
        }
    }

    void MessageQueue::Dispatch(const DispatchFunc & dispatchFunc,
                                const ActionFunc & postFullAction)
    {
        //This is set to true by the guard if the queue has gone from a full to a not-full state.
        bool isNoLongerFull = false;

        {
            QueueData toDispatch;
            size_t numDispatched = 0;

            //This will cause the FinishDispatch method to be called when the scope exits
            //normally or if exitDispatch is used or there is an exception.
            ScopeExit spliceBackGuard(boost::bind(&MessageQueue::FinishDispatch,
                                                  this,
                                                  boost::ref(toDispatch),
                                                  boost::cref(numDispatched),
                                                  boost::ref(isNoLongerFull)));

            //Algorith: Swap the list into a local variable, to hold the lock as little as possible.
            //Then dispatch the messages, and then retake the lock and put any remaining messages back.
            {
                ScopedMessageQueueLock lck(m_lock);
                m_data.swap(toDispatch);
            }

            const size_t oldSize = toDispatch.size();

            if (oldSize == 0)
            {
                return;
            }

            bool exitDispatch;
            bool dontRemove;

            for (QueueData::iterator it = toDispatch.begin();
                 it != toDispatch.end();) //note the missing ++it, see below for explanation
            {
                dispatchFunc(*it,exitDispatch,dontRemove);

                //since list.erase returns an iterator to the next element we handle the
                //iterator incrementation here rather than in the for statement
                if (!dontRemove)
                {
                    it = toDispatch.erase(it);
                }
                else
                {
                    ++it;
                }

                if (exitDispatch)
                {
                    break;
                }
            }

            numDispatched = oldSize - toDispatch.size();

            //guard gets executed here
        }

        //Execute the postaction if the queue was full.
        if (isNoLongerFull && postFullAction != NULL)
        {
            postFullAction();
        }
    }
}
}
}
