/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#include <Safir/Dob/Internal/SubscriptionQueue.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Internal/Atomic.h>
#include <Safir/Dob/Internal/State.h>
#include <Safir/Dob/Internal/Subscription.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Internal/ScopeExit.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{

    //Returns false if queue is full
    void SubscriptionQueue::push(const SubscriptionPtr& subscription)
    {
        ScopedSubscriptionQueueLock lck(m_lock);

        m_queue.push_back(subscription);

    }

    void SubscriptionQueue::SpliceBack(Queue& queue)
    {
        if (!queue.empty())
        {
            ScopedSubscriptionQueueLock lck(m_lock);

            m_queue.splice(m_queue.begin(),queue);
        }
    }

    void SubscriptionQueue::Dispatch(const DispatchFunc& dispatchFunc)
    {
        Queue toDispatch;

        //This will cause any undispatched parts of the queue to be spliced back
        //onto the queue if exitDispatch is used or there is an exception.
        ScopeExit spliceBackGuard(boost::bind(&SubscriptionQueue::SpliceBack,
                                              this,
                                              boost::ref(toDispatch)));

        //Algorithm: Swap the list into a local variable, to hold the lock as little as possible.
        //Then dispatch the subscriptions, and then retake the lock and put any remaining messages back.
        {
            ScopedSubscriptionQueueLock lck(m_lock);
            m_queue.swap(toDispatch);
        }

        bool exitDispatch;
        bool dontRemove;

        for (Queue::iterator it = toDispatch.begin();
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
    }
}
}
}

