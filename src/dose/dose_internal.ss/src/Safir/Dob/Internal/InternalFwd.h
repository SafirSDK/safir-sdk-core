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

#ifndef __DOSE_INTERNAL_FWD_H__
#define __DOSE_INTERNAL_FWD_H__

#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/UpgradeablePtr.h>
#include <Safir/Dob/Internal/SharedLock.h>
#include <Safir/Dob/Typesystem/HandlerId.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    struct ConnectionId;

    class DistributionData;

    class Connection;
    typedef SharedMemoryObject::SmartPointers<Connection>::shared_ptr ConnectionPtr;
    typedef SharedMemoryObject::SmartPointers<const Connection>::shared_ptr ConnectionConstPtr;
    
    class Instance;
    typedef boost::interprocess::offset_ptr<Instance> InstancePtr;

    class EntityType;
    typedef boost::interprocess::offset_ptr<EntityType> EntityTypePtr;

    class ServiceType;
    typedef boost::interprocess::offset_ptr<ServiceType> ServiceTypePtr;

    class MessageType;
    typedef boost::interprocess::offset_ptr<MessageType> MessageTypePtr;

    class State;
    typedef SharedMemoryObject::SmartPointers<State>::shared_ptr StatePtr;

    class Subscription;
    typedef UpgradeablePtr<Subscription>            UpgradeableSubscriptionPtr;
    typedef UpgradeableSubscriptionPtr::SharedPtr   SubscriptionPtr;

    class StateDeleter;
    typedef boost::interprocess::weak_ptr
    <
        State,
        SharedMemoryObject::my_allocator<State>,
        StateDeleter
    >
    StateWeakPtr;

    typedef boost::interprocess::shared_ptr
    <
        State,
        SharedMemoryObject::my_allocator<State>,
        StateDeleter
    >
    StateSharedPtr;

    struct LockedStateResult
    {
        StateSharedPtr first;
        SharedLock second;
        LockedStateResult() : first(), second() {}
        LockedStateResult(const StateSharedPtr& f, const SharedLock& s) :
            first(f), second(s) {}
        LockedStateResult(const LockedStateResult& other):
            first(other.first), second(other.second) {}
        LockedStateResult& operator=(const LockedStateResult& other)
        {
            first = other.first;
            second = other.second;
            return *this;
        }
    };

    class RequestInQueue;
    class RequestOutQueue;
}
}
}
#endif
