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

#ifndef __DOSE_PENDING_REGISTRATION_H__
#define __DOSE_PENDING_REGISTRATION_H__
#include <Safir/Dob/Internal/ShmWrappers.h>
#include <Safir/Dob/InstanceIdPolicy.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    struct PendingRegistration
    {
        PendingRegistration():
            typeId(0),
            handlerId(Dob::Typesystem::HandlerId(0)),
            instanceIdPolicy(Dob::InstanceIdPolicy::RequestorDecidesInstanceId),
            isInjectionHandler(false),
            consumer(NULL,0L),
            accepted(false),
            remove(false),
            id(0)
        {}

        // Pending entity registration (including instance is poliy)
        PendingRegistration(const Dob::Typesystem::TypeId type,
                            const Dob::Typesystem::HandlerId& handler,
                            const Dob::InstanceIdPolicy::Enumeration instIdPolicy,
                            const bool injectionHandler,
                            const ConsumerId& cons):
            typeId(type),
            handlerId(handler),
            instanceIdPolicy(instIdPolicy),
            isInjectionHandler(injectionHandler),
            consumer(cons),
            accepted(false),
            remove(false),
            id(0)
        {}

        // Pending service registration
        PendingRegistration(const Dob::Typesystem::TypeId type,
                            const Dob::Typesystem::HandlerId& handler,
                            const ConsumerId& cons):
            typeId(type),
            handlerId(handler),
            instanceIdPolicy(Dob::InstanceIdPolicy::RequestorDecidesInstanceId), // Not used for service registrations
            consumer(cons),
            accepted(false),
            remove(false),
            id(0)
        {}

        PendingRegistration(const PendingRegistration& other):
            typeId(other.typeId),
            handlerId(other.handlerId),
            instanceIdPolicy(other.instanceIdPolicy),
            isInjectionHandler(other.isInjectionHandler),
            consumer(other.consumer),
            accepted(other.accepted),
            remove(other.remove),
            id(other.id) {}

        PendingRegistration& operator=(const PendingRegistration& other)
        {
            typeId = other.typeId;
            handlerId = other.handlerId;
            instanceIdPolicy = other.instanceIdPolicy;
            isInjectionHandler = other.isInjectionHandler;
            consumer = other.consumer;
            accepted = other.accepted;
            remove = other.remove;
            id = other.id;
            return *this;
        }

        Dob::Typesystem::TypeId typeId;
        ShmHandlerId handlerId;
        InstanceIdPolicy::Enumeration instanceIdPolicy;
        bool isInjectionHandler;
        ConsumerId consumer;

        bool accepted;
        bool remove;
        long id;

        //some functions for use with std::algorithms
        bool IsRemoved() const {return remove;}
    };

    typedef std::vector<PendingRegistration> PendingRegistrationVector;

}
}
}

#endif
