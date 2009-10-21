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

#ifndef _dose_internal_defs_h
#define _dose_internal_defs_h

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Defs.h>
#include <Safir/Dob/Internal/WrapAroundCounter.h>
#include <boost/function.hpp>
#include <boost/tuple/tuple.hpp>
#include <Safir/Dob/Internal/LamportClocks.h>
#include <Safir/Dob/Internal/ConsumerId.h>


namespace Safir
{
namespace Dob
{
namespace Internal
{
    typedef LamportTimestamp RegisterTime;

    struct RegistrationInfo
    {
        Typesystem::TypeId      typeId;
        Typesystem::HandlerId   handlerId;
        ConsumerId              consumer;
    };

    typedef std::vector<RegistrationInfo> RegistrationVector;

    typedef std::vector<Dob::Typesystem::EntityId> EntityIdVector;
    typedef std::vector<Dob::Typesystem::TypeId> TypeIdVector;

    typedef WrapAroundCounter<Typesystem::Int32> InternalRequestId;

    typedef WrapAroundCounter<Typesystem::Int32> ResponseId;

    //-------------------------------------------------
    //  Connection name types
    //-------------------------------------------------
    const size_t MAX_CONNECTION_NAME_LENGTH = 100;

    enum SubscriptionType
    {
        EntityRegistrationSubscription,
        ServiceRegistrationSubscription,
        MessageSubscription,
        EntitySubscription,
        InjectionSubscription,
        NumberOfSubscriptionTypes
    };

    enum RemoteSetResult {RemoteSetAccepted,
                          RemoteSetDiscarded,
                          RemoteSetNeedRegistration};
}
}
}

#endif //_dose_internal_defs_h
