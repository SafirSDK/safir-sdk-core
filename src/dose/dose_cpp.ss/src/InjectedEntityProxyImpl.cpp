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

#include "InjectedEntityProxyImpl.h"

#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Dob/Typesystem/BlobOperations.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Internal/Interface.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    InjectedEntityProxyImpl::InjectedEntityProxyImpl(const char * const injectionBlob,
                                                     const char * const injectionState,
                                                     const char * const currentBlob,
                                                     const char * const currentState):
        m_injectionBlob(injectionBlob),
        m_injectionState(injectionState),
        m_currentBlob(currentBlob),
        m_currentState(currentState)
    {
    }

    const Dob::Typesystem::TypeId
    InjectedEntityProxyImpl::GetTypeId() const
    {
        if (m_injectionBlob == NULL)
        {
            Typesystem::Int64 typeId;
            bool success;
            DoseC_GetTypeId(m_injectionState, typeId, success);
            if (!success)
            {
                Typesystem::LibraryExceptions::Instance().Throw();
            }
            return Typesystem::TypeId(typeId);
        }
        else
        {
            return Safir::Dob::Typesystem::BlobOperations::GetTypeId(m_injectionBlob);
        }
    }

    const Dob::Typesystem::InstanceId
    InjectedEntityProxyImpl::GetInstanceId() const
    {
        Typesystem::Int64 instanceId;
        bool success;
        DoseC_GetInstanceId(m_injectionState, instanceId, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return Typesystem::InstanceId(instanceId);
    }

    const Dob::Typesystem::EntityId
    InjectedEntityProxyImpl::GetEntityId() const
    {
        return Typesystem::EntityId(GetTypeId(),GetInstanceId());
    }

    const Dob::EntityPtr
    InjectedEntityProxyImpl::GetInjection() const
    {
        ENSURE(m_injectionBlob != NULL, << "Not possible to do GetInjection on InjectDeletes!");
        return boost::static_pointer_cast<Safir::Dob::Entity>
            (Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(m_injectionBlob));
    }

    const char *
    InjectedEntityProxyImpl::GetInjectionBlob() const
    {
        ENSURE(m_injectionBlob != NULL, << "No blob available on InjectDeletes!");
        return m_injectionBlob;
    }

    const Dob::EntityPtr
    InjectedEntityProxyImpl::GetCurrent() const
    {
        ENSURE(m_currentBlob != NULL, << "Not possible to do GetCurrent on InjectNews!");
        return boost::static_pointer_cast<Safir::Dob::Entity>
            (Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(m_currentBlob));
    }
}
}
}
