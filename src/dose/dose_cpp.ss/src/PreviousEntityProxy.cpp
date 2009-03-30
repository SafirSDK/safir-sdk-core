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

#include <Safir/Dob/PreviousEntityProxy.h>

#include "PreviousEntityProxyImpl.h"

namespace Safir
{
namespace Dob
{
    PreviousEntityProxy::PreviousEntityProxy(Internal::PreviousEntityProxyImpl* pImpl)
        : m_pImpl(pImpl)
    {
    }

    const Dob::Typesystem::TypeId
    PreviousEntityProxy::GetTypeId() const
    {
        return m_pImpl->GetTypeId();
    }

    const Dob::Typesystem::InstanceId
    PreviousEntityProxy::GetInstanceId() const
    {
        return m_pImpl->GetInstanceId();
    }

    const Dob::Typesystem::EntityId
    PreviousEntityProxy::GetEntityId() const
    {
        return m_pImpl->GetEntityId();
    }

    const Dob::EntityPtr
    PreviousEntityProxy::GetEntity() const
    {
        return m_pImpl->GetEntity();
    }

    const Dob::EntityPtr
    PreviousEntityProxy::GetEntityWithChangeInfo() const
    {
        return m_pImpl->GetEntityWithChangeInfo();
    }

    const Dob::Typesystem::HandlerId
    PreviousEntityProxy::GetOwner() const
    {
        return m_pImpl->GetOwner();
    }

    const Dob::ConnectionInfoPtr
    PreviousEntityProxy::GetOwnerConnectionInfo() const
    {
        return m_pImpl->GetOwnerConnectionInfo();
    }

    const char *
    PreviousEntityProxy::GetBlob() const
    {
        return m_pImpl->GetBlob();
    }

    const char *
    PreviousEntityProxy::GetBlobWithChangeInfo() const
    {
        return m_pImpl->GetBlobWithChangeInfo();
    }

    const Dob::Typesystem::HandlerId
    PreviousEntityProxy::GetOwnerWithStringRepresentation() const
    {
        return m_pImpl->GetOwnerWithStringRepresentation();
    }

    const Dob::Typesystem::Int64
    PreviousEntityProxy::GetTimestamp() const
    {
        return m_pImpl->GetTimestamp();
    }

    const Dob::Typesystem::Int64
    PreviousEntityProxy::GetTimestamp(const Dob::Typesystem::MemberIndex member) const
    {
        return m_pImpl->GetTimestamp(member);
    }

}
}
