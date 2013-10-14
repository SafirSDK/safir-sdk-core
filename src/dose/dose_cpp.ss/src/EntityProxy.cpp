/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#include <Safir/Dob/EntityProxy.h>

#include "EntityProxyImpl.h"

namespace Safir
{
namespace Dob
{
    EntityProxy::EntityProxy(Internal::EntityProxyImpl* pImpl)
        : m_pImpl(pImpl)
    {
    }

    const Dob::Typesystem::TypeId
    EntityProxy::GetTypeId() const
    {
        return m_pImpl->GetTypeId();
    }

    const Dob::Typesystem::InstanceId
    EntityProxy::GetInstanceId() const
    {
        return m_pImpl->GetInstanceId();
    }

    const Dob::Typesystem::EntityId
    EntityProxy::GetEntityId() const
    {
        return m_pImpl->GetEntityId();
    }

    const Dob::EntityPtr
    EntityProxy::GetEntity() const
    {
        return m_pImpl->GetEntity();
    }

    const Dob::EntityPtr
    EntityProxy::GetEntityWithChangeInfo() const
    {
        return m_pImpl->GetEntityWithChangeInfo();
    }

    const Dob::Typesystem::HandlerId
    EntityProxy::GetOwner() const
    {
        return m_pImpl->GetOwner();
    }

    const Dob::ConnectionInfoPtr
    EntityProxy::GetOwnerConnectionInfo() const
    {
        return m_pImpl->GetOwnerConnectionInfo();
    }

    const char *
    EntityProxy::GetBlob() const
    {
        return m_pImpl->GetBlob();
    }

    const char *
    EntityProxy::GetBlobWithChangeInfo() const
    {
        return m_pImpl->GetBlobWithChangeInfo();
    }

    const Dob::PreviousEntityProxy
    EntityProxy::GetPrevious() const
    {
        return m_pImpl->GetPrevious();
    }

    const Dob::Typesystem::HandlerId
    EntityProxy::GetOwnerWithStringRepresentation() const
    {
        return m_pImpl->GetOwnerWithStringRepresentation();
    }

    const Dob::Typesystem::Int64
    EntityProxy::GetTimestamp() const
    {
        return m_pImpl->GetTimestamp();
    }

    const Dob::Typesystem::Int64
    EntityProxy::GetTimestamp(const Dob::Typesystem::MemberIndex member) const
    {
        return m_pImpl->GetTimestamp(member);
    }

}
}
