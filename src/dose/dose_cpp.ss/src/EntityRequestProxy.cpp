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

#include <Safir/Dob/EntityRequestProxy.h>

#include "EntityRequestProxyImpl.h"

namespace Safir
{
namespace Dob
{
    EntityRequestProxy::EntityRequestProxy(Internal::EntityRequestProxyImpl* pImpl)
        : m_pImpl(pImpl)
    {
    }


    const Dob::Typesystem::TypeId EntityRequestProxy::GetTypeId() const
    {
        return m_pImpl->GetTypeId();
    }

    const Dob::Typesystem::InstanceId EntityRequestProxy::GetInstanceId() const
    {
        return m_pImpl->GetInstanceId();
    }

    const Dob::Typesystem::EntityId EntityRequestProxy::GetEntityId() const
    {
        return m_pImpl->GetEntityId();
    }

    const Dob::EntityPtr EntityRequestProxy::GetRequest() const
    {
        return m_pImpl->GetRequest();
    }

    const Dob::ConnectionInfoPtr EntityRequestProxy::GetSenderConnectionInfo() const
    {
        return m_pImpl->GetSenderConnectionInfo();
    }

    const Dob::Typesystem::HandlerId EntityRequestProxy::GetReceivingHandlerId() const
    {
        return m_pImpl->GetReceivingHandlerId();
    }

    const char * EntityRequestProxy::GetBlob() const
    {
        return m_pImpl->GetBlob();
    }

    const Dob::Typesystem::HandlerId EntityRequestProxy::GetReceiverWithStringRepresentation() const
    {
        return m_pImpl->GetReceiverWithStringRepresentation();
    }

}
}
