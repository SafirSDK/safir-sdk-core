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

#include <Safir/Dob/ServiceRequestProxy.h>

#include "ServiceRequestProxyImpl.h"

namespace Safir
{
namespace Dob
{
    ServiceRequestProxy::ServiceRequestProxy(Internal::ServiceRequestProxyImpl* pImpl)
        : m_pImpl(pImpl)
    {
    }

    const Dob::Typesystem::TypeId ServiceRequestProxy::GetTypeId() const
    {
        return m_pImpl->GetTypeId();
    }

    const Dob::ServicePtr ServiceRequestProxy::GetRequest() const
    {
        return m_pImpl->GetRequest();
    }

    const Dob::ConnectionInfoPtr ServiceRequestProxy::GetSenderConnectionInfo() const
    {
        return m_pImpl->GetSenderConnectionInfo();
    }

    const Dob::Typesystem::HandlerId ServiceRequestProxy::GetReceivingHandlerId() const
    {
        return m_pImpl->GetReceivingHandlerId();
    }

    const char * ServiceRequestProxy::GetBlob() const
    {
        return m_pImpl->GetBlob();
    }

    const Dob::Typesystem::HandlerId ServiceRequestProxy::GetReceiverWithStringRepresentation() const
    {
        return m_pImpl->GetReceiverWithStringRepresentation();
    }

}
}
