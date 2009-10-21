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

#include <Safir/Dob/ResponseProxy.h>

#include "ResponseProxyImpl.h"

namespace Safir
{
namespace Dob
{
    ResponseProxy::ResponseProxy(Internal::ResponseProxyImpl* pImpl) :
        m_pImpl(pImpl)
    {
    }

    bool ResponseProxy::IsSuccess() const
    {
        return m_pImpl->IsSuccess();
    }


    const Dob::Typesystem::TypeId ResponseProxy::GetTypeId() const
    {
        return m_pImpl->GetTypeId();
    }


    const Dob::ResponsePtr ResponseProxy::GetResponse() const
    {
        return m_pImpl->GetResponse();
    }

    const Dob::ConnectionInfoPtr ResponseProxy::GetResponseSenderConnectionInfo() const
    {
        return m_pImpl->GetResponseSenderConnectionInfo();
    }


    const char * ResponseProxy::GetBlob() const
    {
        return m_pImpl->GetBlob();
    }


    const Dob::RequestId ResponseProxy::GetRequestId() const
    {
        return m_pImpl->GetRequestId();
    }

    const Dob::Typesystem::TypeId ResponseProxy::GetRequestTypeId() const
    {
        return m_pImpl->GetRequestTypeId();
    }


    const Dob::Typesystem::InstanceId ResponseProxy::GetRequestInstanceId() const
    {
        return m_pImpl->GetRequestInstanceId();
    }


    const Dob::Typesystem::ObjectPtr ResponseProxy::GetRequest() const
    {
        return m_pImpl->GetRequest();
    }

    const char * ResponseProxy::GetRequestBlob() const
    {
        return m_pImpl->GetRequestBlob();
    }

    const Dob::Typesystem::HandlerId ResponseProxy::GetRequestHandlerId() const
    {
        return m_pImpl->GetRequestHandlerId();
    }
}
}
