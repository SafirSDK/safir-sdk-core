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

#include "ResponseProxyImpl.h"

#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Dob/Typesystem/BlobOperations.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Internal/Interface.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    ResponseProxyImpl::ResponseProxyImpl(const long requestId,
                                         const char * const responseBlob,
                                         const char * const responseState,
                                         const char * const requestBlob,
                                         const char * const requestState):
        m_requestId(requestId),
        m_responseBlob(responseBlob),
        m_responseState(responseState),
        m_requestBlob(requestBlob),
        m_requestState(requestState)
    {
    }

    bool ResponseProxyImpl::IsSuccess() const
    {
        return Safir::Dob::Typesystem::Operations::IsOfType(GetTypeId(),Safir::Dob::SuccessResponse::ClassTypeId);
    }


    const Dob::Typesystem::TypeId ResponseProxyImpl::GetTypeId() const
    {
        return Typesystem::BlobOperations::GetTypeId(m_responseBlob);
    }


    const Dob::ResponsePtr ResponseProxyImpl::GetResponse() const
    {
        return boost::static_pointer_cast<Safir::Dob::Response>
            (Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(m_responseBlob));
    }

    const Dob::ConnectionInfoPtr ResponseProxyImpl::GetResponseSenderConnectionInfo() const
    {
        char* blob;
        DoseC_BlobDeleter blobDeleter;
        bool success;
        DoseC_GetConnectionInfo(m_responseState, blob, blobDeleter, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }

        ENSURE (blob != NULL, << "Got NULL blob from DoseC_GetConnectionInfo");

        boost::shared_ptr<char> autoDeleter(blob,blobDeleter);

        ConnectionInfoPtr connInfo = boost::static_pointer_cast<ConnectionInfo>
            (Typesystem::ObjectFactory::Instance().CreateObject(blob));

        return connInfo;
    }



    const Dob::Typesystem::TypeId ResponseProxyImpl::GetRequestTypeId() const
    {
        bool success;
        Typesystem::TypeId typeId;
        DoseC_GetTypeId(m_requestState, typeId, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return typeId;
    }


    const Dob::Typesystem::InstanceId ResponseProxyImpl::GetRequestInstanceId() const
    {
        Typesystem::Int64 instanceId;
        bool success;
        DoseC_GetInstanceId(m_requestState, instanceId, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return Typesystem::InstanceId(instanceId);
    }


    const Dob::Typesystem::ObjectPtr ResponseProxyImpl::GetRequest() const
    {
        if (m_requestBlob == NULL)
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Cannot get Request on ResponseProxies for DeleteRequests",__WFILE__,__LINE__);
        }
        return Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(m_requestBlob);
    }

    const char * ResponseProxyImpl::GetRequestBlob() const
    {
        return m_requestBlob;
    }

    const Dob::Typesystem::HandlerId ResponseProxyImpl::GetRequestHandlerId() const
    {
        Typesystem::Int64 handlerId;
        bool success;
        DoseC_GetHandlerId(m_requestState, handlerId, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return Typesystem::HandlerId(handlerId);
    }

}
}
}
