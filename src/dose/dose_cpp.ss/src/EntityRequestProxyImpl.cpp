/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#include "EntityRequestProxyImpl.h"

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
    EntityRequestProxyImpl::EntityRequestProxyImpl(const char * const requestBlob,
                                                   const char * const state):
        m_requestBlob(requestBlob),
        m_state(state)
    {

    }

    const Dob::Typesystem::TypeId EntityRequestProxyImpl::GetTypeId() const
    {
        if (m_requestBlob == NULL)
        {
            Typesystem::Int64 typeId;
            bool success;
            DoseC_GetTypeId(m_state, typeId, success);
            if (!success)
            {
                Typesystem::LibraryExceptions::Instance().Throw();
            }
            return Typesystem::TypeId(typeId);
        }
        else
        {
            return Safir::Dob::Typesystem::BlobOperations::GetTypeId(m_requestBlob);
        }
    }

    const Dob::Typesystem::InstanceId EntityRequestProxyImpl::GetInstanceId() const
    {
        Typesystem::Int64 instanceId;
        bool success;
        DoseC_GetInstanceId(m_state, instanceId, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return Typesystem::InstanceId(instanceId);
    }

    const Dob::Typesystem::EntityId EntityRequestProxyImpl::GetEntityId() const
    {
        return Typesystem::EntityId(GetTypeId(),GetInstanceId());
    }

    const Dob::EntityPtr EntityRequestProxyImpl::GetRequest() const
    {
        ENSURE(m_requestBlob != NULL, << "Not possible to GetRequest on DeleteRequests!");
        return boost::static_pointer_cast<Safir::Dob::Entity>
            (Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(m_requestBlob));
    }

    const Dob::ConnectionInfoPtr EntityRequestProxyImpl::GetSenderConnectionInfo() const
    {
        char* blob;
        DoseC_BlobDeleter blobDeleter;
        bool success;
        DoseC_GetConnectionInfo(m_state, blob, blobDeleter, success);
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

    const Dob::Typesystem::HandlerId EntityRequestProxyImpl::GetReceivingHandlerId() const
    {
        Typesystem::Int64 handlerId;
        bool success;
        DoseC_GetHandlerId(m_state, handlerId, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return Typesystem::HandlerId(handlerId);
    }

    const char * EntityRequestProxyImpl::GetBlob() const
    {
        ENSURE(m_requestBlob != NULL, << "No blob available on DeleteRequests!");
        return m_requestBlob;
    }


    const Dob::Typesystem::HandlerId EntityRequestProxyImpl::GetReceiverWithStringRepresentation() const
    {
        return GetReceivingHandlerId();
    }

}
}
}
