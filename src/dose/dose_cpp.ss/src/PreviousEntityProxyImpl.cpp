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

#include "PreviousEntityProxyImpl.h"

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
    PreviousEntityProxyImpl::PreviousEntityProxyImpl(const char* const currentBlob,
                                                     const boost::shared_ptr<const char>& currentState,
                                                     const char* const previousBlob,
                                                     const boost::shared_ptr<const char>& previousState,
                                                     const bool timestampDiff):
        m_currentBlob(currentBlob),
        m_currentState(currentState),
        m_previousBlob(previousBlob),
        m_previousState(previousState),
        m_timestampDiff(timestampDiff)
    {

    }


    const Dob::Typesystem::TypeId
    PreviousEntityProxyImpl::GetTypeId() const
    {
        if (m_currentBlob == NULL)
        {
            Typesystem::Int64 typeId;
            bool success;
            DoseC_GetTypeId(m_currentState.get(), typeId, success);
            if (!success)
            {
                Typesystem::LibraryExceptions::Instance().Throw();
            }
            return Typesystem::TypeId(typeId);
        }
        else
        {
            return Safir::Dob::Typesystem::BlobOperations::GetTypeId(m_currentBlob);
        }
    }

    const Dob::Typesystem::InstanceId
    PreviousEntityProxyImpl::GetInstanceId() const
    {
        Typesystem::Int64 instanceId;
        bool success;
        DoseC_GetInstanceId(m_currentState.get(), instanceId, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return Typesystem::InstanceId(instanceId);
    }

    const Dob::Typesystem::EntityId
    PreviousEntityProxyImpl::GetEntityId() const
    {
        return Typesystem::EntityId(GetTypeId(),GetInstanceId());
    }

    const Dob::EntityPtr
    PreviousEntityProxyImpl::GetEntity() const
    {
        if (m_previousBlob == NULL)
        {
            std::wostringstream ostr;
            ostr << "Not possible to do GetEntity on this PreviousEntityProxy of entity "
                << GetEntityId() << "!";
            throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        return boost::static_pointer_cast<Safir::Dob::Entity>
            (Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(m_previousBlob));
    }

    const Dob::EntityPtr
    PreviousEntityProxyImpl::GetEntityWithChangeInfo() const
    {
        if (m_previousBlob == NULL)
        {
            std::wostringstream ostr;
            ostr << "Not possible to do GetEntityWithChangeInfo on this PreviousEntityProxy of entity "
                << GetEntityId() << "!";
            throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        if (m_previousBlobWithChangeInfo == NULL)
        {
            char * diffBlob;
            DoseC_BlobDeleter deleter;
            bool success;

            DoseC_Diff(m_previousState.get(),
                       m_currentState.get(),
                       false, //wantCurrent
                       m_timestampDiff,
                       diffBlob,
                       deleter,
                       success);

            if (!success)
            {
                Typesystem::LibraryExceptions::Instance().Throw();
            }

            m_previousBlobWithChangeInfo.reset(diffBlob,deleter);
        }

        return boost::static_pointer_cast<Safir::Dob::Entity>
            (Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(m_previousBlobWithChangeInfo.get()));
    }

    const Dob::Typesystem::HandlerId
    PreviousEntityProxyImpl::GetOwner() const
    {
        Typesystem::Int64 handlerId;
        bool success;
        DoseC_GetHandlerId(m_previousState.get(), handlerId, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return Typesystem::HandlerId(handlerId);
    }

    const Dob::ConnectionInfoPtr
    PreviousEntityProxyImpl::GetOwnerConnectionInfo() const
    {
        char* blob;
        DoseC_BlobDeleter blobDeleter;
        bool success;
        DoseC_GetConnectionInfo(m_previousState.get(), blob, blobDeleter, success);
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

    const char*
    PreviousEntityProxyImpl::GetBlob() const
    {
        if (m_previousBlob == NULL)
        {
            std::wostringstream ostr;
            ostr << "Not possible to do GetBlob on this PreviousEntityProxy of entity "
                << GetEntityId() << "!";
            throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }
        return m_previousBlob;
    }

    const char*
    PreviousEntityProxyImpl::GetBlobWithChangeInfo() const
    {
        if (m_previousBlob == NULL)
        {
            std::wostringstream ostr;
            ostr << "Not possible to do GetBlobWithChangeInfo on this PreviousEntityProxy of entity "
                << GetEntityId() << "!";
            throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
        }

        if (m_previousBlobWithChangeInfo == NULL)
        {
            char * diffBlob;
            DoseC_BlobDeleter deleter;
            bool success;

            DoseC_Diff(m_previousState.get(),
                       m_currentState.get(),
                       false, //wantCurrent
                       m_timestampDiff,
                       diffBlob,
                       deleter,
                       success);

            if (!success)
            {
                Typesystem::LibraryExceptions::Instance().Throw();
            }

            m_previousBlobWithChangeInfo.reset(diffBlob,deleter);
        }
        return m_previousBlobWithChangeInfo.get();
    }

    const Dob::Typesystem::HandlerId
    PreviousEntityProxyImpl::GetOwnerWithStringRepresentation() const
    {
        return GetOwner();
    }

    const Dob::Typesystem::Int64
    PreviousEntityProxyImpl::GetTimestamp() const
    {
        Typesystem::Int64 timestamp;
        bool success;
        DoseC_GetTopTimestamp(m_previousState.get(), timestamp, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return timestamp;
    }

    const Dob::Typesystem::Int64
    PreviousEntityProxyImpl::GetTimestamp(const Dob::Typesystem::MemberIndex member) const
    {
        Typesystem::Int64 timestamp;
        bool success;
        DoseC_GetMemberTimestamp(m_previousState.get(), member, timestamp, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return timestamp;
    }

}
}
}
