/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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

#ifndef _SAFIR_DOB_ENTITY_PROXY_IMPL_H
#define _SAFIR_DOB_ENTITY_PROXY_IMPL_H

#include <Safir/Dob/ConnectionInfo.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Defs.h>
#include <Safir/Dob/EntityProxy.h>
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class EntityProxyImpl:
        private boost::noncopyable
    {
    public:
        EntityProxyImpl(const char* const currentBlob,
                        const char* const currentState,
                        const char* const previousBlob,
                        const char* const previousState,
                        const bool addReference,
                        const bool timestampDiff);

        Dob::Typesystem::TypeId GetTypeId() const;

        const Dob::Typesystem::InstanceId GetInstanceId() const;

        const Dob::Typesystem::EntityId GetEntityId() const;

        const Dob::EntityPtr GetEntity() const;

        const Dob::EntityPtr GetEntityWithChangeInfo() const;

        const Dob::Typesystem::HandlerId GetOwner() const;

        const Dob::ConnectionInfoPtr GetOwnerConnectionInfo() const;

        const char* GetBlob() const;

        const char* GetBlobWithChangeInfo() const;

        const Dob::PreviousEntityProxy GetPrevious() const;

        const Dob::Typesystem::HandlerId GetOwnerWithStringRepresentation() const;

        Dob::Typesystem::Int64 GetTimestamp() const;

        Dob::Typesystem::Int64 GetTimestamp(const Dob::Typesystem::MemberIndex member) const;

    private:
        const char* const m_currentBlob;
        const boost::shared_ptr<const char> m_currentState;
        const char* const m_previousBlob;
        const boost::shared_ptr<const char> m_previousState;
        const bool m_timestampDiff;
        mutable boost::shared_ptr<char> m_currentBlobWithChangeInfo;
    };
}
}
}

#endif
