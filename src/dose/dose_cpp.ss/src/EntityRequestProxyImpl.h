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

#ifndef _SAFIR_DOB_ENTITY_REQUEST_PROXY_IMPL_H
#define _SAFIR_DOB_ENTITY_REQUEST_PROXY_IMPL_H

#include <Safir/Dob/ConnectionInfo.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Defs.h>
#include <boost/noncopyable.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class EntityRequestProxyImpl:
        private boost::noncopyable
    {
    public:
        EntityRequestProxyImpl(const char * const requestBlob,
                               const char * const state);

        const Dob::Typesystem::TypeId GetTypeId() const;

        const Dob::Typesystem::InstanceId GetInstanceId() const;

        const Dob::Typesystem::EntityId GetEntityId() const;

        const Dob::EntityPtr GetRequest() const;

        const Dob::ConnectionInfoPtr GetSenderConnectionInfo() const;

        const Dob::Typesystem::HandlerId GetReceivingHandlerId() const;

        const char * GetBlob() const;

        const Dob::Typesystem::HandlerId GetReceiverWithStringRepresentation() const;
    private:
        const char * const m_requestBlob;
        const char * const m_state;
    };
}
}
}

#endif
