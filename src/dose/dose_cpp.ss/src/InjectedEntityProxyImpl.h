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

#ifndef _SAFIR_DOB_INJECTED_ENTITY_PROXY_IMPL_H
#define _SAFIR_DOB_INJECTED_ENTITY_PROXY_IMPL_H

#include <Safir/Dob/ConnectionInfo.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Defs.h>
#include <Safir/Dob/InjectedEntityProxy.h>
#include <boost/noncopyable.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class InjectedEntityProxyImpl:
        private boost::noncopyable
    {
    public:

        InjectedEntityProxyImpl(const char * const injectionBlob,
                                const char * const injectionState,
                                const char * const currentBlob,
                                const char * const currentState);

        const Dob::Typesystem::TypeId GetTypeId() const;

        const Dob::Typesystem::InstanceId GetInstanceId() const;

        const Dob::Typesystem::EntityId GetEntityId() const;

        const Dob::EntityPtr GetInjection() const;

        const char * GetInjectionBlob() const;

        const Dob::EntityPtr GetCurrent() const;

    private:
        const char* const m_injectionBlob;
        const char* const m_injectionState;
        const char* const m_currentBlob;
        const char* const m_currentState;
    };
}
}
}

#endif
