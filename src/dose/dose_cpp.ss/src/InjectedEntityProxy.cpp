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

#include <Safir/Dob/InjectedEntityProxy.h>

#include "InjectedEntityProxyImpl.h"

namespace Safir
{
namespace Dob
{
    InjectedEntityProxy::InjectedEntityProxy(Internal::InjectedEntityProxyImpl* pImpl)
        : m_pImpl(pImpl)
    {
    }

    const Dob::Typesystem::TypeId
    InjectedEntityProxy::GetTypeId() const
    {
        return m_pImpl->GetTypeId();
    }

    const Dob::Typesystem::InstanceId
    InjectedEntityProxy::GetInstanceId() const
    {
        return m_pImpl->GetInstanceId();
    }

    const Dob::Typesystem::EntityId
    InjectedEntityProxy::GetEntityId() const
    {
        return m_pImpl->GetEntityId();
    }

    const Dob::EntityPtr
    InjectedEntityProxy::GetInjection() const
    {
        return m_pImpl->GetInjection();
    }

    const char *
    InjectedEntityProxy::GetInjectionBlob() const
    {
        return m_pImpl->GetInjectionBlob();
    }

    const Dob::EntityPtr
    InjectedEntityProxy::GetCurrent() const
    {
        return m_pImpl->GetCurrent();
    }

}
}
