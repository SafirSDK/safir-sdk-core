/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#include <Safir/Dob/MessageProxy.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>

#include "MessageProxyImpl.h"

namespace Safir
{
namespace Dob
{
    MessageProxy::MessageProxy(Internal::MessageProxyImpl* pImpl):
        m_pImpl(pImpl)
    {
        ENSURE(pImpl != NULL, << "MessageProxy constructor was called with a NULL pImpl!");
    }

    const Dob::Typesystem::TypeId MessageProxy::GetTypeId() const
    {
        return m_pImpl->GetTypeId();
    }


    const Dob::MessagePtr
    MessageProxy::GetMessage() const
    {
        return m_pImpl->GetMessage();
    }


    const Dob::ConnectionInfoPtr
    MessageProxy::GetSenderConnectionInfo() const
    {
        return m_pImpl->GetSenderConnectionInfo();
    }


    const Dob::Typesystem::ChannelId
    MessageProxy::GetChannelId() const
    {
        return m_pImpl->GetChannelId();
    }

    const char *
    MessageProxy::GetBlob() const
    {
        return m_pImpl->GetBlob();
    }


    const Dob::Typesystem::ChannelId
    MessageProxy::GetChannelIdWithStringRepresentation() const
    {
        return m_pImpl->GetChannelIdWithStringRepresentation();
    }

}
}
