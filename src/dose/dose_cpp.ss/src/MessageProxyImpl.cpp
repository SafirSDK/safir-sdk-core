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

#include "MessageProxyImpl.h"

#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Dob/Typesystem/Internal/BlobOperations.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Internal/Interface.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{

    MessageProxyImpl::MessageProxyImpl(const char * const messageBlob,
                                       const char * const state):
        m_messageBlob(messageBlob),
        m_state(state)
    {

    }

    const Dob::Typesystem::TypeId
    MessageProxyImpl::GetTypeId() const
    {
        return Typesystem::Internal::BlobOperations::GetTypeId(m_messageBlob);
    }

    const Dob::MessagePtr
    MessageProxyImpl::GetMessage() const
    {
        return boost::static_pointer_cast<Safir::Dob::Message>
            (Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(m_messageBlob));
    }

    const Dob::ConnectionInfoPtr
    MessageProxyImpl::GetSenderConnectionInfo() const
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

    const Dob::Typesystem::ChannelId
    MessageProxyImpl::GetChannelId() const
    {
        Typesystem::Int64 channelId;
        bool success;
        DoseC_GetChannelId(m_state, channelId, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return Typesystem::ChannelId(channelId);
    }

    const Dob::Typesystem::ChannelId
    MessageProxyImpl::GetChannelIdWithStringRepresentation() const
    {
        return GetChannelId();
        //TODO: try to obtain string representation
    }
}
}
}
