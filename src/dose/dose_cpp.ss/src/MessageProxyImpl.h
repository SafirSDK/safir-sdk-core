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

#ifndef _SAFIR_DOB_MESSAGE_PROXY_IMPL_H
#define _SAFIR_DOB_MESSAGE_PROXY_IMPL_H

#include <Safir/Dob/ConnectionInfo.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/Defs.h>
#include <boost/noncopyable.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class MessageProxyImpl:
        private boost::noncopyable
    {
    public:
        MessageProxyImpl(const char * const messageBlob,
                         const char * const state);

        const Dob::Typesystem::TypeId GetTypeId() const;

        const Dob::MessagePtr GetMessage() const;

        const Dob::ConnectionInfoPtr GetSenderConnectionInfo() const;

        const Dob::Typesystem::ChannelId GetChannelId() const;

        const char * GetBlob() const {return m_messageBlob;}

        const Dob::Typesystem::ChannelId GetChannelIdWithStringRepresentation() const;
    private:
        const char * const m_messageBlob;
        const char * const m_state;
    };
}
}
}

#endif
