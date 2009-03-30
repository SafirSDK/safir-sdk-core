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

#ifndef _SAFIR_DOB_RESPONSE_PROXY_IMPL_H
#define _SAFIR_DOB_RESPONSE_PROXY_IMPL_H

#include <Safir/Dob/ConnectionInfo.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Response.h>
#include <Safir/Dob/Defs.h>
#include <boost/noncopyable.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class ResponseProxyImpl:
        private boost::noncopyable
    {
    public:
        ResponseProxyImpl(const long requestId,
                          const char * const responseBlob,
                          const char * const responseState,
                          const char * const requestBlob,
                          const char * const requestState);

        bool IsSuccess() const;

        const Dob::Typesystem::TypeId GetTypeId() const;

        const Dob::ResponsePtr GetResponse() const;

        const Dob::ConnectionInfoPtr GetResponseSenderConnectionInfo() const;

        const char * GetBlob() const {return m_responseBlob;}

        const Dob::RequestId GetRequestId() const {return m_requestId;}

        const Dob::Typesystem::TypeId GetRequestTypeId() const;

        const Dob::Typesystem::InstanceId GetRequestInstanceId() const;

        const Dob::Typesystem::ObjectPtr GetRequest() const;

        const Dob::Typesystem::HandlerId GetRequestHandlerId() const;
    private:
        const long m_requestId;
        const char * const m_responseBlob;
        const char * const m_responseState;
        const char * const m_requestBlob;
        const char * const m_requestState;
    };
}
}
}

#endif
