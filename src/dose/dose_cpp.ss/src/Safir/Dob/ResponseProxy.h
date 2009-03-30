/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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

#ifndef _SAFIR_DOB_RESPONSE_PROXY_H
#define _SAFIR_DOB_RESPONSE_PROXY_H

#include <Safir/Dob/Internal/ProxyImplPtr.h>
#include <Safir/Dob/DoseCppExportDefs.h>
#include <Safir/Dob/ConnectionInfo.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Response.h>
#include <Safir/Dob/Defs.h>

#include <string>

namespace Safir
{
namespace Dob
{

    // Forward declaration
    namespace Internal
    {
        class ResponseProxyImpl;
    }

    /**
     * Proxy class for a response.
     */
    class DOSE_CPP_API ResponseProxy
    {
    public:
        /**
         * Get response success or failure status
         *
         * @return Success or failure.
         */
        bool IsSuccess() const;

        /**
         * Get type id.
         *
         * Retrieves type id of the response.
         *
         * @return Type id.
         */
        const Dob::Typesystem::TypeId GetTypeId() const;

        /**
         * Get response.
         *
         * Retrieves a smart pointer to the response.
         *
         * @return Service request
         */
        const Dob::ResponsePtr GetResponse() const;

        /**
         * Get info about the response sender.
         *
         * Retrieves a smart pointer to info about the connection sending the response.
         *
         * @return Connection info.
         */
        const Dob::ConnectionInfoPtr GetResponseSenderConnectionInfo() const;

        /**
         * Get binary blob of the received response.
         *
         * This method will give you a pointer to the underlying representation of the object.
         * Note that this pointer is only valid while the ResponseProxy is in scope.
         * If you want to keep the blob you must copy it using methods in Safir::Dob::Typesystem.
         *
         * This method is mainly useful if all you want to do with a received object is to write it
         * to a database or pass it over a C-interface to a library or plugin.
         *
         * As an example, if you want to copy the bytes into a std::vector<char> you could do it
         * like this "v = std::vector<char>(blob,blob+Safir::Dob::Typesystem::BlobOperations.GetSize())"
         *
         * @return Binary blob of the received response.
         */
        const char * GetBlob() const;

        /**
         * @name Methods to retrieve info about the original request.
         */
        /** @{ */

        /**
         * Get request id.
         *
         * Retrieves the request id generated when the request was sent.
         *
         * @return Request id.
         */
        const Dob::RequestId GetRequestId() const;

        /**
         * Get type id of the entity or service sent in the original request.
         *
         * @return Type id.
         */
        const Dob::Typesystem::TypeId GetRequestTypeId() const;

         /**
         * Get the instance id used in the original request. (Only for entity requests)
         *
         * @return Instance id.
         */
        const Dob::Typesystem::InstanceId GetRequestInstanceId() const;

        /**
         * Get the original request.
         *
         * Retrieves the original request. Depending on the type of request this
         * can be a Dob::EntityPtr or a Dob::ServicePtr.
         *
         * @return Original request.
         */
        const Dob::Typesystem::ObjectPtr GetRequest() const;

        /**
         * Get the handler id to which the original request was sent.
         *
         * @return Handler id.
         */
        const Dob::Typesystem::HandlerId GetRequestHandlerId() const;

        /** @} */


        // The constructor is for internal usage only!
        explicit ResponseProxy(Internal::ResponseProxyImpl* pImpl);

    private:

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251) // To get rid of warning that says that the template needs to have a DLL-interface
#endif

        Internal::ProxyImplPtr<Internal::ResponseProxyImpl> m_pImpl;

#ifdef _MSC_VER
#pragma warning (pop)
#endif

    };
}
}

#endif
