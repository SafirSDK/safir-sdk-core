/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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

#ifndef _SAFIR_DOB_SERVICE_REQUEST_PROXY_H
#define _SAFIR_DOB_SERVICE_REQUEST_PROXY_H

#include <Safir/Dob/Internal/ProxyImplPtr.h>
#include <Safir/Dob/DoseCppExportDefs.h>
#include <Safir/Dob/ConnectionInfo.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/Defs.h>

#include <string>

namespace Safir
{
namespace Dob
{

    // Forward declaration
    namespace Internal
    {
        class ServiceRequestProxyImpl;
    }

    /**
     * Proxy class for a service request.
     */
    class DOSE_CPP_API ServiceRequestProxy
    {
    public:
        /**
         * Get type id.
         *
         * Retrieves type id of the service request.
         *
         * @return Type id.
         */
        const Dob::Typesystem::TypeId GetTypeId() const;

        /**
         * Get service request.
         *
         * Retrieves a smart pointer to the service request.
         *
         * @return Service request
         */
        const Dob::ServicePtr GetRequest() const;

        /**
         * Get info about the sender.
         *
         * Retrieves a smart pointer to info about the connection sending the request.
         *
         * @return Connection info.
         */
        const Dob::ConnectionInfoPtr GetSenderConnectionInfo() const;

        /**
         * Get id of receiving handler.
         *
         * Can be handy when one consumer is used for several handlers.
         *
         * @return Handler id.
         */
        const Dob::Typesystem::HandlerId GetReceivingHandlerId() const;

        /**
         * Get binary blob of the received service request.
         *
         * This method will give you a pointer to the underlying representation of the object.
         * Note that this pointer is only valid while the ServiceRequestProxy is in scope.
         * If you want to keep the blob you must copy it using methods in Safir::Dob::Typesystem.
         *
         * This method is mainly useful if all you want to do with a received object is to write it
         * to a database or pass it over a C-interface to a library or plugin.
         *
         * As an example, if you want to copy the bytes into a std::vector<char> you could do it
         * like this "v = std::vector<char>(blob,blob+Safir::Dob::Typesystem::BlobOperations.GetSize())"
         *
         * @return Binary blob of the received service request.
         */
        const char * GetBlob() const;

        /**
         * @name Trace and Debug stuff
         */
        /** @{ */

        /**
         * Get receiver handler id that also contains the string representation.
         *
         * Mainly for trace and debug purposes.
         * @see GetReceivingHandlerId()
         *
         * @return Handler id.
         */
        const Dob::Typesystem::HandlerId GetReceiverWithStringRepresentation() const;

        /** @} */

        // The constructor is for internal usage only!
        explicit ServiceRequestProxy(Internal::ServiceRequestProxyImpl* pImpl);

    private:

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251) // To get rid of warning that says that the template needs to have a DLL-interface
#endif

        Internal::ProxyImplPtr<Internal::ServiceRequestProxyImpl> m_pImpl;

#ifdef _MSC_VER
#pragma warning (pop)
#endif

    };
}
}

#endif
