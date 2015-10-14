/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
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

#ifndef _SAFIR_DOB_ENTITY_REQUEST_PROXY_H
#define _SAFIR_DOB_ENTITY_REQUEST_PROXY_H

#include <Safir/Dob/Internal/ProxyImplPtr.h>
#include <Safir/Dob/DoseCppExportDefs.h>
#include <Safir/Dob/ConnectionInfo.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Defs.h>

#include <string>

namespace Safir
{
namespace Dob
{

    // Forward declaration
    namespace Internal
    {
        class EntityRequestProxyImpl;
    }

    /**
     * Proxy class for an entity request.
     */
    class DOSE_CPP_API EntityRequestProxy
    {
    public:
        /**
         * Get type id.
         *
         * Retrieves type id of the entity request.
         *
         * @return Type id.
         */
        const Dob::Typesystem::TypeId GetTypeId() const;

        /**
         * Get instance id.
         *
         * Retrieves instance id of the entity request.
         *
         * Note that it is illegal to call this method on proxies received in OnCreateRequest
         * callbacks if the handler is registered as "HandlerDecidesInstanceId".
         * This is because there is no instance id in the request in this case...
         *
         * @return Instance id.
         */
        const Dob::Typesystem::InstanceId GetInstanceId() const;

        /**
         * Get entity id.
         *
         * Aggregation of type id and instance id.
         *
         * Note that it is illegal to call this method on proxies received in OnCreateRequest
         * callbacks if the handler is registered as "HandlerDecidesInstanceId".
         * This is because there is no instance id in the request in this case...
         *
         * @return Entity id.
         */
        const Dob::Typesystem::EntityId GetEntityId() const;

        /**
         * Get entity request.
         *
         * Retrieves a smart pointer to the entity request.
         *
         * Note that it is not valid to call this for a DeleteRequest.
         *
         * @return Entity request
         */
        const Dob::EntityPtr GetRequest() const;

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
         * Get binary blob of the received entity request.
         *
         * This method will give you a pointer to the underlying representation of the object.
         * Note that this pointer is only valid while the EntityRequestProxy is in scope.
         * If you want to keep the blob you must copy it using methods in Safir::Dob::Typesystem.
         *
         * This method is mainly useful if all you want to do with a received object is to write it
         * to a database or pass it over a C-interface to a library or plugin.
         *
         * As an example, if you want to copy the bytes into a std::vector<char> you could do it
         * like this "v = std::vector<char>(blob,blob+Safir::Dob::Typesystem::Internal::BlobOperations.GetSize())"
         *
         * @return Binary blob of the received entity request.
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
        explicit EntityRequestProxy(Internal::EntityRequestProxyImpl* pImpl);

    private:

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251) // To get rid of warning that says that the template needs to have a DLL-interface
#endif

        Internal::ProxyImplPtr<Internal::EntityRequestProxyImpl> m_pImpl;

#ifdef _MSC_VER
#pragma warning (pop)
#endif


    };
}
}

#endif
