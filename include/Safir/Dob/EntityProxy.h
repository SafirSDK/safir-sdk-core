/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
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

#ifndef _SAFIR_DOB_ENTITY_PROXY_H
#define _SAFIR_DOB_ENTITY_PROXY_H

#include <Safir/Dob/Internal/ProxyImplPtr.h>
#include <Safir/Dob/PreviousEntityProxy.h>
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
        class EntityProxyImpl;
    }

    /**
     * Proxy class for an entity.
     */
    class DOSE_CPP_API EntityProxy
    {
    public:

        /**
         * Get type id.
         *
         * Retrieves type id of the Entity.
         *
         * @return Type id.
         */
        const Dob::Typesystem::TypeId GetTypeId() const;

        /**
         * Get instance id.
         *
         * Retrieves instance id of the Entity.
         *
         * @return Instance id.
         */
        const Dob::Typesystem::InstanceId GetInstanceId() const;

        /**
         * Get entity id.
         *
         * Aggregation of type id and instance id.
         *
         * @return Entity id.
         */
        const Dob::Typesystem::EntityId GetEntityId() const;

        /**
         * Get entity.
         *
         * Retrieves a smart pointer to the entity.
         *
         * No change flags will be set in the returned entity.
         *
         * @return entity.
         */
        const Dob::EntityPtr GetEntity() const;

        /**
         * Get entity with change information.
         *
         * Retrieves the entity with change flags set to indicate which members have
         * changed since the last subscription response.
         *
         * @return entity.
         */
        const Dob::EntityPtr GetEntityWithChangeInfo() const;

        /**
         * Get owner handler id.
         *
         * Retrieves the handler id of the handler that owns (has created) this entity instance.
         *
         * @return Handler id.
         */
        const Dob::Typesystem::HandlerId GetOwner() const;

        /**
         * Get info about the connection to which the owner handler is related.
         *
         * @return Connection info.
         */
        const Dob::ConnectionInfoPtr GetOwnerConnectionInfo() const;

        /**
         * @name Retrieve binary blob
         */
        /** @{ */

        /**
         * Get binary blob of the received entity without changeflags set.
         *
         * This method will give you a pointer to the underlying representation of the object.
         * Note that this pointer is only valid while the EntityProxy is in scope.
         * If you want to keep the blob you must copy it using methods in Safir::Dob::Typesystem.
         *
         * This method is mainly useful if all you want to do with a received object is to write it
         * to a database or pass it over a C-interface to a library or plugin.
         *
         * As an example, if you want to copy the bytes into a std::vector<char> you could do it
         * like this "v = std::vector<char>(blob,blob+Safir::Dob::Typesystem::Internal::BlobOperations.GetSize())"
         *
         * @return Binary blob of the received entity.
         */
        const char * GetBlob() const;

        /**
         * Get binary blob with change information.
         *
         * Retrieves the entity with change flags set to indicate which members have
         * changed since the last subscription response.
         *
         * @see GetBlob
         *
         * @return Binary blob.
         */
        const char * GetBlobWithChangeInfo() const;

        /** @} */

        /**
         * @name Retrieve previous entity state
         */
        /** @{ */

        /**
         * Get previous entity state.
         *
         * Used to get the entity state that preceeded this state.
         *
         * Can be used when a "previous" state exists, that is, from within the following callbacks:
         * @li EntitySubscriber#OnUpdatedEntity
         * @li EntitySubscriber#OnDeletedEntity
         *
         * No change flags will be set in the returned entity.
         *
         * Calling this function inside an OnDeletedEntity when the subscription was set up with 
         * includeUpdates set to false may yield an entity state that you have not received in an 
         * OnNewEntity callback. In fact it will most likely give you one of the updated entity
         * states that were filtered out because you didn't include updates.
         *
         * @return Previous entity.
         */
        const Dob::PreviousEntityProxy GetPrevious() const;

        /** @} */

        /**
         * @name Trace and Debug stuff
         */
        /** @{ */

        /**
         * Get owner handler id that also contains the string representation.
         *
         * Mainly for trace and debug purposes.
         * @see GetOwner()
         *
         * @return Handler id.
         */
        const Dob::Typesystem::HandlerId GetOwnerWithStringRepresentation() const;

        /** @} */

        /**
         * @name Retrieve timestamps. (Extended info for applications with special need)
         *       Note that timestamps is only available for types configured with this option.
         */
        /** @{ */

        /**
         * Retrieves the timestamp for the latest create, update or delete.
         *
         * Note that this operation is only valid for Injectable types.
         *
         * @return Timestamp.
         */
        const Dob::Typesystem::Int64 GetTimestamp() const;

        /**
         * Retrieves the timestamp for the given top member.
         *
         * Note that this operation is only valid for Injectable types.
         *
         * @param [in] member Top level member index.
         * @return Timestamp.
         */
        const Dob::Typesystem::Int64 GetTimestamp(const Dob::Typesystem::MemberIndex member) const;

        /** @} */


        // The constructor is for internal usage only!
        explicit EntityProxy(Internal::EntityProxyImpl* pImpl);

    private:

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251) // To get rid of warning that says that the template needs to have a DLL-interface
#endif

        Internal::ProxyImplPtr<Internal::EntityProxyImpl> m_pImpl;

#ifdef _MSC_VER
#pragma warning (pop)
#endif

    };

}
}

#endif
