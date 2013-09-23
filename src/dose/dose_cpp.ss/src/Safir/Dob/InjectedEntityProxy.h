/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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

#ifndef _SAFIR_DOB_INJECTED_ENTITY_PROXY_H
#define _SAFIR_DOB_INJECTED_ENTITY_PROXY_H

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
        class InjectedEntityProxyImpl;
    }

    /**
     * Proxy class for entity injections.
     */
    class DOSE_CPP_API InjectedEntityProxy
    {
    public:

        /**
         * Get type id.
         *
         * Retrieves type id of the Entity that is about to be injected.
         *
         * @return Type id.
         */
        const Dob::Typesystem::TypeId GetTypeId() const;

        /**
         * Get instance id.
         *
         * Retrieves instance id of the Entity that is about to be injected.
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
         * Get the entity state that is about to be injected.
         *
         * Retrieves a smart pointer to the entity that is about to be injected.
         *
         * Change flags will be set in the entity to indicate which members
         * are part of the injection.
         *
         * Note that this method cannot be called in an OnInjectedDeletedEntity,
         * since there then is no entity to get...
         *
         * @return entity.
         */
        const Dob::EntityPtr GetInjection() const;

        /**
         * Get binary blob of the entity that is about to be injected.
         *
         * This method will give you a pointer to the underlying representation of the object.
         * Note that this pointer is only valid while the InjectedEntityProxy is in scope.
         * If you want to keep the blob you must copy it using methods in Safir::Dob::Typesystem.
         *
         * This method is mainly useful if all you want to do with a received object is to write it
         * to a database or pass it over a C-interface to a library or plugin.
         *
         * As an example, if you want to copy the bytes into a std::vector<char> you could do it
         * like this "v = std::vector<char>(blob,blob+Safir::Dob::Typesystem::BlobOperations.GetSize())"
         *
         * Change flags will be set in the entity to indicate which members
         * are part of the injection.
         *
         * @return Binary blob of the entity that is about to be injected.
         */
        const char * GetInjectionBlob() const;


        /**
         * Get the current entity state.
         *
         * This method retrieves the entity as it is before the injection has been completed.
         *
         * Can be used when a "current" state exists, i.e. from within the following callbacks:
         * @li EntityInjectionHandler#OnInjectedUpdatedEntity
         * @li EntityInjectionHandler#OnInjectedDeletedEntity
         *
         * No change flags will be set in the returned entity.
         *
         * @return Previous entity.
         */
        const Dob::EntityPtr GetCurrent() const;

        //TODO: does anyone need a GetCurrentWithChangeInfo? Please tell your nearest
        //Dob developer if so.

        // The constructor is for internal usage only!
        explicit InjectedEntityProxy(Internal::InjectedEntityProxyImpl* pImpl);

    private:

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251) // To get rid of warning that says that the template needs to have a DLL-interface
#endif

        Internal::ProxyImplPtr<Internal::InjectedEntityProxyImpl> m_pImpl;

#ifdef _MSC_VER
#pragma warning (pop)
#endif

    };

}
}

#endif
