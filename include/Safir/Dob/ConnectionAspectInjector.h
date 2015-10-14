/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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

#ifndef _SAFIR_DOB_CONNECTION_ASPECT_INJECTOR_H
#define _SAFIR_DOB_CONNECTION_ASPECT_INJECTOR_H

#include <Safir/Dob/DoseCppExportDefs.h>
#include <Safir/Dob/ConnectionAspectBase.h>
#include <Safir/Dob/ConnectionBase.h>
#include <Safir/Dob/Typesystem/EntityId.h>

#include <string>

namespace Safir
{
namespace Dob
{
    /**
     * Class that provides methods for special applications that injects entities into
     * the system apart from the normal handler.
     */
    class DOSE_CPP_API ConnectionAspectInjector : public ConnectionAspectBase
    {
    public:
        /** 
         * Constructor.
         *
         * @param [in] connection The connection to operate through.
         */
        ConnectionAspectInjector(const ConnectionBase& connection) : ConnectionAspectBase(connection) {}

        /**
         * Merge the changed members based on the timestamps.
         *
         * All members of the given entity that are marked as changed will be merged into the
         * current entity object in the pool given that the top member has a timestamp that is "newer" than
         * the corresponding timestamp in the Dob.
         *
         * @param [in] entity Entity to create or update.
         * @param [in] instanceId Instance id.
         * @param [in] timestamp Timestamp valid for the top members that are marked as changed.
         * @param [in] handlerId Handler id.
         */
        void InjectChanges(const Dob::EntityPtr&                entity,
                           const Dob::Typesystem::InstanceId&   instanceId,
                           const Dob::Typesystem::Int64         timestamp,
                           const Dob::Typesystem::HandlerId&    handlerId) const;

        /**
         * Delete the given instance based on the timestamp.
         *
         * The given instance is deleted if the timestamp is "newer" than all top member timestamps
         * for the current instance.
         *
         * @param [in] entityId Entity id of the instance to delete.
         * @param [in] timestamp Timestamp Time of deletion.
         * @param [in] handlerId Handler id.
         */
        void InjectDelete(const Dob::Typesystem::EntityId&     entityId,
                          const Dob::Typesystem::Int64         timestamp,
                          const Dob::Typesystem::HandlerId&    handlerId) const;


        /**
         * Allows an application to inject an initial entity state.
         *
         * @param [in] entity Entity to create.
         * @param [in] instanceId Instance id.
         * @param [in] handlerId The handler id to which the state belongs.
         */
        void InitialSet(const Dob::EntityPtr&              entity,
                        const Dob::Typesystem::InstanceId& instanceId,
                        const Dob::Typesystem::HandlerId&  handlerId) const;

        /**
         * Special entity subscription
         *
         * Special subscription that also give the subscriber "ghost" entities, that is, entities
         * with no current owner.
         *
         * @param [in] typeId Type id of the entity to subscribe for.
         * @param [in] includeUpdates True => Subscription includes update, as well as create and delete.
         *                            False => Subscription includes no updates, only create and deletion.
         * @param [in] includeSubclasses True => Subscription for this entity type and all its subclasses.
         *                               False => No subclasses will be included.
         * @param [in] restartSubscription True => OnNewEntity callbacks are generated even if the subscription already exists.
         *                                 False => OnNewEntity callbacks are generated only for instances that are not previously subscribed.
         * @param [in] wantsGhostDelete True => Wants notification that an "inject new" or InitialSet was immediately deleted by owner.
         *                              False => Normal subscription, only get deletes when an OnNew has been called.
         * @param [in] wantsLastState True => Guarantee OnUpdated/OnNew even if application dies immediately after set.
         *                            False => Normal subscription, call OnDeleted only in this case.
         * @param [in] doesntWantSourceIsPermanentStore True => Doesnt want OnNew if only accept of an accept of an InitialSet.
         *                                              False => Normal Subscription. Gets OnNew for all states.
         *                                               (This flag is only applicable for AsynchronousPermanent types)
         * @param [in] wantsAllStateChanges True => Wants OnDoDispatch called for *all* state changes, even if they do not
         *                                          result in a callback. ONLY MEANT FOR DOSE_MAIN!
         *                                  False => Normal Subscription.
         * @param [in] timestampChangeInfo True => ChangeInfo is based on timestamps instead of contents
         *                                 False => ChangeInfo is based on object contents, as with normal subscribe.
         * @param [in] entitySubscriber EntitySubscriber that will receive the entities.
         */
        void SubscribeEntity(const Dob::Typesystem::TypeId      typeId,
                             const bool                         includeUpdates,
                             const bool                         includeSubclasses,
                             const bool                         restartSubscription,
                             const bool                         wantsGhostDelete,
                             const bool                         wantsLastState,
                             const bool                         doesntWantSourceIsPermanentStore,
                             const bool                         wantsAllStateChanges,
                             const bool                         timestampChangeInfo,
                             Dob::EntitySubscriber* const       entitySubscriber) const;
    };

}
}

#endif
