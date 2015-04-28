// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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

package com.saabgroup.safir.dob;
/**
 * Class that provides methods for special applications that injects entities into
 * the system apart from the normal handler.
 */
public class ConnectionAspectInjector
    extends ConnectionAspectBase
{
    /**
     * Constructor.
     *
     * @param connection The connection to operate through.
     */
    public ConnectionAspectInjector(ConnectionBase connection){
        super(connection);
    }

    /**
     * Merge the changed members based on the timestamps.
     *
     * All members of the given entity that are marked as changed will be merged into the
     * current entity object in the pool given that the top member has a timestamp that is "newer" than
     * the corresponding timestamp in the Dob.
     *
     * @param entity Entity to create or update.
     * @param instanceId Instance id.
     * @param timestamp Timestamp valid for the top members that are marked as changed.
     * @param handlerId Handler id.
     */
    public void injectChanges(Entity entity,
                              com.saabgroup.safir.dob.typesystem.InstanceId instanceId,
                              long timestamp,
                              com.saabgroup.safir.dob.typesystem.HandlerId handlerId)
    {
        boolean [] success = new boolean [1];

        //TODO: serialize directly to shared memory
        java.nio.ByteBuffer blob = com.saabgroup.safir.dob.typesystem.BlobOperations.writeToBlob(entity);

        Interface.InjectEntity(getControllerId(),
                               blob,
                               instanceId.getRawValue(),
                               instanceId.getRawString(),
                               handlerId.getRawValue(),
                               handlerId.getRawString(),
                               timestamp,
                               success);
        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    /**
     * Delete the given instance based on the timestamp.
     *
     * The given instance is deleted if the timestamp is "newer" than all top member timestamps
     * for the current instance.
     *
     * @param entityId Entity id of the instance to delete.
     * @param timestamp Timestamp Time of deletion.
     * @param handlerId Handler id.
     */
    public void injectDelete(com.saabgroup.safir.dob.typesystem.EntityId entityId,
                             long timestamp,
                             com.saabgroup.safir.dob.typesystem.HandlerId handlerId)
    {
        boolean [] success = new boolean [1];

        Interface.InjectDeletedEntity(getControllerId(),
                                      entityId.getTypeId(),
                                      entityId.getInstanceId().getRawValue(),
                                      entityId.getInstanceId().getRawString(),
                                      handlerId.getRawValue(),
                                      handlerId.getRawString(),
                                      timestamp,
                                      success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    /**
     * Allows an application to inject an initial entity state.
     *
     * @param entity Entity to create.
     * @param instanceId Instance id.
     * @param handlerId The handler that should be handed the ghost.
     */
    public void initialSet(Entity entity,
                           com.saabgroup.safir.dob.typesystem.InstanceId instanceId,
                           com.saabgroup.safir.dob.typesystem.HandlerId handlerId)
    {
        boolean [] success = new boolean [1];

        //TODO: serialize directly to shared memory
        java.nio.ByteBuffer blob = com.saabgroup.safir.dob.typesystem.BlobOperations.writeToBlob(entity);

        Interface.SetEntity(getControllerId(),
                            blob,
                            instanceId.getRawValue(),
                            instanceId.getRawString(),
                            handlerId.getRawValue(),
                            handlerId.getRawString(),
                            false,   // false => don't consider change flags
                            true,    // true => this is an initial injection
                            success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    /**
     * Special entity subscription
     *
     * Special subscription that also give the subscriber "ghost" entities, that is, entities
     * with no current owner.
     *
     * @param typeId Type id of the entity to subscribe for.
     * @param includeUpdates True => Subscription includes update, as well as create and delete.
     *                            False => Subscription includes no updates, only create and deletion.
     * @param includeSubclasses True => Subscription for this entity type and all its subclasses.
     *                               False => No subclasses will be included.
     * @param restartSubscription True => OnNewEntity callbacks are generated even if the subscription already exists.
     *                                 False => OnNewEntity callbacks are generated only for instances that are not previously subscribed.
     * @param wantsGhostDelete True => Wants notification that an "inject new" or InitialSet was immediately deleted by owner.
     *                              False => Normal subscription, only get deletes when an OnNew has been called.
     * @param wantsLastState True => Guarantee OnUpdated/OnNew even if application dies immediately after set.
     *                            False => Normal subscription, call OnDeleted only in this case.
     * @param doesntWantSourceIsPermanentStore True => Doesnt want OnNew if only accept of an accept of an InitialSet.
     *                                              False => Normal Subscription. Gets OnNew for all states.
     *                                               (This flag is only applicable for AsynchronousPermanent types)
     * @param wantsAllStateChanges True => Wants OnDoDispatch called for *all* state changes, even if they do not
     *                                          result in a callback. ONLY MEANT FOR DOSE_MAIN!
     *                                  False => Normal Subscription.
     * @param timestampChangeInfo True => ChangeInfo is based on timestamps instead of contents
     *                                 False => ChangeInfo is based on object contents, as with normal subscribe.
     * @param entitySubscriber EntitySubscriber that will receive the entities.
     */
    public void subscribeEntity(long typeId,
                                boolean includeUpdates,
                                boolean includeSubclasses,
                                boolean restartSubscription,
                                boolean wantsGhostDelete,
                                boolean wantsLastState,
                                boolean doesntWantSourceIsPermanentStore,
                                boolean wantsAllStateChanges,
                                boolean timestampChangeInfo,
                                EntitySubscriber entitySubscriber)
    {
        boolean [] success = new boolean [1];

        Interface.InjectorSubscribeEntity(getControllerId(),
                                          typeId,
                                          includeUpdates,
                                          includeSubclasses,
                                          restartSubscription,
                                          wantsGhostDelete,
                                          wantsLastState,
                                          doesntWantSourceIsPermanentStore,
                                          wantsAllStateChanges,
                                          timestampChangeInfo,
                                          entitySubscriber,
                                          success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }
}
