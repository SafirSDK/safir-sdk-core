/* ****************************************************************************
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

using System;
using System.Runtime.InteropServices;

namespace Safir.Dob
{
    /// <summary>
    /// Class that provides methods for special applications that injects entities into
    /// the system apart from the normal handler.
    /// </summary>
    public sealed class ConnectionAspectInjector : ConnectionAspectBase
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="connection">The connection to operate through.</param>
        public ConnectionAspectInjector(ConnectionBase connection) : base(connection) { }

        /// <summary>
        /// Merge the changed members based on the timestamps.
        /// <para/>
        /// All members of the given entity that are marked as changed will be merged into the
        /// current entity object in the pool given that the top member has a timestamp that is "newer" than
        /// the corresponding timestamp in the Dob.
        /// </summary>
        /// <param name="entity">Entity to create or update.</param>
        /// <param name="instanceId">Instance id.</param>
        /// <param name="timestamp">Timestamp valid for the top members that are marked as changed.</param>
        /// <param name="handlerId">Handler id.</param>
        public void InjectChanges(Entity entity,
                                  Typesystem.InstanceId instanceId,
                                  System.Int64 timestamp,
                                  Typesystem.HandlerId handlerId)
        {
            byte success;

            //TODO: serialize directly to shared memory
            System.Int32 blobSize = entity.CalculateBlobSize();
            System.IntPtr blob = Marshal.AllocHGlobal(blobSize); //allocate blob
            System.IntPtr beginningOfUnused;
            Typesystem.Internal.InternalOperations.FormatBlob(blob, blobSize, entity.GetTypeId(), out beginningOfUnused);
            entity.WriteToBlob(blob, ref beginningOfUnused);

            System.IntPtr instanceIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(instanceId.RawString);
            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_InjectEntity(ControllerId,
                                         blob,
                                         instanceId.RawValue,
                                         instanceIdStr,
                                         handlerId.RawValue,
                                         handlerIdStr,
                                         timestamp,
                                         out success);

            Marshal.FreeHGlobal(instanceIdStr);
            Marshal.FreeHGlobal(handlerIdStr);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Delete the given instance based on the timestamp.
        /// <para/>
        /// The given instance is deleted if the timestamp is "newer" than all top member timestamps
        /// for the current instance.
        /// </summary>
        /// <param name="entityId">Entity id of the instance to delete.</param>
        /// <param name="timestamp">Timestamp Time of deletion.</param>
        /// <param name="handlerId">Handler id.</param>
        public void InjectDelete(Typesystem.EntityId entityId,
                                 System.Int64 timestamp,
                                 Typesystem.HandlerId handlerId)
        {
            byte success;

            System.IntPtr instanceIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(entityId.InstanceId.RawString);
            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_InjectDeletedEntity(ControllerId,
                                                entityId.TypeId,
                                                entityId.InstanceId.RawValue,
                                                instanceIdStr,
                                                handlerId.RawValue,
                                                handlerIdStr,
                                                timestamp,
                                                out success);

            Marshal.FreeHGlobal(instanceIdStr);
            Marshal.FreeHGlobal(handlerIdStr);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Allows an application to inject an initial entity state.
        /// </summary>
        /// <param name="entity">Entity to create.</param>
        /// <param name="instanceId">Instance id of entity to inject.</param>
        /// <param name="handlerId">The handler id to inject to.</param>
        public void InitialSet(Entity entity,
                               Typesystem.InstanceId instanceId,
                               Typesystem.HandlerId handlerId)
        {
            byte success;

            //TODO: serialize directly to shared memory
            System.Int32 blobSize = entity.CalculateBlobSize();
            System.IntPtr blob = Marshal.AllocHGlobal(blobSize); //allocate blob
            System.IntPtr beginningOfUnused;
            Typesystem.Internal.InternalOperations.FormatBlob(blob, blobSize, entity.GetTypeId(), out beginningOfUnused);
            entity.WriteToBlob(blob, ref beginningOfUnused);

            System.IntPtr instanceIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(instanceId.RawString);
            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_SetEntity(ControllerId,
                                      blob,
                                      instanceId.RawValue,
                                      instanceIdStr,
                                      handlerId.RawValue,
                                      handlerIdStr,
                                      Interface.ByteOf(false),   // false => don't consider change flags
                                      Interface.ByteOf(true),    // true => this is an initial injection
                                      out success);

            Marshal.FreeHGlobal(instanceIdStr);
            Marshal.FreeHGlobal(handlerIdStr);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Special entity subscription
        ///<para/>
        /// Special subscription that also give the subscriber "ghost" entities, that is, entities
        /// with no current owner.
        /// </summary>
        /// <param name="typeId">Type id of the entity to subscribe for.</param>
        /// <param name="includeUpdates">True => Subscription includes update, as well as create and delete.
        ///                              False => Subscription includes no updates, only create and deletion.</param>
        /// <param name="includeSubclasses">True => Subscription for this entity type and all its subclasses.
        ///                                 False => No subclasses will be included.</param>
        /// <param name="restartSubscription">True => OnNewEntity callbacks are generated even if the subscription already exists.
        ///                                   False => OnNewEntity callbacks are generated only for instances that are not previously subscribed.</param>
        /// <param name="wantsGhostDelete">True => Wants notification that an "inject new" or InitialSet was immediately deleted by owner.
        ///                                False => Normal subscription, only get deletes when an OnNew has been called.</param>
        /// <param name="wantsLastState">True => Guarantee OnUpdated/OnNew even if application dies immediately after set.
        ///                              False => Normal subscription, call OnDeleted only in this case.</param>
        /// <param name="doesntWantSourceIsPermanentStore">True => Doesnt want OnNew if only accept of an accept of an InitialSet.
        ///                                                False => Normal Subscription. Gets OnNew for all states.
        ///                                                (This flag is only applicable for AsynchronousPermanent types)</param>
        /// <param name="wantsAllStateChanges">True => Wants OnDoDispatch called for *all* state changes, even if they do not
        ///                                            result in a callback. ONLY MEANT FOR DOSE_MAIN!
        ///                                    False => Normal Subscription.</param>
        /// <param name="timestampChangeInfo">True => ChangeInfo is based on timestamps instead of contents
        ///                                   False => ChangeInfo is based on object contents, as with normal subscribe.</param>
        /// <param name="entitySubscriber">EntitySubscriber that will receive the entities.</param>
        public void SubscribeEntity(System.Int64 typeId,
                                    bool includeUpdates,
                                    bool includeSubclasses,
                                    bool restartSubscription,
                                    bool wantsGhostDelete,
                                    bool wantsLastState,
                                    bool doesntWantSourceIsPermanentStore,
                                    bool wantsAllStateChanges,
                                    bool timestampChangeInfo,
                                    EntitySubscriber entitySubscriber)
        {
            byte success;

            Interface.DoseC_InjectorSubscribeEntity(ControllerId,
                                                    typeId,
                                                    Interface.ByteOf(includeUpdates),
                                                    Interface.ByteOf(includeSubclasses),
                                                    Interface.ByteOf(restartSubscription),
                                                    Interface.ByteOf(wantsGhostDelete),
                                                    Interface.ByteOf(wantsLastState),
                                                    Interface.ByteOf(doesntWantSourceIsPermanentStore),
                                                    Interface.ByteOf(wantsAllStateChanges),
                                                    Interface.ByteOf(timestampChangeInfo),
                                                    Interface.DOSE_LANGUAGE_DOTNET,
                                                    ConsumerHandler.Instance.AddReference(entitySubscriber),
                                                    out success);
            if (!Interface.BoolOf(success))
            {
                ConsumerHandler.Instance.DropReference(entitySubscriber);

                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }
    }
}
