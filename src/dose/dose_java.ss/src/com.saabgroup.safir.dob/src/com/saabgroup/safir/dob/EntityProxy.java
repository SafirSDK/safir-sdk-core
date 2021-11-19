// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2007-2013, 2021 (http://safirsdkcore.com)
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
 * Proxy class for an entity.
 */
public class EntityProxy {

    /**
     * Get type id.
     *
     * Retrieves type id of the Entity.
     *
     * @return Type id.
     */
    public long getTypeId()
    {
        checkNotDisposed();
        if (m_state.currentBlob == null) {
            long [] typeId = new long[1];
            boolean [] success = new boolean [1];

            Interface.GetTypeId(m_state.currentState, typeId, success);

            if (!success[0]) {
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
            }
            return typeId[0];
        }
        else
        {
            return com.saabgroup.safir.dob.typesystem.BlobOperations.getTypeId(m_state.currentBlob);
        }
    }

    /**
     * Get instance id.
     *
     * Retrieves instance id of the Entity.
     *
     * @return Instance id.
     */
    public com.saabgroup.safir.dob.typesystem.InstanceId getInstanceId()
    {
        checkNotDisposed();
        long [] instanceId = new long[1];
        boolean [] success = new boolean [1];
        Interface.GetInstanceId(m_state.currentState, instanceId, success);
        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        return new com.saabgroup.safir.dob.typesystem.InstanceId(instanceId[0]);
    }

    /**
     * Get entity id.
     *
     * Aggregation of type id and instance id.
     *
     * @return Entity id.
     */
    public com.saabgroup.safir.dob.typesystem.EntityId getEntityId()
    {
        checkNotDisposed();
        return new com.saabgroup.safir.dob.typesystem.EntityId(getTypeId(), getInstanceId());
    }

    /**
     * Get entity.
     *
     * No change flags will be set in the returned entity.
     *
     * @return entity.
     */
    public Entity getEntity()
    {

        checkNotDisposed();
        if (m_state.currentBlob == null)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Not possible to use call getEntity on onDeletedEntity proxy!");
        }
        return (Entity)com.saabgroup.safir.dob.typesystem.ObjectFactory.getInstance().createObject(m_state.currentBlob);
    }


    /**
     * Get entity with change information.
     *
     * Retrieves the entity with change flags set to indicate which members have
     * changed since the last subscription response.
     *
     * @return entity.
     */
    public Entity getEntityWithChangeInfo()
    {
        checkNotDisposed();
        if (m_state.currentBlob == null)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Not possible to use call getEntityWithChangeInfo on onDeletedEntity proxy!");
        }

        if (m_state.currentBlobWithChangeInfo == null)
        {
            boolean [] success = new boolean [1];
            java.nio.ByteBuffer [] currentBlobWithChangeInfo  = new java.nio.ByteBuffer[1];
            java.nio.ByteBuffer [] blobDeleter  = new java.nio.ByteBuffer[1];

            java.nio.ByteBuffer previous = m_state.previousState;
            if (previous == null){
                //this will cause a null to be sent down.
                previous = java.nio.ByteBuffer.allocate(0);;
            }

            java.nio.ByteBuffer current = m_state.currentState;
            if (current == null){
                //this will cause a null to be sent down.
                current = java.nio.ByteBuffer.allocate(0);;
            }


            Interface.Diff(previous,
                           current,
                           true, //wantCurrent
                           m_state.timestampDiff,
                           currentBlobWithChangeInfo,
                           blobDeleter,
                           success);

            if (!success[0]) {
                m_state.currentBlobWithChangeInfo = null;
                m_state.blobDeleter = null;
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
            }

            m_state.currentBlobWithChangeInfo = currentBlobWithChangeInfo[0];
            m_state.blobDeleter = blobDeleter[0];

        }
        return (Entity)com.saabgroup.safir.dob.typesystem.ObjectFactory.getInstance().createObject(m_state.currentBlobWithChangeInfo);
    }

    /**
     * Get owner handler id.
     *
     * Retrieves the handler id of the handler that owns (has created) this entity instance.
     *
     * @return Handler id.
     */
    public com.saabgroup.safir.dob.typesystem.HandlerId getOwner()
    {
        checkNotDisposed();
        long [] handlerId = new long[1];
        boolean [] success = new boolean [1];
        Interface.GetHandlerId(m_state.currentState, handlerId, success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        return new com.saabgroup.safir.dob.typesystem.HandlerId(handlerId[0]);
    }

    /**
     * Get info about the connection to which the owner handler is related.
     *
     * @return Connection info.
     */
    public ConnectionInfo getOwnerConnectionInfo()
    {
        checkNotDisposed();
        java.nio.ByteBuffer [] blob = new java.nio.ByteBuffer[1];
        java.nio.ByteBuffer [] blobDeleter = new java.nio.ByteBuffer[1];
        boolean [] success = new boolean [1];
        Interface.GetConnectionInfo(m_state.currentState, blob, blobDeleter, success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        try
        {
            return (ConnectionInfo)com.saabgroup.safir.dob.typesystem.ObjectFactory.getInstance().createObject(blob[0]);
        }
        finally
        {
            Interface.InvokeDeleter(blobDeleter[0],blob[0]);
        }
    }

    //
    // Retrieve binary blob
    //

    /**
     * Get binary blob of the received entity without changeflags set.
     *
     * This method will give you a pointer to the underlying representation of the object.
     * Note that this pointer is only valid while the EntityProxy is in scope.
     * If you want to keep the blob you must copy it using methods in com.saabgroup.safir.dob.typesystem.
     *
     * This method is mainly useful if all you want to do with a received object is to write it
     * to a database or pass it over a C-interface to a library or plugin.
     *
     * @return Binary blob of the received entity.
     */
    public java.nio.ByteBuffer getBlob()
    {
        checkNotDisposed();
        if (m_state.currentBlob == null)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Not possible to do getBlob on proxies from onDeletedEntity (entity = " +
                                                                                    getEntityId());
        }
        return m_state.currentBlob;
    }

    /**
     * Get binary blob with change information.
     *
     * Retrieves the entity with change flags set to indicate which members have
     * changed since the last subscription response.
     *
     * @see #getBlob
     *
     * @return Binary blob.
     */
    public java.nio.ByteBuffer getBlobWithChangeInfo()
    {
        checkNotDisposed();
        if (m_state.currentBlob == null)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException
                ("Not possible to do getBlobWithChangeInfo on proxies from onDeletedEntity (entity = " +
                 getEntityId());
        }
        if (m_state.currentBlobWithChangeInfo == null)
        {
            java.nio.ByteBuffer [] currentBlobWithChangeInfo  = new java.nio.ByteBuffer[1];
            java.nio.ByteBuffer [] blobDeleter  = new java.nio.ByteBuffer[1];
            boolean [] success = new boolean [1];

            java.nio.ByteBuffer previous = m_state.previousState;
            if (previous == null){
                //this will cause a null to be sent down.
                previous = java.nio.ByteBuffer.allocate(0);;
            }

            java.nio.ByteBuffer current = m_state.currentState;
            if (current == null){
                //this will cause a null to be sent down.
                current = java.nio.ByteBuffer.allocate(0);
            }

            Interface.Diff(previous,
                           current,
                           true, //wantCurrent
                           m_state.timestampDiff,
                           currentBlobWithChangeInfo,
                           blobDeleter,
                           success);

            if (!success[0]) {
                m_state.currentBlobWithChangeInfo = null;
                m_state.blobDeleter = null;
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
            }

            m_state.currentBlobWithChangeInfo = currentBlobWithChangeInfo[0];
            m_state.blobDeleter = blobDeleter[0];

        }
        return m_state.currentBlobWithChangeInfo;
    }
    //
    // Retrieve previous entity state
    //


    /**
     * Get previous entity state.
     *
     * Used to get the entity state that preceeded this state.
     *
     * Can be used when a "previous" state exists, that is, from within the following callbacks:
     * <ul>
     * <li>EntitySubscriber#onUpdatedEntity</li>
     * <li>EntitySubscriber#onDeletedEntity</li>
     * </ul>
     * No change flags will be set in the returned entity.
     *
     * Calling this function inside an onDeletedEntity when the subscription was set up with
     * includeUpdates set to false may yield an entity state that you have not received in an
     * onNewEntity callback. In fact it will most likely give you one of the updated entity
     * states that were filtered out because you didn't include updates.
     *
     * @return Previous entity.
     */
    public PreviousEntityProxy getPrevious()
    {
        checkNotDisposed();
        if (m_state.previousEntityProxy == null) {
            m_state.previousEntityProxy = new PreviousEntityProxy(m_state.currentBlob,m_state.currentState,m_state.previousBlob,m_state.previousState,m_state.timestampDiff);
        }
        return m_state.previousEntityProxy;
    }

    //
    // Trace and Debug stuff
    //

    /**
     * Get owner handler id that also contains the string representation.
     *
     * Mainly for trace and debug purposes.
     * @see #getOwner()
     *
     * @return Handler id.
     */
    public com.saabgroup.safir.dob.typesystem.HandlerId getOwnerWithStringRepresentation()
    {
        checkNotDisposed();
        return getOwner();
    }

    //
    // Retrieve timestamps. (Extended info for applications with special need)
    //

    /**
     * Retrieves the timestamp for the latest create, update or delete.
     *
     * @return Timestamp.
     *
     * Note that this operation is only valid for Injectable types.
     */
    public long getTimestamp()
    {
        checkNotDisposed();
        long [] timestamp = new long [1];
        boolean [] success = new boolean [1];
        Interface.GetTopTimestamp(m_state.currentState, timestamp, success);
        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
        return timestamp[0];
    }

    /**
     * Retrieves the timestamp for the given top member.
     *
     * Note that this operation is only valid for Injectable types.
     *
     * @param member Top level member index.
     * @return Timestamp.
     *
     */
    public long getTimestamp(int member)
    {
        checkNotDisposed();
        long [] timestamp = new long[1];
        boolean [] success = new boolean [1];
        Interface.GetMemberTimestamp(m_state.currentState, member, timestamp, success);
        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        return timestamp [0];
    }

    //
    // Private and internal stuff
    //

    /**
     * These objects can only be constructed by the Dob!
     */
    EntityProxy(java.nio.ByteBuffer currentBlob,
                java.nio.ByteBuffer currentState,
                java.nio.ByteBuffer previousBlob,
                java.nio.ByteBuffer previousState,
                boolean addReference,
                boolean timestampDiff)
    {
        m_state = new State(currentBlob,currentState,previousBlob,previousState,timestampDiff);
        m_cleanable = ResourceHelper.register(this,m_state);

        if (addReference)
        {
            Interface.addReference(currentState);
            Interface.addReference(previousState);
        }
    }

    /**
     * Drop the references into Dob shared memory that the proxy holds.
     */
    public void dispose()
    {
        m_state.dispose();
    }

    private void checkNotDisposed()
    {
        if (m_state.disposed)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Attempt to use an EntityProxy that is disposed!");
        }
    }


    private static class State implements Runnable {
        private boolean disposed = false;

        private final java.nio.ByteBuffer currentBlob;
        private final java.nio.ByteBuffer currentState;
        private final java.nio.ByteBuffer previousBlob;
        private final java.nio.ByteBuffer previousState;

        private java.nio.ByteBuffer currentBlobWithChangeInfo = null;
        private java.nio.ByteBuffer blobDeleter = null;

        private PreviousEntityProxy previousEntityProxy = null;

        private final boolean timestampDiff;

        public State(java.nio.ByteBuffer currentBlob_,
                     java.nio.ByteBuffer currentState_,
                     java.nio.ByteBuffer previousBlob_,
                     java.nio.ByteBuffer previousState_,
                     boolean timestampDiff_)
        {
            currentBlob = currentBlob_;
            currentState = currentState_;
            previousBlob = previousBlob_;
            previousState = previousState_;
            timestampDiff = timestampDiff_;
        }

        public void dispose() {
            if (!disposed)
            {
                disposed = true;
                if (previousEntityProxy != null) {
                    previousEntityProxy.dispose();
                    previousEntityProxy = null;
                }

                if (blobDeleter != null) {
                    Interface.InvokeDeleter(blobDeleter,currentBlobWithChangeInfo);
                }

                Interface.dropReference(currentState);
                Interface.dropReference(previousState);
            }
        }

        //-------------------
        //Clean up code
        //-------------------
        public void run() {
            try
            {
                if (!disposed)
                {
                    dispose();

                    com.saabgroup.safir.Logging.sendSystemLog
                        (com.saabgroup.safir.Logging.Severity.CRITICAL,
                         "Programming Error! An EntityProxy was not disposed correctly when Cleanable was called.");
                }
            }
            catch (Exception exc)
            {
                com.saabgroup.safir.Logging.sendSystemLog
                    (com.saabgroup.safir.Logging.Severity.CRITICAL,
                     "EntityProxy.State.run: Caught exception: " + exc);
            }

        }
    }

    private final State m_state;
    private final java.lang.ref.Cleaner.Cleanable m_cleanable;
}
