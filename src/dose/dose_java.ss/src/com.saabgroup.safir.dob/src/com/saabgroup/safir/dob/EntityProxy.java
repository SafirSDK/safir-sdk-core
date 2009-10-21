// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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
        if (m_currentBlob == null) {
            long [] typeId = new long[1];
            boolean [] success = new boolean [1];

            Interface.GetTypeId(m_currentState, typeId, success);

            if (!success[0]) {
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
            }
            return typeId[0];
        }
        else
        {
            return com.saabgroup.safir.dob.typesystem.BlobOperations.getTypeId(m_currentBlob);
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
        Interface.GetInstanceId(m_currentState, instanceId, success);
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
        if (m_currentBlob == null)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Not possible to use call Entity on OnDeletedEntity proxy!");
        }
        return (Entity)com.saabgroup.safir.dob.typesystem.ObjectFactory.getInstance().createObject(m_currentBlob);
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
        if (m_currentBlob == null)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Not possible to use call getEntityWithChangeInfo on onDeletedEntity proxy!");
        }

        if (m_currentBlobWithChangeInfo == null)
        {
            boolean [] success = new boolean [1];
            java.nio.ByteBuffer [] currentBlobWithChangeInfo  = new java.nio.ByteBuffer[1];
            java.nio.ByteBuffer [] blobDeleter  = new java.nio.ByteBuffer[1];

            java.nio.ByteBuffer previous = m_previousState;
            if (previous == null){
                //this will cause a null to be sent down.
                previous = java.nio.ByteBuffer.allocate(0);;
            }

            java.nio.ByteBuffer current = m_currentState;
            if (current == null){
                //this will cause a null to be sent down.
                current = java.nio.ByteBuffer.allocate(0);;
            }


            Interface.Diff(previous,
                           current,
                           true, //wantCurrent
                           m_timestampDiff,
                           currentBlobWithChangeInfo,
                           blobDeleter,
                           success);

            if (!success[0]) {
                m_currentBlobWithChangeInfo = null;
                m_blobDeleter = null;
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
            }

            m_currentBlobWithChangeInfo = currentBlobWithChangeInfo[0];
            m_blobDeleter = blobDeleter[0];

        }
        return (Entity)com.saabgroup.safir.dob.typesystem.ObjectFactory.getInstance().createObject(m_currentBlobWithChangeInfo);
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
        Interface.GetHandlerId(m_currentState, handlerId, success);

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
        Interface.GetConnectionInfo(m_currentState, blob, blobDeleter, success);

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
     * If you want to keep the blob you must copy it using methods in Safir::Dob::com.saabgroup.safir.dob.typesystem.
     *
     * This method is mainly useful if all you want to do with a received object is to write it
     * to a database or pass it over a C-interface to a library or plugin.
     *
     * @return Binary blob of the received entity.
     */
    public java.nio.ByteBuffer getBlob()
    {
        checkNotDisposed();
        if (m_currentBlob == null)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Not possible to do GetBlob on proxies from OnDeletedEntity (entity = " +
                                                                                    getEntityId());
        }
        return m_currentBlob;
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
        if (m_currentBlob == null)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException
                ("Not possible to do GetBlobWithChangeInfo on proxies from OnDeletedEntity (entity = " +
                 getEntityId());
        }
        if (m_currentBlobWithChangeInfo == null)
        {
            java.nio.ByteBuffer [] currentBlobWithChangeInfo  = new java.nio.ByteBuffer[1];
            java.nio.ByteBuffer [] blobDeleter  = new java.nio.ByteBuffer[1];
            boolean [] success = new boolean [1];

            java.nio.ByteBuffer previous = m_previousState;
            if (previous == null){
                //this will cause a null to be sent down.
                previous = java.nio.ByteBuffer.allocate(0);;
            }

            java.nio.ByteBuffer current = m_currentState;
            if (current == null){
                //this will cause a null to be sent down.
                current = java.nio.ByteBuffer.allocate(0);
            }

            Interface.Diff(previous,
                           current,
                           true, //wantCurrent
                           m_timestampDiff,
                           currentBlobWithChangeInfo,
                           blobDeleter,
                           success);

            if (!success[0]) {
                m_currentBlobWithChangeInfo = null;
                m_blobDeleter = null;
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
            }

            m_currentBlobWithChangeInfo = currentBlobWithChangeInfo[0];
            m_blobDeleter = blobDeleter[0];

        }
        return m_currentBlobWithChangeInfo;
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
     * @li EntitySubscriber#OnUpdatedEntity
     * @li EntitySubscriber#OnDeletedEntity
     *
     * No change flags will be set in the returned entity.
     *
     * @return Previous entity.
     */
    public PreviousEntityProxy getPrevious()
    {
        checkNotDisposed();
        if (m_previousEntityProxy == null) {
            m_previousEntityProxy = new PreviousEntityProxy(m_currentBlob,m_currentState,m_previousBlob,m_previousState,m_timestampDiff);
        }
        return m_previousEntityProxy;
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
        //TODO: try to obtain string representation
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
        Interface.GetTopTimestamp(m_currentState, timestamp, success);
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
        Interface.GetMemberTimestamp(m_currentState, member, timestamp, success);
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
        m_currentBlob = currentBlob;
        m_currentState = currentState;
        m_previousBlob = previousBlob;
        m_previousState = previousState;
        m_timestampDiff = timestampDiff;

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
        if (!disposed)
        {
            disposed = true;
            if (m_previousEntityProxy != null) {
                m_previousEntityProxy.dispose();
                m_previousEntityProxy = null;
            }

            if (m_blobDeleter != null)
            {
                Interface.InvokeDeleter(m_blobDeleter,m_currentBlobWithChangeInfo);
            }

            Interface.dropReference(m_currentState);
            Interface.dropReference(m_previousState);
        }
    }



    protected void finalize() throws java.lang.Throwable {
        try
        {
            if (!disposed)
            {
                dispose();

                System.out.println("An EntityProxy was not disposed correctly when the finalizer was called.\n" +
                                   "Make sure you call dispose() on the proxy when you are done with it!\n" +
                                   "(Use a try/finally clause, or you may get into trouble!)\n" +
                                   "The program will now exit!");

                PanicLogging.log("An EntityProxy was not disposed correctly when the finalizer was called.\n" +
                                 "Make sure you call dispose() on the proxy when you are done with it!\n" +
                                 "(Use a try/finally clause, or you may get into trouble!)\n" +
                                 "The program will now exit!");

                System.exit(123);
            }
        }
        catch (Exception exc)
        {
            System.out.println("EntityProxy.finalize: Caught exception: " + exc);
            System.out.println("Will return as if nothing happened!");
        }
        finally
        {
            super.finalize();
        }
    }


    private void checkNotDisposed()
    {
        if (disposed)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Attempt to use an EntityProxy that is disposed!");
        }
    }

    private boolean disposed = false;

    private final java.nio.ByteBuffer m_currentBlob;
    private final java.nio.ByteBuffer m_currentState;
    private final java.nio.ByteBuffer m_previousBlob;
    private final java.nio.ByteBuffer m_previousState;

    private java.nio.ByteBuffer m_currentBlobWithChangeInfo = null;
    private java.nio.ByteBuffer m_blobDeleter = null;

    private PreviousEntityProxy m_previousEntityProxy = null;

    private final boolean m_timestampDiff;
}

