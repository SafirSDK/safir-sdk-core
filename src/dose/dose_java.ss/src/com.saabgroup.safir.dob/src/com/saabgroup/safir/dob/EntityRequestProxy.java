// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
 * Proxy class for an entity request.
 */
public class EntityRequestProxy {
    /**
     * Get type id.
     *
     * Retrieves type id of the entity request.
     *
     * @return Type id.
     */
    public long getTypeId() {
        checkNotDisposed();
        if (m_requestBlob == null)
        {
            long [] typeId = new long [1];
            boolean [] success = new boolean [1];
            Interface.GetTypeId(m_state, typeId, success);

            if (!success[0]) {
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
            }

            return typeId[0];
        }
        else
        {
            return com.saabgroup.safir.dob.typesystem.BlobOperations.getTypeId(m_requestBlob);
        }
    }

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
    public com.saabgroup.safir.dob.typesystem.InstanceId getInstanceId() {
        checkNotDisposed();
        long [] instanceId = new long [1];
        boolean [] success = new boolean [1];
        Interface.GetInstanceId(m_state, instanceId, success);

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
     * Note that it is illegal to call this method on proxies received in OnCreateRequest
     * callbacks if the handler is registered as "HandlerDecidesInstanceId".
     * This is because there is no instance id in the request in this case...
     *
     * @return Entity id.
     */
    public com.saabgroup.safir.dob.typesystem.EntityId getEntityId(){
        checkNotDisposed();
        return new com.saabgroup.safir.dob.typesystem.EntityId(getTypeId(), getInstanceId());
    }


    /**
     * Get entity request.
     *
     * Note that it is not valid to call this for a DeleteRequest.
     *
     * @return Entity request
     */
    public Entity getRequest() {
        checkNotDisposed();
        return (Entity)com.saabgroup.safir.dob.typesystem.ObjectFactory.getInstance().createObject(m_requestBlob);
    }

    /**
     * Get info about the connection sending the request.
     *
     * @return Connection info.
     */
    public ConnectionInfo getSenderConnectionInfo() {
        checkNotDisposed();

        java.nio.ByteBuffer [] blob = new java.nio.ByteBuffer[1];
        java.nio.ByteBuffer [] blobDeleter = new java.nio.ByteBuffer[1];
        boolean [] success = new boolean [1];

        Interface.GetConnectionInfo(m_state, blob, blobDeleter, success);

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


    /**
     * Get id of receiving handler.
     *
     * Can be handy when one consumer is used for several handlers.
     *
     * @return Handler id.
     */
    public com.saabgroup.safir.dob.typesystem.HandlerId getReceivingHandlerId() {
        checkNotDisposed();
        long [] handlerId = new long[1];
        boolean [] success = new boolean [1];
        Interface.GetHandlerId(m_state, handlerId, success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        return new com.saabgroup.safir.dob.typesystem.HandlerId(handlerId[0]);
    }

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
     * like this "v = std::vector<char>(blob,blob+Safir::Dob::Typesystem::BlobOperations.GetSize())"
     *
     * @return Binary blob of the received entity request.
     */
    public java.nio.ByteBuffer getBlob() {
        checkNotDisposed();
        if (m_requestBlob == null)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("No blob available on DeleteRequests!");
        }
        return m_requestBlob;
    }

    //
    // Trace and Debug stuff
    //

        /**
         * Get receiver handler id that also contains the string representation.
         *
         * Mainly for trace and debug purposes.
         * @see #getReceivingHandlerId()
         *
         * @return Handler id.
         */
    public com.saabgroup.safir.dob.typesystem.HandlerId getReceiverWithStringRepresentation() {
        checkNotDisposed();
        return getReceivingHandlerId();
    }


    //
    // Private and internal stuff
    //

    /**
     * These objects can only be constructed by the Dob!
     */
    EntityRequestProxy(java.nio.ByteBuffer requestBlob, java.nio.ByteBuffer state)
    {
        m_requestBlob = requestBlob;
        m_state = state;
    }

    /**
     * Drop the references into Dob shared memory that the proxy holds.
     */
    public void dispose()
    {
        if (!disposed)
        {
            disposed = true;
        }
    }

    protected void finalize() throws java.lang.Throwable {
        try
        {
            dispose();
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
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Attempt to use a EntityRequestProxy that is disposed! Please do not use a EntityRequestProxy outside the on*Request callbacks!");
        }
    }


    private boolean disposed = false;

    java.nio.ByteBuffer m_requestBlob;
    java.nio.ByteBuffer m_state;
}

