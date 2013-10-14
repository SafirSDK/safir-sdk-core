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
 * Proxy class for a response.
 */
public class ResponseProxy {
    /**
     * Get response success or failure status
     *
     * @return Success or failure.
     */
    public boolean isSuccess() {
        checkNotDisposed();
        return com.saabgroup.safir.dob.typesystem.Operations.isOfType(getTypeId(), com.saabgroup.safir.dob.SuccessResponse.ClassTypeId);
    }

    /**
     * Get type id.
     *
     * Retrieves type id of the response.
     *
     * @return Type id.
     */
    public long getTypeId() {
        checkNotDisposed();
        return com.saabgroup.safir.dob.typesystem.BlobOperations.getTypeId(m_responseBlob);
    }

    /**
     * Get response.
     *
     * @return Service request
     */
    public Response getResponse() {
        checkNotDisposed();
        return (Response)com.saabgroup.safir.dob.typesystem.ObjectFactory.getInstance().createObject(m_responseBlob);
    }

    /**
     * Get info about the connection sending the response.
     *
     * @return Connection info.
     */
    public ConnectionInfo getResponseSenderConnectionInfo() {
        checkNotDisposed();

        java.nio.ByteBuffer [] blob = new java.nio.ByteBuffer[1];
        java.nio.ByteBuffer [] blobDeleter = new java.nio.ByteBuffer[1];
        boolean [] success = new boolean [1];

        Interface.GetConnectionInfo(m_responseState, blob, blobDeleter, success);

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
     * Get binary blob of the received response.
     *
     * This method will give you a pointer to the underlying representation of the object.
     * Note that this pointer is only valid while the ResponseProxy is in scope.
     * If you want to keep the blob you must copy it using methods in Safir::Dob::Typesystem.
     *
     * This method is mainly useful if all you want to do with a received object is to write it
     * to a database or pass it over a C-interface to a library or plugin.
     *
     * @return Binary blob of the received response.
     */
    public java.nio.ByteBuffer getBlob() {
        checkNotDisposed();
        return m_responseBlob;
    }

    //
    // Methods to retrieve info about the original request.
    //

    /**
     * Get request id.
     *
     * Retrieves the request id generated when the request was sent.
     *
     * @return Request id.
     */
    public int getRequestId() {
        checkNotDisposed();
        return m_requestId;
    }

    /**
     * Get type id of the entity or service sent in the original request.
     *
     * @return Type id.
     */
    public long getRequestTypeId() {
        checkNotDisposed();

        boolean [] success = new boolean [1];
        long [] typeId = new long[1];
        Interface.GetTypeId(m_requestState, typeId, success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        return typeId[0];
    }

    /**
     * Get the instance id used in the original request. (Only for entity requests)
     *
     * @return Instance id.
     */
    public com.saabgroup.safir.dob.typesystem.InstanceId getRequestInstanceId() {
        checkNotDisposed();
        long [] instanceId = new long[1];
        boolean [] success = new boolean [1];
        Interface.GetInstanceId(m_requestState, instanceId, success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        return new com.saabgroup.safir.dob.typesystem.InstanceId(instanceId[0]);
    }

    /**
     * Get the original request.
     *
     * Retrieves the original request. Depending on the type of request this
     * can be a Dob::EntityPtr or a Dob::ServicePtr.
     *
     * @return Original request.
     */
    public com.saabgroup.safir.dob.typesystem.Object getRequest() {
        checkNotDisposed();
        if (m_requestBlob == null)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Cannot get Request on ResponseProxies for DeleteRequests");
        }
        return com.saabgroup.safir.dob.typesystem.ObjectFactory.getInstance().createObject(m_requestBlob);
    }


    /**
     * Get the original request blob.
     *
     * Retrieves the blob of the original request.
     * If the request was a delete request null will be returned.
     *
     * This method will give you a pointer to the underlying representation of the object.
     * Note that this pointer is only valid while the ResponseProxy is in scope.
     * If you want to keep the blob you must copy it using methods in Safir::Dob::Typesystem.
     *
     * This method is mainly useful if all you want to do with a received object is to write it
     * to a database or pass it over a C-interface to a library or plugin.
     *
     * @return Original request blob.
     */
    public java.nio.ByteBuffer getRequestBlob() {
        checkNotDisposed();
        return m_requestBlob;
    }

    /**
     * Get the handler id to which the original request was sent.
     *
     * @return Handler id.
     */
    public com.saabgroup.safir.dob.typesystem.HandlerId getRequestHandlerId() {
        checkNotDisposed();
        long [] handlerId = new long [1];
        boolean [] success = new boolean [1];
        Interface.GetHandlerId(m_requestState, handlerId, success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        return new com.saabgroup.safir.dob.typesystem.HandlerId(handlerId[0]);
    }

    //
    // Private and internal stuff
    //

    /**
     * These objects can only be constructed by the Dob!
     */
    ResponseProxy(int requestId,
                  java.nio.ByteBuffer responseBlob,
                  java.nio.ByteBuffer responseState,
                  java.nio.ByteBuffer requestBlob,
                  java.nio.ByteBuffer requestState)
    {
        m_requestId = requestId;
        m_responseBlob = responseBlob;
        m_responseState = responseState;
        m_requestBlob = requestBlob;
        m_requestState = requestState;
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
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Attempt to use a ResponseProxy that is disposed! Please do not use a ResponseProxy outside the onResponse callback!");
        }
    }


    private boolean disposed = false;

    private int m_requestId;
    private java.nio.ByteBuffer m_responseBlob;
    private java.nio.ByteBuffer m_responseState;
    private java.nio.ByteBuffer m_requestBlob;
    private java.nio.ByteBuffer m_requestState;

}
