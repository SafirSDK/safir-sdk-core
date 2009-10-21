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
 * Proxy class for a message
 */
public class MessageProxy {

    /**
     * Retrieves type id of the message.
     *
     * @return Type id.
     */
    public long getTypeId()
    {
        checkNotDisposed();
        return com.saabgroup.safir.dob.typesystem.BlobOperations.getTypeId(m_messageBlob);
    }

    /**
     * Get the message.
     *
     * @return Message
     */
    public Message getMessage()
    {
        checkNotDisposed();
        return (Message)com.saabgroup.safir.dob.typesystem.ObjectFactory.getInstance().createObject(m_messageBlob);
    }

    /**
     * Get info about the connection sending the message.
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
     * Retrieves the channel on which the message is sent.
     *
     * @return Channel id.
     */
    public com.saabgroup.safir.dob.typesystem.ChannelId getChannelId()
    {
        checkNotDisposed();
        long [] channelId = new long[1];
        boolean [] success = new boolean [1];
        Interface.GetChannelId(m_state, channelId, success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        return new com.saabgroup.safir.dob.typesystem.ChannelId(channelId[0]);
    }

    /**
     * Get binary blob of the received message.
     *
     * This method will give you a pointer to the underlying representation of the object.
     * Note that this pointer is only valid while the MessageProxy is in scope.
     * If you want to keep the blob you must copy it using methods in Safir::Dob::Typesystem.
     *
     * This method is mainly useful if all you want to do with a received object is to write it
     * to a database or pass it over a C-interface to a library or plugin.
     *
     * @return Binary blob of the received message.
     */
    public java.nio.ByteBuffer getBlob()
    {
        checkNotDisposed();
        return m_messageBlob;
    }

    /**
     * Get channel id that also contains the string representation.
     *
     * Mainly for trace and debug purposes.
     * @see #getChannelId()
     *
     * @return Channel id.
     */
    public com.saabgroup.safir.dob.typesystem.ChannelId getChannelIdWithStringRepresentation()
    {
        checkNotDisposed();
        return getChannelId();
        //TODO: try to obtain string representation
    }

    /**
     * These objects can only be constructed by the Dob!
     */
    MessageProxy(java.nio.ByteBuffer messageBlob, java.nio.ByteBuffer state)
    {
        m_messageBlob = messageBlob;
        m_state = state;
    }

    /**
     *
     */
    public void dispose() {
        if (!disposed) {
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
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Attempt to use a MessageProxy that is disposed! Please do not use a MessageProxy outside the onMessage callback!");
        }
    }

    private boolean disposed = false;

    java.nio.ByteBuffer m_messageBlob;
    java.nio.ByteBuffer m_state;
}
