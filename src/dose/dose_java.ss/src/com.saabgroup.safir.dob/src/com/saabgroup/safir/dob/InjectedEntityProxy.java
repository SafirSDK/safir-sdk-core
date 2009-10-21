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
 * Proxy class for entity injections.
 */
public class InjectedEntityProxy {

    /**
     * Get type id.
     *
     * Retrieves type id of the Entity that is about to be injected.
     *
     * @return Type id.
     */
    public long getTypeId() {
        checkNotDisposed();
        if (m_injectionBlob == null)
        {
            long [] typeId = new long[1];
            boolean [] success = new boolean [1];
            Interface.GetTypeId(m_injectionState, typeId, success);

            if (!success[0]) {
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
            }

            return typeId[0];
        }
        else
        {
            return com.saabgroup.safir.dob.typesystem.BlobOperations.getTypeId(m_injectionBlob);
        }
    }


    /**
     * Get instance id.
     *
     * Retrieves instance id of the Entity that is about to be injected.
     *
     * @return Instance id.
     */
    public com.saabgroup.safir.dob.typesystem.InstanceId getInstanceId() {
        checkNotDisposed();
        long [] instanceId = new long[1];
        boolean [] success = new boolean [1];
        Interface.GetInstanceId(m_injectionState, instanceId, success);

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
    public com.saabgroup.safir.dob.typesystem.EntityId getEntityId() {
        checkNotDisposed();
        return new com.saabgroup.safir.dob.typesystem.EntityId(getTypeId(), getInstanceId());
    }

    /**
     * Get the entity state that is about to be injected.
     *
     * Change flags will be set in the entity to indicate which members
     * are part of the injection.
     *
     * Note that this method cannot be called in an OnInjectedDeletedEntity,
     * since there then is no entity to get...
     *
     * @return entity.
     */
    public Entity getInjection() {
        checkNotDisposed();
        if (m_injectionBlob == null)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Not possible to use call Injection on InjectDeletes proxy!");
        }
        return (Entity)com.saabgroup.safir.dob.typesystem.ObjectFactory.getInstance().createObject(m_injectionBlob);

    }

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
     * Change flags will be set in the entity to indicate which members
     * are part of the injection.
     *
     * @return Binary blob of the entity that is about to be injected.
     */
    public java.nio.ByteBuffer getInjectionBlob() {
        checkNotDisposed();
        if (m_injectionBlob == null)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Not possible to do Blob on proxies from OnDeletedEntity (entity = " +
                                                                                    getEntityId());
        }
        return m_injectionBlob;
    }


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
    public Entity getCurrent() {
        checkNotDisposed();
        if (m_currentBlob == null)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Not possible to use call Current on InjectDeletes proxy!");
        }
        return (Entity)com.saabgroup.safir.dob.typesystem.ObjectFactory.getInstance().createObject(m_currentBlob);
    }

    //
    // Private and internal stuff
    //

    /**
     * These objects can only be constructed by the Dob!
     */
    InjectedEntityProxy(java.nio.ByteBuffer injectionBlob,
                        java.nio.ByteBuffer injectionState,
                        java.nio.ByteBuffer currentBlob,
                        java.nio.ByteBuffer currentState)
    {
        m_injectionBlob = injectionBlob;
        m_injectionState = injectionState;
        m_currentBlob = currentBlob;
        m_currentState = currentState;
    }

    /**
     * Drop the references into Dob shared memory that the proxy holds.
     */
    public void dispose() {
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
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Attempt to use an InjectedEntityProxy that is disposed! Please do not use an InjectedEntityProxy outside the onInjected*Entity callbacks!");
        }
    }


    private boolean disposed = false;

    private java.nio.ByteBuffer m_injectionBlob;
    private java.nio.ByteBuffer m_injectionState;
    private java.nio.ByteBuffer m_currentBlob;

  //we're currently not using this for anything, but it is here for completeness sake
    @SuppressWarnings("unused")
    private java.nio.ByteBuffer m_currentState;

}

