// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009 (http://www.safirsdk.com)
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
 * An iterator that can be used to traverse entity instances.
 */
public class EntityIterator
    implements java.util.Iterator<EntityProxy> {
    /** Iterator constructor is internal to dose_java, since only
     * Connections (and SecondaryConnections) can instantiate them.
     */
    EntityIterator(int ctrl,
                   long typeId,
                   boolean includeSubclasses){
        m_ctrl = ctrl;

        int [] iteratorId = new int [1];
        boolean [] success = new boolean [1];

        Interface.EntityIteratorCreate(ctrl,
                                       typeId,
                                       includeSubclasses,
                                       iteratorId,
                                       m_end,
                                       success);
        m_iteratorId = iteratorId[0];

        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    /**
     * @see java.util.Iterator#hasNext()
     */
    @Override
    public boolean hasNext() {
        checkNotDisposed();
        if (m_moveToNext) {
            moveToNext();
            m_moveToNext = false;
        }

        return !m_end[0];
    }

    /**
     * @see java.util.Iterator#next()
     */
    @Override
    public EntityProxy next() {
        checkNotDisposed();
        if (m_moveToNext) {
            moveToNext();
        }
        //next time we will want to move on.
        m_moveToNext = true;

        //have we gone beyond the end of the instances?
        if (m_end[0]) {
            throw new java.util.NoSuchElementException("There are no more instances to iterate over! Have you used hasNext correctly?");
        }

        java.nio.ByteBuffer [] entityBlob = new java.nio.ByteBuffer[1];
        java.nio.ByteBuffer [] entityState = new java.nio.ByteBuffer[1];
        boolean [] success = new boolean [1];
        Interface.EntityIteratorDereference(m_ctrl,
                                            m_iteratorId,
                                            entityBlob,
                                            entityState,
                                            success);
        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        if (m_current != null) {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Internal error in EntityIterator.next()! m_current was not null");
        }

        m_current = new EntityProxy(entityBlob[0].asReadOnlyBuffer(),entityState[0].asReadOnlyBuffer(),null,null,true,false);
        return m_current;
    }


    /**
     * @see java.util.Iterator#remove()
     */
    @Override
    public void remove() {
        throw new UnsupportedOperationException("EntityIterators do not support the remove operation");
    }

    private void moveToNext() {
        if (m_current != null) {
            m_current.dispose();
            m_current = null;
        }

        boolean [] success = new boolean [1];
        Interface.EntityIteratorIncrement(m_ctrl,
                                          m_iteratorId,
                                          m_end,
                                          success);
        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    /**
     * Release resources held by this iterator.
     */
    public void dispose() {
        if (!m_disposed) {
            m_disposed = true;

            if (m_current != null) {
                m_current.dispose();
            }

            Interface.EntityIteratorDestroy(m_ctrl,m_iteratorId);
        }
    }

    @Override
    protected void finalize() throws Throwable {
        try {
            if (!m_disposed) {
                dispose();
                
                com.saabgroup.safir.Logging.sendSystemLog
                    (com.saabgroup.safir.Logging.Severity.CRITICAL,
                     "Programming Error! An EntityIterator was not disposed correctly when the finalizer was called!" + 
                     "See the ConnectionBase#getEntityIterator(...).");
            }
        }
        catch (Exception exc) {
            com.saabgroup.safir.Logging.sendSystemLog
                (com.saabgroup.safir.Logging.Severity.CRITICAL,
                 "EntityIterator.finalize: Caught exception: " + exc);
        }
        finally {
            super.finalize();
        }
    }

    private void checkNotDisposed()
    {
        if (m_disposed)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Attempt to use an EntityIterator that is disposed!");
        }
    }


    private int m_ctrl;
    private int m_iteratorId;
    private boolean [] m_end = new boolean[1];
    private boolean m_moveToNext = false;
    private EntityProxy m_current = null;
    private boolean m_disposed = false;
}
