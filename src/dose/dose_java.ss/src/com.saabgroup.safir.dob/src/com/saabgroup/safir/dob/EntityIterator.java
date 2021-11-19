// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009-2013, 2021 (http://safirsdkcore.com)
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
    implements java.util.Iterator<EntityProxy>, AutoCloseable {
    /** Iterator constructor is internal to dose_java, since only
     * Connections (and SecondaryConnections) can instantiate them.
     */
    EntityIterator(int ctrl,
                   long typeId,
                   boolean includeSubclasses){
        m_state = new State();
        m_cleanable = ResourceHelper.register(this,m_state);

        m_state.ctrl = ctrl;

        int [] iteratorId = new int [1];
        boolean [] success = new boolean [1];

        Interface.EntityIteratorCreate(ctrl,
                                       typeId,
                                       includeSubclasses,
                                       iteratorId,
                                       m_state.end,
                                       success);
        m_state.iteratorId = iteratorId[0];

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
        if (m_state.moveToNext) {
            moveToNext();
            m_state.moveToNext = false;
        }

        return !m_state.end[0];
    }

    /**
     * @see java.util.Iterator#next()
     */
    @Override
    public EntityProxy next() {
        checkNotDisposed();
        if (m_state.moveToNext) {
            moveToNext();
        }
        //next time we will want to move on.
        m_state.moveToNext = true;

        //have we gone beyond the end of the instances?
        if (m_state.end[0]) {
            throw new java.util.NoSuchElementException("There are no more instances to iterate over! Have you used hasNext correctly?");
        }

        java.nio.ByteBuffer [] entityBlob = new java.nio.ByteBuffer[1];
        java.nio.ByteBuffer [] entityState = new java.nio.ByteBuffer[1];
        boolean [] success = new boolean [1];
        Interface.EntityIteratorDereference(m_state.ctrl,
                                            m_state.iteratorId,
                                            entityBlob,
                                            entityState,
                                            success);
        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        if (m_state.current != null) {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Internal error in EntityIterator.next()! m_state.current was not null");
        }

        m_state.current = new EntityProxy(entityBlob[0].asReadOnlyBuffer(),entityState[0].asReadOnlyBuffer(),null,null,true,false);
        return m_state.current;
    }


    /**
     * @see java.util.Iterator#remove()
     */
    @Override
    public void remove() {
        throw new UnsupportedOperationException("EntityIterators do not support the remove operation");
    }

    private void moveToNext() {
        if (m_state.current != null) {
            m_state.current.dispose();
            m_state.current = null;
        }

        boolean [] success = new boolean [1];
        Interface.EntityIteratorIncrement(m_state.ctrl,
                                          m_state.iteratorId,
                                          m_state.end,
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
        m_state.dispose();
    }

    @Override
    public void close() {
        m_state.dispose();
    }

    private void checkNotDisposed()
    {
        if (m_state.disposed)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Attempt to use an EntityIterator that is disposed!");
        }
    }

    private static class State implements Runnable {
        public boolean disposed = false;
        public int ctrl;
        public int iteratorId;
        public boolean [] end = new boolean[1];
        public boolean moveToNext = false;
        public EntityProxy current = null;

        public void dispose() {
            if (!disposed) {
                disposed = true;

                if (current != null) {
                    current.dispose();
                }

                Interface.EntityIteratorDestroy(ctrl, iteratorId);
            }
        }

        /**
         * Clean up code
         */
        public void run() {
            try {
                if (!disposed) {
                    dispose();

                    com.saabgroup.safir.Logging.sendSystemLog
                        (com.saabgroup.safir.Logging.Severity.CRITICAL,
                         "Programming Error! An EntityIterator was not disposed correctly when cleanup was called!" +
                         "See the ConnectionBase#getEntityIterator(...).");
                }
            }
            catch (Exception exc) {
                com.saabgroup.safir.Logging.sendSystemLog
                    (com.saabgroup.safir.Logging.Severity.CRITICAL,
                     "EntityIterator.State.run: Caught exception: " + exc);
            }
        }
    }

    private final State m_state;
    private final java.lang.ref.Cleaner.Cleanable m_cleanable;
}
