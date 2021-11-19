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
 * A connection to the DOB.
 *
 * This class represents a "real" (as opposed to SecondaryConnection) connection to the dob.
 * Each DOB application must have at least one connection. Connections are not thread safe.
 */
public final class Connection
    extends ConnectionBase
    implements AutoCloseable
{
    /** Constructor.
     */
    public Connection()
    {
        m_state = new State();
        m_cleanable = ResourceHelper.register(this,m_state);

        int [] ctrl = new int[1];
        boolean [] success = new boolean [1];
        Interface.Constructor(ctrl, success);
        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
        m_state.ctrl = ctrl[0];
    }
    /**
     * Open a connection to the DOB.
     *
     * The connection uses the OnDoDispatch callback to signal that there is incoming data available.
     * When OnDoDispatch is called the application shall set an event or similar and then call
     * Dispatch() in this class from the thread that owns (has called Open)
     * the connection.
     *
     * There can be a number of contexts in the DOB. A connection is linked to the context specified in Open.
     * All operations using a connection is affecting only the context linked to that connection.
     * The intended primary usage is for recording/replay functionality. 0 is defined as the default
     * context.
     *
     * Note that connectionNameCommonPart together with connectionNameInstancePart must be unique
     * in the node.
     *
     * If NULL  is passed as the stopHandler argument the connection will not receive a stop order.
     * Normally only the main thread of an application should pass a non-NULL stopHandler, and it
     * should then tell other parts of the application to exit. If multiple stop handlers are specified
     * there is NO guaranteed order between which gets called first when a process receives a stop signal.
     *
     * @param connectionNameCommonPart Name that identifies the program but not any particular
     *                                        program instance.
     * @param connectionNameInstancePart Name that identifies a particular program instance.
     * @param context Context functionality not implemented yet!
     * @param stopHandler Object that implements the StopHandler interface.
     * @param dispatcher Object that implements the Dispatcher interface.
     *
     * @throws NotOpenException The connection name is already used by someone else.
     *                                      Try another!
     */
    public void open(String connectionNameCommonPart,
                     String connectionNameInstancePart,
                     int context,
                     StopHandler stopHandler,
                     Dispatcher dispatcher)
    {
        // This check guarantees that there will be no call to DoseC_Connect if the connection is already opened.
        // This solves the problem with dropping the incremented refrences in case of a NOP.
        if (isOpen())
        {
            return;
        }

        boolean [] success = new boolean [1];
        Interface.Connect(m_state.ctrl,
                          connectionNameCommonPart,
                          connectionNameInstancePart,
                          context,
                          stopHandler,
                          dispatcher,
                          success);
        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

    }

    /**
     * Close the connection to the DOB.
     *
     * Closes the connection to the DOB and deallocates all resources. All subscriptions
     * and registrations will automatically be deleted and there is no need to call
     * Unsubscribe and Unregister before calling Close.
     * Note that all connections that were set up using Attach will also be closed after
     * a call to this method.
     */
    @Override
    public void close()
    {
        m_state.close();
    }

    /**
     * Check if this Connection instance is open.
     *
     * @return True if the connection is open, otherwise false.
     */
    public boolean isOpen()
    {
        boolean [] isConn = new boolean [1];
        boolean [] success = new boolean [1];
        Interface.IsConnected(m_state.ctrl, isConn, success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
        return isConn[0];
    }

    /**
     * When the dispatch event or callback is signalled, the application MUST call
     * this method. A call to Dispatch will result in that all queues for this connection
     * are emptied and that each message in the queues are passed to the associated
     * consumer.
     * Calls to dispatch from connection instances that are not open will be ignored.
     */
    public void dispatch()
    {
        boolean [] success = new boolean [1];
        Interface.Dispatch(m_state.ctrl, success);
        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }



    //-------------------------------------
    // Private
    //-------------------------------------

    int getControllerId() {
        return m_state.ctrl;
    }


    private static class State implements Runnable {
        public int ctrl = -1;

        public void close() {
            boolean [] success = new boolean [1];
            Interface.Disconnect(ctrl, success);

            if (!success[0]) {
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
            }
        }
        //-------------------
        //Clean up code
        //-------------------
        public void run() {
            try
            {
                close();
            }
            catch (Exception exc)
            {
                com.saabgroup.safir.Logging.sendSystemLog
                    (com.saabgroup.safir.Logging.Severity.CRITICAL,
                     "Connection.State.run: Caught exception: " + exc);
            }

            Interface.Destructor(ctrl);
        }
    }

    private final State m_state;
    private final java.lang.ref.Cleaner.Cleanable m_cleanable;

}
