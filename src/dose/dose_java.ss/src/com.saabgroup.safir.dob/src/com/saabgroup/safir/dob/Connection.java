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
 * A connection to the DOB.
 *
 * This class represents a "real" (as opposed to SecondaryConnection) connection to the dob.
 * Each DOB application must have at least one connection. Connections are not thread safe.
 */
public final class Connection extends ConnectionBase
{
    /** Constructor.
     */
    public Connection()
    {
        int [] ctrl = new int[1];
        boolean [] success = new boolean [1];
        Interface.Constructor(ctrl, success);
        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
        m_ctrl = ctrl[0];
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
        Interface.Connect(m_ctrl,
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
    public void close()
    {
        close(true);
    }

    /**
     * Internal Close function that allows for enabling/disabling of the thread check.
     * @param checkThread Whether to check which thread the call is being made from.
     */
    private void close(boolean checkThread)
    {
        boolean [] success = new boolean [1];
        Interface.Disconnect(m_ctrl, checkThread, success);


        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
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
        Interface.IsConnected(m_ctrl, isConn, success);

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
        Interface.Dispatch(m_ctrl, success);
        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }



    //-------------------------------------
    // Private
    //-------------------------------------

    int getControllerId() {
        return m_ctrl;
    }

    private int m_ctrl = -1;

    //-------------------
    //Clean up code
    //-------------------
    protected void finalize() throws java.lang.Throwable
    {
        try
        {
            try
            {
                close(false);
            }
            catch (Exception exc)
            {
                com.saabgroup.safir.Logging.sendSystemLog
                    (com.saabgroup.safir.Logging.Severity.CRITICAL,
                     "Connection.finalize: Caught exception: " + exc);
            }
            Interface.Destructor(getControllerId());
        }
        finally
        {
            super.finalize();
        }
    }
}
