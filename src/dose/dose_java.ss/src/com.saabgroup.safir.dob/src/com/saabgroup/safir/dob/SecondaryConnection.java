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
 * A secondary connection attached to a "real" connection.
 *
 * This class is used to attach yourself to an existing connection in the
 * same thread.
 * All attach calls ensure that you will get a connection that is valid in the current thread, but
 * each SecondaryConnection must still only be used from within one thread.
 */
public class SecondaryConnection extends ConnectionBase
{
    /** Constructor.
     */
    public SecondaryConnection()
    {

    }

    /**
     * Attach to a connection in this thread.
     *
     * This method will attach the SecondaryConnection to the first Connection that was
     * opened in this thread.
     *
     * This method can be used to let part of a program, for example a module or a dll, attach to an
     * existing open connection.
     *
     * @throws NotOpenException There is no open Connection in this thread.
     */
    public void attach()
    {
        if (isAttached()) //already attached!
        {
            detach();
        }

        attach("", "");
    }

    /**
     * Attach to a named connection in this thread.
     *
     * This method will attach the SecondaryConnection to the named Connection if that
     * Connection was opened in this thread.
     *
     * This method can be used to let part of a program, for example a module or a dll, attach to an
     * existing open connection.
     * The connection name parameters are used to identify the connection to attach to.
     * This connection must already be opened, otherwise an exception will be
     * thrown.
     *
     * @param connectionNameCommonPart Name that identifies the connection but not any particular
     *                                        instance.
     * @param connectionNameInstancePart Name that identifies a particular connection instance.
     *
     * @throws NotOpenException If the Connection instance we are trying to
     *                                      attach to is not open.
     */
    public void attach(String connectionNameCommonPart,
                       String connectionNameInstancePart)
    {
        if (isAttached()) //already attached!
        {
            detach();
        }
        boolean [] success = new boolean [1];
        int [] newCtrlId = new int [1];
        Interface.ConnectSecondary(connectionNameCommonPart,
                                   connectionNameInstancePart,
                                   newCtrlId,
                                   success);
        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        m_ctrl=newCtrlId[0];
    }

    /**
     * Detach a SecondaryConnection.
     *
     * When a connection has been detached it can be attached again.
     */
    public void detach()
    {
        m_ctrl = -1;
    }

    /**
     * Check if a SecondaryConnection is attached to an open connection.
     *
     * @return True if the SecondaryConnection is attached to a Connection and that Connection is open.
     */
    public boolean isAttached()
    {
        if (m_ctrl < 0)
        {
            return false;
        }

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
     * For a secondary connection this is the same as the IsAttached check.
     *
     * @return True if the SecondaryConnection is attached to a Connection and that Connection is open.
     */
    public boolean isOpen() {
        return isAttached();
    }



    //-------------------------------------
    // Private
    //-------------------------------------
    int getControllerId() {
        if (!isAttached())
        {
            throw new com.saabgroup.safir.dob.NotOpenException("This SecondaryConnection is not attached!");
        }
        return m_ctrl;
    }

    private int m_ctrl = -1;
}

