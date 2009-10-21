// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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
 * Class that provides miscellaneous methods that are used less frequently.
 *
 */
public class ConnectionAspectMisc
    extends ConnectionAspectBase
{
    /**
     * Constructor
     *
     * @param connection The connection that you want to operate through.
     */
    public ConnectionAspectMisc(ConnectionBase connection) {
        super(connection);
    }


    /**
     * Get info about which callback you are currently executing in.
     *
     * @return Id of the callback you are currently inside, or None if not in a callback.
     */
    public CallbackId getCurrentCallbackId()
    {
        boolean [] success = new boolean [1];
        int [] callbackId = new int [1];
        Interface.GetCurrentCallbackId(getControllerId(), callbackId, success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        return CallbackId.values()[callbackId[0]];
    }

    //
    // Connection name
    //


    /**
     * Get the name for this connection used in the system.
     *
     * The connection name is composed of the name parts given by the application
     * when opening the connection, with some additional decoration made by the DOB.
     *
     * @return The connection name.
     */
    public String getConnectionName()
    {
        boolean [] success = new boolean [1];
        String [] name = new String [1];
        Interface.GetConnectionName(getControllerId(), name, success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        return name[0];

    }

    /**
     * Get the common part of the connection name.
     *
     * @return The connection name common part specified when opening the connection.
     */
    public String getConnectionNameCommonPart()
    {
        boolean [] success = new boolean [1];
        String [] name = new String[1];
        Interface.GetConnectionNameCommonPart(getControllerId(), name, success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        return name[0];
    }

    /**
     * Get the instance part of the connection name.
     *
     * @return The connection name instance part specified when opening the connection.
     */
    public String getConnectionNameInstancePart()
    {
        boolean [] success = new boolean [1];
        String [] name = new String[1];
        Interface.GetConnectionNameInstancePart(getControllerId(), name, success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
        return name[0];
    }

    //
    // Queue Status
    //


    /**
     * Get the capacity of the specified queue.
     *
     * This method returns the maximum number of items that the queue can hold.
     *
     * @param queue The queue to get info for.
     * @return The capacity of the queue.
     */
    public int getQueueCapacity(com.saabgroup.safir.dob.ConnectionQueueId queue)
    {
        int [] queueCapacity = new int[1];
        boolean [] success = new boolean [1];
        Interface.GetQueueCapacity(getControllerId(), queue.ordinal(), queueCapacity, success);
        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
        return queueCapacity[0];
    }

    /**
     * Get the number of items currently in the queue.
     *
     * This method returns the number of items that is currently in the specified queue.
     *
     * @param queue [in] The queue to get info for.
     * @return The current size of the queue.
     */
    public int getQueueSize(com.saabgroup.safir.dob.ConnectionQueueId queue)
    {
        int [] queueSize = new int[1];
        boolean [] success = new boolean [1];
        Interface.GetQueueSize(getControllerId(), queue.ordinal(), queueSize, success);
        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
        return queueSize[0];
    }

    //
    // Debug
    //

    /**
     * Turn simulation of overflow on/off. For test purposes.
     *
     * Setting inQueues to true means that no messages or requests are handled by the application.
     * An incoming request will result in an overflow, and an incoming message will be discarded.
     * Setting outQueues to true means that no messages or requests can be sent from the application,
     * instead these calls will throw a Safir::Dob::OverflowException. When reset to false
     * OnXxxxNotOverflow will be called as expected.
     * Use this to verify that your application handles overflows correctly.
     *
     * Note that the inQueues flag is not applied to new consumers added after this call.
     *
     * @param inQueues If true all incoming queues are simulated full.
     * @param outQueues If true all outgoing queues are simulated full.
     */
    public void simulateOverflows(boolean inQueues, boolean outQueues)
    {
        boolean [] success = new boolean [1];
        Interface.SimulateOverflows(getControllerId(),
                                    inQueues,
                                    outQueues,
                                    success);
        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

}

