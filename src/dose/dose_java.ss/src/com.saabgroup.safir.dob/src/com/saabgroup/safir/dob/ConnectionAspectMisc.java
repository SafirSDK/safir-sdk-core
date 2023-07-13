// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2008-2013, 2022-2023 (http://safirsdkcore.com)
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
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().Throw();
        }

        return CallbackId.values()[callbackId[0]];
    }

    //
    // Connection info
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
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().Throw();
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
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().Throw();
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
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().Throw();
        }
        return name[0];
    }

    /**
     * Get the context that the connection is opened in.
     *
     * @return The context of the connection
     */
    public int getContext()
    {
        int [] context = new int[1];
        boolean [] success = new boolean [1];
        Interface.GetContext(getControllerId(), context, success);
        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().Throw();
        }
        return context[0];
    }

    /**
     * Get the Node Identifier of the current node.
     *
     * Be aware that this identifier changes every time the node restarts.
     *
     * @return NodeId of current node
     */
    public long getNodeId()
    {
        long [] nodeId = new long[1];
        boolean [] success = new boolean [1];
        Interface.GetNodeId(nodeId, success);
        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().Throw();
        }
        return nodeId[0];
    }


    /**
     * Check whether the current node is a light node.
     *
     * This is slightly easier than looking through the parameters to work this out.
     *
     * @return true if the current node is a light node.
     */
    public boolean isLightNode()
    {
        boolean [] isLight = new boolean[1];
        boolean [] success = new boolean [1];
        Interface.IsLightNode(getControllerId(), isLight, success);
        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().Throw();
        }
        return isLight[0];
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
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().Throw();
        }
        return queueCapacity[0];
    }

    /**
     * Get the number of items currently in the queue.
     *
     * This method returns the number of items that is currently in the specified queue.
     * NOTE: This method is only implemented for out-queues (MessageOutQueue, RequestOutQueue)
     *       If this method is called for an in-queue, a Safir::Dob::SoftwareViolationException 
     *       will be thrown.
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
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().Throw();
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
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().Throw();
        }
    }

    //
    // Shared Memory statistics
    //

    /**
     * Get the number of currently used bytes in the shared memory.
     *
     * The size of the shared memory is defined by the parameter
     * Safir.Dob.NodeParameters.SharedMemorySize, which is defined in megabytes (1024*1024 bytes).
     *
     * Calling this function does not require the underlying Connection to have been opened.
     *
     * @return The amount of shared memory used, in bytes.
     */
    public long getSharedMemoryUsage()
    {
        boolean [] success = new boolean [1];
        long [] usage = new long [1];
        Interface.GetSharedMemoryUsage(usage,
                                       success);
        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().Throw();
        }
        return usage[0];
    }
}

