/******************************************************************************
*
* Copyright Saab AB, 2007-2013, 2022 (http://safirsdkcore.com)
*
* Created by: Anders Widén / stawi
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

#pragma once

#include <Safir/Dob/DoseCppExportDefs.h>
#include <Safir/Dob/ConnectionAspectBase.h>
#include <Safir/Dob/ConnectionBase.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/ConnectionQueueId.h>
#include <Safir/Dob/CallbackId.h>
#include <Safir/Dob/MemoryLevel.h>

#include <string>

namespace Safir
{
namespace Dob
{
    /**
     * Class that provides miscellaneous methods that are used less frequently.
     *
     */
    class DOSE_CPP_API ConnectionAspectMisc : public ConnectionAspectBase
    {
    public:
        /**
         * Constructor
         *
         * @param connection The connection that you want to operate through.
         */
        explicit ConnectionAspectMisc(const ConnectionBase& connection) : ConnectionAspectBase(connection) {}

        /**
         * Get info about which callback you are currently executing in.
         *
         * @return Id of the callback you are currently inside, or None if not in a callback.
         */
        Dob::CallbackId::Enumeration GetCurrentCallbackId() const;

        /**
         * @name Connection info
         */
        /** @{ */

        /**
         * Get the name for this connection used in the system.
         *
         * The connection name is composed of the name parts given by the application
         * when opening the connection, with some additional decoration made by the DOB.
         *
         * @return The connection name.
         */
        const std::wstring GetConnectionName() const;


        /**
         * Get the common part of the connection name.
         *
         * @return The connection name common part specified when opening the connection.
         */
        const std::wstring GetConnectionNameCommonPart() const;

        /**
         * Get the instance part of the connection name.
         *
         * @return The connection name instance part specified when opening the connection.
         */
        const std::wstring GetConnectionNameInstancePart() const;

        /**
         * Get the context that the connection is opened in.
         *
         * @return Context
         */
        Dob::Typesystem::Int32 GetContext() const;

        /**
         * Get the Node Identifier of the current node.
         *
         * Be aware that this identifier changes every time the node restarts.
         *
         * @return NodeId of current node
         */
        Dob::Typesystem::Int64 GetNodeId() const;

        /**
         * Check whether the current node is a light node.
         *
         * This is slightly easier than looking through the parameters to work this out.
         *
         * @return true if the current node is a light node.
         */
        bool IsLightNode() const;

        /** @} */

        /**
         * @name Queue Status
         */
        /** @{ */

        /**
         * Get the capacity of the specified queue.
         *
         * This method returns the maximum number of items that the queue can hold.
         *
         * @param queue [in] The queue to get info for.
         * @return The capacity of the queue.
         */
        Dob::Typesystem::Int32 GetQueueCapacity(const Dob::ConnectionQueueId::Enumeration queue) const;


        /**
         * Get the number of items currently in the queue.
         *
         * This method returns the number of items that is currently in the specified queue.
         * NOTE: This method is only implemented for out-queues (MessageOutQueue, RequestOutQueue)
         *       If this method is called for an in-queue, a SoftwareViolationException will be thrown.
         *
         * @param queue [in] The queue to get info for.
         * @return The current size of the queue.
         */
        Dob::Typesystem::Int32 GetQueueSize(const Dob::ConnectionQueueId::Enumeration queue) const;

        /** @} */

        /**
         * @name Debug
         */
        /** @{ */

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
         * @param [in] inQueues If true all incoming queues are simulated full.
         * @param [in] outQueues If true all outgoing queues are simulated full.
         */
         void SimulateOverflows(const bool inQueues, const bool outQueues) const;

        /** @} */

        /**
         * @name Shared Memory statistics
         */
        /** @{ */

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
         Typesystem::Int64 GetSharedMemoryUsage() const;

         /**
         * Get the usage level of the shared memory.
         *
         * When the memory level gets to Low or below that there will be operations that are
         * no longer allowed. See the User's Guide for more information.
         *
         * Calling this function does not require the underlying Connection to have been opened.
         *
         * @return The memory level.
         */
         Dob::MemoryLevel::Enumeration GetSharedMemoryLevel() const;

        /** @} */

    };

}
}

