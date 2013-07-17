/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#ifndef _SAFIR_DOB_CONNECTION_ASPECT_MISC_H
#define _SAFIR_DOB_CONNECTION_ASPECT_MISC_H

#include <Safir/Dob/DoseCppExportDefs.h>
#include <Safir/Dob/ConnectionAspectBase.h>
#include <Safir/Dob/ConnectionBase.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/ConnectionQueueId.h>
#include <Safir/Dob/CallbackId.h>

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
        const Dob::CallbackId::Enumeration GetCurrentCallbackId() const;

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
    };

}
}

#endif
