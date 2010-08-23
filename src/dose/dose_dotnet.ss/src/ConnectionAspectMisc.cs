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

using System;

namespace Safir.Dob
{
    /// <summary>
    ///  Class that provides miscellaneous methods that are used less frequently.
    /// </summary>
    public sealed class ConnectionAspectMisc : ConnectionAspectBase
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="connection">The connection that you want to operate through</param>
        public ConnectionAspectMisc(ConnectionBase connection) : base(connection) { }

        /// <summary>
        /// Get info about which callback you are currently executing in.
        /// </summary>
        /// <returns>Id of the callback you are currently inside, or None if not in a callback.</returns>
        public CallbackId.Enumeration GetCurrentCallbackId()
        {
            byte success;
            System.Int32 callbackId;
            Interface.DoseC_GetCurrentCallbackId(ControllerId, out callbackId, out success);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return (Safir.Dob.CallbackId.Enumeration)callbackId;
        }

        #region Connection info
        /// <summary>
        /// Get the name for this connection used in the system.
        /// <para/>
        /// The connection name is composed of the name parts given by the application
        /// when opening the connection, with some additional decoration made by the DOB.
        /// </summary>
        /// <returns>The connection name.</returns>
        public string GetConnectionName()
        {
            byte success;
            System.IntPtr name;
            Interface.DoseC_GetConnectionName(ControllerId, out name, out success);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return Typesystem.Internal.InternalOperations.StringOf(name);

        }

        /// <summary>
        /// Get the common part of the connection name.
        /// </summary>
        /// <returns>The connection name common part specified when opening the connection.</returns>
        public string GetConnectionNameCommonPart()
        {
            byte success;
            System.IntPtr name;
            Interface.DoseC_GetConnectionNameCommonPart(ControllerId, out name, out success);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return Typesystem.Internal.InternalOperations.StringOf(name);
        }

        /// <summary>
        /// Get the instance part of the connection name.
        /// </summary>
        /// <returns>The connection name instance part specified when opening the connection.</returns>
        public string GetConnectionNameInstancePart()
        {
            byte success;
            System.IntPtr name;
            Interface.DoseC_GetConnectionNameInstancePart(ControllerId, out name, out success);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return Typesystem.Internal.InternalOperations.StringOf(name);
        }

        /// <summary>
        /// Get the context that the connection is opened in.
        /// </summary>
        /// <returns>Context</returns>
        public System.Int32 GetContext()
        {
            System.Int32 context;
            byte success;
            Interface.DoseC_GetContext(ControllerId, out context, out success);
            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return context;
        }
        #endregion

        #region Queue Status

        /// <summary>
        /// Get the capacity of the specified queue.
        /// <para/>
        /// This method returns the maximum number of items that the queue can hold.
        /// </summary>
        /// <param name="queue">The queue to get info for</param>
        /// <returns>The capacity of the queue</returns>
        public System.Int32 GetQueueCapacity(Safir.Dob.ConnectionQueueId.Enumeration queue)
        {
            System.Int32 queueCapacity;
            byte success;
            Interface.DoseC_GetQueueCapacity(ControllerId, (System.Int32)queue, out queueCapacity, out success);
            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return queueCapacity;
        }

        /// <summary>
        /// Get the number of items currently in the queue.
        /// <para/>
        /// This method returns the number of items that is currently in the specified queue.
        /// </summary>
        /// <param name="queue">The queue to get info for.</param>
        /// <returns>The current size of the queue.</returns>
        public System.Int32 GetQueueSize(Safir.Dob.ConnectionQueueId.Enumeration queue)
        {
            System.Int32 queueSize;
            byte success;
            Interface.DoseC_GetQueueSize(ControllerId, (System.Int32)queue, out queueSize, out success);
            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return queueSize;
        }
        #endregion

        #region Debug

        /// <summary>
        /// Turn simulation of overflow on/off. For test purposes.
        /// <para/>
        /// Setting inQueues to true means that no messages or requests are handled by the application.
        /// An incoming request will result in an overflow, and an incoming message will be discarded.
        /// Setting outQueues to true means that no messages or requests can be sent from the application,
        /// instead these calls will throw a Safir::Dob::OverflowException. When reset to false
        /// OnXxxxNotOverflow will be called as expected.
        /// Use this to verify that your application handles overflows correctly.
        /// <para/>
        /// Note that the inQueues flag is not applied to new consumers added after this call.
        /// </summary>
        /// <param name="inQueues">If true all incoming queues are simulated full.</param>
        /// <param name="outQueues">If true all outgoing queues are simulated full.</param>
        public void SimulateOverflows(bool inQueues, bool outQueues)
        {
            byte success;
            Interface.DoseC_SimulateOverflows(ControllerId,
                                              Interface.ByteOf(inQueues),
                                              Interface.ByteOf(outQueues),
                                              out success);
            if (!Interface.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        #endregion
    }
}
