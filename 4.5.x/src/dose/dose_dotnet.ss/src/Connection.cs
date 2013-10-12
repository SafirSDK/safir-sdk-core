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

using System;
using System.Runtime.InteropServices;

namespace Safir.Dob
{
    /// <summary>
    /// A connection to the DOB.
    /// 
    /// This class represents a "real" (as opposed to SecondaryConnection) connection to the dob.
    /// Each DOB application must have at least one connection. Connections are not thread safe.
    /// </summary>
    public sealed class Connection : ConnectionBase, IDisposable
    {
        /// <summary>
        /// Constructor
        /// </summary>
        public Connection()
        {
            byte success;
            Interface.DoseC_Constructor(out m_ctrl, out success);
            if (!Interface.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Open a connection to the DOB.
        /// <para/>
        /// The connection uses the OnDoDispatch callback to signal that there is incoming data available.
        /// When OnDoDispatch is called the application shall set an event or similar and then call
        /// Dispatch() in this class from the thread that owns (has called Open)
        /// the connection.
        /// <para/>
        /// There can be a number of contexts in the DOB. A connection is linked to the context specified in Open.
        /// All operations using a connection is affecting only the context linked to that connection.
        /// The intended primary usage is for recording/replay functionality. 0 is defined as the default
        /// context.
        /// <para/>
        /// Note that connectionNameCommonPart together with connectionNameInstancePart must be unique
        /// in the node.
        /// <para/>
        /// If null is passed as the stopHandler argument the connection will not receive a stop order.
        /// Normally only the main thread of an application should pass a non-null stopHandler, and it
        /// should then tell other parts of the application to exit. If multiple stop handlers are specified
        /// there is NO guaranteed order between which gets called first when a process receives a stop signal.
        /// <para/>
        /// </summary>
        /// <param name="connectionNameCommonPart">Name that identifies the program but not any particular
        ///                                        program instance.</param>
        /// <param name="connectionNameInstancePart">Name that identifies a particular program instance.</param>
        /// <param name="context">Context functionality not implemented yet!</param>
        /// <param name="stopHandler">Object that implements the StopHandler interface.</param>
        /// <param name="dispatcher">Object that implements the Dispatcher interface.</param>
        /// <exception cref="Safir.Dob.NotOpenException">The connection name is already used by someone else.
        ///                                              Try another!</exception>
        public void Open(string connectionNameCommonPart,
                         string connectionNameInstancePart,
                         System.Int32 context,
                         StopHandler stopHandler,
                         Dispatcher dispatcher)
        {
            // This check guarantees that there will be no call to DoseC_Connect if the connection is already opened.
            // This solves the problem with dropping the incremented refrences in case of a NOP.
            if (IsOpen())
            {
                return;
            }

            System.IntPtr nameCommonPart = Dob.Typesystem.Internal.InternalOperations.CStringOf(connectionNameCommonPart);
            System.IntPtr nameInstancePart = Dob.Typesystem.Internal.InternalOperations.CStringOf(connectionNameInstancePart);

            byte success;

            Interface.DoseC_Connect(m_ctrl,
                                    nameCommonPart,
                                    nameInstancePart,
                                    context,
                                    Interface.DOSE_LANGUAGE_DOTNET,
                                    ConsumerHandler.Instance.AddReference(stopHandler),
                                    ConsumerHandler.Instance.AddReference(dispatcher),
                                    Callbacks.onDispatchCb,
                                    Callbacks.onStopOrderCb,
                                    Callbacks.onNewEntityCb,
                                    Callbacks.onUpdatedEntityCb,
                                    Callbacks.onDeletedEntityCb,
                                    Callbacks.onCreateRequestCb,
                                    Callbacks.onUpdateRequestCb,
                                    Callbacks.onDeleteRequestCb,
                                    Callbacks.onServiceRequestCb,
                                    Callbacks.onResponseCb,
                                    Callbacks.onMessageCb,
                                    Callbacks.onRegisteredCb,
                                    Callbacks.onUnregisteredCb,
                                    Callbacks.onRevokedRegistrationCb,
                                    Callbacks.onCompletedRegistrationCb,
                                    Callbacks.onInjectedNewEntityCb,
                                    Callbacks.onInjectedUpdatedEntityCb,
                                    Callbacks.onInjectedDeletedEntityCb,
                                    Callbacks.onInitialInjectionsDoneCb,
                                    Callbacks.onNotRequestOverflowCb,
                                    Callbacks.onNotMessageOverflowCb,
                                    Callbacks.onDropReferenceCb,
                                    out success);

            Marshal.FreeHGlobal(nameCommonPart);
            Marshal.FreeHGlobal(nameInstancePart);

            if (!Interface.BoolOf(success))
            {
                ConsumerHandler.Instance.DropReference(stopHandler);
                ConsumerHandler.Instance.DropReference(dispatcher);

                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Close the connection to the DOB.
        /// <para/>
        /// Closes the connection to the DOB and deallocates all resources. All subscriptions
        /// and registrations will automatically be deleted and there is no need to call
        /// Unsubscribe and Unregister before calling Close.
        /// Note that all connections that were set up using Attach will also be closed after
        /// a call to this method.
        /// </summary>
        public void Close()
        {
            Close(true);
        }

        /// <summary>
        /// Internal Close function that allows for enabling/disabling of the thread check.
        /// </summary>
        /// <param name="checkThread">Whether to check which thread the call is being made from.</param>
        private void Close(bool checkThread)
        {
            byte success;
            Interface.DoseC_Disconnect(m_ctrl, Interface.ByteOf(checkThread), out success);

            if (!Interface.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Check if this Connection instance is open.
        /// </summary>
        /// <returns>True if the connection is open, otherwise false.</returns>
        public override bool IsOpen()
        {
            byte isConn;
            byte success;
            Interface.DoseC_IsConnected(m_ctrl, out isConn, out success);

            if (!Interface.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
            return Interface.BoolOf(isConn);
        }

        /// <summary>
        /// When the dispatch event or callback is signalled, the application MUST call
        /// this method. A call to Dispatch will result in that all queues for this connection
        /// are emptied and that each message in the queues are passed to the associated
        /// consumer.
        /// <para/>
        /// Calls to dispatch from connection instances that are not open will be ignored.
        /// </summary>
        public void Dispatch()
        {
            byte success;
            Interface.DoseC_Dispatch(m_ctrl, out success);
            if (!Interface.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }



        //-------------------------------------
        // Implemented interfaces
        //-------------------------------------
        #region Interfaces
        /// <summary>
        /// Destroy and dispose the connection.
        /// </summary>
        public void Dispose()
        {
            if (!disposed)
            {
                try
                {
                    Close(false);
                }
                catch (System.Exception exc)
                {
                    Console.WriteLine("Connection.Dispose: Caught exception: " + exc);
                    Console.WriteLine("Will return as if nothing has happened!");
                }
                Interface.DoseC_Destructor(ControllerId);
                disposed = true;
            }
            GC.SuppressFinalize(this);
        }
        #endregion

        //-------------------------------------
        // Private
        //-------------------------------------
        #region PrivatePart

        internal override System.Int32 ControllerId
        {
            get { return m_ctrl; }
        }

        private System.Int32 m_ctrl = -1;
        private bool disposed = false;

        /// <summary>
        /// Destroy the connection (and dispose it).
        /// </summary>
        ~Connection()
        {
            Dispose();
        }
        #endregion



    }
}
