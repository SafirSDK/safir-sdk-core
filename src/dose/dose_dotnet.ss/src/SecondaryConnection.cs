/* ****************************************************************************
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
    ///  A secondary connection attached to a "real" connection.
    /// <para/>
    /// This class is used to attach yourself to an existing connection in the
    /// same thread. <para/>
    /// All attach calls ensure that you will get a connection that is valid in the current thread, but
    /// each SecondaryConnection must still only be used from within one thread.
    /// </summary>
    public sealed class SecondaryConnection : ConnectionBase
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        public SecondaryConnection()
        {

        }

        /// <summary>
        /// Attach to a connection in this thread.
        /// <para/>
        /// This method will attach the SecondaryConnection to the first Connection that was
        /// opened in this thread.
        /// <para/>
        /// This method can be used to let part of a program, for example a module or a dll, attach to an
        /// existing open connection.
        /// </summary>
        /// <exception cref="Safir.Dob.NotOpenException">There is no open Connection in this thread.</exception>
        public void Attach()
        {
            if (IsAttached()) //already attached!
            {
                Detach();
                //throw new Dob.Typesystem.SoftwareViolationException("SecondaryConnection is already attached");
            }

            Attach("", "");
        }

        /// <summary>
        /// Attach to a named connection in this thread.
        /// <para/>
        /// This method will attach the SecondaryConnection to the named Connection if that
        /// Connection was opened in this thread.
        /// <para/>
        /// This method can be used to let part of a program, for example a module or a dll, attach to an
        /// existing open connection. <para/>
        /// The connection name parameters are used to identify the connection to attach to.
        /// This connection must already be opened, otherwise an exception will be
        /// thrown.
        /// </summary>
        /// <param name="connectionNameCommonPart"> Name that identifies the connection but not any particular
        ///                                        instance.</param>
        /// <param name="connectionNameInstancePart">Name that identifies a particular connection instance.</param>
        /// <exception cref="Safir.Dob.NotOpenException">If the Connection instance we are trying to
        ///                                              attach to is not open.</exception>
        public void Attach(string connectionNameCommonPart,
                           string connectionNameInstancePart)
        {
            if (IsAttached()) //already attached!
            {
                Detach();
                //throw new Dob.Typesystem.SoftwareViolationException("SecondaryConnection is already attached");
            }

            int newCtrlId;
            byte success;

            System.IntPtr nameCommonPart = Dob.Typesystem.Internal.InternalOperations.CStringOf(connectionNameCommonPart);
            System.IntPtr nameInstancePart = Dob.Typesystem.Internal.InternalOperations.CStringOf(connectionNameInstancePart);

            Interface.DoseC_ConnectSecondary(nameCommonPart,
                                             nameInstancePart,
                                             Interface.DOSE_LANGUAGE_DOTNET,
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
                                             out newCtrlId,
                                             out success);

            Marshal.FreeHGlobal(nameCommonPart);
            Marshal.FreeHGlobal(nameInstancePart);

            if (!Interface.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }

            m_ctrl = newCtrlId;
        }

        /// <summary>
        /// Detach a SecondaryConnection.
        /// <para/>
        /// When a connection has been detached it can be attached again.
        /// </summary>
        public void Detach()
        {
            m_ctrl = -1;
        }

        /// <summary>
        ///  Check if a SecondaryConnection is attached to an open connection.
        /// </summary>
        /// <returns>True if the SecondaryConnection is attached to a 
        ///          Connection and that Connection is open.</returns>
        public bool IsAttached()
        {
            if (m_ctrl < 0)
            {
                return false;
            }
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
        /// For a secondary connection this is the same as the IsAttached check.
        /// </summary>
        /// <returns>True if the SecondaryConnection is attached to a Connection and that Connection is open.</returns>
        public override bool IsOpen()
        {
            return IsAttached();
        }


        //-------------------------------------
        // Private
        //-------------------------------------
        #region PrivatePart

        internal override int ControllerId
        {
            get
            {
                if (!IsAttached())
                {
                    throw new Dob.NotOpenException("This SecondaryConnection is not attached!");
                }
                return m_ctrl;
            }
        }

        private System.Int32 m_ctrl = -1;

        #endregion

    }
}
