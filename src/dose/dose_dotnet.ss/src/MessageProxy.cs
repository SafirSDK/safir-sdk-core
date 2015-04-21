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

namespace Safir.Dob
{
    /// <summary>
    /// Proxy class for a message.
    /// </summary>
    public sealed class MessageProxy : IDisposable
    {
        /// <summary>
        /// Retrieves type id of the message.
        /// </summary>
        public System.Int64 TypeId
        {
            get
            {
                CheckNotDisposed();
                return Typesystem.Internal.BlobOperations.GetTypeId(m_messageBlob);
            }
        }

        /// <summary>
        /// Get the message.
        /// </summary>
        public Message Message
        {
            get
            {
                CheckNotDisposed();
                return (Message)Typesystem.ObjectFactory.Instance.CreateObject(m_messageBlob);
            }
        }

        /// <summary>
        /// Get info about the connection sending the message.
        /// </summary>
        public ConnectionInfo SenderConnectionInfo
        {
            get
            {
                CheckNotDisposed();
                System.IntPtr blob;

                Interface.DoseC_BlobDeleter blobDeleter;

                byte success;
                Interface.DoseC_GetConnectionInfo(m_state, out blob, out blobDeleter, out success);

                if (!Interface.BoolOf(success))
                {
                    Typesystem.LibraryExceptions.Instance.Throw();
                }

                try
                {
                    return (ConnectionInfo)Typesystem.ObjectFactory.Instance.CreateObject(blob);
                }
                finally
                {
                    blobDeleter(ref blob);
                }
            }
        }

        /// <summary>
        /// Retrieves the channel on which the message is sent.
        /// </summary>
        public Typesystem.ChannelId ChannelId
        {
            get
            {
                CheckNotDisposed();
                System.Int64 channelId;
                byte success;
                Interface.DoseC_GetChannelId(m_state, out channelId, out success);

                if (!Interface.BoolOf(success))
                {
                    Typesystem.LibraryExceptions.Instance.Throw();
                }

                return new Typesystem.ChannelId(channelId);
            }
        }

        /// <summary>
        /// Get binary blob of the received message.
        /// <para/>
        /// This method will give you a pointer to the underlying representation of the object.
        /// Note that this pointer is only valid while the MessageProxy is in scope.
        /// If you want to keep the blob you must copy it using methods in Safir::Dob::Typesystem.
        /// <para/>
        /// This method is mainly useful if all you want to do with a received object is to write it
        /// to a database or pass it over a C-interface to a library or plugin.
        /// </summary>
        public System.IntPtr Blob
        {
            get
            {
                CheckNotDisposed();
                return m_messageBlob;
            }
        }

        #region Trace and Debug stuff
        /// <summary>
        /// Get channel id that also contains the string representation.
        /// <para/>
        /// Mainly for trace and debug purposes. <para/>
        /// See also <seealso cref="ChannelId"/>
        /// </summary>
        public Typesystem.ChannelId ChannelIdWithStringRepresentation
        {
            get
            {
                CheckNotDisposed();
                return ChannelId;
                //TODO: try to obtain string representation
            }
        }
        #endregion

        #region Private and internal stuff

        /// <summary>
        /// These objects can only be constructed by the Dob!
        /// </summary>
        internal MessageProxy(System.IntPtr messageBlob, System.IntPtr state)
        {
            m_messageBlob = messageBlob;
            m_state = state;
        }

        /// <summary>
        /// Drop the references into Dob shared memory that the proxy holds.
        /// </summary>
        public void Dispose()
        {
            if (!disposed)
            {
                disposed = true;
            }
        }

        /// <summary>
        /// Dispose and destroy the object.
        /// </summary>
        ~MessageProxy()
        {
            Dispose();
        }

        private void CheckNotDisposed()
        {
            if (disposed)
            {
                throw new Typesystem.SoftwareViolationException("Attempt to use a MessageProxy that is disposed! Please do not use a MessageProxy outside the OnMessage callback!");
            }
        }

        
        private bool disposed = false;

        private System.IntPtr m_messageBlob;
        private System.IntPtr m_state;
        #endregion
    }
}
