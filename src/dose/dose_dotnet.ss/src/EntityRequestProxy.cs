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

namespace Safir.Dob
{
    /// <summary>
    /// Proxy class for an entity request.
    /// </summary>
    public sealed class EntityRequestProxy : IDisposable
    {
        /// <summary>
        /// Retrieves type id of the entity request.
        /// </summary>
        public System.Int64 TypeId
        {
            get
            {
                CheckNotDisposed();
                if (m_requestBlob == IntPtr.Zero)
                {
                    System.Int64 typeId;
                    byte success;
                    Interface.DoseC_GetTypeId(m_state, out typeId, out success);

                    if (!Interface.BoolOf(success))
                    {
                        Typesystem.LibraryExceptions.Instance.Throw();
                    }

                    return typeId;
                }
                else
                {
                    return Typesystem.Internal.BlobOperations.GetTypeId(m_requestBlob);
                }
            }
        }

        /// <summary>
        /// Retrieves instance id of the entity request.
        /// <para/>
        /// Note that it is illegal to call this method on proxies received in OnCreateRequest
        /// callbacks if the handler is registered as "HandlerDecidesInstanceId".
        /// This is because there is no instance id in the request in this case...
        /// </summary>
        public Typesystem.InstanceId InstanceId
        {
            get
            {
                CheckNotDisposed();
                System.Int64 instanceId;
                byte success;
                Interface.DoseC_GetInstanceId(m_state, out instanceId, out success);

                if (!Interface.BoolOf(success))
                {
                    Typesystem.LibraryExceptions.Instance.Throw();
                }

                return new Typesystem.InstanceId(instanceId);
            }
        }

        /// <summary>
        /// Retrieve the aggregation of type id and instance id.
        /// <para/>
        /// Note that it is illegal to call this method on proxies received in OnCreateRequest
        /// callbacks if the handler is registered as "HandlerDecidesInstanceId".
        /// This is because there is no instance id in the request in this case...
        /// </summary>
        public Typesystem.EntityId EntityId
        {
            get
            {
                CheckNotDisposed();
                return new Typesystem.EntityId(TypeId, InstanceId);
            }
        }

        /// <summary>
        /// Get the entity request.
        ///<para/>
        /// Note that it is not valid to call this for a DeleteRequest.
        /// </summary>
        public Entity Request
        {
            get
            {
                CheckNotDisposed();
                return (Entity)Typesystem.ObjectFactory.Instance.CreateObject(m_requestBlob);
            }
        }

        /// <summary>
        /// Get info about the connection sending the request.
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
        /// Get id of receiving handler.
        /// <para/>
        /// Can be handy when one consumer is used for several handlers.
        /// </summary>
        public Typesystem.HandlerId ReceivingHandlerId
        {
            get
            {
                CheckNotDisposed();
                System.Int64 handlerId;
                byte success;
                Interface.DoseC_GetHandlerId(m_state, out handlerId, out success);

                if (!Interface.BoolOf(success))
                {
                    Typesystem.LibraryExceptions.Instance.Throw();
                }

                return new Typesystem.HandlerId(handlerId);
            }
        }

        /// <summary>
        /// Get binary blob of the received entity request.
        /// <para/>
        /// This method will give you a pointer to the underlying representation of the object.
        /// Note that this pointer is only valid while the EntityRequestProxy is in scope.
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
                if (m_requestBlob == IntPtr.Zero)
                {
                    throw new Typesystem.SoftwareViolationException("No blob available on DeleteRequests!");
                }
                return m_requestBlob;
            }
        }

        #region Trace and Debug stuff
        /// <summary>
        /// Get receiver handler id that also contains the string representation.
        /// <para/>
        /// Mainly for trace and debug purposes.
        /// <para/>
        /// See also<seealso cref="ReceivingHandlerId"/>
        /// </summary>
        public Typesystem.HandlerId ReceiverWithStringRepresentation
        {
            get
            {
                CheckNotDisposed();
                return ReceivingHandlerId;
            }
        }

        #endregion

        #region Private and internal stuff

        /// <summary>
        /// These objects can only be constructed by the Dob!
        /// </summary>
        internal EntityRequestProxy(System.IntPtr requestBlob, System.IntPtr state)
        {
            m_requestBlob = requestBlob;
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
        ~EntityRequestProxy()
        {
            Dispose();
        }

        private void CheckNotDisposed()
        {
            if (disposed)
            {
                throw new Safir.Dob.Typesystem.SoftwareViolationException("Attempt to use a EntityRequestProxy that is disposed! Please do not use a EntityRequestProxy outside the On*Request callbacks!");
            }
        }


        private bool disposed = false;

        private System.IntPtr m_requestBlob;
        private System.IntPtr m_state;
        #endregion
    }
}
