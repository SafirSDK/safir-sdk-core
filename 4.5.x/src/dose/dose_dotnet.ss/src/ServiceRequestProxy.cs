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
    /// Proxy class for a service request.
    /// </summary>
    public sealed class ServiceRequestProxy : IDisposable
    {
        /// <summary>
        /// Retrieves type id of the service request.
        /// </summary>
        public System.Int64 TypeId
        {
            get
            {
                CheckNotDisposed();
                return Typesystem.BlobOperations.GetTypeId(m_requestBlob);
            }
        }

        /// <summary>
        /// Get the service request.
        /// </summary>
        public Service Request
        {
            get
            {
                CheckNotDisposed();
                if (m_requestBlob == IntPtr.Zero)
                {
                    throw new Typesystem.SoftwareViolationException("Not possible to use property Request on DeleteRequests!");
                }
                return (Service)Typesystem.ObjectFactory.Instance.CreateObject(m_requestBlob);
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
#if FUNC_PTR_WORKAROUND
                System.IntPtr blobDeleter;
#else
                Interface.DoseC_BlobDeleter blobDeleter;
#endif
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
#if FUNC_PTR_WORKAROUND
                    Safir.Dob.Typesystem.Internal.InternalOperations.Delete(ref blob);
#else
                    blobDeleter(ref blob);
#endif
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
        /// Get binary blob of the received service request.
        /// <para/>
        /// This method will give you a pointer to the underlying representation of the object.
        /// Note that this pointer is only valid while the ServiceRequestProxy is in scope.
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
        /// <para/>>
        /// Mainly for trace and debug purposes.
        /// <para/>
        /// See also <seealso cref="ReceivingHandlerId"/>
        /// </summary>
        public Typesystem.HandlerId ReceiverWithStringRepresentation
        {
            get
            {
                CheckNotDisposed();
                return ReceivingHandlerId;
                //TODO: try to obtain string representation
            }
        }

        #endregion

        #region Private and internal stuff

        /// <summary>
        /// These objects can only be constructed by the Dob!
        /// </summary>
        internal ServiceRequestProxy(System.IntPtr requestBlob, System.IntPtr state)
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
        ~ServiceRequestProxy()
        {
            Dispose();
        }

        private void CheckNotDisposed()
        {
            if (disposed)
            {
                throw new Typesystem.SoftwareViolationException("Attempt to use a ServiceRequestProxy that is disposed! Please do not use a ServiceRequestProxy outside the onServiceRequest callback!");
            }
        }


        private bool disposed = false;

        private System.IntPtr m_requestBlob;
        private System.IntPtr m_state;
        #endregion


    }
}
