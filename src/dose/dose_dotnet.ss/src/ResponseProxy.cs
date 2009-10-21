/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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
    /// Proxy class for a response.
    /// </summary>
    public sealed class ResponseProxy : IDisposable
    {
        /// <summary>
        /// Get response success or failure status.
        /// </summary>
        public bool IsSuccess
        {
            get
            {
                CheckNotDisposed();
                return Typesystem.Operations.IsOfType(TypeId, SuccessResponse.ClassTypeId);
            }
        }

        /// <summary>
        /// Retrieves type id of the response.
        /// </summary>
        public System.Int64 TypeId
        {
            get
            {
                CheckNotDisposed();
                return Typesystem.BlobOperations.GetTypeId(m_responseBlob);
            }
        }

        /// <summary>
        /// Get the response.
        /// </summary>
        public Response Response
        {
            get
            {
                CheckNotDisposed();
                return (Response)Typesystem.ObjectFactory.Instance.CreateObject(m_responseBlob);
            }
        }

        /// <summary>
        /// Get info about the connection sending the response.
        /// </summary>
        public ConnectionInfo ResponseSenderConnectionInfo
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
                Interface.DoseC_GetConnectionInfo(m_responseState, out blob, out blobDeleter, out success);

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
        /// Get binary blob of the received response.
        /// <para/>
        /// This method will give you a pointer to the underlying representation of the object.
        /// Note that this pointer is only valid while the ResponseProxy is in scope.
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
                return m_responseBlob;
            }
        }

        #region Methods to retrieve info about the original request.

        /// <summary>
        /// Retrieves the request id generated when the request was sent.
        /// </summary>
        public int RequestId
        {
            get
            {
                CheckNotDisposed();
                return m_requestId;
            }
        }

        /// <summary>
        /// Get type id of the entity or service sent in the original request.
        /// </summary>
        public System.Int64 RequestTypeId
        {
            get
            {
                CheckNotDisposed();

                byte success;
                System.Int64 typeId;
                Interface.DoseC_GetTypeId(m_requestState, out typeId, out success);
                if (!Interface.BoolOf(success))
                {
                    Typesystem.LibraryExceptions.Instance.Throw();
                }

                return typeId;
            }
        }

        /// <summary>
        /// Get the instance id used in the original request. (Only for entity requests)
        /// </summary>
        public Typesystem.InstanceId RequestInstanceId
        {
            get
            {
                CheckNotDisposed();
                System.Int64 instanceId;
                byte success;
                Interface.DoseC_GetInstanceId(m_requestState, out instanceId, out success);

                if (!Interface.BoolOf(success))
                {
                    Typesystem.LibraryExceptions.Instance.Throw();
                }

                return new Typesystem.InstanceId(instanceId);
            }
        }

        /// <summary>
        /// Get the original request.
        /// <para/>
        /// Retrieves the original request. Depending on the type of request this
        /// can be a <see cref="Safir.Dob.Entity"/> or a <see cref="Safir.Dob.Service"/>
        /// </summary>
        public Typesystem.Object Request
        {
            get
            {
                CheckNotDisposed();
                if (m_requestBlob == System.IntPtr.Zero)
                {
                    throw new Safir.Dob.Typesystem.SoftwareViolationException("Cannot get Request on ResponseProxies for DeleteRequests");
                }
                return (Typesystem.Object)Typesystem.ObjectFactory.Instance.CreateObject(m_requestBlob);
            }
        }


        /// <summary>
        /// Get the blob of the original request.
        /// <para/>
        /// Retrieves the blob of the original request.
        /// If the request was a delete request NULL will be returned.
        /// <para/>
        /// This method will give you a pointer to the underlying representation of the object.
        /// Note that this pointer is only valid while the ResponseProxy is in scope.
        /// If you want to keep the blob you must copy it using methods in Safir::Dob::Typesystem.
        /// <para/>
        /// This method is mainly useful if all you want to do with a received object is to write it
        /// to a database or pass it over a C-interface to a library or plugin.
        /// </summary>
        public System.IntPtr RequestBlob
        {
            get
            {
                CheckNotDisposed();
                return m_requestBlob;
            }
        }


        /// <summary>
        /// Get the handler id to which the original request was sent.
        /// </summary>
        public Typesystem.HandlerId RequestHandlerId
        {
            get
            {
                CheckNotDisposed();
                System.Int64 handlerId;
                byte success;
                Interface.DoseC_GetHandlerId(m_requestState, out handlerId, out success);

                if (!Interface.BoolOf(success))
                {
                    Typesystem.LibraryExceptions.Instance.Throw();
                }

                return new Typesystem.HandlerId(handlerId);
            }
        }
        #endregion

        #region Private and internal stuff

        /// <summary>
        /// These objects can only be constructed by the Dob!
        /// </summary>
        internal ResponseProxy(System.Int32 requestId,
                               System.IntPtr responseBlob,
                               System.IntPtr responseState,
                               System.IntPtr requestBlob,
                               System.IntPtr requestState)
        {
            m_requestId = requestId;
            m_responseBlob = responseBlob;
            m_responseState = responseState;
            m_requestBlob = requestBlob;
            m_requestState = requestState;
        }

        /// <summary>
        /// Drop the references into Dob shared memory that the proxy holds.
        /// </summary>
        public void Dispose()
        {
            if (!disposed)
            {
                disposed = true;

                //This bit just makes sure we get rid of a mono warning
                if (m_responseState != System.IntPtr.Zero)
                {
                    m_responseState = System.IntPtr.Zero;
                }
            }
        }

        /// <summary>
        /// Dispose and destroy the object.
        /// </summary>
        ~ResponseProxy()
        {
            Dispose();
        }

        private void CheckNotDisposed()
        {
            if (disposed)
            {
                throw new Typesystem.SoftwareViolationException("Attempt to use a ResponseProxy that is disposed! Please do not use a ResponseProxy outside the OnResponse callback!");
            }
        }


        private bool disposed = false;

        private System.Int32 m_requestId;
        private System.IntPtr m_responseBlob;
        private System.IntPtr m_responseState;
        private System.IntPtr m_requestBlob;
        private System.IntPtr m_requestState;
        #endregion

    }
}
