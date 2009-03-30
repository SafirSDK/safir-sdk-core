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
    /// Proxy class for a "previous" entity.
    /// </summary>
    public sealed class PreviousEntityProxy : IDisposable
    {
        /// <summary>
        /// Retrieves type id of the Entity.
        /// </summary>
        public System.Int64 TypeId
        {
            get
            {
                CheckNotDisposed();
                if (m_currentBlob == System.IntPtr.Zero)
                {
                    System.Int64 typeId;
                    byte success;
                    Interface.DoseC_GetTypeId(m_currentState, out typeId, out success);
                    if (!Interface.BoolOf(success))
                    {
                        Typesystem.LibraryExceptions.Instance.Throw();
                    }
                    return typeId;
                }
                else
                {
                    return Typesystem.BlobOperations.GetTypeId(m_currentBlob);
                }
            }
        }

        /// <summary>
        /// Retrieves instance id of the Entity.
        /// </summary>
        public Typesystem.InstanceId InstanceId
        {
            get
            {
                CheckNotDisposed();
                System.Int64 instanceId;
                byte success;
                Interface.DoseC_GetInstanceId(m_currentState, out instanceId, out success);
                if (!Interface.BoolOf(success))
                {
                    Typesystem.LibraryExceptions.Instance.Throw();
                }
                return new Typesystem.InstanceId(instanceId);
            }
        }

        /// <summary>
        /// Retrieve aggregation of type id and instance id.
        /// </summary>
        public Typesystem.EntityId EntityId
        {
            get
            {
                CheckNotDisposed();
                return new Safir.Dob.Typesystem.EntityId(TypeId, InstanceId);
            }
        }

        /// <summary>
        /// Retrieves the entity.
        /// <para/>
        /// No change flags will be set in the returned entity.
        /// </summary>
        public Entity Entity
        {
            get
            {
                CheckNotDisposed();
                if (m_previousBlob == IntPtr.Zero)
                {
                    throw new Typesystem.SoftwareViolationException("Not possible to use call Entity on this PreviousEntityProxy of entity "
                                                                    + EntityId);
                }
                return (Entity)Typesystem.ObjectFactory.Instance.CreateObject(m_previousBlob);            
            }
        }

        /// <summary>
        /// Get entity with change information.
        /// <para/>
        /// Retrieves the entity with change flags set to indicate which members have
        /// changed since the last subscription response.
        /// </summary>
        public Entity EntityWithChangeInfo
        {
            get
            {
                CheckNotDisposed();
                if (m_previousBlob == IntPtr.Zero)
                {
                    throw new Typesystem.SoftwareViolationException("Not possible to use call Entity on this PreviousEntityProxy of entity "
                                                                    + EntityId);
                }
                if (m_previousBlobWithChangeInfo == System.IntPtr.Zero)
                {
                    byte success;
#if FUNC_PTR_WORKAROUND
                    System.IntPtr m_blobDeleter;
#endif
                    Interface.DoseC_Diff(m_previousState,
                                         m_currentState,
                                         Interface.ByteOf(false), //wantCurrent
                                         Interface.ByteOf(m_timestampDiff),
                                         out m_previousBlobWithChangeInfo,
                                         out m_blobDeleter,
                                         out success);

                    if (!Interface.BoolOf(success))
                    {
                        m_previousBlobWithChangeInfo = System.IntPtr.Zero;
#if ! FUNC_PTR_WORKAROUND
                        m_blobDeleter = null;
#endif
                        Typesystem.LibraryExceptions.Instance.Throw();
                    }
                }
                return (Entity)Typesystem.ObjectFactory.Instance.CreateObject(m_previousBlobWithChangeInfo);   
            }
        }

        /// <summary>
        /// Retrieves the handler id of the handler that owns (has created) this entity instance.
        /// </summary>
        public Typesystem.HandlerId Owner
        {
            get
            {
                CheckNotDisposed();
                System.Int64 handlerId;
                byte success;
                Interface.DoseC_GetHandlerId(m_previousState, out handlerId, out success);

                if (!Interface.BoolOf(success))
                {
                    Typesystem.LibraryExceptions.Instance.Throw();
                }

                return new Typesystem.HandlerId(handlerId);
            }
        }

        /// <summary>
        /// Get info about the connection to which the owner handler is related.
        /// </summary>
        public ConnectionInfo OwnerConnectionInfo
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
                Interface.DoseC_GetConnectionInfo(m_previousState, out blob, out blobDeleter, out success);

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

        #region Blob operations

        /// <summary>
        /// Get binary blob of the received entity without changeflags set.
        /// <para/>
        /// This method will give you a pointer to the underlying representation of the object.
        /// Note that this pointer is only valid while the PreviousEntityProxy is in scope.
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
                if (m_previousBlob == System.IntPtr.Zero)
                {
                    throw new Typesystem.SoftwareViolationException("Not possible to do Blob on proxies from OnDeletedEntity (entity = " +
                                                                    EntityId);
                }
                return m_previousBlob;
            }
        }

        /// <summary>
        /// Retrieves the entity with change flags set to indicate which members have
        /// changed since the last subscription response.
        /// <para/>
        /// See also <seealso cref="Blob"/>
        /// </summary>
        public System.IntPtr BlobWithChangeInfo
        {
            get
            {
                CheckNotDisposed();
                if (m_previousBlob == System.IntPtr.Zero)
                {
                    throw new Typesystem.SoftwareViolationException("Not possible to do BlobWithChangeInfo on proxies from OnDeletedEntity (entity = " +
                                                                    EntityId);
                }
                if (m_previousBlobWithChangeInfo == System.IntPtr.Zero)
                {
                    byte success;
#if FUNC_PTR_WORKAROUND
                    System.IntPtr m_blobDeleter;
#endif
                    Interface.DoseC_Diff(m_previousState,
                                         m_currentState,
                                         Interface.ByteOf(false), //wantCurrent
                                         Interface.ByteOf(m_timestampDiff),
                                         out m_previousBlobWithChangeInfo,
                                         out m_blobDeleter,
                                         out success);

                    if (!Interface.BoolOf(success))
                    {
                        m_previousBlobWithChangeInfo = System.IntPtr.Zero;
#if ! FUNC_PTR_WORKAROUND
                        m_blobDeleter = null;
#endif
                        Typesystem.LibraryExceptions.Instance.Throw();
                    }
                }
                return m_previousBlobWithChangeInfo; 
            }
        }
        #endregion

        #region Trace and Debug stuff

        /// <summary>
        /// Get owner handler id that also contains the string representation.
        /// <para/>
        /// Mainly for trace and debug purposes.
        /// <para/>
        /// See also <seealso cref="Owner"/>
        /// </summary>
        public Typesystem.HandlerId OwnerWithStringRepresentation
        {
            get
            {
                CheckNotDisposed();
                return Owner;
                //TODO: try to obtain string representation
            }
        }

        #endregion

        #region Timestamp functionality

        /// <summary>
        /// Retrieves the timestamp for the latest create, update or delete.
        /// <para/>
        /// Note that timestamps is only available for types configured with this option.
        /// </summary>
        /// <returns>Timestamp</returns>
        /// <exception cref="Safir.Dob.NotFoundException">No timestamp available for this entity type.</exception>
        public System.Int64 GetTimestamp()
        {
            CheckNotDisposed();
            Int64 timestamp;
            byte success;
            Interface.DoseC_GetTopTimestamp(m_previousState, out timestamp, out success);
            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return timestamp;
        }

        /// <summary>
        /// Retrieves the timestamp for the given top member.
        /// </summary>
        /// <param name="member">Top level member index.</param>
        /// <returns>Timestamp</returns>
        /// <exception cref="Safir.Dob.NotFoundException">No timestamp available for this entity type.</exception>
        public System.Int64 GetTimestamp(System.Int32 member)
        {
            CheckNotDisposed();
            Int64 timestamp;
            byte success;
            Interface.DoseC_GetMemberTimestamp(m_previousState, member, out timestamp, out success);
            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return timestamp;
        }

        #endregion

        #region Private and internal stuff

        /// <summary>
        /// These objects can only be constructed by the Dob!
        /// </summary>
        internal PreviousEntityProxy(System.IntPtr currentBlob,
                                     System.IntPtr currentState,
                                     System.IntPtr previousBlob,
                                     System.IntPtr previousState,
                                     bool timestampDiff)
        {
            m_currentBlob = currentBlob;
            m_currentState = currentState;
            m_previousBlob = previousBlob;
            m_previousState = previousState;
            m_timestampDiff = timestampDiff;

            Interface.DoseC_AddReference(currentState);
            Interface.DoseC_AddReference(previousState);
        }

        /// <summary>
        /// Drop the references into Dob shared memory that the proxy holds.
        /// </summary>
        public void Dispose()
        {
            if (!disposed)
            {
                disposed = true;
#if FUNC_PTR_WORKAROUND
                if (m_previousBlobWithChangeInfo != System.IntPtr.Zero)
                {
                    Safir.Dob.Typesystem.Internal.InternalOperations.Delete(ref m_previousBlobWithChangeInfo);
                }
#else
                if (m_blobDeleter != null)
                {
                    m_blobDeleter(ref m_previousBlobWithChangeInfo);
                }
#endif
                Interface.DoseC_DropReference(m_currentState);
                Interface.DoseC_DropReference(m_previousState);
            }
        }

        /// <summary>
        /// Dispose and destroy the object.
        /// </summary>
        ~PreviousEntityProxy()
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

        private readonly System.IntPtr m_currentBlob;
        private readonly System.IntPtr m_currentState;
        private readonly System.IntPtr m_previousBlob;
        private readonly System.IntPtr m_previousState;
        private System.IntPtr m_previousBlobWithChangeInfo = System.IntPtr.Zero;

#if !FUNC_PTR_WORKAROUND
        private Interface.DoseC_BlobDeleter m_blobDeleter = null;
#endif
        private readonly bool m_timestampDiff;
        #endregion
    }
}
