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
using System.Runtime.InteropServices;

namespace Safir.Dob
{
    /// <summary>
    /// Proxy class for an entity.
    /// </summary>
    public sealed class EntityProxy : IDisposable
    {
        /// <summary>
        /// Retrieve type id of the Entity.
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
        /// Retrieve aggregation of type id and instance id of the entity.
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
        /// Get the entity.
        /// <para/>
        /// No change flags will be set in the returned entity.
        /// </summary>
        public Entity Entity
        {
            get
            {
                CheckNotDisposed();
                if (m_currentBlob == IntPtr.Zero)
                {
                    throw new Typesystem.SoftwareViolationException("Not possible to use call Entity on OnDeletedEntity proxy!");
                }
                return (Entity)Typesystem.ObjectFactory.Instance.CreateObject(m_currentBlob);
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

                if (m_currentBlob == IntPtr.Zero)
                {
                    throw new Typesystem.SoftwareViolationException("Not possible to use call EntityWithChangeInfo on OnDeletedEntity proxy!");
                }

                if (m_currentBlobWithChangeInfo == System.IntPtr.Zero)
                {
                    byte success;
#if FUNC_PTR_WORKAROUND
                    System.IntPtr m_blobDeleter;
#endif
                    Interface.DoseC_Diff(m_previousState,
                                         m_currentState,
                                         Interface.ByteOf(true), //wantCurrent
                                         Interface.ByteOf(m_timestampDiff),
                                         out m_currentBlobWithChangeInfo,
                                         out m_blobDeleter,
                                         out success);

                    if (!Interface.BoolOf(success))
                    {
                        m_currentBlobWithChangeInfo = System.IntPtr.Zero;
#if ! FUNC_PTR_WORKAROUND
                        m_blobDeleter = null;
#endif
                        Typesystem.LibraryExceptions.Instance.Throw();
                    }
                }
                return (Entity)Typesystem.ObjectFactory.Instance.CreateObject(m_currentBlobWithChangeInfo);
            }
        }

        /// <summary>
        /// Get owner handler id.
        /// <para/>
        /// Retrieves the handler id of the handler that owns (has created) this entity instance.
        /// </summary>
        public Typesystem.HandlerId Owner
        {
            get
            {
                CheckNotDisposed();
                System.Int64 handlerId;
                byte success;
                Interface.DoseC_GetHandlerId(m_currentState, out handlerId, out success);

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
                Interface.DoseC_GetConnectionInfo(m_currentState, out blob, out blobDeleter, out success);

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

        #region Retrieve binary blob

        /// <summary>
        /// Get binary blob of the received entity without changeflags set.
        /// <para/>
        /// This method will give you a pointer to the underlying representation of the object.
        /// Note that this pointer is only valid while the EntityProxy is in scope.
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
                if (m_currentBlob == System.IntPtr.Zero)
                {
                    throw new Typesystem.SoftwareViolationException("Not possible to do GetBlob on proxies from OnDeletedEntity (entity = " +
                                                                    EntityId);
                }
                return m_currentBlob;
            }
        }

        /// <summary>
        /// Get binary blob with change information.
        /// <para/>
        /// Retrieves the entity with change flags set to indicate which members have
        /// changed since the last subscription response.
        /// </summary>
        public System.IntPtr BlobWithChangeInfo
        {
            get
            {
                CheckNotDisposed();
                if (m_currentBlob == System.IntPtr.Zero)
                {
                    throw new Typesystem.SoftwareViolationException("Not possible to do GetBlobWithChangeInfo on proxies from OnDeletedEntity (entity = " +
                                                                    EntityId);
                }
                if (m_currentBlobWithChangeInfo == System.IntPtr.Zero)
                {
                    byte success;
#if FUNC_PTR_WORKAROUND
                    System.IntPtr m_blobDeleter;
#endif
                    Interface.DoseC_Diff(m_previousState,
                                         m_currentState,
                                         Interface.ByteOf(true), //wantCurrent
                                         Interface.ByteOf(m_timestampDiff),
                                         out m_currentBlobWithChangeInfo,
                                         out m_blobDeleter,
                                         out success);

                    if (!Interface.BoolOf(success))
                    {
                        m_currentBlobWithChangeInfo = System.IntPtr.Zero;
#if ! FUNC_PTR_WORKAROUND
                        m_blobDeleter = null;
#endif
                        Typesystem.LibraryExceptions.Instance.Throw();
                    }
                }
                return m_currentBlobWithChangeInfo;
            }
        }
        #endregion

        #region Retrieve previous entity state

        /// <summary>
        /// Get previous entity state.
        /// <para/>
        /// Used to get the entity state that preceeded this state.
        /// <para/>
        /// Can be used when a "previous" state exists, that is, from within the following callbacks:<para/>
        /// <list type="bullet">
        /// <item><description>EntitySubscriber.OnUpdatedEntity</description></item>
        /// <item><description>EntitySubscriber.OnDeletedEntity</description></item>
        /// </list><para/>
        /// Calling this function inside an OnDeletedEntity when the subscription was set up with 
        /// includeUpdates set to false may yield an entity state that you have not received in an 
        /// OnNewEntity callback. In fact it will most likely give you one of the updated entity
        /// states that were filtered out because you didn't include updates.
        /// <para/>
        /// No change flags will be set in the returned entity.
        /// </summary>
        public PreviousEntityProxy Previous
        {
            get
            {
                CheckNotDisposed();
                if (m_previousEntityProxy == null)
                {
                    m_previousEntityProxy = new PreviousEntityProxy(m_currentBlob,m_currentState,m_previousBlob,m_previousState,m_timestampDiff);
                }
                return m_previousEntityProxy;
            }
        }

        #endregion

        #region Trace and Debug stuff

        /// <summary>
        /// Get owner handler id that also contains the string representation.
        /// <para/>
        /// Mainly for trace and debug purposes.
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

        #region Retrieve timestamps. (Extended info for applications with special need)

        /// <summary>
        /// Retrieves the timestamp for the latest create, update or delete.
        /// <para/>
        /// Note that this operation is only valid for Injectable types.
        /// </summary>
        /// <returns>Timestamp</returns>
        public System.Int64 GetTimestamp()
        {
            CheckNotDisposed();
            Int64 timestamp;
            byte success;
            Interface.DoseC_GetTopTimestamp(m_currentState, out timestamp, out success);
            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return timestamp;
        }

        /// <summary>
        /// Retrieves the timestamp for the given top member.
        /// <para/>
        /// Note that this operation is only valid for Injectable types.
        /// </summary>
        /// <param name="member">Top level member index.</param>
        /// <returns>Timestamp.</returns>
        public System.Int64 GetTimestamp(System.Int32 member)
        {
            CheckNotDisposed();
            Int64 timestamp;
            byte success;
            Interface.DoseC_GetMemberTimestamp(m_currentState, member, out timestamp, out success);
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
        internal EntityProxy(System.IntPtr currentBlob,
                             System.IntPtr currentState,
                             System.IntPtr previousBlob,
                             System.IntPtr previousState,
                             bool addReference,
                             bool timestampDiff)
        {
            m_currentBlob = currentBlob;
            m_currentState = currentState;
            m_previousBlob = previousBlob;
            m_previousState = previousState;
            m_timestampDiff = timestampDiff;

            if (addReference)
            {
                Interface.DoseC_AddReference(currentState);
                Interface.DoseC_AddReference(previousState);
            }
        }

        /// <summary>
        /// Drop the references into Dob shared memory that the proxy holds.
        /// </summary>
        public void Dispose()
        {
            if (!disposed)
            {
                disposed = true;

                if (m_previousEntityProxy != null)
                {
                    m_previousEntityProxy.Dispose();
                    m_previousEntityProxy = null;
                }

#if FUNC_PTR_WORKAROUND
                if (m_currentBlobWithChangeInfo != System.IntPtr.Zero)
                {
                    Safir.Dob.Typesystem.Internal.InternalOperations.Delete(ref m_currentBlobWithChangeInfo);
                }
#else
                if (m_blobDeleter != null)
                {
                    m_blobDeleter(ref m_currentBlobWithChangeInfo);
                }
#endif

                Interface.DoseC_DropReference(m_currentState);
                Interface.DoseC_DropReference(m_previousState);
            }
        }

        /// <summary>
        /// Check that the object was Disposed correctly and, if it wasn't, emit a log.
        /// </summary>
        ~EntityProxy()
        {
            if (!disposed)
            {
                Dispose();

                Safir.Logging.SendSystemLog(Safir.Logging.Severity.Critical,
                                            "Programming Error! An EntityProxy was not disposed correctly when destructor was called!");
            }
        }

        private void CheckNotDisposed()
        {
            if (disposed)
            {
                throw new Typesystem.SoftwareViolationException("Attempt to use an EntityProxy that is disposed!");
            }
        }

        private bool disposed = false;

        private readonly System.IntPtr m_currentBlob;
        private readonly System.IntPtr m_currentState;
        private readonly System.IntPtr m_previousBlob;
        private readonly System.IntPtr m_previousState;
        private System.IntPtr m_currentBlobWithChangeInfo = System.IntPtr.Zero;

#if ! FUNC_PTR_WORKAROUND
        private Interface.DoseC_BlobDeleter m_blobDeleter = null;
#endif
        private readonly bool m_timestampDiff;

        private PreviousEntityProxy m_previousEntityProxy = null;

        #endregion
    }
}
