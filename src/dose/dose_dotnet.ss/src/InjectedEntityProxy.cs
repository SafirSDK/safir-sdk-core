/* ****************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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
    /// Proxy class for entity injections.
    /// </summary>
    public sealed class InjectedEntityProxy : IDisposable
    {
        /// <summary>
        /// Retrieves type id of the Entity that is about to be injected.
        /// </summary>
        public System.Int64 TypeId
        {
            get
            {
                CheckNotDisposed();
                if (m_injectionBlob == System.IntPtr.Zero)
                {
                    System.Int64 typeId;
                    byte success;
                    Interface.DoseC_GetTypeId(m_injectionState, out typeId, out success);
                    if (!Interface.BoolOf(success))
                    {
                        Typesystem.LibraryExceptions.Instance.Throw();
                    }
                    return typeId;
                }
                else
                {
                    return Typesystem.Internal.BlobOperations.GetTypeId(m_injectionBlob);
                }
            }
        }

        /// <summary>
        /// Retrieves instance id of the Entity that is about to be injected.
        /// </summary>
        public Typesystem.InstanceId InstanceId
        {
            get
            {
                CheckNotDisposed();
                System.Int64 instanceId;
                byte success;
                Interface.DoseC_GetInstanceId(m_injectionState, out instanceId, out success);
                if (!Interface.BoolOf(success))
                {
                    Typesystem.LibraryExceptions.Instance.Throw();
                }
                return new Typesystem.InstanceId(instanceId);
            }
        }

        /// <summary>
        /// Retrieves aggregation of type id and instance id.
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
        /// Get the entity state that is about to be injected.
        /// <para/>
        /// Change flags will be set in the entity to indicate which members
        /// are part of the injection.
        /// <para/>
        /// Note that this method cannot be called in an OnInjectedDeletedEntity,
        /// since there then is no entity to get...
        /// </summary>
        public Entity Injection
        {
            get
            {
                CheckNotDisposed();
                if (m_injectionBlob == IntPtr.Zero)
                {
                    throw new Typesystem.SoftwareViolationException("Not possible to use call Injection on InjectDeletes proxy!");
                }
                return (Entity)Typesystem.ObjectFactory.Instance.CreateObject(m_injectionBlob);

            }
        }


        /// <summary>
        /// Get binary blob of the entity that is about to be injected.
        /// <para/>
        /// This method will give you a pointer to the underlying representation of the object.
        /// Note that this pointer is only valid while the InjectedEntityProxy is in scope.
        /// If you want to keep the blob you must copy it using methods in Safir::Dob::Typesystem.
        /// <para/>
        /// This method is mainly useful if all you want to do with a received object is to write it
        /// to a database or pass it over a C-interface to a library or plugin.
        /// <para/>
        /// Change flags will be set in the entity to indicate which members
        /// are part of the injection.
        /// </summary>
        public System.IntPtr InjectionBlob
        {
            get
            {
                CheckNotDisposed();
                if (m_injectionBlob == System.IntPtr.Zero)
                {
                    throw new Typesystem.SoftwareViolationException("Not possible to do Blob on proxies from OnDeletedEntity (entity = " +
                                                                    EntityId);
                }
                return m_injectionBlob;
            }
        }

        /// <summary>
        /// Get the current entity state.
        /// <para/>
        /// This method retrieves the entity as it is before the injection has been completed.
        /// <para/>
        /// Can be used when a "current" state exists, i.e. from within the following callbacks:
        /// <list type="bullet">
        /// <item><description>EntityInjectionHandler.OnInjectedUpdatedEntity</description></item>
        /// <item><description>EntityInjectionHandler.OnInjectedDeletedEntity</description></item>
        /// </list>
        /// <para/>
        /// No change flags will be set in the returned entity.
        /// </summary>
        public Entity Current
        {
            get
            {
                CheckNotDisposed();
                if (m_currentBlob == IntPtr.Zero)
                {
                    throw new Typesystem.SoftwareViolationException("Not possible to use call Current on InjectDeletes proxy!");
                }
                return (Entity)Typesystem.ObjectFactory.Instance.CreateObject(m_currentBlob);
            }
        }

        #region Private and internal stuff

        /// <summary>
        /// These objects can only be constructed by the Dob!
        /// </summary>
        internal InjectedEntityProxy(System.IntPtr injectionBlob,
                                     System.IntPtr injectionState,
                                     System.IntPtr currentBlob,
                                     System.IntPtr currentState)
        {
            m_injectionBlob = injectionBlob;
            m_injectionState = injectionState;
            m_currentBlob = currentBlob;
            m_currentState = currentState;
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
                if (m_currentState != System.IntPtr.Zero)
                {
                    m_currentState = System.IntPtr.Zero;
                }
            }
        }

        /// <summary>
        /// Dispose and destroy the object.
        /// </summary>
        ~InjectedEntityProxy()
        {
            Dispose();
        }

        private void CheckNotDisposed()
        {
            if (disposed)
            {
                throw new Typesystem.SoftwareViolationException("Attempt to use an InjectedEntityProxy that is disposed! Please do not use an InjectedEntityProxy outside the OnInjected*Entity callbacks!");
            }
        }

        private bool disposed = false;

        private System.IntPtr m_injectionBlob;
        private System.IntPtr m_injectionState;
        private System.IntPtr m_currentBlob;
        private System.IntPtr m_currentState;

        #endregion
    }
}
