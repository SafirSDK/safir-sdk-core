/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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
using System.Collections.Generic;
using System.Text;

namespace Safir.Dob.Typesystem
{
    /// <summary>
    /// Base class for all object containers.
    /// <para/>
    /// The reason for the existence of this class is that code that uses the reflection
    /// functionality must be able to get hold of members of items.
    /// </summary>
    public abstract class ObjectContainerBase : ContainerBase
    {
        /// <summary>
        /// Default constructor.
        /// </summary>
        public ObjectContainerBase() : base() { }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other">ObjectContainerBase.</param>
        protected ObjectContainerBase(ObjectContainerBase other) : base(other) { }

        /// <summary>
        /// Is the change flag in the container set?
        /// <para/>
        /// This method is like IsChanged without the recursion (on object containers IsChanged is recursive).
        /// </summary>
        /// <returns>True if the containers change flag is set.</returns>
        public bool IsChangedHere() { return m_bIsChanged; }

        /// <summary>
        /// Set the change flag in the container.
        /// <para/>
        /// This method is like SetChanged without the recursion (on object containers SetChanged is recursive).
        /// </summary>
        /// <param name="changed">The value to set the change flag to.</param>
        public void SetChangedHere(bool changed) { m_bIsChanged = changed; }

        /**
         * Get or set the pointer to the contained object.
         * 
         * Directly access the pointer that the container contains, but in the form
         * of a DOB Object base class.
         *
         * Warning: Does not update the change flag!
         *          Does not check if the container is null!
         *
         * Note: Do not use this property unless you're very sure it is the one you need!
         */
        public abstract Object InternalObj
        {
            get;
            set;
        }

        /// <summary>
        /// Get a member container from an object.
        /// <para/>
        /// Use the methods in Members to get member indices and array sizes for use
        /// with this method.
        /// <para/>
        /// Note: For most applications there should be no reason to use these methods.
        /// Do not use this method unless you're very sure it is the one you need.
        /// </summary>
        /// <param name="member">The index of the member to get.</param>
        /// <param name="index">The array index of the member to get.</param>
        /// <returns>The member container.</returns>
        /// <exception cref="IllegalValueException">The index is not in the range of the array.</exception>
        /// <exception cref="SoftwareViolationException">The element is not an array and the index is not 0.</exception>
        public abstract ContainerBase GetMember(System.Int32 member,
                                                System.Int32 index);
    }

    /// <summary>
    /// Template class for all containers of automatically generated DOB objects.
    /// <para/>
    /// This class holds a smart pointer to an object, and has operations
    /// to get information from it and modify it.
    /// </summary>
    /// <typeparam name="T">The type to contain. Must inherit from Dob.Typesystem.Object</typeparam>
    public class ObjectContainerImpl <T> : ObjectContainerBase, ICloneable where T : Object
    {
        /// <summary>
        /// Default constructor.
        /// <para/>
        /// Creates a null and not changed container.
        /// </summary>
        public ObjectContainerImpl() : base() { }

        #region Cloning

        object ICloneable.Clone()
        {
            return new ObjectContainerImpl<T>(this);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        /// <returns>ObjectContainerImpl</returns>
        public new ObjectContainerImpl<T> Clone()
        {
            return (ObjectContainerImpl<T>)((ICloneable)this).Clone(); 
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="other"></param>
        protected ObjectContainerImpl(ObjectContainerImpl<T> other)
            : base(other)
        {
            if (other.IsNull())
            {
                m_Object = null;
            }
            else
            {
                m_Object = (T)(other.m_Object.Clone());
            }
        }

        /// <summary>
        /// Implementation of abstract in ContainerBase.
        /// </summary>
        /// <param name="other">ContainerBase</param>
        public override void Copy(ContainerBase other)
        {
            base.Copy(other);
            ObjectContainerImpl<T> that = other as ObjectContainerImpl<T>;
            if (that.IsNull())
            {
                m_Object = null;
            }
            else
            {
                InternalObj = that.m_Object.Clone();
            }
        }

        #endregion

        /// <summary>
        /// Obj property.
        /// </summary>
        public T Obj
        {
            set 
            { 
                m_Object = value; 
                m_bIsChanged = true; 
            }
            get 
            {
                if (IsNull())
                {
                    throw new NullException("Object is null");
                }
                return m_Object;
            }
        }

        /// <summary>
        /// Override of inherited method. See comment for parent class.
        /// </summary>
        /// <returns></returns>
        public override bool IsChanged()
        {
            return m_bIsChanged || (!IsNull() && m_Object.IsChanged());
        }

        /// <summary>
        /// Override of inherited method. See comment for parent class.
        /// </summary>
        /// <returns></returns>
        public override void SetChanged(bool changed)
        {
            base.SetChanged(changed);
            if (!IsNull())
            {
                m_Object.SetChanged(changed);
            }
        }

        /// <summary>
        /// Override of inherited method. See comment for parent class.
        /// </summary>
        /// <returns></returns>
        public override bool IsNull()
        {
            return (m_Object == null);
        }

        /// <summary>
        /// Override of inherited method. See comment for parent class.
        /// </summary>
        /// <returns></returns>
        public override void SetNull()
        {
            m_Object = null;
            m_bIsChanged = true;
        }
        
        /// <summary>
        /// Calculate the size of the blob-serialized form of the contained object.
        /// </summary>
        /// <returns>The needed size in bytes. 0 if the container is null.</returns>
        public System.Int32 CalculateBlobSize() {if (IsNull()) return 0; else return m_Object.CalculateBlobSize();}

        /// <summary>
        /// Access the contained object directly, without any checks.
        /// No checks are made of the isNull flag, and the isChanged flag is not updated
        /// <para/>
        /// Note: Do not use this method unless you're very sure it is the one you need!
        /// </summary>
        public override Object InternalObj
        {
            get
            {
                return m_Object;
            }
            set
            {
                if (value == null)
                {
                    m_Object = null;
                }
                else
                {
                    m_Object = value as T;
                    if (m_Object == null)
                    {
                        throw new IncompatibleTypesException("The types are not compatible!");
                    }
                }
            }
        }

        /// <summary>
        /// Reflection part (Don't use unless you really know what you're doing!!). See comments in ObjectContainerBase.
        /// </summary>
        /// <param name="member"></param>
        /// <param name="index"></param>
        /// <returns></returns>
        public override ContainerBase GetMember(int member, int index)
        {
            if (IsNull())
            {
                throw new NullException("Object is null!");
            }
            return m_Object.GetMember(member,index);
        }

        private T m_Object;
    }

}
