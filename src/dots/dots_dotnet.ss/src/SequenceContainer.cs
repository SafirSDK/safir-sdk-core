/******************************************************************************
*
* Copyright Saab AB, 2005-2015 (http://safir.sourceforge.net)
* 
* Created by: Joel Ottosson / joot
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
    /// Container for base types.
    /// 
    /// <para/>
    /// This class holds a value of the template argument type and a null flag.
    /// The operations that modify the value update the null flag and the change flag
    /// (which is inherited from ContainerBase).
    /// 
    /// <para/>
    /// This container is intended for the simple types of the DOB typesystem.
    /// </summary>
    /// <typeparam name="T">The type to contain.</typeparam>
    public class SequenceContainer<T> : ContainerBase, ICloneable
    {
        /// <summary>
        /// Default constructor
        /// <para/>
        /// Creates a null and not changed container.
        /// </summary>
        public SequenceContainer(): base()
        {
            m_bIsNull = true;
        }

        #region Cloning

        object ICloneable.Clone()
        {
            return new SequenceContainer<T>(this);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public new SequenceContainer<T> Clone()
        {
            return (SequenceContainer<T>)((ICloneable)this).Clone(); 
        }

        /// <summary>
        /// Copy constructor for use by Clone
        /// </summary>
        /// <param name="other"></param>
        protected SequenceContainer(SequenceContainer<T> other):
            base(other)
        {
            m_bIsNull = other.m_bIsNull;
            m_Value = other.m_Value;
        }

        /// <summary>
        /// Override ContainerBase.
        /// </summary>
        /// <param name="other"></param>
        public override void Copy(ContainerBase other)
        {
            base.Copy(other);
            SequenceContainer<T> that = other as SequenceContainer<T>;
            m_bIsNull = that.m_bIsNull;
            m_Value = that.m_Value;
        }

        #endregion

        /// <summary>
        /// The value of the container. 
        /// Null and change flags are updated accordingly.
        /// </summary>
        public T Val
        {
            get { if (m_bIsNull) throw new NullException("Value is null"); return m_Value; }
            set { m_Value = value; m_bIsNull = false; m_bIsChanged = true; }
        }

        /// <summary>
        /// Override ContainerBase.
        /// </summary>
        public override bool IsNull()
        {
            return m_bIsNull;
        }

        /// <summary>
        /// Override ContainerBase.
        /// </summary>
        public override void SetNull()
        {
            m_bIsNull = true;
            m_bIsChanged = true;
        }

        /// <summary>
        /// Equality operator for SequenceContainer and value.
        /// 
        /// <para/>
        /// This operator lets you compare the container with a value of the contained type.
        /// It will return false if the container is null or the values are not equal.
        /// The change flag is ignored.
        /// </summary>
        /// <param name="first">First value to compare.</param>
        /// <param name="second">Second value to compare.</param>
        /// <returns>True if the container is non-null and the values are equal.</returns>
        public static bool operator ==(SequenceContainer<T> first, T second)
        {
            // If both are null, or both are same instance, return true.
            if (System.Object.ReferenceEquals(first, second))
            {
                return true;
            }

            // If one is null, but not both, return false.
            if (((object)first == null) || ((object)second == null))
            {
                return false;
            }

            // Return true if the fields match:
            return !first.IsNull() && first.m_Value.Equals(second);
        }

        /// <summary>
        /// Inequality operator for SequenceContainer and container.
        /// </summary>
        /// <param name="first">First value to compare.</param>
        /// <param name="second">Second value to compare.</param>
        /// <returns>True if the container is null or the values are different.</returns>
        public static bool operator !=(SequenceContainer<T> first, T second)
        {
            return !(first == second);
        }

        /// <summary>
        /// Equality operator for value and SequenceContainer.
        /// </summary>
        /// <param name="first">First value to compare.</param>
        /// <param name="second">Second value to compare.</param>
        /// <returns>True if the container is non-null and the values are equal.</returns>
        public static bool operator ==(T first, SequenceContainer<T> second)
        {
            return second == first;
        }

        /// <summary>
        /// Equality operator for value and SequenceContainer.
        /// </summary>
        /// <param name="first">First value to compare.</param>
        /// <param name="second">Second value to compare.</param>
        /// <returns>True if the container is null or the values are different.</returns>
        public static bool operator !=(T first, SequenceContainer<T> second)
        {
            return !(second == first);
        }

        /// <summary>
        /// Not implementd!
        /// </summary>
        /// <returns></returns>
        public override int GetHashCode()
        {
            throw new System.Exception("The method or operation is not implemented.");
        }

        /// <summary>
        /// Not implementd!
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        public override bool Equals(object obj)
        {
            throw new System.Exception("The method or operation is not implemented.");
        }



        internal bool m_bIsNull;
        internal T m_Value;
    }


}
