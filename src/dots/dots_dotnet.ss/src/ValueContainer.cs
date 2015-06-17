/* ****************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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
    public class ValueContainer<T> : ContainerBase
    {
        /// <summary>
        /// Default constructor
        /// <para/>
        /// Creates a null and not changed container.
        /// </summary>
        public ValueContainer(): base()
        {
            m_bIsNull = true;
        }



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
        /// Equality operator for ValueContainer and value.
        ///
        /// <para/>
        /// This operator lets you compare the container with a value of the contained type.
        /// It will return false if the container is null or the values are not equal.
        /// The change flag is ignored.
        /// </summary>
        /// <param name="first">First value to compare.</param>
        /// <param name="second">Second value to compare.</param>
        /// <returns>True if the container is non-null and the values are equal.</returns>
        public static bool operator==(ValueContainer<T> first, T second)
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
        /// Inequality operator for ValueContainer and container.
        /// </summary>
        /// <param name="first">First value to compare.</param>
        /// <param name="second">Second value to compare.</param>
        /// <returns>True if the container is null or the values are different.</returns>
        public static bool operator !=(ValueContainer<T> first, T second)
        {
            return !(first == second);
        }

        /// <summary>
        /// Equality operator for value and ValueContainer.
        /// </summary>
        /// <param name="first">First value to compare.</param>
        /// <param name="second">Second value to compare.</param>
        /// <returns>True if the container is non-null and the values are equal.</returns>
        public static bool operator ==(T first, ValueContainer<T> second)
        {
            return second == first;
        }

        /// <summary>
        /// Equality operator for value and ValueContainer.
        /// </summary>
        /// <param name="first">First value to compare.</param>
        /// <param name="second">Second value to compare.</param>
        /// <returns>True if the container is null or the values are different.</returns>
        public static bool operator !=(T first, ValueContainer<T> second)
        {
            return !(second == first);
        }

        /// <summary>
        /// Compare two ValueContainers for equality.
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        public override bool Equals(object obj)
        {
            ValueContainer<T> vc = obj as ValueContainer<T>;
            if (vc == null)
            {
                return false;
            }

            if (IsChanged() != vc.IsChanged())
            {
                return false;
            }

            if (IsNull() && vc.IsNull())
            {
                return true;
            }

            if (IsNull() != vc.IsNull())
            {
                return false;
            }

            return m_Value.Equals(vc.m_Value);
        }

        /// <summary>
        /// Get a hash code for a ValueContainer.
        /// </summary>
        /// <returns>a hash code</returns>
        public override int GetHashCode()
        {
            unchecked // Overflow is fine, just wrap
            {
                int hash = 17;

                hash = hash * 486187739 + m_bIsNull.GetHashCode();
                hash = hash * 486187739 + m_bIsChanged.GetHashCode();
                if (!m_bIsNull)
                {
                    hash = hash * 486187739 + m_Value.GetHashCode();
                }

                return hash;
            }
        }

        internal override void ShallowCopy(ContainerBase other)
        {
            base.ShallowCopy(other);
            ValueContainer<T> that = (ValueContainer<T>)other;
            m_bIsNull = that.m_bIsNull;
            m_Value = that.m_Value;
        }

        internal bool m_bIsNull;
        internal T m_Value;
    }


}
