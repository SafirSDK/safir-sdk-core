/* ****************************************************************************
*
* Copyright Saab AB, 2005-2013,2015 (http://safir.sourceforge.net)
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
    /// Container for strings.
    ///
    /// <para/>
    /// This is a container for strings. It differs from the ordinary ValueContainer
    /// in that it has methods for converting to UTF8 strings. These
    /// are really only meant for blob serialization to use.
    /// </summary>
    public class StringContainer : ContainerBase
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        public StringContainer(): base()
        {
            m_bIsNull = true;
        }



        /// <summary>
        /// Val.
        /// </summary>
        public string Val
        {
            get { if (m_bIsNull) throw new NullException("Value is null"); return m_Value; }
            set { m_Value = value; m_bIsNull = false; m_bIsChanged = true; m_CachedUtf8String = null; }
        }

        /// <summary>
        /// Is the container set to null?
        /// </summary>
        /// <returns>True if the container is set to null.</returns>
        public override bool IsNull()
        {
            return m_bIsNull;
        }

        /// <summary>
        /// Set the container to null.
        /// </summary>
        public override void SetNull()
        {
            m_bIsNull = true;
            m_bIsChanged = true;
            m_CachedUtf8String = null;
        }

        #region UTF8 encoding methods

        /// <summary>
        /// Calculate the length needed for this string in UTF8 encoding.
        /// <para/>
        /// This method converts the string to utf8 (and caches it) and returns the
        /// length of the converted string.
        /// <para/>
        /// The returned value includes space for null termination (adds 1 to string length).
        /// </summary>
        /// <returns>The length of the string when converted to UTF8, or 0 if the container is null.</returns>
        public System.Int32 Utf8StringLength()
        {
            if (IsNull())
            {
                return 0;
            }

            if (m_Value.Length == 0)
            {
                return 1;
            }

            if (m_CachedUtf8String == null)
            {
                m_CachedUtf8String = System.Text.Encoding.UTF8.GetBytes(m_Value);
            }

            return m_CachedUtf8String.Length + 1;
        }

        /// <summary>
        /// Convert the string to a UTF8 encoded std::string.
        /// <para/>
        /// This method converts the string to utf8 (and caches it) and returns the result.
        /// </summary>
        /// <returns>UTF8 string.</returns>
        /// <exception cref="NullException">The container is null.</exception>
        public byte [] Utf8String()
        {
            if (IsNull())
            {
                throw new NullException("The string is null, cannot convert!");
            }

            if (m_CachedUtf8String == null)
            {
                if (m_Value.Length != 0)
                {
                    m_CachedUtf8String = System.Text.Encoding.UTF8.GetBytes(m_Value);
                }
                else
                {
                    m_CachedUtf8String = System.Text.Encoding.UTF8.GetBytes("");
                }
            }

            return m_CachedUtf8String;
        }

        #endregion

        /// <summary>
        /// Equality operator for StringContainer and a string.
        /// </summary>
        /// <param name="first">String container.</param>
        /// <param name="second">string</param>
        /// <returns>True if the container is non-null and the strings are equal.</returns>
        public static bool operator ==(StringContainer first, string second)
        {
            return !first.IsNull() && first.m_Value.Equals(second);
        }

        /// <summary>
        /// Inequality operator for StringContainer and a string.
        /// </summary>
        /// <param name="first">String container.</param>
        /// <param name="second">string</param>
        /// <returns>True if the container is null or the strings are different.</returns>
        public static bool operator !=(StringContainer first, string second)
        {
            return !(first == second);
        }

        /// <summary>
        /// Equality operator for string and StringContainer.
        /// </summary>
        /// <param name="first"></param>
        /// <param name="second"></param>
        /// <returns></returns>
        public static bool operator ==(string first, StringContainer second)
        {
            return second == first;
        }

        /// <summary>
        /// Inequality operator for string and a StringContainer.
        /// </summary>
        /// <param name="first">string</param>
        /// <param name="second">StringContainer</param>
        /// <returns>True if the container is null or the strings are different.</returns>
        public static bool operator !=(string first, StringContainer second)
        {
            return !(second == first);
        }

        /// <summary>
        /// Get a hash code for a StringContainer.
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

        /// <summary>
        ///
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        public override bool Equals(object obj)
        {
            StringContainer sc = obj as StringContainer;
            if (sc == null)
            {
                return false;
            }

            if (IsChanged() != sc.IsChanged())
            {
                return false;
            }

            if (IsNull() && sc.IsNull())
            {
                return true;
            }

            if (IsNull() != sc.IsNull())
            {
                return false;
            }

            return m_Value.Equals(sc.m_Value);
        }


        internal override void ShallowCopy(ContainerBase other)
        {
            base.ShallowCopy(other);
            StringContainer that = (StringContainer)other;
            m_bIsNull = that.m_bIsNull;
            m_Value = that.m_Value;
            m_CachedUtf8String = that.m_CachedUtf8String;
        }

        internal bool m_bIsNull;
        internal string m_Value;
        private byte[] m_CachedUtf8String;
    }



}
