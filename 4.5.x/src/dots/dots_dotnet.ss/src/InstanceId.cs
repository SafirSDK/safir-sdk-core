/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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
    /// Class containing the identity of an instance.
    /// </summary>
    public class InstanceId : IComparable
    {
        /// <summary>
        /// Returns a random instance id.
        /// </summary>
        /// <returns>Random instance id.</returns>
        public static InstanceId GenerateRandom()
        {
            return new InstanceId(Internal.Id.DotsId_GenerateRandom64());
        }

        /// <summary>
        /// Default constructor.
        /// <para>
        /// Creates an unspecified instance id.
        /// </para>
        /// </summary>
        public InstanceId()
        {
            m_instanceId = -1;
            m_instanceIdStr = "";
        }

        /// <summary>
        /// Creates an instance id from the given string.
        /// </summary>
        /// <param name="id">String identifying the instance.</param>
        public InstanceId(string id)
        {
            m_instanceId = Internal.Id.Generate64BitHash(id);
            m_instanceIdStr = id;
        }

        /// <summary>
        /// Creates an instance id using a 64 bit integer.
        /// </summary>
        /// <param name="id">The 64bit integer id of the instance.</param>
        public InstanceId(Int64 id)
        {
            m_instanceId = id;
            m_instanceIdStr = "";
        }

        /// <summary>
        /// Creates an instance id from the given data.
        /// </summary>
        /// <param name="id">Identifier identifying the instance.</param>
        /// <param name="idStr">String identifying the instance.</param>
        public InstanceId(Int64 id, string idStr)
        {
            m_instanceId = id;
            m_instanceIdStr = idStr;
            if (idStr == null)
            {
                throw new NullReferenceException("String argument to InstanceId constructor cannot be null");
            }
#if DEBUG
            if (idStr.Length != 0 && id != Internal.Id.Generate64BitHash(idStr))
            {
                throw new SoftwareViolationException("InstanceId two-argument constructor got an inconsistent id. Got ("
                    + id + ", '" + idStr + "'), but the string evaluates to "
                    + Internal.Id.Generate64BitHash(idStr) + ".");
            }
#endif
        }

        /// <summary>
        /// Remove the included string from the instance id.
        /// <para>
        /// This is meant to be used when this type is used as a member of a Dob object.
        /// Using this call before the object gets serialized to binary or xml (i.e.
        /// also before sending it anywhere) means that the string will not be included
        /// when the object is sent.
        /// </para>
        /// </summary>
        public void RemoveString()
        {
            m_instanceIdStr = "";
            m_CachedUtf8String = null;
        }

        /// <summary>
        /// Equals method
        /// </summary>
        /// <param name="obj">The instance id to compare with.</param>
        /// <returns>True if the instance ids are equal.</returns>
        public override bool Equals(object obj)
        {
            InstanceId other = obj as InstanceId;
            if (other == null)
            {
                return false;
            }
            else
            {
                return m_instanceId == other.m_instanceId;
            }
        }

        /// <summary>
        /// Return a string representation of the instance id.
        /// <para/>
        /// If the string that created the instance id is available this is the string that will be returned,
        /// otherwise it is the number that will be returned.
        /// <para/>
        /// The purpose of this function is for debug output and such.
        /// The resulting string can *not* reliably be used in the "string constructor" for InstanceId to
        /// recreate the same InstanceId.
        /// </summary>
        /// <returns>String representation of the instance id.</returns>
        public override string ToString()
        {
            if (m_instanceIdStr.Length != 0)
            {
                return m_instanceIdStr;
            }
            else
            {
                return m_instanceId.ToString();
            }
        }

        /// <summary>
        /// Overridden base class method.
        /// </summary>
        /// <returns>Hash code.</returns>
        public override int GetHashCode()
        {
            return (int)m_instanceId;
        }

        /// <summary>
        /// Static == operator.
        /// </summary>
        /// <param name="first">First instance id.</param>
        /// <param name="second">Second instance id.</param>
        /// <returns>True if the instance ids are equal.</returns>
        public static bool operator ==(InstanceId first, object second)
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
            return first.Equals(second);
        }

        /// <summary>
        /// Static != operator
        /// </summary>
        /// <param name="first">First instance id.</param>
        /// <param name="second">Second instance id.</param>
        /// <returns>True if the instance ids not are equal.</returns>
        public static bool operator !=(InstanceId first, object second)
        {
            return !(first == second);
        }

        #region IComparable Members

        /// <summary>
        /// Compare to specified object.
        /// </summary>
        /// <param name="obj">Object to compare to.</param>
        /// <returns>Relative order</returns>
        public int CompareTo(object obj)
        {
            if (obj is InstanceId)
            {
                return m_instanceId.CompareTo(((InstanceId)obj).m_instanceId);
            }
            else
            {
                throw new ArgumentException("object is not an InstanceId.");
            }
        }

        #endregion

        /// <summary>
        /// Get the raw 64 bit integer identifier.
        /// </summary>
        public Int64 RawValue
        {
            get { return m_instanceId; }
        }

        /// <summary>
        /// Get the string that was used to create this id.
        /// <para/>
        /// If no string was used this method returns an empty string.
        /// </summary>
        public string RawString
        {
            get { return m_instanceIdStr; }
        }

        /// <summary>
        /// Get the length of the string when converted to UTF-8 encoding.
        /// <para/>
        /// Includes one byte for a null termination.
        /// </summary>
        /// <returns>The length of the string of the id when converted to UTF-8.</returns>
        public System.Int32 Utf8StringLength()
        {
            if (m_instanceIdStr.Length == 0)
            {
                return 0;
            }

            if (m_CachedUtf8String == null)
            {
                m_CachedUtf8String = System.Text.Encoding.UTF8.GetBytes(m_instanceIdStr);
            }

            return m_CachedUtf8String.Length + 1;
        }

        /// <summary>
        /// Convert the string to UTF-8.
        /// <para/>
        /// Returns an empty string if there is no string.
        /// </summary>
        /// <returns>UTF-8 representation of the string.</returns>
        public byte[] Utf8String()
        {
            if (m_CachedUtf8String == null)
            {
                m_CachedUtf8String = System.Text.Encoding.UTF8.GetBytes(m_instanceIdStr);
            }

            return m_CachedUtf8String;
        }

        #region Private part
        private Int64 m_instanceId = -1;
        private string m_instanceIdStr;
        private byte[] m_CachedUtf8String;
#endregion


    }
}
