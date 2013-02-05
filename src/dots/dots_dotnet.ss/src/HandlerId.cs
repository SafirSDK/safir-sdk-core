/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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
    /// Class containing the identity of a handler.
    /// </summary>
    public class HandlerId
    {
        /// <summary>
        /// Represents all handlers.
        /// </summary>
        public static readonly HandlerId ALL_HANDLERS = new HandlerId("ALL_HANDLERS");

        /// <summary>
        /// Constructs a default handler id.
        /// </summary>
        public HandlerId()
        {
            m_handlerId = Internal.Constants.DEFAULT_HANDLER_ID;
            m_handlerIdStr = Internal.Constants.DEFAULT_HANDLER_ID_STR;
        }

        /// <summary>
        /// Constructs a handler id from the given string.
        /// </summary>
        /// <param name="id">String identifying the handler.</param>
        public HandlerId(string id)
        {
            m_handlerId = Internal.Id.Generate64BitHash(id);
            m_handlerIdStr = id;
        }

        /// <summary>
        /// Constructs a handler id from the given id.
        /// </summary>
        /// <param name="id">Identifier identifying the handler.</param>
        public HandlerId(Int64 id)
        {
            m_handlerId = id;
            m_handlerIdStr = "";
        }

        /// <summary>
        /// Constructs a handler id from the given data.
        /// </summary>
        /// <param name="id">Identifier identifying the handler.</param>
        /// <param name="idStr">String identifying the handler.</param>
        public HandlerId(Int64 id, string idStr)
        {
            m_handlerId = id;
            m_handlerIdStr = idStr;
            if (idStr == null)
            {
                throw new NullReferenceException("String argument to HandlerId constructor cannot be null");
            }
#if DEBUG
            if (idStr.Length != 0 && id != Internal.Id.Generate64BitHash(idStr))
            {
                throw new SoftwareViolationException("HandlerId two-argument constructor got an inconsistent id. Got ("
                    + id + ", '" + idStr +"'), but the string evaluates to "
                    + Internal.Id.Generate64BitHash(idStr) + ".");
            }
#endif
        }

        /// <summary>
        /// Remove the included string from the handler id.
        /// <para>
        /// This is meant to be used when this type is used as a member of a Dob object.
        /// Using this call before the object gets serialized to binary or xml (i.e.
        /// also before sending it anywhere) means that the string will not be included
        /// when the object is sent.
        /// </para>
        /// </summary>
        public void RemoveString()
        {
            m_handlerIdStr = "";
            m_CachedUtf8String = null;
        }

        /// <summary>
        /// Equality operator.
        /// </summary>
        /// <param name="obj">The HandlerId to compare with.</param>
        /// <returns>True if equal.</returns>
        public override bool Equals(object obj)
        {
            HandlerId other = obj as HandlerId;
            if (other == null)
            {
                return false;
            }
            else
            {
                return m_handlerId == other.m_handlerId;
            }
        }

        /// <summary>
        /// Equality operator.
        /// </summary>
        /// <param name="first">First HandlerId.</param>
        /// <param name="second">Second HandlerId.</param>
        /// <returns>True if equal.</returns>
        public static bool operator ==(HandlerId first, object second)
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
        /// Inequality operator.
        /// </summary>
        /// <param name="first">First HandlerId.</param>
        /// <param name="second">Second HandlerId.</param>
        /// <returns>True if not equal.</returns>
        public static bool operator !=(HandlerId first, object second)
        {
            return !(first == second);
        }

        /// <summary>
        /// Return a string representation of the handler id.
        /// </summary>
        /// <returns>A string representation of the handler id.</returns>
        public override string ToString()
        {
            if (m_handlerIdStr.Length != 0)
            {
                return m_handlerIdStr;
            }
            else if (m_handlerId == Internal.Constants.DEFAULT_HANDLER_ID)
            {
                return Internal.Constants.DEFAULT_HANDLER_ID_STR;
            }
            else if (m_handlerId == Internal.Constants.ALL_HANDLERS_ID)
            {
                return Internal.Constants.ALL_HANDLERS_ID_STR;
            }
            else
            {
                return m_handlerId.ToString();
            }
        }

        /// <summary>
        /// Hash code.
        /// </summary>
        /// <returns>Hash code.</returns>
        public override int GetHashCode()
        {
            return (int)m_handlerId;
        }

        #region Internal methods

        /// <summary>
        /// Get the raw 64 bit integer identifier.
        /// </summary>
        public Int64 RawValue
        {
            get { return m_handlerId; }
        }

        /// <summary>
        /// Get the string that was used to create this id.
        /// <para/>
        /// If no string was used this method returns an empty string.
        /// </summary>
        public string RawString
        {
            get { return m_handlerIdStr; }
        }

        /// <summary>
        /// Get the length of the string when converted to UTF-8 encoding.
        /// <para/>
        /// Includes one byte for a null termination.
        /// </summary>
        /// <returns>The length of the string of the id when converted to UTF-8.</returns>
        public System.Int32 Utf8StringLength()
        {
            if (m_handlerIdStr.Length == 0)
            {
                return 0;
            }

            if (m_CachedUtf8String == null)
            {
                m_CachedUtf8String = System.Text.Encoding.UTF8.GetBytes(m_handlerIdStr);
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
                m_CachedUtf8String = System.Text.Encoding.UTF8.GetBytes(m_handlerIdStr);
            }

            return m_CachedUtf8String;
        }
        #endregion

        #region Private part
        private Int64 m_handlerId = -1;
        private string m_handlerIdStr;
        private byte[] m_CachedUtf8String;
#endregion
    }
}
