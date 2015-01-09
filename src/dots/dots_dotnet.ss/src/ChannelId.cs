/* ****************************************************************************
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
    /// Class containing the identity of a channel.
    /// </summary>
    public class ChannelId
    {
        /// <summary>
        /// Constant representing all channels.
        /// </summary>
        public static readonly ChannelId ALL_CHANNELS = new ChannelId("ALL_CHANNELS");

        /// <summary>
        /// Creates a default channel id.
        /// </summary>
        public ChannelId()
        {
            m_channelId = Internal.Constants.DEFAULT_CHANNEL_ID;
            m_channelIdStr = Internal.Constants.DEFAULT_CHANNEL_ID_STR;
        }

        /// <summary>
        /// Creates a channel id from the given string.
        /// </summary>
        /// <param name="id">String identifying the channel.</param>
        public ChannelId(string id)
        {
            m_channelId = Internal.Id.Generate64BitHash(id);
            m_channelIdStr = id;
        }

        /// <summary>
        /// Creates a channel id from the given id.
        /// </summary>
        /// <param name="id">Identifier identifying the channel.</param>
        public ChannelId(Int64 id)
        {
            m_channelId = id;
            m_channelIdStr = "";
        }

        /// <summary>
        /// Creates a channel id from the given data.
        /// </summary>
        /// <param name="id">Identifier identifying the channel.</param>
        /// <param name="idStr">String identifying the channel.</param>
        public ChannelId(Int64 id, string idStr)
        {
            m_channelId = id;
            m_channelIdStr = idStr;
            if (idStr == null)
            {
                throw new NullReferenceException("String argument to ChannelId constructor cannot be null");
            }
#if DEBUG
            if (idStr.Length != 0 && id != Internal.Id.Generate64BitHash(idStr))
            {
                throw new SoftwareViolationException("ChannelId two-argument constructor got an inconsistent id. Got ("
                    + id + ", '" + idStr + "'), but the string evaluates to "
                    + Internal.Id.Generate64BitHash(idStr) + ".");
            }
#endif
        }

        /// <summary>
        /// Remove the included string from the channel id.
        /// <para/>
        /// This is meant to be used when this type is used as a member of a Dob object.
        /// Using this call before the object gets serialized to binary or xml (i.e.
        /// also before sending it anywhere) means that the string will not be included
        /// when the object is sent.
        /// </summary>
        public void RemoveString()
        {
            m_channelIdStr = "";
            m_CachedUtf8String = null;
        }

        /// <summary>
        /// Equals.
        /// </summary>
        /// <param name="obj">The channel id to compare with.</param>
        /// <returns>True if the channel ids are equal.</returns>
        public override bool Equals(object obj)
        {
            ChannelId other = obj as ChannelId;
            if (other == null)
            {
                return false;
            }
            else
            {
                return m_channelId == other.m_channelId;
            }
        }

        /// <summary>
        /// Static == operator.
        /// </summary>
        /// <param name="first">First channel id.</param>
        /// <param name="second">Second channel id.</param>
        /// <returns>True if the channel ids are equal.</returns>
        public static bool operator ==(ChannelId first, object second)
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
        /// <param name="first">First channel id.</param>
        /// <param name="second">Second channel id.</param>
        /// <returns>True if the channel ids not are equal.</returns>
        public static bool operator !=(ChannelId first, object second)
        {
            return !(first == second);
        }

        /// <summary>
        /// Return a string representation of the channel id.
        /// </summary>
        /// <returns>String representation of the channel id.</returns>
        public override string ToString()
        {
            if (m_channelIdStr.Length != 0)
            {
                return m_channelIdStr;
            }
            else if (m_channelId == Internal.Constants.DEFAULT_CHANNEL_ID)
            {
                return Internal.Constants.DEFAULT_CHANNEL_ID_STR;
            }
            else if (m_channelId == Internal.Constants.ALL_CHANNELS_ID)
            {
                return Internal.Constants.ALL_CHANNELS_ID_STR;
            }
            else
            {
                return m_channelId.ToString();
            }
        }

        /// <summary>
        /// Overridden base class method.
        /// </summary>
        /// <returns>Hash code.</returns>
        public override int GetHashCode()
        {
            return (int)m_channelId;
        }

        #region Internal methods

        /// <summary>
        /// Get the raw 64 bit integer identifier.
        /// </summary>
        public Int64 RawValue
        {
            get { return m_channelId; }
        }

        /// <summary>
        /// Get the string that was used to create this id.
        /// <para/>
        /// If no string was used this method returns an empty string.
        /// </summary>
        public string RawString
        {
            get { return m_channelIdStr; }
        }

        /// <summary>
        /// Get the length of the string when converted to UTF-8 encoding.
        /// <para/>
        /// Includes one byte for a null termination.
        /// </summary>
        /// <returns>The length of the string of the id when converted to UTF-8.</returns>
        public System.Int32 Utf8StringLength()
        {
            if (m_channelIdStr.Length == 0)
            {
                return 0;
            }

            if (m_CachedUtf8String == null)
            {
                m_CachedUtf8String = System.Text.Encoding.UTF8.GetBytes(m_channelIdStr);
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
                m_CachedUtf8String = System.Text.Encoding.UTF8.GetBytes(m_channelIdStr);
            }

            return m_CachedUtf8String;
        }

        #endregion

        #region Private part
        private Int64 m_channelId = -1;
        private string m_channelIdStr;
        private byte[] m_CachedUtf8String;
        #endregion
    }
}
