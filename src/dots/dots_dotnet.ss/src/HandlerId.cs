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
    public class HandlerId
    {
        public static readonly HandlerId ALL_HANDLERS = new HandlerId("ALL_HANDLERS");

        public HandlerId()
        {
            m_handlerId = Internal.Constants.DEFAULT_HANDLER_ID;
            m_handlerIdStr = Internal.Constants.DEFAULT_HANDLER_ID_STR;
        }

        public HandlerId(string id)
        {
            m_handlerId = Internal.Id.Generate64BitHash(id);
            m_handlerIdStr = id;
        }

        public HandlerId(Int64 id)
        {
            m_handlerId = id;
            m_handlerIdStr = "";
        }

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

        public void RemoveString()
        {
            m_handlerIdStr = "";
            m_CachedUtf8String = null;
        }

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

        public static bool operator !=(HandlerId first, object second)
        {
            return !(first == second);
        }

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

        public override int GetHashCode()
        {
            return (int)m_handlerId;
        }

        public Int64 RawValue
        {
            get { return m_handlerId; }
        }

        public string RawString
        {
            get { return m_handlerIdStr; }
        }

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

        public byte[] Utf8String()
        {
            if (m_CachedUtf8String == null)
            {
                /*if (m_handlerIdStr.Length != 0)
                {*/
                    m_CachedUtf8String = System.Text.Encoding.UTF8.GetBytes(m_handlerIdStr);
                /*}
                else
                {
                    m_CachedUtf8String = System.Text.Encoding.UTF8.GetBytes("");
                }*/
            }

            return m_CachedUtf8String;
        }

        #region Private part
        private Int64 m_handlerId = -1;
        private string m_handlerIdStr;
        private byte[] m_CachedUtf8String;
#endregion
    }
}
