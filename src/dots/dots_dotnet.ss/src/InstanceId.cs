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
    public class InstanceId : IComparable
    {
        public static InstanceId GenerateRandom()
        {
            return new InstanceId(Internal.Id.DotsId_GenerateRandom64());
        }

        public InstanceId()
        {
            m_instanceId = -1;
            m_instanceIdStr = "";
        }

        public InstanceId(string id)
        {
            m_instanceId = Internal.Id.Generate64BitHash(id);
            m_instanceIdStr = id;
        }

        public InstanceId(Int64 id)
        {
            m_instanceId = id;
            m_instanceIdStr = "";
        }

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

        public void RemoveString()
        {
            m_instanceIdStr = "";
            m_CachedUtf8String = null;
        }

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

        public override int GetHashCode()
        {
            return (int)m_instanceId;
        }

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

        public static bool operator !=(InstanceId first, object second)
        {
            return !(first == second);
        }

        #region IComparable Members

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

        public Int64 RawValue
        {
            get { return m_instanceId; }
        }

        public string RawString
        {
            get { return m_instanceIdStr; }
        }

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
