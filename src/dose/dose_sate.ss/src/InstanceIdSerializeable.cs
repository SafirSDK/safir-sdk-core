/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
* 
* Created by: Stefan Lindström / stsyli
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

using Safir.Dob.Typesystem;

namespace Sate
{
    public class InstanceIdSerializeable
    {
        public long m_instanceId = -1;
        public string m_instanceIdStr;

        public InstanceIdSerializeable(long instanceId, string instanceIdStr)
        {
            m_instanceId = instanceId;
            m_instanceIdStr = instanceIdStr;
        }

        public InstanceIdSerializeable(long instanceId)
        {
            m_instanceId = instanceId;
        }

        public InstanceIdSerializeable(string instanceIdStr)
        {
            m_instanceIdStr = instanceIdStr;
        }

        public InstanceIdSerializeable()
        {
            /*   m_instanceId = -1;
            m_instanceIdStr = ""; */
            var instanceId = new InstanceId();
            m_instanceId = instanceId.RawValue;
            m_instanceIdStr = instanceId.RawString;
        }

        public InstanceIdSerializeable(InstanceId instanceId)
        {
            m_instanceId = instanceId.RawValue;
            m_instanceIdStr = instanceId.RawString;
        }

        public InstanceId InstanceId()
        {
            if (m_instanceIdStr == "")
            {
                return new InstanceId(m_instanceId);
            }
            if (m_instanceId == -1)
            {
                return new InstanceId(m_instanceIdStr);
            }
            if (m_instanceId == -1 && m_instanceIdStr == "")
            {
                return new InstanceId();
            }
            return new InstanceId(m_instanceId, m_instanceIdStr);
        }
    }
}