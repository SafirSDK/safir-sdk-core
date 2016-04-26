/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
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
    public class HandlerIdSerializeable
    {
        public long m_handlerId = -1;
        public string m_handlerIdStr;

        public HandlerIdSerializeable(long handlerId, string handlerIdStr)
        {
            m_handlerId = handlerId;
            m_handlerIdStr = handlerIdStr;
        }

        public HandlerIdSerializeable(long handlerId)
        {
            m_handlerId = handlerId;
            m_handlerIdStr = "";
        }

        public HandlerIdSerializeable(string handlerIdStr)
        {
            m_handlerId = -1;
            m_handlerIdStr = handlerIdStr;
        }

        public HandlerIdSerializeable(HandlerId handlerId)
        {
            m_handlerId = handlerId.RawValue;
            m_handlerIdStr = handlerId.RawString;
        }

        public HandlerIdSerializeable()
        {
            m_handlerId = -1;
            m_handlerIdStr = "";
        }

        public HandlerId HandlerId()
        {
            if (m_handlerId == -1 && m_handlerIdStr == "")
            {
                return new HandlerId();
            }
            if (m_handlerIdStr == "")
            {
                return new HandlerId(m_handlerId);
            }
            if (m_handlerId == -1)
            {
                return new HandlerId(m_handlerIdStr);
            }

            return new HandlerId(m_handlerId, m_handlerIdStr);
        }
    }
}