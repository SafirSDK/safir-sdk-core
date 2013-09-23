/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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

using System;
using System.Collections.Generic;
using System.Text;

namespace Sate
{
    public class HandlerIdSerializeable
    {
        public Int64 m_handlerId = -1;
        public string m_handlerIdStr;
    
        public HandlerIdSerializeable(Int64 handlerId, string handlerIdStr)
        {
            m_handlerId = handlerId;
            m_handlerIdStr = handlerIdStr;
        }

        public HandlerIdSerializeable(Int64 handlerId)
        {
            m_handlerId = handlerId;
            m_handlerIdStr = "";
        }

        public HandlerIdSerializeable(string handlerIdStr)
        {
            m_handlerId = -1;
            m_handlerIdStr = handlerIdStr;
        }
        public HandlerIdSerializeable(Safir.Dob.Typesystem.HandlerId handlerId)
        {
            m_handlerId = handlerId.RawValue;
            m_handlerIdStr = handlerId.RawString;
        }

        public HandlerIdSerializeable()
        {
            m_handlerId = -1;
            m_handlerIdStr = "";
        }

        public Safir.Dob.Typesystem.HandlerId HandlerId()
        {
            if (m_handlerId == -1 && m_handlerIdStr == "")
            {
                return new Safir.Dob.Typesystem.HandlerId();
            }
            if (m_handlerIdStr == "")
            {
                return new Safir.Dob.Typesystem.HandlerId(m_handlerId);
            }
            if (m_handlerId == -1)
            {
                return new Safir.Dob.Typesystem.HandlerId(m_handlerIdStr);
            }
            
            return new Safir.Dob.Typesystem.HandlerId(m_handlerId, m_handlerIdStr);
        }
    }
}
