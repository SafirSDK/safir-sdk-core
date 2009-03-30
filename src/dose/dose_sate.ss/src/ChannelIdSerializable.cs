/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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
    public class ChannelIdSerializable
    {
        public Int64 m_channelId = -1;
        public string m_channelIdStr;

        public ChannelIdSerializable()
        {
            Safir.Dob.Typesystem.ChannelId channelId = new Safir.Dob.Typesystem.ChannelId();
            m_channelId = channelId.RawValue;
            m_channelIdStr = channelId.RawString;
        }

        public ChannelIdSerializable(Int64 channelId, string channelIdStr)
        {
            m_channelId = channelId;
            m_channelIdStr = channelIdStr;
        }

        public ChannelIdSerializable(Int64 channelId)
        {
            m_channelId = channelId;
            m_channelIdStr = "";
        }

        public ChannelIdSerializable(string channelIdStr)
        {
            m_channelId = -1;
            m_channelIdStr = channelIdStr;
        }

        public ChannelIdSerializable(Safir.Dob.Typesystem.ChannelId channelId)
        {
            m_channelId = channelId.RawValue;
            m_channelIdStr = channelId.RawString;
        }

        public Safir.Dob.Typesystem.ChannelId ChannelId()
        {
            if (m_channelId == -1 && m_channelIdStr == "")
            {
                return new Safir.Dob.Typesystem.ChannelId();
            }
           
            if (m_channelIdStr == "")
            {
                return new Safir.Dob.Typesystem.ChannelId(m_channelId);
            }
            if (m_channelId == -1)
            {
                return new Safir.Dob.Typesystem.ChannelId(m_channelIdStr);
            }
            return new Safir.Dob.Typesystem.ChannelId(m_channelId, m_channelIdStr);
        }
    }
}
