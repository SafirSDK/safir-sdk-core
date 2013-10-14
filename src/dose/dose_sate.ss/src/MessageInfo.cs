/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

namespace Sate
{
    public class MessageInfo : ObjectInfo
    {
        public MessageInfo()
        {
        }

        private ChannelIdSerializable channelIdSer;
        public ChannelIdSerializable ChannelIdSer
        {
            get { return channelIdSer; }
            set { channelIdSer = value; }
        }
        public Safir.Dob.Typesystem.ChannelId getChannelId()
        {
            if (channelIdSer != null)
            {
                return channelIdSer.ChannelId();
            }
            return null;
        
        }
        public void setChannelId(Safir.Dob.Typesystem.ChannelId channelId)
        {
            channelIdSer = new ChannelIdSerializable(channelId);
        }
    }
}
