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

namespace Sate
{
    public class EntityInfo : ObjectInfo
    {
        private HandlerIdSerializeable handlerIdSer;
        public HandlerIdSerializeable HandlerIdSer
        {
            get { return handlerIdSer; }
            set { handlerIdSer = value; }
        }
        public Safir.Dob.Typesystem.HandlerId getHandlerId()
        {
            if (handlerIdSer != null)
            {
                return handlerIdSer.HandlerId();
            }
            return null;
        }
        public void setHandlerId(Safir.Dob.Typesystem.HandlerId handlerId)
        {
            handlerIdSer = new HandlerIdSerializeable(handlerId);
        }

        private InstanceIdSerializeable instanceIdSer;
        public InstanceIdSerializeable InstanceIdSer
        {
            get { return instanceIdSer; }
            set { instanceIdSer = value; }
        }
        public Safir.Dob.Typesystem.InstanceId getInstanceId()
        {
            if (instanceIdSer != null)
            {
                return instanceIdSer.InstanceId();
            }
            return null;
        }
        public void setInstanceId(Safir.Dob.Typesystem.InstanceId instanceId)
        {
            instanceIdSer = new InstanceIdSerializeable(instanceId);
        }

        private bool requestorDecides;
        public bool RequestorDecides
        {
            get { return requestorDecides; }
            set { requestorDecides = value; }
        }
        private Int64 timestamp;
        public Int64 Timestamp
        {
            get { return timestamp; }
            set { timestamp = value; }
        }
    }
}
