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
    public class EntityIdSerializeable
    {
        public System.Int64 m_typeId = -1;
        public InstanceIdSerializeable m_instanceIdSerializeable;

        public EntityIdSerializeable(Int64 typeId, InstanceIdSerializeable instanceIdSerializeable)
        {
            m_typeId = typeId;
            m_instanceIdSerializeable = instanceIdSerializeable;
        }

        public EntityIdSerializeable()
        {
            m_typeId = -1;
            m_instanceIdSerializeable = new InstanceIdSerializeable();
        }

        public EntityIdSerializeable(Safir.Dob.Typesystem.EntityId entityId)
        {
            m_typeId = entityId.TypeId;
            m_instanceIdSerializeable = new InstanceIdSerializeable(entityId.InstanceId);
        }

        public Safir.Dob.Typesystem.EntityId EntityId()
        {
            return new Safir.Dob.Typesystem.EntityId(m_typeId, m_instanceIdSerializeable.InstanceId());
        }
    }
}
