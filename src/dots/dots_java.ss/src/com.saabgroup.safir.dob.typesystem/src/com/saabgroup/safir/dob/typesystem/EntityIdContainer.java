// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009 (http://www.safirsdk.com)
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

package com.saabgroup.safir.dob.typesystem;

/**
 * Container for EntityId members.
 */
public class EntityIdContainer
    extends ValueContainer<EntityId>
    implements Cloneable {

    /**
     * Default constructor.
     */
    public EntityIdContainer(){
        super();
        m_value = null;
    }

    EntityIdContainer(EntityId value, boolean isNull, boolean isChanged) {
        super(isNull,isChanged);
        m_value = value;
    }


    protected EntityIdContainer(EntityIdContainer other) {
        super(other);
    }

    /**
     * @see com.saabgroup.safir.dob.typesystem.ValueContainer#clone()
     */
    @Override
    public EntityIdContainer clone() {
        return new EntityIdContainer(this);
    }

}
