// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009-2013 (http://safir.sourceforge.net)
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
 * Container for TypeId members.
 */
public class TypeIdContainer
    extends Int64Container
    implements Cloneable {

    /**
     * Default constructor.
     */
    public TypeIdContainer(){
        super();
    }

    TypeIdContainer(long value, boolean isNull, boolean isChanged) {
        super(value, isNull,isChanged);
    }

    protected TypeIdContainer(TypeIdContainer other) {
        super(other);
    }

    /**
     * @see com.saabgroup.safir.dob.typesystem.Float32Container#clone()
     */
    @Override
    public TypeIdContainer clone() {
        return new TypeIdContainer(this);
    }


}
