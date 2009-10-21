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
 * Container for Int32 members.
 */
public class Int32Container
    extends ValueContainer<Integer>
    implements Cloneable {

    /**
     * Default constructor.
     */
    public Int32Container(){
        super();
        m_value = 0;
    }

    Int32Container(int value, boolean isNull, boolean isChanged) {
        super(isNull,isChanged);
        m_value = value;
    }

    protected Int32Container(Int32Container other) {
        super(other);
    }

    /**
     * @see com.saabgroup.safir.dob.typesystem.ValueContainer#clone()
     */
    @Override
    public Int32Container clone() {
        return new Int32Container(this);
    }
}
