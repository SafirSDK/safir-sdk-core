//-*- coding: utf-8 -*-
/******************************************************************************
 *
 * Copyright Saab AB, 2009-2015 (http://safirsdkcore.com)
 *
 * Created by: Joel Ottosson / joot
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

public class ValueSequenceContainer<T>
extends SequenceContainer<T>  {
    
    public ValueSequenceContainer() {
        super();
    }
    
    public ValueSequenceContainer(ValueSequenceContainer<T> other) {
        super();
        m_isChanged=other.m_isChanged;
        for (T v : other.m_values) {
            m_values.add(v);
        }
    }
    
    /**
     * @see com.saabgroup.safir.dob.typesystem.ValueContainer#clone()
     */
    @Override
    public ValueSequenceContainer<T> clone() {
        return new ValueSequenceContainer<T>(this);
    }
}
