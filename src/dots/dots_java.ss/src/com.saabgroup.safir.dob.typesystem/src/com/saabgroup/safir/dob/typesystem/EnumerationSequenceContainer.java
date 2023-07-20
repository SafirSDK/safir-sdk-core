//-*- coding: utf-8 -*-
/******************************************************************************
 *
 * Copyright Saab AB, 2022 (http://safirsdkcore.com)
 *
 * Created by: Lars Hagström / lars@foldspace.nu
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

public abstract class EnumerationSequenceContainer<TEnum extends Enum<TEnum>>
    extends SequenceContainer<TEnum>
    implements EnumerationSequenceContainerBase {

    /**
     * Protected constructor has to take a Class as an argument for us to be able to use java reflection on it.
     * @param clazz The class to operate on.
     */
    protected EnumerationSequenceContainer(Class<TEnum> clazz) {
        m_class = clazz;
    }


    @Override
    public int getOrdinal(int index) {
        return get(index).ordinal();
    }

    @Override
    public void setOrdinal(int index, int value) {
        set(index, toEnum(value));
    }

    @Override
    public void addOrdinal (int value) {
        add(toEnum(value));
    }

    private TEnum toEnum(int value) {
        if (value < 0 || value >= m_class.getEnumConstants().length)
        {
            throw new IllegalValueException("Value " + value + " is not in the enumeration range");
        }
        return m_class.getEnumConstants()[value];
    }

    private Class<TEnum> m_class = null;
}
