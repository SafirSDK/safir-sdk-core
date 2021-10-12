// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2015-2016 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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

import java.util.Collection;
import java.util.Map;
import java.util.Set;

public class ValueDictionaryContainer<K, C extends ValueContainer<V>, V>
    extends DictionaryContainer<K,C>
{
    @SuppressWarnings("unchecked")
    public ValueDictionaryContainer(final Class containerClass) {
        super();
        m_containerClass = (Class<C>)containerClass;
    }

    public C putVal(K key, V value) {
        m_isChanged=true;
        C container;
        try {
            container = m_containerClass.getDeclaredConstructor().newInstance();
        }
        catch (InstantiationException |
               IllegalAccessException |
               java.lang.NoSuchMethodException |
               java.lang.reflect.InvocationTargetException e) {
            throw new SoftwareViolationException("Internal error in ValueDictionaryContainer: " +
                                                 "Failed to instantiate container.");
        }
        container.setVal(value);

        return m_values.put(key, container);
    }

    private final Class<C> m_containerClass;
}
