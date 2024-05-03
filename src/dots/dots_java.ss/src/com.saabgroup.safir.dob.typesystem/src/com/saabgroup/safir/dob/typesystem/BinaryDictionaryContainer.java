// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2015-2016, 2024 (http://safirsdkcore.com)
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

public class BinaryDictionaryContainer<K>
    extends DictionaryContainer<K,BinaryContainer>
{
    /**
     * Set the value for a particular key.
     *
     * @param key The key to assign a value for
     * @param value The value to assign.
     * @return The container that was modified/created.
     */
    public BinaryContainer putVal(K key, byte[] value) {
        m_isChanged=true;
        BinaryContainer container = new BinaryContainer();
        container.setVal(value);

        return m_values.put(key, container);
    }

    @Override
    @SuppressWarnings("unchecked")
    public BinaryContainer putNull(java.lang.Object key) {
        m_isChanged=true;
        BinaryContainer container = new BinaryContainer();
        container.setChanged(true);
        m_values.put((K)key, container);
        return container;
    }

}
