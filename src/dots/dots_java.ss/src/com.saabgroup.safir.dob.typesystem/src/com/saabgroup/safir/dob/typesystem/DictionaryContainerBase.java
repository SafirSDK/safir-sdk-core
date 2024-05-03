// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2022, 2024 (http://safirsdkcore.com)
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

public abstract class DictionaryContainerBase
    extends ContainerBase
{
    public DictionaryContainerBase() {
        super();
    }

    /**
     * Get the size of the dictionary, i.e number of contained keys.
     *
     * @return The number of values in the dictionary.
     */
    public abstract int size();

    /**
     * Get the key at a particular position in the dictionary.
     *
     * Note that the order of keys in the dictionary is not guaranteed. Any insertion
     * may reorder the contents.
     *
     * This is a super slow and expensive of accessing the contents of a
     * dictionary. Use only for reflection.
     * For normal use cases it is much better to use normal iteration.
     *
     * For enumeration values the returned value is an Enum object. Use Enum.ordinal()
     * to get the ordinal value.
     *
     * @param index an index between 0 and size().
     * @return The key at a position in the dictionary.
     */
    public abstract java.lang.Object getKeyAt(int index);

    /**
     * Get the container of the value at a particular position in the dictionary.
     *
     * Note that the order of keys in the dictionary is not guaranteed. Any insertion
     * may reorder the contents.
     *
     * This is a super slow and expensive of accessing the contents of a
     * dictionary. Use only for reflection.
     * For normal use cases it is much better to use normal iteration.
     *
     * @param index an index between 0 and size().
     * @return The container at a position in the dictionary.
     */
    public abstract ContainerBase getValueContainerAt(int index);

    /**
     * Set the value for a particular key to null.
     *
     * This is really meant to be part of the reflection functionality, to allow setting
     * values in a dictionary without having instantiated the types directly.
     * After you have called this you will have an empty container that you
     * know inherits from ContainerBase, and that can be cast to the correct type
     * for further use.
     *
     * Warning: There are very few checks implemented in this function, please take care!
     * It is for example likely that you will be able successfully to call this with an
     * incorrect key type, but something will blow up on you later.  It is probably as
     * bad as calling it with an long instead of an int will blow things up.
     *
     * @param key The key to assign to null.
     * @return The newly created empty container.
     */
    public abstract ContainerBase putNull(java.lang.Object key);
}
