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

import java.util.ArrayList;

/**
 * Array of Dob-containers.
 *
 * This generic class is used for arrays of containers in objects.
 * The arrays cannot change size once they have been created.
 * Apart from that they behave like a normal java collection.
 *
 * @param <T> type to contain.
 */
public abstract class ArrayContainer<T extends ContainerBase>
    extends java.util.AbstractList<T>
    implements Cloneable {

    protected ArrayContainer(java.util.ArrayList<T> initializedArray) {
        m_array = initializedArray;
    }

    /**
     * @see java.util.AbstractList#get(int)
     */
    @Override
    public T get(int index) {
        return m_array.get(index);
    }

    /**
     * @see java.util.AbstractCollection#size()
     */
    @Override
    public int size() {
        return m_array.size();
    }

    /**
     * Check if any element has a change flag set on it.
     *
     * Note that if this array contains objects this call will be recursive.
     *
     * @return true if any element has changed.
     */
    public boolean isChanged() {
        for (T cont : m_array) {
            if (cont.isChanged()) {
                return true;
            }
        }
        return false;
    }

    /**
     * Set the change flag on all elements in the array.
     *
     * Note that if this array contains objects this call will be recursive.
     *
     * @param changed The value to set the change flags to.
     */

    public void setChanged(boolean changed) {
        for (T cont : m_array) {
            cont.setChanged(true);
        }
    }


    @SuppressWarnings("unchecked")
    protected ArrayContainer(ArrayContainer<T> other) {
        m_array = (ArrayList<T>) other.m_array.clone();
        //need to clone the elements, currently we've just got two arrays
        //pointing to the same elements
        for (int i = 0; i < m_array.size(); ++i) {
            m_array.set(i, (T)m_array.get(i).clone());
        }
    }

    /**
     * @see com.saabgroup.safir.dob.typesystem.ContainerBase#clone()
     */
    @Override
    public abstract ArrayContainer<T> clone();


    private java.util.ArrayList<T> m_array;
}
