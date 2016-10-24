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

public class DictionaryContainer<K, V extends ContainerBase>
extends ContainerBase
implements java.util.Map<K, V> {

    public DictionaryContainer() {
        super();
    }

    @Override
    public boolean isNull() {
        return isEmpty();
    }

    @Override
    public void setNull() {
        clear();
    }

    @Override
    public boolean isChanged() {
        if (m_isChanged)
            return m_isChanged;

        for (V val : m_values.values()) {
            if (val.isChanged())
                return true;
        }

        return false;
    }

    /**
     * Is the change flag in the container set?
     *
     * This method is like IsChanged without the recursion.
     *
     * @return True if the containers change flag is set.
     */
    public boolean isChangedHere() {
        return m_isChanged;
    }

    @Override
    public void setChanged(boolean changed) {
        m_isChanged = changed;
        for (V val : m_values.values()) {
            val.setChanged(changed);
        }
    }

    /**
     * Set the change flag in the container.
     *
     * This method is like SetChanged without the recursion
     *
     * @param changed [in] - The value to set the change flag to.
     */
    public void setChangedHere(boolean changed) {
        m_isChanged = changed;
    }

    @Override
    public void clear() {
        m_isChanged = true;
        m_values.clear();
    }

    @Override
    public boolean containsKey(java.lang.Object key) {
        return m_values.containsKey(key);
    }

    @Override
    public boolean containsValue(java.lang.Object value) {
        return m_values.containsValue(value);
    }

    @Override
    public Set<java.util.Map.Entry<K, V>> entrySet() {
        return m_values.entrySet();
    }

    @Override
    public V get(java.lang.Object key) {
        return m_values.get(key);
    }

    @Override
    public boolean isEmpty() {
        return m_values.isEmpty();
    }

    @Override
    public Set<K> keySet() {
        return m_values.keySet();
    }

    @Override
    public V put(K key, V value) {
        m_isChanged=true;
        if (value == null)
        {
            throw new SoftwareViolationException("Container cannot contain null objects");
        }
        return m_values.put(key, value);
    }

    @Override
    public void putAll(Map<? extends K, ? extends V> m) {
        m_isChanged=true;
        m_values.putAll(m);
    }

    @Override
    public V remove(java.lang.Object key) {
        m_isChanged=true;
        return m_values.remove(key);
    }

    @Override
    public int size() {
        return m_values.size();
    }

    @Override
    public Collection<V> values() {
        return m_values.values();
    }

    @Override
    @SuppressWarnings("unchecked")
    void shallowCopy(ContainerBase other)
    {
        super.shallowCopy(other);
        DictionaryContainer that = (DictionaryContainer)other;
        m_values = that.m_values;
    }

    /** Used by MergeChanges to do its magic. */
    void merge(DictionaryContainer that)
    {
        DictionaryContainer<K,V> other = that;
        for (Map.Entry<K,V> entry : ((DictionaryContainer<K,V>)that).m_values.entrySet())
        {
            //is it an object member?
            if (entry.getValue() instanceof ObjectContainerBase)
            {
                ObjectContainerBase fromContainerOB = (ObjectContainerBase)entry.getValue();
                if (fromContainerOB.isChangedHere())
                {
                    V findVal = get(entry.getKey());

                    if (findVal == null)
                    {
                        throw new SoftwareViolationException
                            ("DictionaryContainer::Merge: Changed key not found in target!");
                    }

                    findVal.shallowCopy(fromContainerOB);
                }
                else if (fromContainerOB.isChanged())
                {
                    V findVal = get(entry.getKey());

                    if (findVal == null)
                    {
                        throw new SoftwareViolationException
                            ("DictionaryContainer::Merge: Changed key not found in target!");
                    }

                    ObjectContainerBase intoContainerOB = (ObjectContainerBase)findVal;

                    //recurse
                    Utilities.mergeChanges(intoContainerOB.getObjInternal(),fromContainerOB.getObjInternal());
                }
            }
            else
            {
                if (entry.getValue().isChanged())
                {
                    V findVal = get(entry.getKey());

                    if (findVal == null)
                    {
                        throw new SoftwareViolationException
                            ("DictionaryContainer::Merge: Changed key not found in target!");
                    }

                    findVal.shallowCopy(entry.getValue());
                }
            }
        }
    }

    protected java.util.TreeMap<K, V> m_values = new java.util.TreeMap<K, V>();
}
