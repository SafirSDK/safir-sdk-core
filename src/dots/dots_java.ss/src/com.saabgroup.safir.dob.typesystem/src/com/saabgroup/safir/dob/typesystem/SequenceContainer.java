//------------
//-*- coding: utf-8 -*-
/******************************************************************************
 *
 * Copyright Saab AB, 2009-2015 (http://safir.sourceforge.net)
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

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

/**
 * Sequence container.
 *
 * This generic class is used for sequences of all types supported
 * by the Dob. The sequenceContainer does not contain containers but
 * only the raw values of the contained type. That means that individual
 * items does not have change flags and can't be null.
 * The entire sequenceContainer has a change flag, but can't be null.
 *
 * @param <T> type to contain.
 */
public abstract class SequenceContainer<T>
extends ContainerBase
implements java.util.List<T>{

    @Override
    public boolean isNull() {
        return false;
    }

    @Override
    public void setNull() {
        throw new SoftwareViolationException("Sequences cannot be null!");
    }

    @Override
    public boolean add(T e) {
        m_isChanged=true;
        return m_values.add(e);
    }

    @Override
    public void add(int index, T element) {
        m_isChanged=true;
        m_values.add(index, element);
    }

    @Override
    public boolean addAll(Collection<? extends T> c) {
        m_isChanged=true;
        return m_values.addAll(c);
    }

    @Override
    public boolean addAll(int index, Collection<? extends T> c) {
        m_isChanged=true;
        return m_values.addAll(index, c);
    }

    @Override
    public void clear() {
        m_isChanged=true;
        m_values.clear();
    }

    @Override
    public boolean contains(java.lang.Object o) {
        return m_values.contains(o);
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        return m_values.containsAll(c);
    }

    @Override
    public T get(int index) {
        return m_values.get(index);
    }

    @Override
    public int indexOf(java.lang.Object o) {
        return m_values.indexOf(o);
    }

    @Override
    public boolean isEmpty() {
        return m_values.isEmpty();
    }

    @Override
    public Iterator<T> iterator() {
        return m_values.iterator();
    }

    @Override
    public int lastIndexOf(java.lang.Object o) {
        return m_values.lastIndexOf(o);
    }

    @Override
    public ListIterator<T> listIterator() {
        return m_values.listIterator();
    }

    @Override
    public ListIterator<T> listIterator(int index) {
        return m_values.listIterator(index);
    }

    @Override
    public boolean remove(java.lang.Object o) {
        boolean changed=m_values.remove(o);
        if (changed)
            m_isChanged=true;
        return changed;
    }

    @Override
    public T remove(int index) {
        m_isChanged=true;
        return m_values.remove(index);
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        boolean changed=m_values.removeAll(c);
        if (changed)
            m_isChanged=true;
        return changed;
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        boolean changed=m_values.retainAll(c);
        if (changed)
            m_isChanged=true;
        return changed;
    }

    @Override
    public T set(int index, T element) {
        m_isChanged=true;
        return m_values.set(index, element);
    }

    @Override
    public int size() {
        return m_values.size();
    }

    @Override
    public List<T> subList(int fromIndex, int toIndex) {
        return m_values.subList(fromIndex, toIndex);
    }

    @Override
    public java.lang.Object[] toArray() {
        return m_values.toArray();
    }

    @SuppressWarnings("hiding")
    @Override
    public <T> T[] toArray(T[] a) {
        return m_values.toArray(a);
    }

    @Override
    @SuppressWarnings("unchecked")
    void shallowCopy(ContainerBase other)
    {
        super.shallowCopy(other);
        SequenceContainer that = (SequenceContainer)other;
        m_values = that.m_values;
    }


    protected java.util.ArrayList<T> m_values = new java.util.ArrayList<T>();
}
