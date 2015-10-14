// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009-2013 (http://safirsdkcore.com)
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
 * Template class for all containers of automatically generated DOB objects.
 *
 * This class holds a smart pointer to an object, and has operations
 * to get information from it and modify it.
 * The -> operator is overloaded to make this class more transparent to use.
 *
 * It is called ObjectContainerImpl because the name ObjectContainer is "taken"
 * by the container that contains a Dob.Typesystem.Object object.
 *
 * @param <T> The type to contain. Must inherit from Dob::Typesystem::Object.
 */
public class ObjectContainerImpl<T extends Object>
    extends ObjectContainerBase {

    /**
     * Default constructor.
     *
     * Creates a null and not changed container.
     */
    public ObjectContainerImpl()
    {
        super();
    }

    //internal constructor.
    ObjectContainerImpl(T object, boolean isChanged) {
        super(isChanged);
        m_object = object;
    }

    /**
     * Set object in the container.
     *
     * This method will set the contained pointer to point to another
     * object.
     * The change flag of the container will be updated.
     *
     * @param obj An object to point to.
    */
    public void setObj(T obj)
    {
        m_object = obj;
        m_isChanged = true;
    }

    /**
     * Get the object from the container.
     *
     * This method will return the contained object unless the container is null, then
     * an exception will be thrown.
     *
     * @return the contained object.
     * @throws NullException The container is null.
    */
    public T getObj()
    {
        if (isNull())
        {
            throw new NullException("Object is null");
        }
        return m_object;
    }

    /**
     * @see com.saabgroup.safir.dob.typesystem.ContainerBase#isChanged()
     */
    @Override
    public boolean isChanged() {
        return m_isChanged || (!isNull() && m_object.isChanged());
    }

    /**
     * @see com.saabgroup.safir.dob.typesystem.ContainerBase#setChanged(boolean)
     */
    @Override
    public void setChanged(boolean changed) {
        m_isChanged = changed;
        if (!isNull())
            m_object.setChanged(changed);
    }

    /**
     * @see com.saabgroup.safir.dob.typesystem.ContainerBase#isNull()
     */
    @Override
    public boolean isNull() {
        return m_object == null;
    }

    /**
     * @see com.saabgroup.safir.dob.typesystem.ContainerBase#setNull()
     */
    @Override
    public void setNull() {
        m_object = null;
        m_isChanged = true;
    }

    /**
     * @see com.saabgroup.safir.dob.typesystem.ObjectContainerBase#getMember(int, int)
     */
    @Override
    public ContainerBase getMember(int member, int index) {
        if (isNull())
        {
            throw new NullException("Object is null!");
        }
        return m_object.getMember(member,index);
    }

    /**
     * @see com.saabgroup.safir.dob.typesystem.ObjectContainerBase#getObjInternal()
     */
    @Override
    public Object getObjInternal() {
        return m_object;
    }

    /**
     * @see com.saabgroup.safir.dob.typesystem.ObjectContainerBase#setObjInternal(com.saabgroup.safir.dob.typesystem.Object)
     */
    @SuppressWarnings("unchecked")
    @Override
    public void setObjInternal(Object obj) {
        m_object = (T)obj;
    }


    @Override
    @SuppressWarnings("unchecked")
    void shallowCopy(ContainerBase other)
    {
        super.shallowCopy(other);
        ObjectContainerImpl<T> that = (ObjectContainerImpl<T>)other;
        m_object = that.m_object;
    }

    private T m_object;
}
