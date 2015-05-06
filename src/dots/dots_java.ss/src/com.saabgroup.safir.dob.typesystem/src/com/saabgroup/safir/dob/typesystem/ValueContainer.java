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

/**
 * Container for base types.
 *
 * This class holds a value of the template argument type and a null flag.
 * The operations that modify the value update the null flag and the change flag
 * (which is inherited from ContainerBase).
 *
 * This container is intended for the simple types of the DOB typesystem.
 *
 * @param <T> The type to contain.
 */
public abstract class ValueContainer<T> extends ContainerBase {

    /**
     * Default constructor
     *
     * Creates a null and not changed container.
     */
    protected ValueContainer()
    {
        super();
        m_isNull = true;
    }

    protected ValueContainer(boolean isNull, boolean isChanged) {
        super(isChanged);
        m_isNull = isNull;
    }

    /**
     * Get the value of the container.
     *
     * @return The value of the container.
     * @throws NullException The container is null.
     */
    public T getVal()
    {
        if (m_isNull)
            throw new NullException("Value is null");
        return m_value;
    }

    /**
     * Set the value of the container.
     *
     * Null and change flags are updated accordingly.
     *
     * @param value [in] - The new value.
     */
    public void setVal(T value)
    {
        m_value = value;
        m_isNull = false;
        m_isChanged = true;
    }


    /**
     * @see com.saabgroup.safir.dob.typesystem.ContainerBase#isNull()
     */
    @Override
    public boolean isNull() {
        return m_isNull;
    }

    /**
     * @see com.saabgroup.safir.dob.typesystem.ContainerBase#setNull()
     */
    @Override
    public void setNull() {
        m_isNull = true;
        m_isChanged = true;
    }

    @Override
    @SuppressWarnings("unchecked")
    void shallowCopy(ContainerBase other)
    {
        super.shallowCopy(other);
        ValueContainer<T> that = (ValueContainer<T>)other;
        m_isNull = that.m_isNull;
        m_value = that.m_value;
    }

    //variables are accessible internally.
    protected T m_value;
    protected boolean m_isNull;

}
