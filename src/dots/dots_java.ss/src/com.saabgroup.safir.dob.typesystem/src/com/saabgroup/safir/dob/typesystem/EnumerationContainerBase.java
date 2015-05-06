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
 * Base class for containers of enumeration values.
 *
 * The containers for enumerations are defined in the automatically generated code,
 * but this class defines the common functionality for them.
 * Enumeration containers really store the ordinal values (integer representation of
 * the enumeration), and this class has methods for setting and getting the ordinal.
 * The derived class (in the generated code) has methods for setting and getting the
 * value as an enumeration value.
 * Most applications should not use the getOrdinal/setOrdinal functions, but should
 * use the setVal and getVal methods defined in the derived classes.
 * @param <E> Enumeration type
 */
public abstract class EnumerationContainerBase<E extends Enum<E>>
    extends ContainerBase {

    /**
     * Default constructor that constructs a null and not changed enumeration container.
     */
    public EnumerationContainerBase()
    {
        super();
        m_bIsNull = true;
        m_Value = 0;
    }


    EnumerationContainerBase(int value, boolean isNull, boolean isChanged) {
        super(isChanged);
        m_bIsNull = isNull;
        m_Value = value;
    }


    /**
     * @see com.saabgroup.safir.dob.typesystem.ContainerBase#setNull()
     */
    @Override
    public void setNull() {
        m_bIsNull = true;
        m_isChanged = true;
    }

    /**
     * @see com.saabgroup.safir.dob.typesystem.ContainerBase#isNull()
     */
    @Override
    public boolean isNull() {
        return m_bIsNull;
    }

    /**
     * Get the ordinal value of the enumeration container.
     *
     * @return The ordinal value of the container.
     * @throws NullException The container is null.
     */
    public int getOrdinal()
    {
        if (m_bIsNull)
            throw new NullException("Value is null");
        return m_Value;
    }

    protected abstract int getNumValues();

    /**
     * Set the value of the container.
     *
     * Null and change flags are updated accordingly.
     *
     * @param value The new value.
     */
    public void setOrdinal(int value)
    {

        if (value < 0 || value >= getNumValues())
        {
            throw new IllegalValueException("Value " + value + " is not in the valid range for DotsTest.TestEnum");
        }
        m_bIsNull = false;
        m_isChanged = true;
        m_Value = value;
    }



    /**
     * Get the value of the container.
     *
     * @return The value of the container.
     * @throws NullException The container is null.
     */
    abstract public E getVal();

    /**
     * Set the value of the container.
     *
     * Null and change flags are updated accordingly.
     *
     * @param value [in] - The new value.
     */
    public void setVal(E value) {
        m_bIsNull = false;
        m_isChanged = true;
        m_Value = value.ordinal();
    }

    @Override
    void shallowCopy(ContainerBase other)
    {
        super.shallowCopy(other);
        EnumerationContainerBase that = (EnumerationContainerBase)other;
        m_bIsNull = that.m_bIsNull;
        m_Value = that.m_Value;
    }

    //variables are accessible internally.
    protected int m_Value;
    protected boolean m_bIsNull;
}
