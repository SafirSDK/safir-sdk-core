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

import java.io.UnsupportedEncodingException;

/**
 * Container for String members.
 */
public class StringContainer extends ContainerBase {

    /**
     * Default constructor.
     *
     * Creates a null and not changed container.
     */
    public StringContainer(){
        super();
        m_isNull = true;
        m_value = null;
    }

    StringContainer(String value, boolean isNull, boolean isChanged) {
        super(isChanged);
        m_isNull = isNull;
        m_value = value;
    }

    /**
     * Get the value of the container.
     *
     * @return The value of the container.
     * @throws NullException The container is null.
     */
    public String getVal()
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
    public void setVal(String value)
    {
        m_value = value;
        m_isNull = false;
        m_isChanged = true;
        m_cachedUtf8String = null;
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
        m_value = null;
        m_cachedUtf8String = null;
    }

    /**
     * Get the length of the string when converted to UTF-8 encoding.
     * Includes one byte for a null termination.
     *
     * @return The length of the string of the id when converted to UTF-8
     */
    public int utf8StringLength() {
        if (isNull()) {
            return 0;
        }

        if (m_value.length() == 0) {
            return 1;
        }

        if (m_cachedUtf8String == null) {
            try {
                m_cachedUtf8String = m_value.getBytes("UTF-8");
            } catch (UnsupportedEncodingException e) {
                throw new SoftwareViolationException("Failed to convert string to UTF-8!!!");
            }
        }

        return m_cachedUtf8String.length + 1;
    }

    /**
     * Convert the string to UTF-8.
     *
     * Returns an empty string if there is no string.
     *
     * @return UTF-8 representation of the string.
     */
    public byte [] utf8String() {
        if (isNull()) {
            throw new NullException("The string is null, cannot convert!");
        }
        if (m_cachedUtf8String == null) {
            try {
                m_cachedUtf8String = m_value.getBytes("UTF-8");
            } catch (UnsupportedEncodingException e) {
                throw new SoftwareViolationException("Failed to convert string to UTF-8!!!");
            }
        }
        return m_cachedUtf8String;
    }

    @Override
    void shallowCopy(ContainerBase other)
    {
        super.shallowCopy(other);
        StringContainer that = (StringContainer)other;
        m_value = that.m_value;
        m_cachedUtf8String = that.m_cachedUtf8String;
        m_isNull = that.m_isNull;
    }

    //variables are accessible internally.
    protected String m_value;
    protected byte [] m_cachedUtf8String;
    protected boolean m_isNull;
}
