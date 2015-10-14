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
 * Base class for all Containers.
 *
 * This class contains common functionality for all Containers.
 * Basically this amounts to the interface for "nullability" and
 * the change flag.
 */
public abstract class ContainerBase
{
    /**
     * Default Constructor.
     *
     * Construct a container that is not changed.
     */
    protected ContainerBase()
    {
        m_isChanged = false;
    }

    protected ContainerBase(boolean isChanged)
    {
        m_isChanged = isChanged;
    }


    /**
     * Is the container set to null?
     *
     * @return True if the container is set to null.
     */
    public abstract boolean isNull();

    /**
     * Set the container to null.
     */
    public abstract void setNull();

    /**
     * Is the change flag set on the container?
     * The change flag gets updated every time the contained value changes.
     * Note: If this is a container containing an object this call will recursively
     *       check change flags in the contained object.
     *
     * @return True if the containers change flag is set.
     */
    public boolean isChanged()
    {
        return m_isChanged;
    }

    /**
     * Set the containers change flag.
     *
     * It should be fairly unusual for an application to have to use this
     * operation. There is nothing dangerous about it, but are you sure this
     * is the operation you were after?
     *
     * The change flag is how receivers of objects can work out what the
     * sender really wanted done on the object.
     *
     * Note: If this is a container containing an object this call will recursively
     *       set all the change flags in the contained object.
     * @param changed [in] - The value to set the change flag(s) to.
     */
    public void setChanged(boolean changed)
    {
        m_isChanged = changed;
    }

    void shallowCopy(ContainerBase other)
    {
        m_isChanged = other.m_isChanged;
    }

    /** Flag must be accessible internally. */
    protected boolean m_isChanged;
}
