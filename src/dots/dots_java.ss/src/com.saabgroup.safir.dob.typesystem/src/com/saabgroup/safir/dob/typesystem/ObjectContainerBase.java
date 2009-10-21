// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009 (http://www.safirsdk.com)
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
 * Base class for all object containers.
 * The reason for the existence of this class is that code that uses the reflection
 * functionality must be able to get hold of members of items.
 */
public abstract class ObjectContainerBase
    extends ContainerBase
    implements Cloneable {

    /** Default constructor. */
    ObjectContainerBase() { super();}

    protected ObjectContainerBase(boolean isChanged) {
        super(isChanged);
    }

    /**
     * Is the change flag in the container set?
     *
     * This method is like IsChanged without the recursion (on object containers IsChanged is recursive).
     *
     * @return True if the containers change flag is set.
     */
    public boolean isChangedHere() {return m_isChanged;}

    /**
     * Set the change flag in the container.
     *
     * This method is like SetChanged without the recursion (on object containers SetChanged is recursive).
     *
     * @param changed [in] - The value to set the change flag to.
     */
    public void setChangedHere(boolean changed) {m_isChanged = changed;}

    /**
     * Get a reference to a member container from an object.
     *
     * Use the methods in Members to get member indices and array sizes for use
     * with this method.
     *
     * Note: Do not use this method unless you're very sure it is the one you need!
     *
     * @param member The index of the member to get.
     * @param index The array index of the member to get.
     * @return The member container.
     * @throws IllegalValueException If the index is not in the range of the array.
     * @throws SoftwareViolationException If the element is not an array and the index is not 0.
     */
    abstract public ContainerBase getMember(int member, int index);

    /**
     * Set the contained object without any checks.
     *
     * This method will set the contained object to point to another
     * object.
     *
     * Warning: This method does not update the change flag!
     *
     * Note: Do not use this method unless you're very sure it is the one you need!
     *
     * @param obj The new object to point to.
    */
    abstract public void setObjInternal(Object obj);

    /**
     * Get the contained object without any checks.
     *
     * This method will cast the object
     * contained by the container to an Object (the DOB
     * object base class.
     *
     * This method does not check if the container is null!
     *
     * Note: Do not use this method unless you're very sure it is the one you need!
     *
     * @return The contained object.
    */
    abstract public Object getObjInternal();

    protected ObjectContainerBase(ObjectContainerBase other) {
        super(other);
    }

    /**
     * @see com.saabgroup.safir.dob.typesystem.ContainerBase#clone()
     */
    abstract public ObjectContainerBase clone();
}
