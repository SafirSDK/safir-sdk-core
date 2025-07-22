//-*- coding: utf-8 -*-
/******************************************************************************
 *
 * Copyright Saab AB, 2022 (http://safirsdkcore.com)
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

/**
 * Base class for containers for sequences of enumeration values. It allows for
 * reflection on enumeration values, using ordinal values.
 */
public interface EnumerationSequenceContainerBase {

    /**
     * Get the number of elements contained in the sequence.
     * @return the number of elements.
     */
    public int size();

    /**
    * Get the ordinal value of the value at the specified index.
    *
    * @param index Index of value to get
    * @return The ordinal at the specified index.
    */
    int getOrdinal(int index);

    /**
    * Update a specific value, using ordinal value. Will not add new values to the sequence.
    *
    * @param index The index of the value to change
    * @param value Value to change to.
    * @throws IllegalValueException If the value is not in the enumeration.
    */
    void setOrdinal(int index, int value);

    /**
     * Insert a new ordinal value last in the sequence.
     *
     * If the sequence was null before it will no longer be null after this call.
     *
     * @param item Value to add.
     * @throws IllegalValueException If the value is not in the enumeration.
    */
    void addOrdinal (int item);

    /**
    * Remove all values from this instance.
    */
    void clear();

    /**
    * Set the container to null. Same as clearing the contents of the sequence.
    */
    void setNull();

    /**
    * Is the container set to null. Null is the same as empty.
    *
    * @return True if the container is set to null.
    */
    boolean isNull();

    /**
     * Does the container have a value? The opposite to isNull.
     *
     * @return True if the container has a value, i.e is not set to null.
     */
    boolean hasVal();

    /**
    * Is the change flag set on the container?
    *
    * @return True if the containers change flag is set.
    */
    boolean isChanged();

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
    * @param changed The value to set the change flag to.
    */
    void setChanged (boolean changed);
}
