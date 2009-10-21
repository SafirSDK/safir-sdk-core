// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / stjoot
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
 * The base class of all Dob-classes.
 *
 * This class is the base class for all automatically generated DOB classes.
 */
public class Object implements Cloneable
{


    /**
     * Default constructor.
     */
    public Object()
    {

    }

    /** The TypeId of the Object class. */
    public static final long ClassTypeId = 5955188366590963785L;

    /**
     * Get the type id of this object.
     *
     * Note: this method is overridden by all auto-generated classes.
     *
     * @return The TypeId of the object.
     */
    public long getTypeId()
    {
        return ClassTypeId;
    }

    /**
     * Check if any member of this object is changed.
     *
     * This method will recursively check if any member of the object has its change flag set.
     *
     * Note: this method is overridden by all auto-generated classes.
     *
     * @return True if any member has changed.
     */
    public boolean isChanged() {
        return false;
    }

    /**
     * Recursively set change flags in all members of this object.
     *
     * Note: this method is overridden by all auto-generated classes.
     *
     * @param changed [in] - The value to set the change flags to
     */
    public void setChanged(boolean changed) { }


    protected Object(Object other){

    }

    /**
     * Member wise deep clone of object.
     */
    @Override
    public Object clone(){
        return new Object(this);
    }

    /**
     * Create an Object from a blob.
     *
     * @param blob The blob to deserialize.
     */
    public Object(java.nio.ByteBuffer blob)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }
    }

    /**
     * Calculate the size of the blob-serialized form of this object.
     *
     * @return The needed size in bytes.
     */
    public int calculateBlobSize()
    {
        if (m_initialSize == -1)
        {
            m_initialSize = BlobOperations.getInitialSize(ClassTypeId);
        }
        return m_initialSize;
    }

    /**
     * Write the object to a blob.
     * Note that the size of the blob is assumed to be correct! No checks are made!
     *
     * @param blob The blob to write to.
     * @param beginningOfUnused The beginning of unused dynamic blob space.
     * @return beginningOfUnused after the object has been written to the blob.
     */
    public int writeToBlob(java.nio.ByteBuffer blob, int beginningOfUnused)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        return beginningOfUnused;
    }

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
     * @return A reference to the member container.
     * @throws IllegalValueException If the index is not in the range of the array.
     * @throws SoftwareViolationException If the element is not an array and the index is not 0.
     */
    public ContainerBase getMember(int member, int index)
    {
        throw new SoftwareViolationException("Object does not have any members!");
    }

    private static int m_initialSize = -1;


    /**
     * Array for ObjectContainers
     */
    static public class ContainerArray
        extends ArrayContainer<ObjectContainerImpl<Object>>
        implements Cloneable {

        private static java.util.ArrayList<ObjectContainerImpl<Object>> createBlankArray(int size){
            java.util.ArrayList<ObjectContainerImpl<Object>> initializedArray = new java.util.ArrayList<ObjectContainerImpl<Object>>(size);
            for (int i = 0; i < size; ++i) {
                initializedArray.add(new ObjectContainerImpl<Object>());
            }
            return initializedArray;
        }

        /**
         * Constructor with size.
         *
         * Creates an array of the given size. Remember that once it has been created the size cannot be changed.
         *
         * @param size The desired size of the array. Must be > 0.
         */
        public ContainerArray(int size) {
            super(createBlankArray(size));
        }

        /**
         * Construct an array containing the specified array.
         *
         * @param initializedArray the array to use.
         */
        public ContainerArray(java.util.ArrayList<ObjectContainerImpl<Object>> initializedArray) {
            super(initializedArray);
        }


        protected ContainerArray(ContainerArray other) {
            super(other);
        }

        /**
         * @see com.saabgroup.safir.dob.typesystem.ContainerBase#clone()
         */
        @Override
        public ContainerArray clone() {
            return new ContainerArray(this);
        }

    }
}

