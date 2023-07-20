//-*- coding: utf-8 -*-
/******************************************************************************
 *
 * Copyright Saab AB, 2009-2015 (http://safirsdkcore.com)
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

public class GenericObjectSequenceContainer<T extends com.saabgroup.safir.dob.typesystem.Object>
    extends SequenceContainer<T>  {

    /** Default constructor */
    public GenericObjectSequenceContainer() {
        super();
    }

    @Override
    public boolean isChanged() {
        if (m_isChanged) {
            return m_isChanged;
        }

        for (T obj : this) {
            if (obj.isChanged())
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

        for (T obj : this) {
            obj.setChanged(changed);
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

    /** Used by MergeChanges to do its magic. */
    void merge(GenericObjectSequenceContainer that)
    {
        if (!getClass().equals(that.getClass()))
        {
            throw new SoftwareViolationException("Invalid call to Merge, containers are not of same type");
        }

        //Note: this function only gets called when IsChangedHere() == false

        if (!that.isChanged())
        {
            return;
        }

        if (size() != that.size())
        {
            throw new SoftwareViolationException("It is not possible to merge two object sequences of different sizes.");
        }

        @SuppressWarnings("unchecked")
        GenericObjectSequenceContainer<T> other = that;
        for (int i = 0; i < size(); ++i)
        {
            if (other.get(i).isChanged())
            {
                //recurse
                Utilities.mergeChanges(this.get(i),other.get(i));
            }
        }
    }
}
