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

package com.saabgroup.safir.dob.typesystem.si64;

/**
 * Array for WattContainers
 */
public class WattContainerArray
    extends com.saabgroup.safir.dob.typesystem.ArrayContainer<WattContainer>
    implements Cloneable {

    private static java.util.ArrayList<WattContainer> createBlankArray(int size){
        java.util.ArrayList<WattContainer> initializedArray = new java.util.ArrayList<WattContainer>(size);
        for (int i = 0; i < size; ++i) {
            initializedArray.add(new WattContainer());
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
    public WattContainerArray(int size) {
        super(createBlankArray(size));
    }

    /**
     * Construct an array containing the specified array.
     *
     * @param initializedArray the array to use.
     */
    public WattContainerArray(java.util.ArrayList<WattContainer> initializedArray) {
        super(initializedArray);
    }

    protected WattContainerArray(WattContainerArray other) {
        super(other);
    }

    /**
     * @see com.saabgroup.safir.dob.typesystem.ContainerBase#clone()
     */
    @Override
    public WattContainerArray clone() {
        return new WattContainerArray(this);
    }
}
