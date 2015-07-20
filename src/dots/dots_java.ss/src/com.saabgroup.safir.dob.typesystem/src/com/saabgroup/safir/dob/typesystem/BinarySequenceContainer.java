//------------
//-*- coding: utf-8 -*-
/******************************************************************************
 *
 * Copyright Saab AB, 2009-2015 (http://safir.sourceforge.net)
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

public class BinarySequenceContainer
extends ValueSequenceContainer<Byte[]> {

    public BinarySequenceContainer() {
        super();
    }
    
    public BinarySequenceContainer(BinarySequenceContainer other) {
        super(other);
    }
    
    public boolean add(byte[] binary) {
        return add(toByteArray(binary));
    }
    
    public void add(int index, byte[] binary) {
        add(index, toByteArray(binary));
    }
    
    /**
     * @see com.saabgroup.safir.dob.typesystem.ValueSequenceContainer#clone()
     */
    @Override
    public BinarySequenceContainer clone() {
        return new BinarySequenceContainer(this);
    }
    
    private Byte[] toByteArray(byte[] bytes) {
        Byte[] byteObjects = new Byte[bytes.length];
        for (int i=0; i<bytes.length; i++) {
            byteObjects[i++] = bytes[i];
        }
        return byteObjects;
    }
    
}
