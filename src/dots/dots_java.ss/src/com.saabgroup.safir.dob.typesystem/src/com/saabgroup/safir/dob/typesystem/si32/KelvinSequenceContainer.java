//------------
//-*- coding: utf-8 -*-
/******************************************************************************
 *
 * Copyright Consoden AB, 2009-2015 (http://safir.sourceforge.net)
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
package com.saabgroup.safir.dob.typesystem.si32;
import com.saabgroup.safir.dob.typesystem.Float32SequenceContainer;

public class KelvinSequenceContainer
extends Float32SequenceContainer {

    public KelvinSequenceContainer() {
        super();
    }
    
    public KelvinSequenceContainer(KelvinSequenceContainer other) {
        super(other);
    }
    
    /**
     * @see com.saabgroup.safir.dob.typesystem.ValueContainer#clone()
     */
    @Override
    public KelvinSequenceContainer clone() {
        return new KelvinSequenceContainer(this);
    }
}
