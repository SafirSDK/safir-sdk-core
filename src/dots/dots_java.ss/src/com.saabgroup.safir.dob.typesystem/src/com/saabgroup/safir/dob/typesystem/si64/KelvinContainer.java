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

import com.saabgroup.safir.dob.typesystem.Float64Container;

/**
 * Container for Kelvin64 members.
 */
public class KelvinContainer
    extends Float64Container
    implements Cloneable {

    /**
     * Default constructor.
     */
    public KelvinContainer(){
        super();
    }

    /**
     * Construct an KelvinContainer from value and flags.
     *
     * Although this constructor is public it is really only meant for BlobOperations.
     *
     * @param value The value.
     * @param isNull the null flag.
     * @param isChanged the is changed flag.
     */
    public KelvinContainer(double value, boolean isNull, boolean isChanged) {
        super(value,isNull,isChanged);
    }


    protected KelvinContainer(KelvinContainer other) {
        super(other);
    }

    /**
     * @see com.saabgroup.safir.dob.typesystem.Float64Container#clone()
     */
    @Override
    public KelvinContainer clone() {
        return new KelvinContainer(this);
    }

}
