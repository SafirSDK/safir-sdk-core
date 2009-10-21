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
 * Used when someone tries to set a property that is mapped
 *  to something that cannot be changed.
 */
public class ReadOnlyException
  extends com.saabgroup.safir.dob.typesystem.FundamentalException
{
    /**
     * Constructor with exception information.
     *
     * @param message A message describing why the exception occurred.
     */
    public ReadOnlyException(String message)
    {
        super(message);
    }

    private static final long serialVersionUID = 1;

    /**
     * The TypeId of this exception type.
     */
    public static final long ExceptionTypeId = -4804695341042352897L;

    /**
     * Get the type id of this exception.
     *
     * Gets the type id of the exception. Method is virtual to ensure that the right
     * value gets returned for pointers or references.
     *
     * Note: This method is overridden by all auto-generated classes.
     *
     * @return The TypeId of the exception object.
     */
    public long getTypeId()
    {
        return ExceptionTypeId;
    }
}
