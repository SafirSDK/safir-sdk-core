// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safirsdkcore.com)
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
 * This is the base class of all (non-Fundamental) Exceptions.
 * In comparison to FundamentalException the exceptions that derive from Exception
 * are more of a dynamic nature. These exceptions are expected to be handled directly
 * by the calling function and are possible to recover from.
 */
abstract public class Exception
  extends java.lang.RuntimeException
{
    /**
     * Constructor with exception information.
     *
     * @param message A message describing why the exception occurred.
     */
    public Exception(String message)
    {
        super(message);
    }

    /**
     * The TypeId of this exception type.
     */
    public static final long ExceptionTypeId = 8409897425067168944L;

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

    private static final long serialVersionUID = 1;
}
