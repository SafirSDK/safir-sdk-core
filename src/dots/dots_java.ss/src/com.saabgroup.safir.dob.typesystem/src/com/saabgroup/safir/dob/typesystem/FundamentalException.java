// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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
 * This is the base class of all Fundamental Exceptions.
 * All exceptions that signify "static errors" should inherit from this class.
 * Fundamental Exceptions are exceptions that are only expected to be caught
 * by the main loop in applications. They usually mean that something has gone very
 * wrong.
 */
public abstract class FundamentalException
    extends java.lang.RuntimeException
{
    /**
     * Constructor with exception information.
     *
     * @param message A message describing why the exception occurred.
     */
    public FundamentalException(String message)
    {
        super(message);
    }

    /**
     * The TypeId of this exception type.
     */
    public static final long ExceptionTypeId = 5177142987005172374L;

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
