// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009-2013 (http://safir.sourceforge.net)
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
package com.saabgroup.safir.dob;

/**
 * Interface for reception of a dispatch order.
 */
public interface Dispatcher extends com.saabgroup.safir.dob.internal.ConsumerBase
{
    /**
     * Indicates that there is incoming data for the connection so the application shall
     * call dispatch().
     *
     * When this method is called the application MUST call the dispatch() method for the connection.
     * Note that dispatch() is NOT to be called directly from within this method. Instead the application
     * shall set an event or similar and then call dispatch() from the thread that owns (has called open())
     * the connection.
     */
    void onDoDispatch();
}

