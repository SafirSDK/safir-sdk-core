// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009-2013 (http://safirsdkcore.com)
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
 * Interface to be implemented by an application that sends requests (Request on entities or
 * service requests).
 */
public interface Requestor extends com.saabgroup.safir.dob.internal.ConsumerBase
{
    /**
     * Called when a response is received on a sent request.
     *
     * @param responseProxy Response proxy.
     */
    void onResponse(com.saabgroup.safir.dob.ResponseProxy responseProxy);

    /**
     * Called to indicate that it is meningful to make a retry after an overflow situation.
     */
    void onNotRequestOverflow();
}

