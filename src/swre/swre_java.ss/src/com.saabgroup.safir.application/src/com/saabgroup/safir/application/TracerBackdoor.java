// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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
package com.saabgroup.safir.application;


public final class TracerBackdoor
{
    public static void start(com.saabgroup.safir.dob.ConnectionBase connection)
    {
        com.saabgroup.safir.dob.ConnectionAspectMisc misc = new com.saabgroup.safir.dob.ConnectionAspectMisc(connection);
        boolean [] success = new boolean [1];
        Library.StartTraceBackdoor(misc.getConnectionNameCommonPart(),
                                   misc.getConnectionNameInstancePart(),
                                   success);
        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }
    
    public static void stop()
    {
        Library.StopTraceBackdoor();
    }

}
