// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
*
* Created by: Mikael Wennerberg / stmiwn
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

package com.saabgroup.safir.dob.test.util;


/**
 * Utility functions for users of the DOB test system.
 */
public class Utilities {

    /**
     * Inhibit dose_com communication (sending).
     *
     *
     * @param inhibit turn inhibit on/off.
     * @return successful operation
     */
    public static boolean InhibitOutgoingTraffic(boolean inhibit) {
        boolean success[] = new boolean[1];
        DoseTestUtil.InhibitOutgoingTraffic(inhibit, success);
        return success[0];
    }

    /**
     * Status of inhibit dose_com communication (sending).
     * @return inhibit is on/off
     */
    public static boolean IsOutgoingTrafficInhibited() {
        boolean inhibited[] = new boolean[1];
        DoseTestUtil.InhibitOutgoingTrafficStatus(inhibited);
        return inhibited[0];
       
    }
    
}
