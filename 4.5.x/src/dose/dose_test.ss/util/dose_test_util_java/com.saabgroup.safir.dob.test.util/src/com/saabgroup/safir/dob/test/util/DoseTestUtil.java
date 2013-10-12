// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2011-2013 (http://safir.sourceforge.net)
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
 * Java interface to dose_test_util
 * The DoseTestUtil class is the raw interface to the C-world.
 */
final class DoseTestUtil {

    static
    {
        System.loadLibrary("dose_test_util_java_jni");
    }


    
     static native void InhibitOutgoingTraffic(boolean inhibit, boolean success[]);
     static native void InhibitOutgoingTrafficStatus(boolean isInhibited[]);

}
