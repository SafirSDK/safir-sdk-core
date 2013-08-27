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

package com.saabgroup.safir.time;

/**
 * Java interface to douf_time_library.
 */
final class Library {
    static {
        //load the jni dll
        System.loadLibrary("douf_java_jni");
    }


    //************************************************************************************
    //* Functions for hash numbers
    //************************************************************************************
    public static native double getUtcTime();
    public static native int getLocalTimeOffset();
}
