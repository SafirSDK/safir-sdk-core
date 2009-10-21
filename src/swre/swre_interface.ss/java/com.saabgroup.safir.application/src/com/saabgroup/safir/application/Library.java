// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

final class Library
{
    static
    {
        System.loadLibrary("swre_java_jni");
    }


    //SwreC_SetCommandLineArguments
    static native void SetProgramName(String programName,
                                      boolean [] success);

    //SwreC_TraceAppendStringPrefix
    static native void TraceAppendStringPrefix(long prefixId,
                                               byte [] str,
                                               boolean [] success);

    //SwreC_TraceAppendCharPrefix
    static native void TraceAppendCharPrefix(long prefixId,
                                             byte b,
                                             boolean [] success);


    //SwreC_TraceSyncBuffer
    static native void TraceSyncBuffer(boolean [] success);


    //SwreC_TraceFlushBuffer
    static native void TraceFlushBuffer(boolean [] success);


    //SwreC_TracePrefixAdd
    static native void TracePrefixAdd(String prefix,
                                      long [] prefixId,
                                      boolean [] success);


    //SwreC_TracePrefixSetEnabled
    static native void TracePrefixSetEnabled(long id,
                                             boolean enabled,
                                             boolean [] success);

    //SwreC_TracePrefixIsEnabled
    static native boolean TracePrefixIsEnabled(long id);

}
