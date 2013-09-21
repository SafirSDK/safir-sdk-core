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
package com.saabgroup.safir.swreports;

final class Library
{
    static
    {
        System.loadLibrary("swre_java_jni");
    }

    //SwreC_SendFatalErrorReport
    static native void SendFatalErrorReport(String errorCode,
                                            String location,
                                            String text,
                                            boolean [] success);


    //SwreC_SendErrorReport
    static native void SendErrorReport(String errorCode,
                                       String location,
                                       String text,
                                       boolean [] success);


    //SwreC_SendResourceReport
    static native void SendResourceReport(String resourceId,
                                          boolean allocated,
                                          String text,
                                          boolean [] success);


    //SwreC_SendProgrammingErrorReport
    static native void SendProgrammingErrorReport(String errorCode,
                                                  String location,
                                                  String text,
                                                  boolean [] success);


    //SwreC_SendProgramInfoReport
    static native void SendProgramInfoReport(String text,
                                             boolean [] success);

}
