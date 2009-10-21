// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Lars Engdahl / stlsen
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

/********************************************************************
* DoseMonJni.java - a part of DoseMon
*
* Loads DoseComDll.dll (WIN32) or libDoseJni.so
* Note that there are different name in LINUX/WIN32 so we must try both
**********************************************************************/

import java.io.*;
import java.util.*;
import java.io.IOException;
import java.lang.Exception;

/*************************************************************
* Load the Native lib DoseComDll/DoseJni
*************************************************************/
class DoseJni
{
    static boolean m_isLoaded = false;

    //-------------------------------------------------------
    // Define all APIs in the DLL/SO that shall be used
    //
    // C-code ( not using a .def file )
    //
    // extern "C" JNIEXPORT jint
    // Java_DoseJni_GetInfo(JNIEnv *env, jclass myclass, int Cmd, int Param,
    //               jbyteArray arr)

    public native int GetInfo(int cmd, int param, byte[] buf);

    /****************************************
    *
    *****************************************/

    public int loadDoseLib() //throws ??????
    {
        if(m_isLoaded) return(0);

        // First try the LINUX version
        try
        {
            //System.out.println("Load dose_com_jni");

           System.loadLibrary("dose_com_jni"); // Note: no ".so" or ".dll"

            //System.out.println("dose_com_jni loaded OK");
            return(0); // OK
        }
        catch (Throwable e)
        {
            e.printStackTrace();
        }

        System.out.println("ERROR: dose_com_jni load FAILED");
        return(-1); // Failed
    }
}
