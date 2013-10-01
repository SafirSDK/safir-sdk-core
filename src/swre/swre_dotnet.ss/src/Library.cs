/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
using System;
using System.Runtime.InteropServices;

namespace Safir.SwReports
{
    internal class Library
    {
        internal const string SWRE_LIBRARY_NAME = "swre_library.dll";

        //SwreC_StartTraceBackdoory
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        internal static extern void SwreC_StartTraceBackdoor(byte [] connectionNameCommonPart,
                                                             byte [] connectionNameInstancePart,
                                                             out byte success);
        

        //SwreC_StopTraceBackdoory
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void SwreC_StopTraceBackdoor();

        //SwreC_StartCrashReporting
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        internal static extern void SwreC_StartCrashReporting(out byte success);


        //SwreC_StopCrashReporting
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void SwreC_StopCrashReporting();


        //SwreC_TraceAppendSubstring
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void SwreC_TraceAppendSubstring(System.Int64 prefixId,
                                                               byte [] str,
                                                               System.Int32 offset,
                                                               System.Int32 length,
                                                               out byte success);
        
        
        //SwreC_TraceFlush
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void SwreC_TraceFlush(out byte success);

        
        //SwreC_TracePrefixAdd
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void SwreC_TracePrefixAdd(byte [] prefix,
                                                         out System.Int64 prefixId,
                                                         out byte success);

        
        //SwreC_TracePrefixSetEnabled
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void SwreC_TracePrefixSetEnabled(System.Int64 id,
                                                                byte enabled,
                                                                out byte success);

        //SwreC_TracePrefixIsEnabled
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern byte SwreC_TracePrefixIsEnabled(System.Int64 id);

       
        
        //SwreC_SendFatalErrorReport
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void SwreC_SendFatalErrorReport(byte [] errorCode,
                                                               byte [] location,
                                                               byte [] text,
                                                               out byte success);

       
        //SwreC_SendErrorReport
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void SwreC_SendErrorReport(byte [] errorCode,
                                                          byte [] location,
                                                          byte [] text,
                                                          out byte success);

      
        //SwreC_SendResourceReport
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void SwreC_SendResourceReport(byte [] resourceId,
                                                             byte allocated,
                                                             byte [] text,
                                                             out byte success);

      
        //SwreC_SendProgrammingErrorReport
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void SwreC_SendProgrammingErrorReport(byte [] errorCode,
                                                                     byte [] location,
                                                                     byte [] text,
                                                                     out byte success);
        
      
        //SwreC_SendProgramInfoReport
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void SwreC_SendProgramInfoReport(byte [] text,
                                                                out byte success);

    }
}
