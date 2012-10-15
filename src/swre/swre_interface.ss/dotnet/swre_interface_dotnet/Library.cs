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
using System;
using System.Runtime.InteropServices;

namespace Safir.SwReports
{
    internal class Library
    {
        internal const string SWRE_LIBRARY_NAME = "swre_library.dll";

        //--------------------------------------------------------------------
        // Exported interface from swre_library interface.h (unmanaged code)
        //--------------------------------------------------------------------
        //SwreC_SetProgramName
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi, EntryPoint = "SwreC_SetProgramName")]
        internal static extern void SwreC_SetProgramName(string programName,
                                                         out byte success);


        //SwreC_EnableCrashReporting
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi, EntryPoint = "SwreC_EnableCrashReporting")]
        internal static extern void SwreC_EnableCrashReporting(out byte success);


        //SwreC_Stop
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void SwreC_Stop();

        //SwreC_TraceAppendString
        //[DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl, EntryPoint = "SwreC_TraceAppendString")]
        //internal static extern void SwreC_TraceAppendString(byte [] str,
        //                                                    out byte success);

        //SwreC_TraceAppendStringPrefix
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl, EntryPoint = "SwreC_TraceAppendStringPrefix")]
        internal static extern void SwreC_TraceAppendStringPrefix(System.Int64 prefixId,
                                                                  byte [] str,
                                                                  out byte success);
        
        //SwreC_TraceSyncBuffer
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl, EntryPoint = "SwreC_TraceSyncBuffer")]
        internal static extern void SwreC_TraceSyncBuffer(out byte success);
        
        
        //SwreC_TraceFlushBuffer
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl, EntryPoint = "SwreC_TraceFlushBuffer")]
        internal static extern void SwreC_TraceFlushBuffer(out byte success);

        
        //SwreC_TracePrefixAdd
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl, EntryPoint = "SwreC_TracePrefixAdd")]
        internal static extern void SwreC_TracePrefixAdd(byte [] prefix,
                                                         out System.Int64 prefixId,
                                                         out byte success);

        
        //SwreC_TracePrefixSetEnabled
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl, EntryPoint = "SwreC_TracePrefixSetEnabled")]
        internal static extern void SwreC_TracePrefixSetEnabled(System.Int64 id,
                                                                byte enabled,
                                                                out byte success);

        //SwreC_TracePrefixIsEnabled
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl, EntryPoint = "SwreC_TracePrefixIsEnabled")]
        internal static extern byte SwreC_TracePrefixIsEnabled(System.Int64 id);

       
        
        //SwreC_SendFatalErrorReport
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl, EntryPoint = "SwreC_SendFatalErrorReport")]
        internal static extern void SwreC_SendFatalErrorReport(byte [] errorCode,
                                                               byte [] location,
                                                               byte [] text,
                                                               out byte success);

       
        //SwreC_SendErrorReport
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl, EntryPoint = "SwreC_SendErrorReport")]
        internal static extern void SwreC_SendErrorReport(byte [] errorCode,
                                                          byte [] location,
                                                          byte [] text,
                                                          out byte success);

      
        //SwreC_SendResourceReport
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl, EntryPoint = "SwreC_SendResourceReport")]
        internal static extern void SwreC_SendResourceReport(byte [] resourceId,
                                                             byte allocated,
                                                             byte [] text,
                                                             out byte success);

      
        //SwreC_SendProgrammingErrorReport
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl, EntryPoint = "SwreC_SendProgrammingErrorReport")]
        internal static extern void SwreC_SendProgrammingErrorReport(byte [] errorCode,
                                                                     byte [] location,
                                                                     byte [] text,
                                                                     out byte success);
        
      
        //SwreC_SendProgramInfoReport
        [DllImport(SWRE_LIBRARY_NAME, CallingConvention = CallingConvention.Cdecl, EntryPoint = "SwreC_SendProgramInfoReport")]
        internal static extern void SwreC_SendProgramInfoReport(byte [] text,
                                                                out byte success);

    }
}
