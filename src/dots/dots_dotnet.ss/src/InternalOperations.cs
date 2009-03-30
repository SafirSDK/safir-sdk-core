/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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

namespace Safir.Dob.Typesystem.Internal
{
    /// <summary>
    /// Summary description for InternalOperations.
    /// </summary>
    
    public class InternalOperations
    {
        public static void FormatBlob(System.IntPtr blob,
                        System.Int32 blobSize,
                        System.Int64 typeId,
                        ref System.IntPtr beginningOfUnused)
        {
            Kernel.DotsC_FormatBlob(blob, blobSize, typeId, ref beginningOfUnused);
        }

        //Debugging
        public static void DumpClassDescriptions()
        {
            Kernel.DotsC_DumpClassDescriptions();
        }

        public static void DumpMemoryBlockInfo()
        {
            Kernel.DotsC_DumpMemoryBlockInfo();
        }
    
        public static bool BoolOf(byte b)
        {
            return b>0;
        }

        public static byte ByteOf(bool b)
        {
            if (b)
                return 1;
            else
                return 0;
        }

        public static string StringOf(System.IntPtr p)
        {
            if (p == System.IntPtr.Zero)
            {
                throw new SoftwareViolationException("StringOf does not accept null strings!");
            }

            //count number of bytes
            int offs=0;
            while(true)
            {
                if (Marshal.ReadByte(p, offs)==0)
                    break;
                offs++;
            }

            //copy bytes
            byte[] utf8Bytes=new byte[offs];
            for (int i=0; i<offs; i++)
            {
                utf8Bytes[i]=Marshal.ReadByte(p, i);
            }

            return System.Text.UTF8Encoding.UTF8.GetString(utf8Bytes);
        }

        public static System.IntPtr CStringOf(string s)
        {
            byte[] utf8Bytes=System.Text.Encoding.UTF8.GetBytes(s);
            System.IntPtr p=Marshal.AllocHGlobal(utf8Bytes.Length+1); //one extra for '\0'
            for (int i=0; i<utf8Bytes.Length; i++)
            {
                Marshal.WriteByte(p, i, utf8Bytes[i]);
            }
            Marshal.WriteByte(p, utf8Bytes.Length, 0); //add '\0'
            return p;
        }

        public static System.IntPtr CreateCopy(System.IntPtr blob)
        {
            System.IntPtr copy;
            Kernel.DotsC_CreateCopyOfBlob(out copy, blob);
            return copy;
        }

        public static void Delete(ref System.IntPtr blob)
        {
            Kernel.DotsC_DeleteBlob(ref blob);
        }


        //Sets all changed flags in the blob to false
        public static void SetChanged(System.IntPtr blob, bool changed)
        {
            Kernel.DotsC_SetChanged(blob, changed);
        }

        //Compare the two blobs and set the change flags in "mine" on all members that have
        //changed between "base" and "mine".
        public static void Diff(System.IntPtr _base,
                                System.IntPtr _mine)
        {
            Kernel.DotsC_SetChangedSinceLastRead(_base, _mine);
            //TODO: rename the function in DOTS.
        }
    }
}
