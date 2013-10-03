/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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
    /// For internal usage. Use only if you know what you are doing.
    /// </summary>    
    public class InternalOperations
    {
        /// <summary>
        /// format a piece of blank memory to be a blob of a desired type
        /// does not check that the size of the blob is correct.
        /// </summary>
        /// <param name="blob">Blob to format.</param>
        /// <param name="blobSize">Size of blob to format.</param>
        /// <param name="typeId">Type id.</param>
        /// <param name="beginningOfUnused">Pointer to unused part.</param>
        public static void FormatBlob(System.IntPtr blob,
                        System.Int32 blobSize,
                        System.Int64 typeId,
                        out System.IntPtr beginningOfUnused)
        {
            Kernel.DotsC_FormatBlob(blob, blobSize, typeId, out beginningOfUnused);
        }
    
        /// <summary>
        /// byte to bool conversion.
        /// </summary>
        /// <param name="b">Byte</param>
        /// <returns>bool</returns>
        public static bool BoolOf(byte b)
        {
            return b>0;
        }

        /// <summary>
        /// bool to byte conversion.
        /// </summary>
        /// <param name="b">bool</param>
        /// <returns>byte</returns>
        public static byte ByteOf(bool b)
        {
            if (b)
                return 1;
            else
                return 0;
        }

        /// <summary>
        /// Converts an UTF-8 encoded byte array to a string.
        /// </summary>
        /// <param name="p">Byte array.</param>
        /// <returns>string</returns>
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

        /// <summary>
        /// Converts a string to an UTF-8 encoded CString.
        /// </summary>
        /// <param name="s">string</param>
        /// <returns>UTF-8 encoded CString.</returns>
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

        /// <summary>
        /// Delete a blob.
        /// <para>
        /// Internal blobs can be deleted w8ith this function. Beware that you may call the wrong runtime!
        /// </para>
        /// </summary>
        /// <param name="blob">The blob to delete.</param>
        public static void Delete(ref System.IntPtr blob)
        {
            Kernel.DotsC_DeleteBlob(ref blob);
        }

        /// <summary>
        /// Sets all changed flags in the blob.
        /// </summary>
        /// <param name="blob">The blob to modify.</param>
        /// <param name="changed">The value to set the change flags to.</param>
        public static void SetChanged(System.IntPtr blob, bool changed)
        {
            Kernel.DotsC_SetChanged(blob, changed);
        }

        //Compare the two blobs and set the change flags in "mine" on all members that have
        //changed between "base" and "mine".
        /// <summary>
        /// Compare two blobs and set the change flags.
        /// <para>
        /// Change flags are set in "mine" on all members that have changed between "base" and "mine".
        /// </para>
        /// </summary>
        /// <param name="_base">Original to compare.</param>
        /// <param name="_mine">Compare to this and set change flags.</param>
        public static void Diff(System.IntPtr _base,
                                System.IntPtr _mine)
        {
            Kernel.DotsC_SetChangedSinceLastRead(_base, _mine);
            //TODO: rename the function in DOTS.
        }

        /// <summary>
        /// Get the full path to the dou file that the type id represents
        /// <para/>
        /// Note that this function looks at the disk every time it is called. No caching
        /// is performed at all. Not meant to be used in "real" code, but useful for debugging
        /// tools, such as sate or dobexplorer.
        /// </summary>
        /// <param name="typeId">Type id.</param>
        /// <returns>The full path to the dou file</returns>
        public static String GetDouFilePath(System.Int64 typeId)
        {
            int BUF_SIZE = 2;
            IntPtr buf = Marshal.AllocHGlobal(BUF_SIZE);
            System.Int32 resultSize = 0;
            Internal.Kernel.DotsC_GetDouFilePathForType(typeId, buf, BUF_SIZE, out resultSize);
            if (resultSize == -1)
            {
                Marshal.FreeHGlobal(buf);
                throw new IllegalValueException("There is no such type defined");
            }
            if (resultSize > BUF_SIZE)
            {
                BUF_SIZE = resultSize;
                Marshal.FreeHGlobal(buf);
                buf = Marshal.AllocHGlobal(BUF_SIZE);
                Internal.Kernel.DotsC_GetDouFilePathForType(typeId, buf, BUF_SIZE, out resultSize);
                if (resultSize != BUF_SIZE)
                {
                    Marshal.FreeHGlobal(buf);
                    throw new SoftwareViolationException("Error in GetDouFilePathForType!");
                }
            }
            string str = Internal.InternalOperations.StringOf(buf);
            Marshal.FreeHGlobal(buf);

            return str;

        }
    }
}
