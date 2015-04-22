/* ****************************************************************************
*
* Copyright Consoden AB, 2005-2015 (http://safir.sourceforge.net)
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
        /// <param name="size">Size of array</param>
        /// <returns>string</returns>
        public static string StringOf(System.IntPtr p, System.Int32 size)
        {
            if (p == System.IntPtr.Zero)
            {
                throw new SoftwareViolationException("StringOf does not accept null strings!");
            }

            //copy bytes
            byte[] utf8Bytes=new byte[size];

            Marshal.Copy(p, utf8Bytes, 0, utf8Bytes.Length);

            return System.Text.UTF8Encoding.UTF8.GetString(utf8Bytes);
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

            Marshal.Copy(p, utf8Bytes, 0, utf8Bytes.Length);

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
            IntPtr p = Kernel.DotsC_GetDouFilePath (typeId);
            if (p == IntPtr.Zero) {
                throw new IllegalValueException ("There is no such type defined");
            }

            return InternalOperations.StringOf (p);
        }
    }
}
