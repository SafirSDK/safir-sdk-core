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
using Safir.Dob.Typesystem.Internal;
using System.Runtime.InteropServices;

namespace Safir.Dob.Typesystem
{
    /// <summary>
    /// Methods for serializing objects to binary and XML forms.
    /// </summary>
    public class Serialization
    {
        /// <summary>
        /// Serialize an object to XML.
        /// </summary>
        /// <param name="obj">The object to serialize</param>
        /// <returns>The xml serialization</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is something wrong with the object.</exception>
        public static string ToXml(Dob.Typesystem.Object obj)
        {
            System.Int32 blobSize = obj.CalculateBlobSize();
            System.IntPtr blob = Marshal.AllocHGlobal(blobSize);
            System.IntPtr beginningOfUnused;
            Internal.Kernel.DotsC_FormatBlob(blob, blobSize, obj.GetTypeId(), out beginningOfUnused);
            obj.WriteToBlob(blob, ref beginningOfUnused);

            int BUF_SIZE = 100000;
            IntPtr buf = Marshal.AllocHGlobal(BUF_SIZE);
            System.Int32 resultSize = 0;
            Internal.Kernel.DotsC_BetterBlobToXml(buf, blob, BUF_SIZE, out resultSize);
            if (resultSize > BUF_SIZE)
            {
                BUF_SIZE = resultSize;
                Marshal.FreeHGlobal(buf);
                buf = Marshal.AllocHGlobal(BUF_SIZE);
                Internal.Kernel.DotsC_BetterBlobToXml(buf, blob, BUF_SIZE, out resultSize);
                if (resultSize != BUF_SIZE)
                {
                    throw new SoftwareViolationException("Error in serialization buffer sizes!!!");
                }
            }
            string str = Internal.InternalOperations.StringOf(buf);
            Marshal.FreeHGlobal(buf);
            Marshal.FreeHGlobal(blob);

            return str;
        }

        /// <summary>
        /// Convert a blob to XML.
        /// </summary>
        /// <param name="binary">The blob to convert to xml.</param>
        /// <returns>The xml of the blob.</returns>
        public static string ToXml(byte[] binary)
        {
            System.IntPtr blob = Marshal.AllocHGlobal(binary.Length);
            Marshal.Copy(binary, 0, blob, binary.Length);
            try
            {
                return ToXml(blob);
            }
            finally
            {
                Marshal.FreeHGlobal(blob);
            }
        }

        /// <summary>
        /// Convert a blob to XML.
        /// </summary>
        /// <param name="blob">The blob to convert to xml.</param>
        /// <returns>The xml of the blob.</returns>
        public static string ToXml(System.IntPtr blob)
        {
            int BUF_SIZE = 100000;
            IntPtr buf = Marshal.AllocHGlobal(BUF_SIZE);
            System.Int32 resultSize = 0;
            Internal.Kernel.DotsC_BetterBlobToXml(buf, blob, BUF_SIZE, out resultSize);
            if (resultSize > BUF_SIZE)
            {
                BUF_SIZE = resultSize;
                Marshal.FreeHGlobal(buf);
                buf = Marshal.AllocHGlobal(BUF_SIZE);
                Internal.Kernel.DotsC_BetterBlobToXml(buf, blob, BUF_SIZE, out resultSize);
                if (resultSize != BUF_SIZE)
                {
                    throw new SoftwareViolationException("Error in serialization buffer sizes!!!");
                }
            }
            string str = Internal.InternalOperations.StringOf(buf);
            Marshal.FreeHGlobal(buf);
            
            return str;
        }

        /// <summary>
        /// Deserialize an XML serialization.
        /// <para>
        /// This method creates a new object from a given xml serialization.
        /// It uses the ObjectFactory to accomplish this.
        /// </para>
        /// </summary>
        /// <param name="xml">The xml serialization to deserialize.</param>
        /// <returns>New object.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">If the type represented by the serialization isn't
        /// found in the ObjectFactory.</exception>
        public static Object ToObject(string xml)
        {
            IntPtr blob;
            System.IntPtr sp = Internal.InternalOperations.CStringOf(xml);
#if FUNC_PTR_WORKAROUND
            System.IntPtr deleter;
#else
            Internal.Kernel.DotsC_BytePointerDeleter deleter;
#endif
            Kernel.DotsC_XmlToBlob(out blob, out deleter, sp);
            Marshal.FreeHGlobal(sp);
            Object obj = ObjectFactory.Instance.CreateObject(blob);

#if FUNC_PTR_WORKAROUND
            Internal.Kernel.DotsC_DeleteBlob(ref blob);
#else
            deleter(ref blob);
#endif
            return obj;
        }

        /// <summary>
        /// Serialize an object to binary form.
        /// </summary>
        /// <param name="obj">The object to serialize.</param>
        /// <returns>Object in binary form.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is something wrong with the object.</exception>
        public static byte[] ToBinary(Dob.Typesystem.Object obj)
        {
            System.Int32 blobSize = obj.CalculateBlobSize();
            if (blobSize == 0)
            {
                return new byte[0];
            }
            System.IntPtr blob = Marshal.AllocHGlobal(blobSize);
            System.IntPtr beginningOfUnused;
            Internal.Kernel.DotsC_FormatBlob(blob, blobSize, obj.GetTypeId(), out beginningOfUnused);
            obj.WriteToBlob(blob, ref beginningOfUnused);
            byte[] result = new byte[blobSize];
            Marshal.Copy(blob, result, 0, blobSize);
            Marshal.FreeHGlobal(blob);
            return result;
        }

        /// <summary>
        /// Deserialize a binary serialization and create an object.
        /// </summary>
        /// <param name="binary">The binary serialization to deserialize.</param>
        /// <returns>New object.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">If the type represented by the serialization isn't found
        /// in the ObjectFactory.</exception>
        public static Object ToObject(byte[] binary)
        {
            System.IntPtr blob = Marshal.AllocHGlobal(binary.Length);
            Marshal.Copy(binary, 0, blob, binary.Length);
            Object obj = ObjectFactory.Instance.CreateObject(blob);
            Marshal.FreeHGlobal(blob);
            return obj;
        }
    }
}
