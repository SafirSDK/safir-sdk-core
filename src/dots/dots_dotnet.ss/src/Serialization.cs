/* ****************************************************************************
*
* Copyright Saab AB, 2005-2015 (http://safirsdkcore.com)
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
    /// Methods for serializing objects to binary, XML and JSON forms.
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
            if (obj == null) {
                throw new SoftwareViolationException("Attempt to serialize a null object to xml!");
            }

            byte[] bin = ToBinary (obj);
            return ToXml (bin);
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
            Internal.Kernel.DotsC_BlobToXml(buf, blob, BUF_SIZE, out resultSize);
            if (resultSize > BUF_SIZE)
            {
                BUF_SIZE = resultSize;
                Marshal.FreeHGlobal(buf);
                buf = Marshal.AllocHGlobal(BUF_SIZE);
                Internal.Kernel.DotsC_BlobToXml(buf, blob, BUF_SIZE, out resultSize);
                if (resultSize != BUF_SIZE)
                {
                    throw new SoftwareViolationException("Error in serialization buffer sizes!!!");
                }
            }
            string str = Internal.InternalOperations.StringOf(buf, resultSize - 1); //remove null
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
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">If there is something wrong with the XML or if the type
        ///  represented by the serialization isn't found in the ObjectFactory.</exception>
        public static Object ToObject(string xml)
        {
            IntPtr blob;
            System.IntPtr sp = Internal.InternalOperations.CStringOf(xml);
            Internal.Kernel.DotsC_BytePointerDeleter deleter;
            Kernel.DotsC_XmlToBlob(out blob, out deleter, sp);
            Marshal.FreeHGlobal(sp);
            if (blob == System.IntPtr.Zero)
            {
                throw new IllegalValueException("Something is wrong with the XML-formated object");
            }
            Object obj = ObjectFactory.Instance.CreateObject(blob);

            deleter(ref blob);
            return obj;
        }


        /// <summary>
        /// Serialize an object to JSON.
        /// </summary>
        /// <param name="obj">The object to serialize</param>
        /// <returns>The json serialization</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is something wrong with the object.</exception>
        public static string ToJson(Dob.Typesystem.Object obj)
        {
            if (obj == null) {
                throw new SoftwareViolationException("Attempt to serialize a null object to json!");
            }

            byte[] bin = ToBinary (obj);
            return ToJson (bin);
        }

        /// <summary>
        /// Convert a blob to JSON.
        /// </summary>
        /// <param name="binary">The blob to convert to json.</param>
        /// <returns>The json of the blob.</returns>
        public static string ToJson(byte[] binary)
        {
            System.IntPtr blob = Marshal.AllocHGlobal(binary.Length);
            Marshal.Copy(binary, 0, blob, binary.Length);
            try
            {
                return ToJson(blob);
            }
            finally
            {
                Marshal.FreeHGlobal(blob);
            }
        }

        /// <summary>
        /// Convert a blob to JSON.
        /// </summary>
        /// <param name="blob">The blob to convert to json.</param>
        /// <returns>The json of the blob.</returns>
        public static string ToJson(System.IntPtr blob)
        {
            int BUF_SIZE = 100000;
            IntPtr buf = Marshal.AllocHGlobal(BUF_SIZE);
            System.Int32 resultSize = 0;
            Internal.Kernel.DotsC_BlobToJson(buf, blob, BUF_SIZE, out resultSize);
            if (resultSize > BUF_SIZE)
            {
                BUF_SIZE = resultSize;
                Marshal.FreeHGlobal(buf);
                buf = Marshal.AllocHGlobal(BUF_SIZE);
                Internal.Kernel.DotsC_BlobToJson(buf, blob, BUF_SIZE, out resultSize);
                if (resultSize != BUF_SIZE)
                {
                    throw new SoftwareViolationException("Error in serialization buffer sizes!!!");
                }
            }
            string str = Internal.InternalOperations.StringOf(buf, resultSize - 1); //remove null
            Marshal.FreeHGlobal(buf);
            
            return str;
        }

        /// <summary>
        /// Deserialize an JSON serialization.
        /// <para>
        /// This method creates a new object from a given json serialization.
        /// It uses the ObjectFactory to accomplish this.
        /// </para>
        /// </summary>
        /// <param name="json">The json serialization to deserialize.</param>
        /// <returns>New object.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">If there is something wrong with the JSON or if the type
        ///  represented by the serialization isn't found in the ObjectFactory.</exception>
        public static Object ToObjectFromJson(string json)
        {
            IntPtr blob;
            System.IntPtr sp = Internal.InternalOperations.CStringOf(json);
            Internal.Kernel.DotsC_BytePointerDeleter deleter;
            Kernel.DotsC_JsonToBlob(out blob, out deleter, sp);
            Marshal.FreeHGlobal(sp);
            if (blob == System.IntPtr.Zero)
            {
                throw new IllegalValueException("Something is wrong with the JSON-formated object");
            }
            Object obj = ObjectFactory.Instance.CreateObject(blob);

            deleter(ref blob);
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
            if (obj == null) {
                throw new SoftwareViolationException("Attempt to serialize a null object to binary!");
            }

            Int64 handle = Kernel.DotsC_CreateBlobWriter (obj.GetTypeId ());
            obj.WriteToBlob (handle);
            Int32 size = Kernel.DotsC_CalculateBlobSize (handle);
            System.IntPtr blob = Marshal.AllocHGlobal(size);
            Kernel.DotsC_WriteBlob (handle, blob);
            byte[] result=new byte[size];
            Marshal.Copy(blob, result, 0, size);
            Marshal.FreeHGlobal (blob);
            Kernel.DotsC_DeleteBlobWriter (handle);
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
