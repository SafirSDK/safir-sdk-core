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
    public class Serialization
    {
        //************************************************************************************
        //* Serialization
        //************************************************************************************
        public static string ToXml(Dob.Typesystem.Object obj)
        {
            System.Int32 blobSize = obj.CalculateBlobSize();
            System.IntPtr blob = Marshal.AllocHGlobal(blobSize);
            System.IntPtr beginningOfUnused = System.IntPtr.Zero;
            Internal.Kernel.DotsC_FormatBlob(blob, blobSize, obj.GetTypeId(), ref beginningOfUnused);
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

        public static Object ToObject(string xml)
        {
            IntPtr blob;
            System.IntPtr sp = Internal.InternalOperations.CStringOf(xml);
            Kernel.DotsC_XmlToBlob(out blob, sp);
            Marshal.FreeHGlobal(sp);
            Object obj = ObjectFactory.Instance.CreateObject(blob);
            Internal.InternalOperations.Delete(ref blob);
            return obj;
        }

        public static byte[] ToBinary(Dob.Typesystem.Object obj)
        {
            System.Int32 blobSize = obj.CalculateBlobSize();
            System.IntPtr blob = Marshal.AllocHGlobal(blobSize);
            System.IntPtr beginningOfUnused = System.IntPtr.Zero;
            Internal.Kernel.DotsC_FormatBlob(blob, blobSize, obj.GetTypeId(), ref beginningOfUnused);
            obj.WriteToBlob(blob, ref beginningOfUnused);
            byte[] result = new byte[blobSize];
            Marshal.Copy(blob, result, 0, blobSize);
            Marshal.FreeHGlobal(blob);
            return result;
        }

        public static Object ToObject(byte[] binary)
        {
            System.IntPtr blob = Marshal.AllocHGlobal(binary.Length);
            Marshal.Copy(binary, 0, blob, binary.Length);
            Object obj = ObjectFactory.Instance.CreateObject(blob);
            Marshal.FreeHGlobal(blob);
            return obj;
        }
        /*
        public static bool ToObject(Dob.Typesystem.Object obj,
                                    System.IntPtr binary)
        {
            if (binary == IntPtr.Zero)
            {
                return false;
            }
            if (Operations.IsOfType(Kernel.DotsC_GetId(ref binary).TypeId, obj.GetObjectId().TypeId))
            {
                Internal.InternalOperations.DeleteBlob(ref obj.blob);
                Internal.InternalOperations.CreateCopy(ref obj.blob, ref binary);
                return true;
            }
            return false;
        }

        public static int SizeOfObject(Dob.Typesystem.Object obj)
        {
            return SizeOfBinary(Internal.InternalOperations.GetBlob(obj));
        }

        public static int SizeOfBinary(System.IntPtr binary)
        {
            return Internal.InternalOperations.GetSizeOfBlob(ref binary);
        }

        public static System.IntPtr GetBinaryReference(Dob.Typesystem.Object obj)
        {
            return Internal.InternalOperations.GetBlob(obj);
        }
        */
    }
}
