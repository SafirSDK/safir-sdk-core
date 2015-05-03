/* ****************************************************************************
*
* Copyright Saab AB, 2005-2015 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joot
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
    /// Operations on blobs.
    /// <para/>
    /// Functions for getting information from blobs and setting and getting
    /// values in blobs.
    /// <para/>
    /// Currently this functionality is meant mainly for internal use, but the functions
    /// can be used to modify binary serializations of objects.
    /// But be careful if you do, and talk to your closest DOB expert first.
    /// <para/>
    /// Note: Most of these methods have no checks on them to make sure that everything
    /// has worked okay. They will just return unexpected values if something went wrong.
    /// </summary>
    public class BlobOperations
    {
        /// <summary>
        /// Writes to BLOB.
        /// </summary>
        /// <returns>The to BLOB.</returns>
        /// <param name="obj">Object.</param>
        public static IntPtr WriteToBlob(Dob.Typesystem.Object obj)
        {
            Int64 objHandle=Kernel.DotsC_CreateBlobWriter(obj.GetTypeId());
            obj.WriteToBlob(objHandle);
            Int32 size=Kernel.DotsC_CalculateBlobSize (objHandle);
            IntPtr blob = Marshal.AllocHGlobal(size);
            Kernel.DotsC_WriteBlob (objHandle, blob);
            Kernel.DotsC_DeleteBlobWriter (objHandle);
            return blob;
        }

        /// <summary>
        /// Deletes the BLOB created with BlobOperations.WriteToBlob
        /// </summary>
        /// <param name="blob">BLOB.</param>
        public static void DeleteBlob(IntPtr blob)
        {
            Marshal.FreeHGlobal (blob);
        }

        /// <summary>
        /// Extract the TypeId from a blob.
        /// </summary>
        /// <param name="blob">The blob to read from.</param>
        /// <returns>The TypeId from the blob.</returns>
        public static System.Int64 GetTypeId(System.IntPtr blob)
        {
            System.Diagnostics.Debug.Assert(blob != System.IntPtr.Zero);
            return Internal.Kernel.DotsC_GetTypeId(blob);
        }

        /// <summary>
        /// Get the size of the blob contained by this object
        /// </summary>
        /// <param name="blob">the blob</param>
        /// <returns>the size of the blob, in bytes.</returns>
        public static Int32 GetSize(System.IntPtr blob)
        {
            System.Diagnostics.Debug.Assert(blob != System.IntPtr.Zero);
            return Internal.Kernel.DotsC_GetSize(blob);
        }

        /// <summary>
        /// Get the number of values contrained in a member.
        /// </summary>
        /// <returns>The number of member values.</returns>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        public static Int32 NumerOfMemberValues(System.Int64 handle, System.Int32 member)
        {
            return Kernel.DotsC_GetNumberOfMemberValues (handle, member);
        }


        #region Container operations on Blobs

        /// <summary>
        /// Get a boolean from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(BooleanContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set a boolean in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(BooleanContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get an enumeration from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(EnumerationContainerBase container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an enumeration in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(EnumerationContainerBase container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get an Int32 from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Int32Container container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Int32 in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Int32Container container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get an Int64 from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Int64Container container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Int64 in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Int64Container container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Float32 from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Float32Container container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set a Float32 in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Float32Container container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Float64 from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Float64Container container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set a Float64 in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Float64Container container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a TypeId from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(TypeIdContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set a TypeId in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(TypeIdContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get an InstanceId from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(InstanceIdContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an InstancId in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(InstanceIdContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get an EntityId from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(EntityIdContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an EntityId in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(EntityIdContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a ChannelId from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(ChannelIdContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set a ChannelId in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(ChannelIdContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a HandlerId from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(HandlerIdContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set a HandlerId in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(HandlerIdContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a string from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(StringContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set a string in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(StringContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get an Object from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(ObjectContainerBase container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            bool isNull;
            Safir.Dob.Typesystem.Object obj;
            Get(out obj, out isNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
            if (!isNull)
            {
                container.InternalObj=obj;
            }
            else
            {
                container.InternalObj=null;
            }
        }

        /// <summary>
        /// Set an Object in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(ObjectContainerBase container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.InternalObj, container.IsNull(), container.IsChanged(), handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a binary from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(BinaryContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set a binary in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(BinaryContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.IsNull(), container.IsChanged(), handle, member, index, KeyValMode.ValueMode);
        }

        #region SI types

        /// <summary>
        /// Get a Si32.AmpereContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.AmpereContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.AmpereContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.AmpereContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.CubicMeterContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.CubicMeterContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.CubicMeterContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.CubicMeterContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.HertzContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.HertzContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.HertzContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.HertzContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.JouleContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.JouleContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.JouleContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.JouleContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.KelvinContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.KelvinContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.KelvinContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.KelvinContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.KilogramContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.KilogramContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.KilogramContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.KilogramContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.MeterContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.MeterContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.MeterContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.MeterContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.MeterPerSecondContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.MeterPerSecondContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.MeterPerSecondContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.MeterPerSecondContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.MeterPerSecondSquaredContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.MeterPerSecondSquaredContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.MeterPerSecondSquaredContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.MeterPerSecondSquaredContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.NewtonContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.NewtonContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.NewtonContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.NewtonContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.PascalContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.PascalContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.PascalContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.PascalContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.RadianContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.RadianContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.RadianContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.RadianContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.RadianPerSecondContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.RadianPerSecondContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.RadianPerSecondContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.RadianPerSecondContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.RadianPerSecondSquaredContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.RadianPerSecondSquaredContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.RadianPerSecondSquaredContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.RadianPerSecondSquaredContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.SecondContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.SecondContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.SecondContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.SecondContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.SquareMeterContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.SquareMeterContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.SquareMeterContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.SquareMeterContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.SteradianContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.SteradianContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.SteradianContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.SteradianContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.VoltContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.VoltContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.VoltContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.VoltContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si32.WattContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.WattContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si32.WattContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.WattContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.AmpereContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.AmpereContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.AmpereContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.AmpereContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.CubicMeterContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.CubicMeterContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.CubicMeterContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.CubicMeterContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.HertzContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.HertzContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.HertzContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.HertzContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.JouleContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.JouleContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.JouleContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.JouleContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.KelvinContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.KelvinContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.KelvinContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.KelvinContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.KilogramContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.KilogramContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.KilogramContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.KilogramContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.MeterContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.MeterContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.MeterContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.MeterContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.MeterPerSecondContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.MeterPerSecondContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.MeterPerSecondContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.MeterPerSecondContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.MeterPerSecondSquaredContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.MeterPerSecondSquaredContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.MeterPerSecondSquaredContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.MeterPerSecondSquaredContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.NewtonContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.NewtonContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.NewtonContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.NewtonContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.PascalContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.PascalContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.PascalContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.PascalContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.RadianContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.RadianContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.RadianContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.RadianContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.RadianPerSecondContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.RadianPerSecondContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.RadianPerSecondContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.RadianPerSecondContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.RadianPerSecondSquaredContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.RadianPerSecondSquaredContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.RadianPerSecondSquaredContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.RadianPerSecondSquaredContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.SecondContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.SecondContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.SecondContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.SecondContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.SquareMeterContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.SquareMeterContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.SquareMeterContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.SquareMeterContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.SteradianContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.SteradianContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.SteradianContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.SteradianContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.VoltContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.VoltContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.VoltContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.VoltContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Get a Si64.WattContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="handle">Handle to BlobReader to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.WattContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get (out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }

        /// <summary>
        /// Set an Si64.WattContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="handle">Handle to BlobWriter to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.WattContainer container,
                               System.Int64 handle,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set (container.m_Value, container.m_bIsNull, container.m_bIsChanged, handle, member, index, KeyValMode.ValueMode);
        }
        #endregion
        #endregion

		//---------------------------------------------------------
		// Get values
		//--------------------------------------------------------
        /// <summary>
        /// Get the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">Is null.</param>
        /// <param name="isChanged">Is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
		public static void Get(out bool val,
                               out bool isNull,
                               out bool isChanged,
		                	   Int64 handle,
				               Int32 member,
                               Int32 valueIndex,
				               KeyValMode mode)
        {
            if (mode==KeyValMode.KeyMode)
            {
                throw new SoftwareViolationException("BlobOperation.Get(bool) called with mode=KeyMode. Only ValueMode is allowed for this type!");
            }

            byte v, n, c;
            Kernel.DotsC_ReadBooleanMember (handle, out v, out n, out c, member, valueIndex, mode);
            val = InternalOperations.BoolOf (v);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
        }

        /// <summary>
        /// Get the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">Is null.</param>
        /// <param name="isChanged">Is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Get(out Int32 val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               KeyValMode mode)
        {
            byte n, c;
            Kernel.DotsC_ReadInt32Member (handle, out val, out n, out c, member, valueIndex, mode);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
        }

        /// <summary>
        /// Get the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">Is null.</param>
        /// <param name="isChanged">Is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Get(out Int64 val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               KeyValMode mode)
        {
            byte n, c;
            Kernel.DotsC_ReadInt64Member (handle, out val, out n, out c, member, valueIndex, mode);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
        }

        /// <summary>
        /// Get the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">Is null.</param>
        /// <param name="isChanged">Is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
		public static void Get(out float val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               KeyValMode mode)
        {
            if (mode==KeyValMode.KeyMode)
            {
                throw new SoftwareViolationException("BlobOperation.Get(float) called with mode=KeyMode. Only ValueMode is allowed for this type!");
            }

            byte n, c;
            Kernel.DotsC_ReadFloat32Member (handle, out val, out n, out c, member, valueIndex, mode);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
        }

        /// <summary>
        /// Get the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">Is null.</param>
        /// <param name="isChanged">Is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
		public static void Get(out double val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               KeyValMode mode)
        {
            if (mode==KeyValMode.KeyMode)
            {
                throw new SoftwareViolationException("BlobOperation.Get(double) called with mode=KeyMode. Only ValueMode is allowed for this type!");
            }

            byte n, c;
            Kernel.DotsC_ReadFloat64Member (handle, out val, out n, out c, member, valueIndex, mode);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
        }

        /// <summary>
        /// Get the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">Is null.</param>
        /// <param name="isChanged">Is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Get(out string val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               KeyValMode mode)
        {
            byte n, c;
            IntPtr str;
            Kernel.DotsC_ReadStringMember (handle, out str, out n, out c, member, valueIndex, mode);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
            if (!isNull)
            {
                val = Internal.InternalOperations.StringOf(str);
            }
            else
            {
                val = null;
            }
        }

        /// <summary>
        /// Get the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">Is null.</param>
        /// <param name="isChanged">Is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Get(out InstanceId val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               KeyValMode mode)
        {
            byte n, c;
            Int64 hash;
            IntPtr str;
            Kernel.DotsC_ReadHashedMember (handle, out hash, out str, out n, out c, member, valueIndex, mode);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
            if (!isNull)
            {
                if (str == IntPtr.Zero)
                    val = new InstanceId (hash);
                else
                    val = new InstanceId (hash, InternalOperations.StringOf(str));
            }
            else
            {
                val = null;
            }
        }

        /// <summary>
        /// Get the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">Is null.</param>
        /// <param name="isChanged">Is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Get(out HandlerId val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               KeyValMode mode)
        {
            byte n, c;
            Int64 hash;
            IntPtr str;
            Kernel.DotsC_ReadHashedMember (handle, out hash, out str, out n, out c, member, valueIndex, mode);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
            if (!isNull)
            {
                if (str == IntPtr.Zero)
                    val = new HandlerId (hash);
                else
                    val = new HandlerId (hash, InternalOperations.StringOf(str));
            }
            else
            {
                val = null;
            }
        }

        /// <summary>
        /// Get the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">Is null.</param>
        /// <param name="isChanged">Is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Get(out ChannelId val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               KeyValMode mode)
        {
            byte n, c;
            Int64 hash;
            IntPtr str;
            Kernel.DotsC_ReadHashedMember (handle, out hash, out str, out n, out c, member, valueIndex, mode);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
            if (!isNull)
            {
                if (str == IntPtr.Zero)
                    val = new ChannelId (hash);
                else
                    val = new ChannelId (hash, InternalOperations.StringOf(str));
            }
            else
            {
                val = null;
            }
        }

        /// <summary>
        /// Get the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">Is null.</param>
        /// <param name="isChanged">Is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Get(out EntityId val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               KeyValMode mode)
        {
            byte n, c;
            DotsC_EntityId eid;
            IntPtr str;
            Kernel.DotsC_ReadEntityIdMember (handle, out eid, out str, out n, out c, member, valueIndex, mode);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
            if (!isNull)
            {
                if (str == IntPtr.Zero)
                    val = new EntityId (eid.TypeId, new InstanceId (eid.InstanceId));
                else
                    val = new EntityId (eid.TypeId, new InstanceId (eid.InstanceId, InternalOperations.StringOf(str)));
            }
            else
            {
                val = null;
            }
        }

        /// <summary>
        /// Get the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">Is null.</param>
        /// <param name="isChanged">Is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Get(out Safir.Dob.Typesystem.Object val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               KeyValMode mode)
        {
            if (mode==KeyValMode.KeyMode)
            {
                throw new SoftwareViolationException("BlobOperation.Get(object) called with mode=KeyMode. Only ValueMode is allowed for this type!");
            }

            byte n, c;
            IntPtr blob;
            Kernel.DotsC_ReadObjectMember(handle, out blob, out n, out c, member, valueIndex, mode);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
            if (!isNull)
            {
                val = ObjectFactory.Instance.CreateObject (blob);
            }
            else
            {
                val = null;
            }
        }

        /// <summary>
        /// Get the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">Is null.</param>
        /// <param name="isChanged">Is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Get(out byte[] val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               KeyValMode mode)
        {
            if (mode==KeyValMode.KeyMode)
            {
                throw new SoftwareViolationException("BlobOperation.Get(binary) called with mode=KeyMode. Only ValueMode is allowed for this type!");
            }

            byte n, c;
            IntPtr bin;
            Int32 size;
            Kernel.DotsC_ReadBinaryMember(handle, out bin, out size, out n, out c, member, valueIndex, mode);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
            if (!isNull)
            {
                val = new byte[size];
                Marshal.Copy(bin, val, 0, size);
            }
            else
            {
                val = null;
            }
        }

		//---------------------------------------------------------
		// Set values
		//--------------------------------------------------------
        /// <summary>
        /// Set the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">If set to <c>true</c> value.</param>
        /// <param name="isNull">If set to <c>true</c> is null.</param>
        /// <param name="isChanged">If set to <c>true</c> is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
		public static void Set(bool val,
        		               bool isNull,
        		               bool isChanged,
        		               Int64 handle,
        		               Int32 member,
        		               Int32 valueIndex,
        		               KeyValMode mode)
        {
            if (mode==KeyValMode.KeyMode)
            {
                throw new SoftwareViolationException("BlobOperation.Set(bool) called with mode=KeyMode. Only ValueMode is allowed for this type!");
            }

            Kernel.DotsC_WriteBooleanMember (handle,
                                             InternalOperations.ByteOf(val),
                                             InternalOperations.ByteOf(isNull),
                                             InternalOperations.ByteOf(isChanged),
                                             member, valueIndex, mode);
        }

        /// <summary>
        /// Set the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">If set to <c>true</c> is null.</param>
        /// <param name="isChanged">If set to <c>true</c> is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Set(Int32 val,
        		               bool isNull,
        		               bool isChanged,
        		               Int64 handle,
        		               Int32 member,
        		               Int32 valueIndex,
        		               KeyValMode mode)
        {
            Kernel.DotsC_WriteInt32Member (handle, val,
                                           InternalOperations.ByteOf(isNull),
                                           InternalOperations.ByteOf(isChanged),
                                           member, valueIndex, mode);
        }

        /// <summary>
        /// Set the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">If set to <c>true</c> is null.</param>
        /// <param name="isChanged">If set to <c>true</c> is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Set(Int64 val,
        		               bool isNull,
        		               bool isChanged,
        		               Int64 handle,
        		               Int32 member,
        		               Int32 valueIndex,
        		               KeyValMode mode)
        {
            Kernel.DotsC_WriteInt64Member (handle, val,
                                           InternalOperations.ByteOf(isNull),
                                           InternalOperations.ByteOf(isChanged),
                                           member, valueIndex, mode);
        }

        /// <summary>
        /// Set the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">If set to <c>true</c> is null.</param>
        /// <param name="isChanged">If set to <c>true</c> is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Set(float val,
        		               bool isNull,
        		               bool isChanged,
        		               Int64 handle,
        		               Int32 member,
        		               Int32 valueIndex,
        		               KeyValMode mode)
        {
            if (mode==KeyValMode.KeyMode)
            {
                throw new SoftwareViolationException("BlobOperation.Set(float) called with mode=KeyMode. Only ValueMode is allowed for this type!");
            }
            Kernel.DotsC_WriteFloat32Member (handle, val,
                                             InternalOperations.ByteOf(isNull),
                                             InternalOperations.ByteOf(isChanged),
                                             member, valueIndex, mode);
        }

        /// <summary>
        /// Set the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">If set to <c>true</c> is null.</param>
        /// <param name="isChanged">If set to <c>true</c> is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Set(double val,
        		               bool isNull,
        		               bool isChanged,
        		               Int64 handle,
        		               Int32 member,
        		               Int32 valueIndex,
        		               KeyValMode mode)
        {
            if (mode==KeyValMode.KeyMode)
            {
                throw new SoftwareViolationException("BlobOperation.Set(double) called with mode=KeyMode. Only ValueMode is allowed for this type!");
            }
            Kernel.DotsC_WriteFloat64Member (handle, val,
                                             InternalOperations.ByteOf(isNull),
                                             InternalOperations.ByteOf(isChanged),
                                             member, valueIndex, mode);
        }

        /// <summary>
        /// Set the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">If set to <c>true</c> is null.</param>
        /// <param name="isChanged">If set to <c>true</c> is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Set(string val,
        		               bool isNull,
        		               bool isChanged,
        		               Int64 handle,
        		               Int32 member,
        		               Int32 valueIndex,
        		               KeyValMode mode)
		{
            if (isNull)
            {
                Kernel.DotsC_WriteStringMember (handle, IntPtr.Zero,
                                                InternalOperations.ByteOf(isNull),
                                                InternalOperations.ByteOf(isChanged),
                                                member, valueIndex, mode);
            }
            else
            {
                if (val == null)
                {
                    throw new SoftwareViolationException("Val was null!");
                }
                IntPtr cstring = InternalOperations.CStringOf (val);
    			Kernel.DotsC_WriteStringMember (handle, cstring,
    			                                InternalOperations.ByteOf(isNull),
    			                                InternalOperations.ByteOf(isChanged),
    			                                member, valueIndex, mode);
                Marshal.FreeHGlobal (cstring);
            }
		}

        /// <summary>
        /// Set the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">If set to <c>true</c> is null.</param>
        /// <param name="isChanged">If set to <c>true</c> is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Set(InstanceId val,
        		               bool isNull,
        		               bool isChanged,
        		               Int64 handle,
        		               Int32 member,
        		               Int32 valueIndex,
        		               KeyValMode mode)
        {
            if (isNull)
            {
                Kernel.DotsC_WriteHashedMember (handle, 0, IntPtr.Zero,
                                                InternalOperations.ByteOf(isNull),
                                                InternalOperations.ByteOf(isChanged),
                                                member, valueIndex, mode);
            }
            else
            {
                if (val == null)
                {
                    throw new SoftwareViolationException("Val was null!");
                }

                if (string.IsNullOrEmpty(val.RawString))
                {
                    Kernel.DotsC_WriteHashedMember (handle, val.RawValue, IntPtr.Zero,
                                                    InternalOperations.ByteOf(isNull),
                                                    InternalOperations.ByteOf(isChanged),
                                                    member, valueIndex, mode);
                }
                else
                {
                    IntPtr cstring = InternalOperations.CStringOf (val.RawString);
                    Kernel.DotsC_WriteHashedMember (handle, val.RawValue, cstring,
                                                    InternalOperations.ByteOf(isNull),
                                                    InternalOperations.ByteOf(isChanged),
                                                    member, valueIndex, mode);
                    Marshal.FreeHGlobal (cstring);
                }
            }
        }

        /// <summary>
        /// Set the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">If set to <c>true</c> is null.</param>
        /// <param name="isChanged">If set to <c>true</c> is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Set(HandlerId val,
        		               bool isNull,
        		               bool isChanged,
        		               Int64 handle,
        		               Int32 member,
        		               Int32 valueIndex,
        		               KeyValMode mode)
        {
            if (isNull)
            {
                Kernel.DotsC_WriteHashedMember (handle, 0, IntPtr.Zero,
                                                InternalOperations.ByteOf(isNull),
                                                InternalOperations.ByteOf(isChanged),
                                                member, valueIndex, mode);
            }
            else
            {
                if (val == null)
                {
                    throw new SoftwareViolationException("Val was null!");
                }

                if (string.IsNullOrEmpty(val.RawString))
                {
                    Kernel.DotsC_WriteHashedMember (handle, val.RawValue, IntPtr.Zero,
                                                    InternalOperations.ByteOf(isNull),
                                                    InternalOperations.ByteOf(isChanged),
                                                    member, valueIndex, mode);
                }
                else
                {
                    IntPtr cstring = InternalOperations.CStringOf (val.RawString);
                    Kernel.DotsC_WriteHashedMember (handle, val.RawValue, cstring,
                                                    InternalOperations.ByteOf(isNull),
                                                    InternalOperations.ByteOf(isChanged),
                                                    member, valueIndex, mode);
                    Marshal.FreeHGlobal (cstring);
                }
            }
        }

        /// <summary>
        /// Set the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">If set to <c>true</c> is null.</param>
        /// <param name="isChanged">If set to <c>true</c> is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Set(ChannelId val,
        		               bool isNull,
        		               bool isChanged,
        		               Int64 handle,
        		               Int32 member,
        		               Int32 valueIndex,
        		               KeyValMode mode)
        {
            if (isNull)
            {
                Kernel.DotsC_WriteHashedMember (handle, 0, IntPtr.Zero,
                                                InternalOperations.ByteOf(isNull),
                                                InternalOperations.ByteOf(isChanged),
                                                member, valueIndex, mode);
            }
            else
            {
                if (val == null)
                {
                    throw new SoftwareViolationException("Val was null!");
                }

                if (string.IsNullOrEmpty(val.RawString))
                {
                    Kernel.DotsC_WriteHashedMember (handle, val.RawValue, IntPtr.Zero,
                                                    InternalOperations.ByteOf(isNull),
                                                    InternalOperations.ByteOf(isChanged),
                                                    member, valueIndex, mode);
                }
                else
                {
                    IntPtr cstring = InternalOperations.CStringOf (val.RawString);
                    Kernel.DotsC_WriteHashedMember (handle, val.RawValue, cstring,
                                                    InternalOperations.ByteOf(isNull),
                                                    InternalOperations.ByteOf(isChanged),
                                                    member, valueIndex, mode);
                    Marshal.FreeHGlobal (cstring);
                }
            }
        }

        /// <summary>
        /// Set the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">If set to <c>true</c> is null.</param>
        /// <param name="isChanged">If set to <c>true</c> is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Set(EntityId val,
        		               bool isNull,
        		               bool isChanged,
        		               Int64 handle,
        		               Int32 member,
        		               Int32 valueIndex,
        		               KeyValMode mode)
        {
            if (isNull)
            {
                DotsC_EntityId eid;
                eid.TypeId = 0;
                eid.InstanceId = 0;
                Kernel.DotsC_WriteEntityIdMember (handle, eid, IntPtr.Zero,
                                                  InternalOperations.ByteOf(isNull),
                                                  InternalOperations.ByteOf(isChanged),
                                                  member, valueIndex, mode);
            }
            else
            {
                if (val == null)
                {
                    throw new SoftwareViolationException("Val was null!");
                }

                DotsC_EntityId eid;
                eid.TypeId = val.TypeId;
                eid.InstanceId = val.InstanceId.RawValue;
                if (string.IsNullOrEmpty(val.InstanceId.RawString))
                {
                    Kernel.DotsC_WriteEntityIdMember (handle, eid, IntPtr.Zero,
                                                    InternalOperations.ByteOf(isNull),
                                                    InternalOperations.ByteOf(isChanged),
                                                    member, valueIndex, mode);
                }
                else
                {
                    IntPtr cstring = InternalOperations.CStringOf (val.InstanceId.RawString);
                    Kernel.DotsC_WriteEntityIdMember (handle, eid, cstring,
                                                      InternalOperations.ByteOf(isNull),
                                                      InternalOperations.ByteOf(isChanged),
                                                      member, valueIndex, mode);
                    Marshal.FreeHGlobal (cstring);
                }
            }
        }

        /// <summary>
        /// Set the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">If set to <c>true</c> is null.</param>
        /// <param name="isChanged">If set to <c>true</c> is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Set(Safir.Dob.Typesystem.Object val,
        		               bool isNull,
        		               bool isChanged,
        		               Int64 handle,
        		               Int32 member,
        		               Int32 valueIndex,
        		               KeyValMode mode)
        {
            if (mode==KeyValMode.KeyMode)
            {
                throw new SoftwareViolationException("BlobOperation.Set(object) called with mode=KeyMode. Only ValueMode is allowed for this type!");
            }

            if (isNull)
            {
                Kernel.DotsC_WriteObjectMember (handle, IntPtr.Zero,
                                                InternalOperations.ByteOf(isNull),
                                                InternalOperations.ByteOf(isChanged),
                                                member, valueIndex, mode);
            }
            else
            {
                if (val == null)
                {
                    throw new SoftwareViolationException ("Val was null!");
                }

                Int64 objHandle=Kernel.DotsC_CreateBlobWriter(val.GetTypeId());
                val.WriteToBlob(objHandle);
                Int32 size=Kernel.DotsC_CalculateBlobSize (objHandle);
                IntPtr blob = Marshal.AllocHGlobal(size);
                Kernel.DotsC_WriteBlob (objHandle, blob);
                Kernel.DotsC_DeleteBlobWriter (objHandle);
                Kernel.DotsC_WriteObjectMember (handle, blob,
                                                InternalOperations.ByteOf(isNull),
                                                InternalOperations.ByteOf(isChanged),
                                                member, valueIndex, mode);
                Marshal.FreeHGlobal (blob);
            }
        }

        /// <summary>
        /// Set the specified val, isNull, isChanged, handle, member, valueIndex and mode.
        /// </summary>
        /// <param name="val">Value.</param>
        /// <param name="isNull">If set to <c>true</c> is null.</param>
        /// <param name="isChanged">If set to <c>true</c> is changed.</param>
        /// <param name="handle">Handle.</param>
        /// <param name="member">Member.</param>
        /// <param name="valueIndex">Value index.</param>
        /// <param name="mode">Mode.</param>
        public static void Set(byte[] val,
        		               bool isNull,
        		               bool isChanged,
        		               Int64 handle,
        		               Int32 member,
        		               Int32 valueIndex,
        		               KeyValMode mode)
        {
            if (mode==KeyValMode.KeyMode)
            {
                throw new SoftwareViolationException("BlobOperation.Set(binary) called with mode=KeyMode. Only ValueMode is allowed for this type!");
            }

            if (isNull || val.Length == 0)
            {
                Kernel.DotsC_WriteBinaryMember (handle, IntPtr.Zero, 0,
                                                InternalOperations.ByteOf(isNull),
                                                InternalOperations.ByteOf(isChanged),
                                                member, valueIndex, mode);
            }
            else
            {
                if (val == null)
                {
                    throw new SoftwareViolationException ("Val was null!");
                }

                IntPtr bytePtr = Marshal.AllocHGlobal(val.Length);
                Marshal.Copy(val, 0, bytePtr, val.Length);

                Kernel.DotsC_WriteBinaryMember (handle, bytePtr, val.Length,
                                                InternalOperations.ByteOf(isNull),
                                                InternalOperations.ByteOf(isChanged),
                                                member, valueIndex, mode);

                Marshal.FreeHGlobal(bytePtr);
            }
        }
	}
}
