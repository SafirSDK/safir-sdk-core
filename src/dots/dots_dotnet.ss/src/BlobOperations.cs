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
        /// Check if any member is changed.
        /// 
        /// This method will recursively check if any member in the blob has its change flag set.
        /// </summary>
        /// <param name="blob">the blob</param>
        /// <returns>True if any member has changed.</returns>
        public static bool IsChanged(System.IntPtr blob)
        {
            System.Diagnostics.Debug.Assert(blob != System.IntPtr.Zero);
            return Internal.InternalOperations.BoolOf(Internal.Kernel.DotsC_IsAnythingChanged(blob));
        }

        #region Value operations on blobs

     
        /// <summary>
        /// Find out if a member is changed.
        /// </summary>
        /// <param name="blob">Blob to look in.</param>
        /// <param name="member">The member to check.</param>
        /// <param name="index">Array index in member to check. Shall be 0 if the member is not an array.</param>
        /// <returns>true if member is changed</returns>
        public static bool IsChanged(System.IntPtr blob,
                                     System.Int32 member,
                                     System.Int32 index)
        {
            return Internal.InternalOperations.BoolOf(Internal.Kernel.DotsC_IsChangedMember(blob, member, index));
        }

        /// <summary>
        /// Set a member to null.
        /// <para/>
        /// This methods sets a given member (with index) to null in a blob.
        /// If the member is not an array the index must be 0.
        /// </summary>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void SetNull(System.IntPtr blob,
                                   System.Int32 member,
                                   System.Int32 index)
        {
            Internal.Kernel.DotsC_SetNullMember(blob, member, index);
        }

        /// <summary>
        /// Set a bool in a blob.
        /// <para/>
        /// This method will set a bool member in a blob.
        /// If the isNull parameter is true then only the isChange and isNull flags are set in the blob,
        /// not the value (so it can be any value).
        /// </summary>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        /// <param name="value">The value that the member is to be set to (unless isNull is true).</param>
        /// <param name="isNull">Should the value be set to null.</param>
        /// <param name="isChanged">Should the value be set to changed.</param>
        public static void Set(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               bool value,
                               bool isNull,
                               bool isChanged)
        {
            Internal.Kernel.DotsC_SetBooleanMemberInPreallocated(Internal.InternalOperations.ByteOf(value),
                                                                 Internal.InternalOperations.ByteOf(isNull),
                                                                 Internal.InternalOperations.ByteOf(isChanged),
                                                                 blob,
                                                                 member,
                                                                 index);
        }

        /// <summary>
        /// Get a bool from a blob.
        /// <para/>
        /// This method will get a bool member and the associated isNull and isChange values from a blob.
        /// The value parameter is not valid if isNull is true.
        /// </summary>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        /// <param name="value">The value of the member (invalid if isNull is true)</param>
        /// <param name="isNull">The isNull flag of the member.</param>
        /// <param name="isChanged">The isChanged flag of the member.</param>
        public static void Get(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               out bool value,
                               out bool isNull,
                               out bool isChanged)
        {
            byte n, c, val;
            Internal.Kernel.DotsC_GetBooleanMember(blob,
                                                   member,
                                                   index,
                                                   out val,
                                                   out n,
                                                   out c);
            isNull = Internal.InternalOperations.BoolOf(n);
            isChanged = Internal.InternalOperations.BoolOf(c);
            value = Internal.InternalOperations.BoolOf(val);
        }

        /// <summary>
        /// Set an Int32 or EnumerationValue in a blob.
        /// <para/>
        /// This method will set a Int32 or EnumerationValue member in a blob.
        /// If the isNull parameter is true then only the isChange and isNull flags are set in the blob,
        /// not the value (so it can be any value).
        /// </summary>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        /// <param name="value">The value that the member is to be set to (unless isNull is true).</param>
        /// <param name="isNull">Should the value be set to null.</param>
        /// <param name="isChanged">Should the value be set to changed.</param>
        public static void Set(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               System.Int32 value,
                               bool isNull,
                               bool isChanged)
        {
            Internal.Kernel.DotsC_SetInt32MemberInPreallocated(value,
                                                               Internal.InternalOperations.ByteOf(isNull),
                                                               Internal.InternalOperations.ByteOf(isChanged),
                                                               blob,
                                                               member,
                                                               index);
        }

        /// <summary>
        /// Get a Int32 or EnumerationValue from a blob.
        /// <para/>
        /// This method will get a Int32 or EnumerationValue member and the associated isNull and isChange values from a blob.
        /// The value parameter is not valid if isNull is true.
        /// </summary>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        /// <param name="value">The value of the member (invalid if isNull is true)</param>
        /// <param name="isNull">The isNull flag of the member.</param>
        /// <param name="isChanged">The isChanged flag of the member.</param>
        public static void Get(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               out System.Int32 value,
                               out bool isNull,
                               out bool isChanged)
        {
            byte n, c;
            Internal.Kernel.DotsC_GetInt32Member(blob,
                                                 member,
                                                 index,
                                                 out value,
                                                 out n,
                                                 out c);
            isNull = Internal.InternalOperations.BoolOf(n);
            isChanged = Internal.InternalOperations.BoolOf(c);
        }

        /// <summary>
        /// Set an Int64 or a TypeId in a blob.
        /// <para/>
        /// This method will set a Int64-based type member in a blob.
        /// If the isNull parameter is true then only the isChange and isNull flags are set in the blob,
        /// not the value (so it can be any value).
        /// </summary>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        /// <param name="value">The value that the member is to be set to (unless isNull is true).</param>
        /// <param name="isNull">Should the value be set to null.</param>
        /// <param name="isChanged">Should the value be set to changed.</param>
        public static void Set(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               System.Int64 value,
                               bool isNull,
                               bool isChanged)
        {
            Internal.Kernel.DotsC_SetInt64MemberInPreallocated(value,
                                                               Internal.InternalOperations.ByteOf(isNull),
                                                               Internal.InternalOperations.ByteOf(isChanged),
                                                               blob,
                                                               member,
                                                               index);
        }

        /// <summary>
        /// Get an Int64 from a blob.
        /// <para/>
        /// This method will get a Int64-based member and the associated isNull and isChange values from a blob.
        /// The value parameter is not valid if isNull is true.
        /// </summary>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        /// <param name="value">The value of the member (invalid if isNull is true)</param>
        /// <param name="isNull">The isNull flag of the member.</param>
        /// <param name="isChanged">The isChanged flag of the member.</param>
        public static void Get(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               out System.Int64 value,
                               out bool isNull,
                               out bool isChanged)
        {
            byte n, c;
            Internal.Kernel.DotsC_GetInt64Member(blob,
                                                 member,
                                                 index,
                                                 out value,
                                                 out n,
                                                 out c);
            isNull = Internal.InternalOperations.BoolOf(n);
            isChanged = Internal.InternalOperations.BoolOf(c);
        }

        /// <summary>
        /// Set a float in a blob.
        /// <para/>
        /// This method will set a Float32 member in a blob.
        /// If the isNull parameter is true then only the isChange and isNull flags are set in the blob,
        /// not the value (so it can be any value).
        /// </summary>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        /// <param name="value">The value that the member is to be set to (unless isNull is true).</param>
        /// <param name="isNull">Should the value be set to null.</param>
        /// <param name="isChanged">Should the value be set to changed.</param>
        public static void Set(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               float value,
                               bool isNull,
                               bool isChanged)
        {
            Internal.Kernel.DotsC_SetFloat32MemberInPreallocated(value,
                                                                 Internal.InternalOperations.ByteOf(isNull),
                                                                 Internal.InternalOperations.ByteOf(isChanged),
                                                                 blob,
                                                                 member,
                                                                 index);
        }

        /// <summary>
        /// Get a float from a blob.
        /// <para/>
        /// This method will get a float member and the associated isNull and isChange values from a blob.
        /// The value parameter is not valid if isNull is true.
        /// </summary>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        /// <param name="value">The value of the member (invalid if isNull is true)</param>
        /// <param name="isNull">The isNull flag of the member.</param>
        /// <param name="isChanged">The isChanged flag of the member.</param>
        public static void Get(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               out float value,
                               out bool isNull,
                               out bool isChanged)
        {
            byte n, c;
            Internal.Kernel.DotsC_GetFloat32Member(blob,
                                                   member,
                                                   index,
                                                   out value,
                                                   out n,
                                                   out c);
            isNull = Internal.InternalOperations.BoolOf(n);
            isChanged = Internal.InternalOperations.BoolOf(c);
        }

        /// <summary>
        /// Set a double in a blob.
        /// <para/>
        /// This method will set a double member in a blob.
        /// If the isNull parameter is true then only the isChange and isNull flags are set in the blob,
        /// not the value (so it can be any value).
        /// </summary>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        /// <param name="value">The value that the member is to be set to (unless isNull is true).</param>
        /// <param name="isNull">Should the value be set to null.</param>
        /// <param name="isChanged">Should the value be set to changed.</param>
        public static void Set(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               double value,
                               bool isNull,
                               bool isChanged)
        {
            Internal.Kernel.DotsC_SetFloat64MemberInPreallocated(value,
                                                                 Internal.InternalOperations.ByteOf(isNull),
                                                                 Internal.InternalOperations.ByteOf(isChanged),
                                                                 blob,
                                                                 member,
                                                                 index);
        }

        /// <summary>
        /// Get a double from a blob.
        /// <para/>
        /// This method will get a double member and the associated isNull and isChange values from a blob.
        /// The value parameter is not valid if isNull is true.
        /// </summary>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        /// <param name="value">The value of the member (invalid if isNull is true)</param>
        /// <param name="isNull">The isNull flag of the member.</param>
        /// <param name="isChanged">The isChanged flag of the member.</param>
        public static void Get(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               out double value,
                               out bool isNull,
                               out bool isChanged)
        {
            byte n, c;
            Internal.Kernel.DotsC_GetFloat64Member(blob,
                                                   member,
                                                   index,
                                                   out value,
                                                   out n,
                                                   out c);
            isNull = Internal.InternalOperations.BoolOf(n);
            isChanged = Internal.InternalOperations.BoolOf(c);
        }

        /// <summary>
        /// Get an InstanceId from a blob.
        /// <para/>
        /// This method will get an InstanceId member and the associated isNull and isChange values from a blob.
        /// The value parameter is not valid if isNull is true.
        /// </summary>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        /// <param name="value">The value of the member (invalid if isNull is true)</param>
        /// <param name="isNull">The isNull flag of the member.</param>
        /// <param name="isChanged">The isChanged flag of the member.</param>
        public static void Get(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               out InstanceId value,
                               out bool isNull,
                               out bool isChanged)
        {
            byte n, c;
            System.Int64 hashVal;
            System.IntPtr strVal;
            Internal.Kernel.DotsC_GetHashedIdMember(blob, member, index,
                                                    out hashVal, out strVal, out n, out c);
            isNull = Internal.InternalOperations.BoolOf(n);
            isChanged = Internal.InternalOperations.BoolOf(c);
            if (!isNull)
            {
                if (strVal == System.IntPtr.Zero)
                {
                    value = new InstanceId(hashVal);
                }
                else
                {
                    value = new InstanceId(hashVal, Internal.InternalOperations.StringOf(strVal));
                }
            }
            else
            {
                value = null;
            }
        }

        /// <summary>
        /// Get an EntityId from a blob.
        /// <para/>
        /// This method will get an EntityId member and the associated isNull and isChange values from a blob.
        /// The value parameter is not valid if isNull is true.
        /// </summary>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        /// <param name="value">The value of the member (invalid if isNull is true)</param>
        /// <param name="isNull">The isNull flag of the member.</param>
        /// <param name="isChanged">The isChanged flag of the member.</param>
        public static void Get(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               out EntityId value,
                               out bool isNull,
                               out bool isChanged)
        {
            byte n, c;
            Internal.DotsC_EntityId eid;
            System.IntPtr instanceIdStr;
            Internal.Kernel.DotsC_GetEntityIdMember(blob,
                                                    member,
                                                    index,
                                                    out eid,
                                                    out instanceIdStr,
                                                    out n,
                                                    out c);
            isNull = Internal.InternalOperations.BoolOf(n);
            isChanged = Internal.InternalOperations.BoolOf(c);
            if (!isNull)
            {
                if (instanceIdStr == System.IntPtr.Zero)
                {
                    value = new EntityId(eid.TypeId, new Dob.Typesystem.InstanceId(eid.InstanceId));
                }
                else
                {
                    value = new EntityId(eid.TypeId, new Dob.Typesystem.InstanceId
                        (eid.InstanceId, Internal.InternalOperations.StringOf(instanceIdStr)));
                }
            }
            else
            {
                value = null;
            }
        }

        /// <summary>
        /// Get an ChannelId from a blob.
        /// <para/>
        /// This method will get an ChannelId member and the associated isNull and isChange values from a blob.
        /// The value parameter is not valid if isNull is true.
        /// </summary>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        /// <param name="value">The value of the member (invalid if isNull is true)</param>
        /// <param name="isNull">The isNull flag of the member.</param>
        /// <param name="isChanged">The isChanged flag of the member.</param>
        public static void Get(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               out ChannelId value,
                               out bool isNull,
                               out bool isChanged)
        {
            byte n, c;
            System.Int64 hashVal;
            System.IntPtr strVal;
            Internal.Kernel.DotsC_GetHashedIdMember(blob, member, index,
                                                    out hashVal, out strVal, out n, out c);
            isNull = Internal.InternalOperations.BoolOf(n);
            isChanged = Internal.InternalOperations.BoolOf(c);
            if (!isNull)
            {
                if (strVal == System.IntPtr.Zero)
                {
                    value = new ChannelId(hashVal);
                }
                else
                {
                    value = new ChannelId(hashVal, Internal.InternalOperations.StringOf(strVal));
                }
            }
            else
            {
                value = null;
            }
        }

        /// <summary>
        /// Get a HandlerId from a blob.
        /// <para/>
        /// This method will get a HandlerId member and the associated isNull and isChange values from a blob.
        /// The value parameter is not valid if isNull is true.
        /// </summary>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        /// <param name="value">The value of the member (invalid if isNull is true)</param>
        /// <param name="isNull">The isNull flag of the member.</param>
        /// <param name="isChanged">The isChanged flag of the member.</param>
        public static void Get(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               out HandlerId value,
                               out bool isNull,
                               out bool isChanged)
        {
            byte n, c;
            System.Int64 hashVal;
            System.IntPtr strVal;
            Internal.Kernel.DotsC_GetHashedIdMember(blob, member, index,
                                                    out hashVal, out strVal, out n, out c);
            isNull = Internal.InternalOperations.BoolOf(n);
            isChanged = Internal.InternalOperations.BoolOf(c);
            if (!isNull)
            {
                if (strVal == System.IntPtr.Zero)
                {
                    value = new HandlerId(hashVal);
                }
                else
                {
                    value = new HandlerId(hashVal, Internal.InternalOperations.StringOf(strVal));
                }
            }
            else
            {
                value = null;
            }
        }

        /// <summary>
        /// Get a string from a blob.
        /// <para/>
        /// This method will get a string member and the associated isNull and isChange values from a blob.
        /// The value parameter is not valid if isNull is true.
        /// </summary>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        /// <param name="value">The value of the member (invalid if isNull is true)</param>
        /// <param name="isNull">The isNull flag of the member.</param>
        /// <param name="isChanged">The isChanged flag of the member.</param>
        public static void Get(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               out string value,
                               out bool isNull,
                               out bool isChanged)
        {
            byte n, c;
            System.IntPtr charStr;
            Internal.Kernel.DotsC_GetStringMember(blob,
                                                  member,
                                                  index,
                                                  out charStr,
                                                  out n,
                                                  out c);
            isNull = Internal.InternalOperations.BoolOf(n);
            isChanged = Internal.InternalOperations.BoolOf(c);
            if (!isNull)
            {
                value = Internal.InternalOperations.StringOf(charStr);
            }
            else
            {
                value = null;
            }
        }

        /// <summary>
        /// Get a blob from a blob.
        /// <para/>
        /// This method will get a blob member and the associated isNull and isChange values from a blob.
        /// The value parameter is not valid if isNull is true.
        /// </summary>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        /// <param name="childBlob">The child blob (invalid if isNull is true)</param>
        /// <param name="isNull">The isNull flag of the member.</param>
        /// <param name="isChanged">The isChanged flag of the member.</param>
        public static void Get(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               out System.IntPtr childBlob,
                               out bool isNull,
                               out bool isChanged)
        {
            byte n, c;
            Internal.Kernel.DotsC_GetObjectMember(blob,
                                                  member,
                                                  index,
                                                  out childBlob,
                                                  out n,
                                                  out c);
            isNull = Internal.InternalOperations.BoolOf(n);
            isChanged = Internal.InternalOperations.BoolOf(c);
        }

        /// <summary>
        /// Get a binary member from a blob.
        /// <para/>
        /// This method will get a binary member and the associated isNull and isChange values from a blob.
        /// The value parameter is not valid if isNull is true.
        /// </summary>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        /// <param name="value">The value of the member (invalid if isNull is true)</param>
        /// <param name="isNull">The isNull flag of the member.</param>
        /// <param name="isChanged">The isChanged flag of the member.</param>
        public static void Get(System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index,
                               out byte[] value,
                               out bool isNull,
                               out bool isChanged)
        {
            byte n, c;
            System.IntPtr bin;
            int size;
            Internal.Kernel.DotsC_GetBinaryMember(blob,
                                                  member,
                                                  index,
                                                  out bin,
                                                  out size,
                                                  out n,
                                                  out c);
            isNull = Internal.InternalOperations.BoolOf(n);
            isChanged = Internal.InternalOperations.BoolOf(c);
            if (!isNull)
            {
                value = new byte[size];
                Marshal.Copy(bin, value, 0, size);
            }
            else
            {
                value = null;
            }
        }

        #endregion

        #region Container operations on Blobs

        /// <summary>
        /// Get a boolean from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(BooleanContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set a boolean in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(BooleanContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get an enumeration from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(EnumerationContainerBase container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an enumeration in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(EnumerationContainerBase container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get an Int32 from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Int32Container container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Int32 in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Int32Container container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get an Int64 from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Int64Container container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Int64 in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Int64Container container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Float32 from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Float32Container container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set a Float32 in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Float32Container container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Float64 from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Float64Container container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set a Float64 in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Float64Container container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a TypeId from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(TypeIdContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set a TypeId in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(TypeIdContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get an InstanceId from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(InstanceIdContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an InstancId in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="beginningOfUnused">Beginning of unused part of dynamic part of blob.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(InstanceIdContainer container,
                               System.IntPtr blob,
                               ref System.IntPtr beginningOfUnused,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (!container.IsNull())
            {
                System.IntPtr stringStart = beginningOfUnused;
                System.Int32 stringLength = container.Val.Utf8StringLength();
                byte[] utf8Bytes = container.Val.Utf8String();
                Internal.Kernel.DotsC_SetHashedIdMemberInPreallocated
                    (container.m_Value.RawValue,
                     (stringLength == 0 ? null : utf8Bytes),
                     stringLength,
                     Internal.InternalOperations.ByteOf(container.m_bIsNull),
                     Internal.InternalOperations.ByteOf(container.m_bIsChanged),
                     blob,
                     member,
                     index,
                     ref beginningOfUnused);
                if (stringLength != 0)
                {
                    Marshal.WriteByte(stringStart, 8 + 4 + utf8Bytes.Length, 0); //add '\0'
                }
            }
            else if (container.IsChanged())
            {
                SetNull(blob, member, index);
            }
        }

        /// <summary>
        /// Get an EntityId from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(EntityIdContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an EntityId in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="beginningOfUnused">Beginning of unused part of dynamic part of blob.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(EntityIdContainer container,
                               System.IntPtr blob,
                               ref System.IntPtr beginningOfUnused,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (!container.IsNull())
            {
                System.IntPtr stringStart = beginningOfUnused;
                System.Int32 stringLength = container.m_Value.InstanceId.Utf8StringLength();
                byte[] utf8Bytes = container.m_Value.InstanceId.Utf8String();
                Safir.Dob.Typesystem.Internal.DotsC_EntityId eid;
                eid.TypeId = container.m_Value.TypeId;
                eid.InstanceId =container.m_Value.InstanceId.RawValue;
                Internal.Kernel.DotsC_SetEntityIdMemberInPreallocated
                    (ref eid,
                     (stringLength == 0 ? null : utf8Bytes),
                     stringLength,
                     Internal.InternalOperations.ByteOf(container.m_bIsNull),
                     Internal.InternalOperations.ByteOf(container.m_bIsChanged),
                     blob,
                     member,
                     index,
                     ref beginningOfUnused);

                if (stringLength != 0)
                {
                    Marshal.WriteByte(stringStart, 16 + 4 + utf8Bytes.Length, 0); //add '\0'
                }
            }
            else if (container.IsChanged())
            {
                SetNull(blob, member, index);
            }
        }

        /// <summary>
        /// Get a ChannelId from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(ChannelIdContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set a ChannelId in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="beginningOfUnused">Beginning of unused part of dynamic part of blob.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(ChannelIdContainer container,
                               System.IntPtr blob,
                               ref System.IntPtr beginningOfUnused,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (!container.IsNull())
            {
                System.IntPtr stringStart = beginningOfUnused;
                System.Int32 stringLength = container.Val.Utf8StringLength();
                byte[] utf8Bytes = container.Val.Utf8String();
                Internal.Kernel.DotsC_SetHashedIdMemberInPreallocated
                    (container.m_Value.RawValue,
                     (stringLength == 0 ? null : utf8Bytes),
                     stringLength,
                     Internal.InternalOperations.ByteOf(container.m_bIsNull),
                     Internal.InternalOperations.ByteOf(container.m_bIsChanged),
                     blob,
                     member,
                     index,
                     ref beginningOfUnused);

                if (stringLength != 0)
                {
                    Marshal.WriteByte(stringStart, 8 + 4 + utf8Bytes.Length, 0); //add '\0'
                }
            }
            else if (container.IsChanged())
            {
                SetNull(blob, member, index);
            }
        }

        /// <summary>
        /// Get a HandlerId from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(HandlerIdContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set a HandlerId in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="beginningOfUnused">Beginning of unused part of dynamic part of blob.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(HandlerIdContainer container,
                               System.IntPtr blob,
                               ref System.IntPtr beginningOfUnused,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (!container.IsNull())
            {
                System.IntPtr stringStart = beginningOfUnused;
                System.Int32 stringLength = container.Val.Utf8StringLength();
                byte[] utf8Bytes = container.Val.Utf8String();
                Internal.Kernel.DotsC_SetHashedIdMemberInPreallocated
                    (container.m_Value.RawValue,
                     (stringLength == 0 ? null : utf8Bytes),
                     stringLength,
                     Internal.InternalOperations.ByteOf(container.m_bIsNull),
                     Internal.InternalOperations.ByteOf(container.m_bIsChanged),
                     blob,
                     member,
                     index,
                     ref beginningOfUnused);

                if (stringLength != 0)
                {
                    Marshal.WriteByte(stringStart, 8 + 4 + utf8Bytes.Length, 0); //add '\0'
                }
            }
            else if (container.IsChanged())
            {
                SetNull(blob, member, index);
            }
        }

        /// <summary>
        /// Get a string from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(StringContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set a string in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="beginningOfUnused">Beginning of unused part of dynamic part of blob.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(StringContainer container,
                               System.IntPtr blob,
                               ref System.IntPtr beginningOfUnused,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (!container.IsNull())
            {
                System.IntPtr stringStart = beginningOfUnused;
                System.Int32 stringLength = container.Utf8StringLength();
                byte[] utf8Bytes = container.Utf8String();
                Internal.Kernel.DotsC_CreateStringMember(blob,
                                                         stringLength,
                                                         member,
                                                         index,
                                                         Internal.InternalOperations.ByteOf(container.IsChanged()),
                                                         ref beginningOfUnused);

                for (int i = 0; i < stringLength - 1; ++i)
                {
                    Marshal.WriteByte(stringStart, i, utf8Bytes[i]);
                }
                Marshal.WriteByte(stringStart, utf8Bytes.Length, 0); //add '\0'
            }
            else if (container.IsChanged())
            {
                SetNull(blob, member, index);
            }
        }

        /// <summary>
        /// Get an Object from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(ObjectContainerBase container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            System.IntPtr childBlob;
            byte childIsNull, childIsChanged;
            Internal.Kernel.DotsC_GetObjectMember(blob, member, index, out childBlob, out childIsNull, out childIsChanged);

            container.m_bIsChanged = Internal.InternalOperations.BoolOf(childIsChanged);
            if (Internal.InternalOperations.BoolOf(childIsNull))
            {
                container.InternalObj = null;
            }
            else
            {
                container.InternalObj = ObjectFactory.Instance.CreateObject(childBlob);
            }
        }

        /// <summary>
        /// Set an Object in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="beginningOfUnused">Beginning of unused part of dynamic part of blob.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(ObjectContainerBase container,
                               System.IntPtr blob,
                               ref System.IntPtr beginningOfUnused,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (!container.IsNull())
            {
                System.IntPtr childBlob = beginningOfUnused;
                Internal.Kernel.DotsC_CreateObjectMember(blob,
                                                         container.InternalObj.CalculateBlobSize(),
                                                         container.InternalObj.GetTypeId(),
                                                         member,
                                                         index,
                                                         Internal.InternalOperations.ByteOf(container.IsChangedHere()),
                                                         ref beginningOfUnused);
                container.InternalObj.WriteToBlob(childBlob, ref beginningOfUnused);
            }
            else if (container.IsChangedHere())
            {
                SetNull(blob, member, index);
            }
        }

        /// <summary>
        /// Get a binary from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(BinaryContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set a binary in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="beginningOfUnused">Beginning of unused part of dynamic part of blob.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(BinaryContainer container,
                               System.IntPtr blob,
                               ref System.IntPtr beginningOfUnused,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (!container.IsNull())
            {
                System.IntPtr binaryStart = beginningOfUnused;
                System.Int32 binarySize = container.Val.Length;

                Internal.Kernel.DotsC_CreateBinaryMember(blob,
                                                         binarySize,
                                                         member,
                                                         index,
                                                         Internal.InternalOperations.ByteOf(container.IsChanged()),
                                                         ref beginningOfUnused);

                Marshal.Copy(container.Val, 0, new IntPtr(binaryStart.ToInt64()), binarySize);
            }
            else if (container.IsChanged())
            {
                SetNull(blob, member, index);
            }
        }

        #region SI types

        /// <summary>
        /// Get a Si32.AmpereContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.AmpereContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.AmpereContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.AmpereContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.CubicMeterContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.CubicMeterContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.CubicMeterContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.CubicMeterContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.HertzContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.HertzContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.HertzContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.HertzContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.JouleContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.JouleContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.JouleContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.JouleContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.KelvinContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.KelvinContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.KelvinContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.KelvinContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.KilogramContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.KilogramContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.KilogramContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.KilogramContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.MeterContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.MeterContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.MeterContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.MeterContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.MeterPerSecondContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.MeterPerSecondContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.MeterPerSecondContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.MeterPerSecondContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.MeterPerSecondSquaredContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.MeterPerSecondSquaredContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.MeterPerSecondSquaredContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.MeterPerSecondSquaredContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.NewtonContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.NewtonContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.NewtonContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.NewtonContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.PascalContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.PascalContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.PascalContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.PascalContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.RadianContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.RadianContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.RadianContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.RadianContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.RadianPerSecondContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.RadianPerSecondContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.RadianPerSecondContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.RadianPerSecondContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.RadianPerSecondSquaredContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.RadianPerSecondSquaredContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.RadianPerSecondSquaredContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.RadianPerSecondSquaredContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.SecondContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.SecondContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.SecondContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.SecondContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.SquareMeterContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.SquareMeterContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.SquareMeterContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.SquareMeterContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.SteradianContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.SteradianContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.SteradianContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.SteradianContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.VoltContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.VoltContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.VoltContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.VoltContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si32.WattContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si32.WattContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si32.WattContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si32.WattContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.AmpereContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.AmpereContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.AmpereContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.AmpereContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.CubicMeterContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.CubicMeterContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.CubicMeterContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.CubicMeterContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.HertzContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.HertzContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.HertzContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.HertzContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.JouleContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.JouleContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.JouleContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.JouleContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.KelvinContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.KelvinContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.KelvinContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.KelvinContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.KilogramContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.KilogramContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.KilogramContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.KilogramContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.MeterContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.MeterContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.MeterContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.MeterContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.MeterPerSecondContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.MeterPerSecondContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.MeterPerSecondContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.MeterPerSecondContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.MeterPerSecondSquaredContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.MeterPerSecondSquaredContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.MeterPerSecondSquaredContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.MeterPerSecondSquaredContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.NewtonContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.NewtonContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.NewtonContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.NewtonContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.PascalContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.PascalContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.PascalContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.PascalContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.RadianContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.RadianContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.RadianContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.RadianContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.RadianPerSecondContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.RadianPerSecondContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.RadianPerSecondContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.RadianPerSecondContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.RadianPerSecondSquaredContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.RadianPerSecondSquaredContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.RadianPerSecondSquaredContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.RadianPerSecondSquaredContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.SecondContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.SecondContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.SecondContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.SecondContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.SquareMeterContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.SquareMeterContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.SquareMeterContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.SquareMeterContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.SteradianContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.SteradianContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.SteradianContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.SteradianContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.VoltContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.VoltContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.VoltContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.VoltContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }

        /// <summary>
        /// Get a Si64.WattContainer from a blob.
        /// <para/>
        /// This method will get the member and the associated isNull and isChange values from a blob and
        /// put them in the container.
        /// </summary>
        /// <param name="container">The container in which to put the values.</param>
        /// <param name="blob">Blob to get the member from.</param>
        /// <param name="member">The member to get.</param>
        /// <param name="index">Array index in member to get. Shall be 0 if the member is not an array.</param>
        public static void Get(Si64.WattContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            if (container == null)
            {
                throw new SoftwareViolationException("Container was null!");
            }
            Get(blob, member, index, out container.m_Value, out container.m_bIsNull, out container.m_bIsChanged);
        }

        /// <summary>
        /// Set an Si64.WattContainer in a blob.
        /// <para/>
        /// If the container is null then the member will be set to null in the blob.
        /// The change flag from the container will be set in the blob.
        /// </summary>
        /// <param name="container">The container whose values to use.</param>
        /// <param name="blob">Blob to set the member in.</param>
        /// <param name="member">The member to be set.</param>
        /// <param name="index">Array index in member to set. Shall be 0 if the member is not an array.</param>
        public static void Set(Si64.WattContainer container,
                               System.IntPtr blob,
                               System.Int32 member,
                               System.Int32 index)
        {
            Set(blob, member, index, container.m_Value, container.m_bIsNull, container.m_bIsChanged);
        }
        #endregion
        #endregion

        //TODO: Rename this function to something that reflects what it actually does.
        /// <summary>
        /// Get the static blob size of an type, but excluding the size that is inherited from parent classes.
        /// <para>
        /// This is very much an internal function!
        /// Unless you have a really good reason to use this function you should stay clear of it.
        /// </para>
        /// </summary>
        /// <param name="typeId">The TypeId of a DOB class.</param>
        /// <returns></returns>
        public static System.Int32 GetInitialSize(System.Int64 typeId)
        {
            return Internal.Kernel.DotsC_GetInitialSize(typeId);
        }

		//----------------------------------------------------------------------------------
		/**
        * Extract the TypeId from a blob
        *
        * @param blob [in] - The blob to read from.
        * @return The TypeId from the blob.
        */
		public static System.Int64 GetTypeId(System.IntPtr blob)
		{
			return Kernel.DotsC_GetTypeId (blob);
		}

		/**
        * Get the size of the blob contained by this object
        *
        * @param blob [in] - the blob.
        * @return the size of the blob, in bytes.
        */
		public static System.Int32 GetSize(System.IntPtr blob)
		{
			return Kernel.DotsC_GetSize(blob);
		}

		/**
         * Allocate and create a copy of a blob. The returned blob must be
         * deallocated using BlobOperations::Delete.
         *
         * @param blob [in] - The blob to be copied.
         * @return Copy of blob;
         */
		public static IntPtr CreateCopy(IntPtr blob)
		{
			IntPtr copy = IntPtr.Zero;
			Kernel.DotsC_CreateCopyOfBlob (out copy, blob);
			return copy;
		}

		/**
         * Delete a blob that has been allocated by CreateCopy.
         *
         * @param blob [in,out] - Blob to be deleted.
         */
		public static void Delete(IntPtr blob)
		{
			Kernel.DotsC_DeleteBlob (blob);
		}

		/** @} */

		/**
         * @name Container operations on Blobs.
         * These operations use the values found in the containers to set the values in the blobs.
         */
		/** @{ */

		/**
         * Set a boolean in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
		static void Set(BooleanContainer value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index)
		{
			if (container == null)
			{
				throw new SoftwareViolationException("Container was null!");
			}
		}

		/**
         * Get a boolean from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
		static void Get(BooleanContainer& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Set an enumeration in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
		static void Set(const EnumerationContainerBase& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Get an enumeration from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
		static void Get(EnumerationContainerBase& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);


		/**
         * Set an Int32 in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
		static void Set(const Int32Container& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Get an Int32 from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
		static void Get(Int32Container& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);


		/**
         * Set an Int64 in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
		static void Set(const Int64Container& value,  //will be used on TypeIdContainers too
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Get an Int64 from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
		static void Get(Int64Container& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);


		/**
         * Set a Float32 in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
		static void Set(const Float32Container& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Get a Float32 from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
		static void Get(Float32Container& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);


		/**
         * Set a Float64 in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
		static void Set(const Float64Container& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Get a Float64 from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
		static void Get(Float64Container& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Set an InstanceId in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
		static void Set(const InstanceIdContainer& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Get an InstanceId from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
		static void Get(InstanceIdContainer& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Set an EntityId in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
		static void Set(const EntityIdContainer& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Get an EntityId from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
		static void Get(EntityIdContainer& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Set an ChannelId in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
		static void Set(const ChannelIdContainer& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Get an ChannelId from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
		static void Get(ChannelIdContainer& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Set an HandlerId in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
		static void Set(const HandlerIdContainer& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Get an HandlerId from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
		static void Get(HandlerIdContainer& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Set a string in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
		static void Set(const StringContainer& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Get a string from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
		static void Get(StringContainer& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);


		/**
         * Set an Object in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
		static void Set(const ObjectContainerBase& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Get an Object from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
		static void Get(ObjectContainerBase& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Set Binary in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
		static void Set(const BinaryContainer& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		/**
         * Get Binary from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
		static void Get(BinaryContainer& value,
		                Safir::Dob::Typesystem::Int64 handle,
		                const Dob::Typesystem::MemberIndex member,
		                const Dob::Typesystem::ArrayIndex index);

		//---------------------------------------------------------
		// Get values
		//--------------------------------------------------------
		public static void Get(out bool val,
                               out bool isNull,
                               out bool isChanged,
		                	   Int64 handle,
				               Int32 member,
                               Int32 valueIndex,
				               DotsC_KeyValMode mode)
        {
            byte v, n, c;
            Kernel.DotsC_ReadBooleanMember (handle, out v, out n, out c, member, valueIndex, mode);
            val = InternalOperations.BoolOf (v);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
        }

        public static void Get(out Int32 val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               DotsC_KeyValMode mode)
        {
            byte n, c;
            Kernel.DotsC_ReadInt32Member (handle, out val, out n, out c, member, valueIndex, mode);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
        }

        public static void Get(out Int64 val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               DotsC_KeyValMode mode)
        {
            byte n, c;
            Kernel.DotsC_ReadInt64Member (handle, out val, out n, out c, member, valueIndex, mode);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
        }
		

		public static void Get(out float val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               DotsC_KeyValMode mode)
        {
            byte n, c;
            Kernel.DotsC_ReadFloat32Member (handle, out val, out n, out c, member, valueIndex, mode);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
        }

		public static void Get(out double val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               DotsC_KeyValMode mode)
        {
            byte n, c;
            Kernel.DotsC_ReadFloat64Member (handle, out val, out n, out c, member, valueIndex, mode);
            isNull = InternalOperations.BoolOf (n);
            isChanged = InternalOperations.BoolOf (c);
        }

        public static void Get(out string val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               DotsC_KeyValMode mode)
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

        public static void Get(out InstanceId val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               DotsC_KeyValMode mode)
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

        public static void Get(out HandlerId val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               DotsC_KeyValMode mode)
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

        public static void Get(out ChannelId val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               DotsC_KeyValMode mode)
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

        public static void Get(out EntityId val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               DotsC_KeyValMode mode)
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

        public static void Get(out Safir.Dob.Typesystem.Object val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               DotsC_KeyValMode mode)
        {
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

        public static void Get(out byte[] val,
                               out bool isNull,
                               out bool isChanged,
                               Int64 handle,
                               Int32 member,
                               Int32 valueIndex,
                               DotsC_KeyValMode mode)
        {
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
		static void Set(bool val,
		                bool isNull,
		                bool isChanged,
		                Int64 handle,
		                Int32 member,
		                Int32 valueIndex,
		                DotsC_KeyValMode mode)
        {
            Kernel.DotsC_WriteBooleanMember (handle,
                                             InternalOperations.ByteOf(val),
                                             InternalOperations.ByteOf(isNull),
                                             InternalOperations.ByteOf(isChanged),
                                             member, valueIndex, mode);
        }

		static void Set(Int32 val,
		                bool isNull,
		                bool isChanged,
		                Int64 handle,
		                Int32 member,
		                Int32 valueIndex,
		                DotsC_KeyValMode mode)
        {
            Kernel.DotsC_WriteInt32Member (handle, val,
                                           InternalOperations.ByteOf(isNull),
                                           InternalOperations.ByteOf(isChanged),
                                           member, valueIndex, mode);
        }

		static void Set(Int64 val,
		                bool isNull,
		                bool isChanged,
		                Int64 handle,
		                Int32 member,
		                Int32 valueIndex,
		                DotsC_KeyValMode mode)
        {
            Kernel.DotsC_WriteInt64Member (handle, val,
                                           InternalOperations.ByteOf(isNull),
                                           InternalOperations.ByteOf(isChanged),
                                           member, valueIndex, mode);
        }

		static void Set(float val,
		                bool isNull,
		                bool isChanged,
		                Int64 handle,
		                Int32 member,
		                Int32 valueIndex,
		                DotsC_KeyValMode mode)
        {
            Kernel.DotsC_WriteFloat32Member (handle, val,
                                             InternalOperations.ByteOf(isNull),
                                             InternalOperations.ByteOf(isChanged),
                                             member, valueIndex, mode);
        }

		static void Set(double val,
		                bool isNull,
		                bool isChanged,
		                Int64 handle,
		                Int32 member,
		                Int32 valueIndex,
		                DotsC_KeyValMode mode)
        {
            Kernel.DotsC_WriteFloat64Member (handle, val,
                                             InternalOperations.ByteOf(isNull),
                                             InternalOperations.ByteOf(isChanged),
                                             member, valueIndex, mode);
        }

		static void Set(string val,
		                bool isNull,
		                bool isChanged,
		                Int64 handle,
		                Int32 member,
		                Int32 valueIndex,
		                DotsC_KeyValMode mode);

		static void Set(InstanceId val,
		                bool isNull,
		                bool isChanged,
		                Int64 handle,
		                Int32 member,
		                Int32 valueIndex,
		                DotsC_KeyValMode mode);

		static void Set(HandlerId val,
		                bool isNull,
		                bool isChanged,
		                Int64 handle,
		                Int32 member,
		                Int32 valueIndex,
		                DotsC_KeyValMode mode);

		static void Set(ChannelId val,
		                bool isNull,
		                bool isChanged,
		                Int64 handle,
		                Int32 member,
		                Int32 valueIndex,
		                DotsC_KeyValMode mode);

		static void Set(EntityId val,
		                bool isNull,
		                bool isChanged,
		                Int64 handle,
		                Int32 member,
		                Int32 valueIndex,
		                DotsC_KeyValMode mode);

		static void Set(Safir.Dob.Typesystem.Object val,
		                bool isNull,
		                bool isChanged,
		                Int64 handle,
		                Int32 member,
		                Int32 valueIndex,
		                DotsC_KeyValMode mode);

		static void Set(byte[] val,
		                bool isNull,
		                bool isChanged,
		                Int64 handle,
		                Int32 member,
		                Int32 valueIndex,
		                DotsC_KeyValMode mode);
	}
}
