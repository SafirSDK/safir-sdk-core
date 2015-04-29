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
using Safir.Dob.Typesystem.Internal;
using System.Runtime.InteropServices;


namespace Safir.Dob.Typesystem
{
    /// <summary>
    /// Methods for getting member information from types.
    /// <para>
    /// With these operations you can get information on types regarding
    /// their members. You can get member names and indexes. You can
    /// get TypeIds of members etc.
    /// </para>
    /// </summary>
    public class Members
    {
        /// <summary>
        /// Get the number of members for a class or property.
        /// Parameters are not included.
        /// </summary>
        /// <param name="typeId">TypeId of class or property.</param>
        /// <returns>Number of members in the class.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is no such type defined.</exception>
        public static int GetNumberOfMembers(System.Int64 typeId)
        {
            int result = Kernel.DotsC_GetNumberOfMembers(typeId);
            if (result == -1)
            {
                throw new IllegalValueException("No such type");
            }
            else
            {
                return result;
            }
        }

        /// <summary>
        /// Get the member index of a named member.
        /// </summary>
        /// <param name="typeId">TypeId of class or property.</param>
        /// <param name="memberName">Name of member as specified in xml description, case sensitive.</param>
        /// <returns>Member index of the member.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is no such type defined or there is no such member in the type.</exception>
        public static int GetIndex(System.Int64 typeId,
                                   string memberName)
        {
            System.IntPtr sp = Internal.InternalOperations.CStringOf(memberName);
            int memberId = Kernel.DotsC_GetMemberId(typeId, sp);
            Marshal.FreeHGlobal(sp);
            if (memberId == -1)
            {
                throw new IllegalValueException("There is no such type or member defined");
            }
            else
            {
                return memberId;
            }
        }

        /// <summary>
        /// Get the name of the specified member as it was defined in the xml description.
        /// </summary>
        /// <param name="typeId">TypeId of class or property.</param>
        /// <param name="member">Index of member.</param>
        /// <returns>Name of member.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is no such type defined or there is no such member in the type.</exception>
        public static string GetName(System.Int64 typeId,
                                     int member)
        {
            MemberType memberType;
            IntPtr memberName;
            Int64 complexType;
            Int32 stringLength;
            CollectionType collectionType;
            Int32 arraySize;

            Kernel.DotsC_GetMemberInfo(typeId, member, out memberType, out memberName, out complexType, out stringLength, out collectionType, out arraySize);

            if (memberName == IntPtr.Zero)
            {
                throw new IllegalValueException("There is no such type or member defined");
            }
            else
            {
                return Internal.InternalOperations.StringOf(memberName);
            }
        }

        /// <summary>
        /// Get type name of a member.
        /// </summary>
        /// <param name="typeId">TypeId of class or property.</param>
        /// <param name="member">Index of member.</param>
        /// <returns>The type name for the member.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is no such type defined or there is no such member
        /// in the type or the member is not an enum or object.</exception>
        public static string GetTypeName(Int64 typeId,
                                         int member)
        {
            MemberType memberType;
            IntPtr memberName;
            Int64 complexType;
            Int32 stringLength;
            CollectionType collectionType;
            Int32 arraySize;

            Kernel.DotsC_GetMemberInfo(typeId, member, out memberType, out memberName, out complexType, out stringLength, out collectionType, out arraySize);

            if (memberName == IntPtr.Zero)
            {
                throw new IllegalValueException("There is no such type or member defined");
            }

            if (memberType==MemberType.ObjectMemberType || memberType==MemberType.EnumerationMemberType)
            {
                IntPtr typeName=Kernel.DotsC_GetTypeName (complexType);
                return InternalOperations.StringOf (typeName);
            }
            else
            {
                IntPtr typeName = Kernel.DotsC_MemberTypeName (memberType);
                return InternalOperations.StringOf (typeName);
            }
        }

        /// <summary>
        /// Get type id of object or enumeration member.
        /// <para>
        /// If a member is of type object or enumeration, this method can be used to get
        /// the typeId for the class or enum that the member is of.
        /// </para>
        /// </summary>
        /// <param name="typeId">TypeId of class or property.</param>
        /// <param name="member">Index of member.</param>
        /// <returns>The TypeId for the object or enumeration member.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is no such type defined or there is no such member
        /// in the type or the member is not an enum or object.</exception>
        public static System.Int64 GetTypeId(System.Int64 typeId,
                                             int member)
        {
            MemberType memberType;
            IntPtr memberName;
            Int64 complexType;
            Int32 stringLength;
            CollectionType collectionType;
            Int32 arraySize;

            Kernel.DotsC_GetMemberInfo(typeId, member, out memberType, out memberName, out complexType, out stringLength, out collectionType, out arraySize);

            if (complexType == -1)
            {
                throw new IllegalValueException("There is no such type or member defined");
            }
            else
            {
                return complexType;
            }
        }

        /// <summary>
        /// Get information about a specific class member.
        /// </summary>
        /// <param name="typeId">TypeId of class or property.</param>
        /// <param name="member">Index of member.</param>
        /// <param name="theMemberType">The type of the member.</param>
        /// <param name="memberTypeId">If memberType is object or enumeration, this is the typeId of that type.
        /// If memberType is something else the value is -1.</param>
        /// <param name="stringLength">Length of the string</param>
        /// <param name="collectionType">Member collection type.</param>
        /// <param name="arrayLength">Maximum capacity of array if the member is an array (1 if not an array). Not applicable if type id is a property.</param>
        /// <returns>The name of the member.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is no such type defined or there is no such member in the type.</exception>
        public static string GetInfo(System.Int64 typeId,
                                     int member,
                                     out MemberType theMemberType,
                                     out System.Int64 memberTypeId,
                                     out int stringLength,
                                     out CollectionType collectionType,
                                     out int arrayLength)
        {
            IntPtr name;
            Kernel.DotsC_GetMemberInfo(typeId, member, out theMemberType, out name, out memberTypeId, out stringLength, out collectionType, out arrayLength);

            if (name == IntPtr.Zero)
            {
                throw new IllegalValueException("There is no such type or member defined");
            }
            else
            {
                return Internal.InternalOperations.StringOf(name);
            }
        }

        /// <summary>
        /// Get the array size of a member.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="member">Index of member.</param>
        /// <returns>The array size of the member.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is no such type defined or there is no such member in the type.</exception>
        public static int GetArraySize(System.Int64 typeId,
                                             int member)
        {
            MemberType memberType;
            IntPtr memberName;
            Int64 complexType;
            Int32 stringLength;
            CollectionType collectionType;
            Int32 arraySize;

            Kernel.DotsC_GetMemberInfo(typeId, member, out memberType, out memberName, out complexType, out stringLength, out collectionType, out arraySize);

            if (arraySize == -1)
            {
                throw new IllegalValueException("No such type or array defined");
            }
            else
            {
                return arraySize;
            }
        }

        /// <summary>
        /// Get the maximum string length of a member.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="member">Index of member.</param>
        /// <returns>The maximum length of the string member.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is no such class defined or there is no such member in the type or the member is not a string.</exception>
        public static int GetMaxStringLength(System.Int64 typeId,
                                             int member)
        {
            MemberType memberType;
            IntPtr memberName;
            Int64 complexType;
            Int32 stringLength;
            CollectionType collectionType;
            Int32 arraySize;

            Kernel.DotsC_GetMemberInfo(typeId, member, out memberType, out memberName, out complexType, out stringLength, out collectionType, out arraySize);

            if (stringLength == -1)
            {
                throw new IllegalValueException("No such type or array defined");
            }
            else
            {
                return stringLength;
            }
        }
    }
}
