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
    public class Members
    {
        //********************************************************
        //* Functions for retrieving member info about object types
        //********************************************************
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

        public static string GetName(System.Int64 typeId,
                                     int member)
        {
            System.IntPtr result = Kernel.DotsC_GetMemberName(typeId, member);
            if (result == IntPtr.Zero)
            {
                throw new IllegalValueException("There is no such type or member defined");
            }
            else
            {
                return Internal.InternalOperations.StringOf(result);
            }
        }

        public static System.Int64 GetTypeId(System.Int64 typeId,
                                             int member)
        {
            System.Int64 result = Kernel.DotsC_GetComplexMemberTypeId(typeId, member);
            if (result == -1)
            {
                throw new IllegalValueException("There is no such type or member defined");
            }
            else
            {
                return result;
            }
        }

        public static string GetInfo(System.Int64 typeId,
                                     int member,
                                     out MemberType theMemberType,
                                     out System.Int64 complexType,
                                     out int theTypeSize,
                                     out bool isArray,
                                     out int arrayLength)
        {
            IntPtr name;
            byte isArr;
            Kernel.DotsC_GetMemberInfo(typeId,
                                       member,
                                       out theMemberType,
                                       out name,
                                       out complexType,
                                       out theTypeSize,
                                       out isArr,
                                       out arrayLength);
            isArray = Internal.InternalOperations.BoolOf(isArr);
            if (name == IntPtr.Zero)
            {
                throw new IllegalValueException("There is no such type or member defined");
            }
            else
            {
                return Internal.InternalOperations.StringOf(name);
            }
        }

        public static int GetArraySize(System.Int64 typeId,
                                             int member)
        {
            int result = Kernel.DotsC_GetMemberArraySize(typeId, member);
            if (result == -1)
            {
                throw new IllegalValueException("No such type or array defined");
            }
            else
            {
                return result;
            }
        }


        public static int GetMaxStringLength(System.Int64 typeId,
                                             int member)
        {
            int result = Kernel.DotsC_GetStringMemberMaxLength(typeId, member);
            if (result == -1)
            {
                throw new IllegalValueException("No such type or member defined");
            }
            else
            {
                return result;
            }
        }

        public static string GetTypeName(System.Int64 typeId,
                                         int member)
        {
            IntPtr sp = Kernel.DotsC_GetMemberTypeName(typeId, member);
            if (sp == IntPtr.Zero)
            {
                throw new IllegalValueException("There is no such type or member defined");
            }
            else
            {
                return Internal.InternalOperations.StringOf(sp);
            }
        }
    }
}
