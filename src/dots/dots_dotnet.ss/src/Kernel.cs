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
    /// Summary description for Kernel.
    /// </summary>
    internal class Id
    {
        internal const string LLUF_ID_NAME = "lluf_id.dll";

        internal static Int64 Generate64BitHash(string str)
        {
            if (str == null || str.Length == 0)
            {
                throw new SoftwareViolationException("Cannot generate a hash from an empty or null string (isNull = " + (str == null) + ")");
            }
            System.IntPtr strPtr = Internal.InternalOperations.CStringOf(str);
            Int64 result = LlufId_Generate64(strPtr);
            Marshal.FreeHGlobal(strPtr);
            return result;
        }

        [DllImport(LLUF_ID_NAME, CallingConvention = CallingConvention.Cdecl)]
        private static extern Int64 LlufId_Generate64(System.IntPtr str);

        [DllImport(LLUF_ID_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern Int64 LlufId_GenerateRandom64();
    }
    /// <summary>
    /// Summary description for Kernel.
    /// </summary>
    internal class Kernel
    {
        internal const string DOTS_KERNEL_NAME = "dots_kernel.dll";

        //********************************************************
        //* Base operations on blobs
        //********************************************************

        //CreateBlob
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_CreateBlob(System.Int64 id,
                                                     out System.IntPtr blob);

        //DeleteBlob
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_DeleteBlob(ref System.IntPtr blob);

        //CreateAndCopyBlob
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_CreateCopyOfBlob(out System.IntPtr to,
                                                           System.IntPtr from);

        //GetTypeId
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern System.Int64 DotsC_GetTypeId(System.IntPtr blob);

        //GetSize
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_GetSize(System.IntPtr blob);

        //ResetChanged
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_ResetChanged(System.IntPtr blob);

        //DotsC_SetChanged
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_SetChanged(System.IntPtr blob, bool changed);

        //SetChangedMembers
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetChangedMembers(System.IntPtr val,
                                                            ref System.IntPtr blob);

        //DotsC_SetChangedSinceLastRead
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_SetChangedSinceLastRead(System.IntPtr _base,
                                                                  System.IntPtr _mine);

        //********************************************************
        //* Type information operations
        //********************************************************
        //GetNumberOfTypeIds
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_NumberOfTypeIds();

        //GetNumberOfClasses
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_NumberOfClasses();

        //GetNumberOfProperties
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_NumberOfProperties();

        //GetNumberOfEnumerations
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_NumberOfEnumerations();

        //GetAllTypeId
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetAllTypeIds(System.IntPtr buf,
                                                        System.Int32 bufSize,
                                                        out System.Int32 size);

        //Exists
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern byte DotsC_TypeExists(System.Int64 id);

        //IsClass
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern byte DotsC_IsClass(System.Int64 id);

        //IsProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern byte DotsC_IsProperty(System.Int64 id);

        //IsEnumeration
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern byte DotsC_IsEnumeration(System.Int64 id);

        //IsException
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern byte DotsC_IsException(System.Int64 id);

        //TypeIdFromName
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int64 DotsC_TypeIdFromName(System.IntPtr str);

        //GetTypeName
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.IntPtr DotsC_GetTypeName(System.Int64 id);

        //GetNumberOfEnumerationValues
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_GetNumberOfEnumerationValues(System.Int64 enumId);

        //GetEnumerationValueName
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.IntPtr DotsC_GetEnumerationValueName(System.Int64 enumId,
                                                                           System.Int32 enumVal);

        //EnumerationValueFromName
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_EnumerationValueFromName(System.Int64 enumId,
                                                                           System.IntPtr enumValueName);

        //********************************************************
        //* Functions for retrieving member info about object types
        //********************************************************
        //GetNumberOfMembers
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_GetNumberOfMembers(System.Int64 id);

        //GetMemberId
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_GetMemberId(System.Int64 id,
                                                              System.IntPtr str);

        //GetMemberName
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.IntPtr DotsC_GetMemberName(System.Int64 id,
                                                                 System.Int32 member);

        //GetObjectMemberTypeId
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int64 DotsC_GetComplexMemberTypeId(System.Int64 typeId,
                                                                         System.Int32 member);

        //GetMemberInfo
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetMemberInfo(System.Int64 id,                //in
                                                        System.Int32 member,            //in
                                                        out MemberType mt,              //out
                                                        out System.IntPtr memberName,   //out
                                                        out System.Int64 complexType,   //out
                                                        out System.Int32 stringLength,  //out
                                                        out byte isArray,               //out
                                                        out System.Int32 arrLength );   //out

        //GetMemberArraySize
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_GetMemberArraySize(System.Int64 id,
                                                                     System.Int32 member);

        //GetStringMemberMaxLength
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_GetStringMemberMaxLength(System.Int64 id,
                                                                           System.Int32 member);

        //GetMemberArraySizeProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_GetMemberArraySizeProperty(System.Int64 classId,
                                                                             System.Int64 propertyId,
                                                                             System.Int32 propertyMember);


        //GetMemberTypeName
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.IntPtr DotsC_GetMemberTypeName(System.Int64 id,
                                                                     System.Int32 member);
/*TODO awaiting changes to dots_kernel
        //GetMappedMemberId
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_GetMappedMemberId(ref System.Int64 classId,
                                                                    ref System.Int64 propertyId,
                                                                    ref System.Int32 propertyMember);
*/
        //IsAnythingChanged
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern byte DotsC_IsAnythingChanged(System.IntPtr blob);

        //********************************************************
        //* Functions handling parameters
        //********************************************************
        //GetNumberOfParameters
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_GetNumberOfParameters(System.Int64 id);

        //GetParameterId
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_GetParameterId(System.Int64 id,
                                                                 System.IntPtr parameterName);

        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.IntPtr DotsC_GetParameterName(System.Int64 id,
                                                                    System.Int32 parameter);

        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern MemberType DotsC_GetParameterType(System.Int64 id,
                                                                 System.Int32 parameter);

        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.IntPtr DotsC_GetParameterTypeName(System.Int64 id,
                                                                        System.Int32 parameter);

        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_GetParameterArraySize(System.Int64 typeId,
                                                                        System.Int32 parameter);


        //************************************************************************************
        //* Type compatibility
        //************************************************************************************
        //Checks if type is an instance of the ofType, direct or by inheritance
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern byte DotsC_IsOfType(System.Int64 theType,
                                                   System.Int64 ofType);

        //GetCompleteType
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetCompleteType(System.Int64 theType,
                                                          System.IntPtr buf,
                                                          System.Int32 bufSize,
                                                          out System.Int32 noResults);

        //GetParentType
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern System.Int64 DotsC_GetParentType(System.Int64 theType);

        //HasProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_HasProperty(System.Int64 classTypeId,
                                                      System.Int64 propertyTypeId,
                                                      out byte hasProperty,
                                                      out byte isInherited);

        //************************************************************************************
        //* Serialization
        //************************************************************************************
        //BlobToXml
        //        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        //        internal static extern void DotsC_BlobToXml(System.IntPtr buf,
        //                                                    ref System.IntPtr blob,
        //                                                    ref System.Int32 bufSize);

        //BetterBlobToXml
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_BetterBlobToXml(System.IntPtr xmlDest,
                                                          System.IntPtr blobSource,
                                                          System.Int32 bufSize,
                                                          out System.Int32 resultSize);

        //BlobToJson
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_BlobToJson(System.IntPtr jsonDest,
                                                     System.IntPtr blobSource,
                                                     System.Int32 bufSize,
                                                     out System.Int32 resultSize);

#if FUNC_PTR_WORKAROUND
        //XmlToBlob
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_XmlToBlob(out System.IntPtr blobDest,
                                                    out System.IntPtr dummy,
                                                    System.IntPtr val);

        //JsonToBlob
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_JsonToBlob(out System.IntPtr blobDest,
                                                     out System.IntPtr dummy,
                                                     System.IntPtr val);


#else
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        internal delegate void DotsC_BytePointerDeleter(ref System.IntPtr ptr);

        //XmlToBlob
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_XmlToBlob(out System.IntPtr blobDest,
                                                    out DotsC_BytePointerDeleter deleter,
                                                    System.IntPtr val);

        //JsonToBlob
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_JsonToBlob(out System.IntPtr blobDest,
                                                    out DotsC_BytePointerDeleter deleter,
                                                    System.IntPtr val);
#endif



        //CalculateBase64BufferSize
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_CalculateBase64BufferSize(System.Int32 binarySourceSize);

        //BinaryToBase64
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_BinaryToBase64(System.IntPtr base64Dest,
                                                         System.Int32 destSize,
                                                         System.IntPtr binarySource,
                                                         System.Int32 sourceSize,
                                                         out System.Int32 resultSize);

        //CalculateBinaryBufferSize
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_CalculateBinaryBufferSize(System.Int32 base64SourceSize);

        //Base64ToBinary
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_Base64ToBinary(System.IntPtr binaryDest,
                                                         System.Int32 destSize,
                                                         System.IntPtr base64Source,
                                                         System.Int32 sourceSize,
                                                         out System.Int32 resultSize);

        //************************************************************************************
        //* Functions for retrieval of parameters
        //************************************************************************************
        //GetBooleanParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetBooleanParameter(System.Int64 id,
                                                              System.Int32 parameter,
                                                              System.Int32 index,
                                                              out byte val);

        //GetEnumerationParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetEnumerationParameter(System.Int64 id,
                                                                  System.Int32 parameter,
                                                                  System.Int32 index,
                                                                  out System.Int32 val);

        //GetInt32Parameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetInt32Parameter(System.Int64 id,
                                                            System.Int32 parameter,
                                                            System.Int32 index,
                                                            out System.Int32 val);

        //GetInt64Parameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetInt64Parameter(System.Int64 id,
                                                            System.Int32 parameter,
                                                            System.Int32 index,
                                                            out System.Int64 val);

        //GetFloat32Parameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetFloat32Parameter(System.Int64 id,
                                                              System.Int32 parameter,
                                                              System.Int32 index,
                                                              out float val);

        //GetFloat64Parameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetFloat64Parameter(System.Int64 id,
                                                              System.Int32 parameter,
                                                              System.Int32 index,
                                                              out double val);

        //GetStringParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetStringParameter(System.Int64 id,
                                                             System.Int32 parameter,
                                                             System.Int32 index,
                                                             out System.IntPtr val);

        //GetHashedIdParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetHashedIdParameter(System.Int64 id,
                                                               System.Int32 parameter,
                                                               System.Int32 index,
                                                               out System.Int64 hashVal,
                                                               out System.IntPtr strVal);

        //GetTypeIdParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetTypeIdParameter(System.Int64 id,
                                                             System.Int32 parameter,
                                                             System.Int32 index,
                                                             out System.Int64 val);

        //GetEntityIdParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetEntityIdParameter(System.Int64 id,
                                                               System.Int32 parameter,
                                                               System.Int32 index,
                                                               out Internal.DotsC_EntityId eid,
                                                               out System.IntPtr strVal);

        //GetObjectParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetObjectParameter(System.Int64 id,
                                                             System.Int32 parameter,
                                                             System.Int32 index,
                                                             out System.IntPtr val);

        //GetBinaryParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetBinaryParameter(System.Int64 id,
                                                             System.Int32 parameter,
                                                             System.Int32 index,
                                                             out System.IntPtr val,
                                                             out System.Int32 size);

        //************************************************************************************
        //* Functions for retrieving member values
        //************************************************************************************
        //IsNullMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern byte DotsC_IsNullMember(System.IntPtr blob,
                                                       System.Int32 member,
                                                       System.Int32 index);

        //IsChangedMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern byte DotsC_IsChangedMember(System.IntPtr blob,
                                                          System.Int32 member,
                                                          System.Int32 index);

        //GetBooleanMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetBooleanMember(System.IntPtr blob,
                                                           System.Int32 member,
                                                           System.Int32 index,
                                                           out byte val,
                                                           out byte isNull,
                                                           out byte isChanged);

        //GetEnumerationMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetEnumerationMember(System.IntPtr blob,
                                                               System.Int32 member,
                                                               System.Int32 index,
                                                               out System.Int32 val,
                                                               out byte isNull,
                                                               out byte isChanged);

        //GetInt32Member
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetInt32Member(System.IntPtr blob,
                                                         System.Int32 member,
                                                         System.Int32 index,
                                                         out System.Int32 val,
                                                         out byte isNull,
                                                         out byte isChanged);

        //GetInt64Member
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetInt64Member(System.IntPtr blob,
                                                         System.Int32 member,
                                                         System.Int32 index,
                                                         out System.Int64 val,
                                                         out byte isNull,
                                                         out byte isChanged);

        //GetFloat32Member
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetFloat32Member(System.IntPtr blob,
                                                           System.Int32 member,
                                                           System.Int32 index,
                                                           out float val,
                                                           out byte isNull,
                                                           out byte isChanged);

        //GetFloat64Member
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetFloat64Member(System.IntPtr blob,
                                                           System.Int32 member,
                                                           System.Int32 index,
                                                           out double val,
                                                           out byte isNull,
                                                           out byte isChanged);

        //GetStringMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetStringMember(System.IntPtr blob,
                                                          System.Int32 member,
                                                          System.Int32 index,
                                                          out System.IntPtr val,
                                                          out byte isNull,
                                                          out byte isChanged);

        //GetTypeIdMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetTypeIdMember(System.IntPtr blob,
                                                          System.Int32 member,
                                                          System.Int32 index,
                                                          out System.Int64 val,
                                                          out byte isNull,
                                                          out byte isChanged);

        //DotsC_GetHashedIdMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetHashedIdMember(System.IntPtr blob,
                                                            System.Int32 member,
                                                            System.Int32 index,
                                                            out System.Int64 hashVal,
                                                            out System.IntPtr strVal,
                                                            out byte isNull,
                                                            out byte isChanged);

        //GetEntityIdMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetEntityIdMember(System.IntPtr blob,
                                                            System.Int32 member,
                                                            System.Int32 index,
                                                            out DotsC_EntityId val,
                                                            out System.IntPtr strVal,
                                                            out byte isNull,
                                                            out byte isChanged);

        //GetObjectMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetObjectMember(System.IntPtr blob,
                                                          System.Int32 member,
                                                          System.Int32 index,
                                                          out System.IntPtr val,
                                                          out byte isNull,
                                                          out byte isChanged);

        //GetBinaryMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetBinaryMember(System.IntPtr blob,
                                                          System.Int32 member,
                                                          System.Int32 index,
                                                          out System.IntPtr val,
                                                          out System.Int32 size,
                                                          out byte isNull,
                                                          out byte isChanged);

        //************************************************************************************
        //* Functions for setting member values
        //************************************************************************************
        //SetNullMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetNullMember(System.IntPtr blob,
                                                        System.Int32 member,
                                                        System.Int32 index);

        //SetBooleanMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetBooleanMember(byte val,
                                                           ref System.IntPtr blob,
                                                           System.Int32 member,
                                                           System.Int32 index);

        //SetEnumerationMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetEnumerationMember(System.Int32 val,
                                                               ref System.IntPtr blob,
                                                               System.Int32 member,
                                                               System.Int32 index);

        //SetInt32Member
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetInt32Member(System.Int32 val,
                                                         ref System.IntPtr blob,
                                                         System.Int32 member,
                                                         System.Int32 index);

        //SetInt64Member
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetInt64Member(System.Int64 val,
                                                         ref System.IntPtr blob,
                                                         System.Int32 member,
                                                         System.Int32 index);

        //SetFloat32Member
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetFloat32Member(float val,
                                                           ref System.IntPtr blob,
                                                           System.Int32 member,
                                                           System.Int32 index);

        //SetFloat64Member
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetFloat64Member(double val,
                                                           ref System.IntPtr blob,
                                                           System.Int32 member,
                                                           System.Int32 index);

        //SetStringMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetStringMember(System.IntPtr val,
                                                          ref System.IntPtr blob,
                                                          System.Int32 member,
                                                          System.Int32 index);

        //SetTypeIdMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetTypeIdMember(System.Int64 val,
                                                          ref System.IntPtr blob,
                                                          System.Int32 member,
                                                          System.Int32 index);
#if obsolete
        //SetEntityIdMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetEntityIdMember(ref DotsC_EntityId val,
                                                            System.IntPtr instanceIdStr,
                                                            ref System.IntPtr blob,
                                                            System.Int32 member,
                                                            System.Int32 index);

        //SetObjectMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetObjectMember(System.IntPtr val,
                                                          ref System.IntPtr blob,
                                                          System.Int32 member,
                                                          System.Int32 index);

        //SetBinaryMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetBinaryMember(System.IntPtr val,
                                                          System.Int32 numberOfBytes,
                                                          ref System.IntPtr blob,
                                                          System.Int32 member,
                                                          System.Int32 index);
#endif
        //************************************************************************************
        //* Functions for retrieving property member values
        //************************************************************************************
#if obsolete
        //IsNullProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern byte DotsC_IsNullProperty(System.IntPtr blob,
                                                         System.Int64  property,
                                                         System.Int32 member,
                                                         System.Int32 index);

        //IsChangedProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern byte DotsC_IsChangedProperty(System.IntPtr blob,
                                                            System.Int64  property,
                                                            System.Int32 member,
                                                            System.Int32 index);

        //GetBooleanProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetBooleanProperty(System.IntPtr blob,
                                                             System.Int64  property,
                                                             System.Int32 member,
                                                             System.Int32 index,
                                                             out byte val,
                                                             out byte isNull,
                                                             out byte isChanged,
                                                             out DotsC_ErrorCode errorCode);

        //GetEnumerationProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void  DotsC_GetEnumerationProperty(System.IntPtr blob,
                                                                  System.Int64 property,
                                                                  System.Int32 member,
                                                                  System.Int32 index,
                                                                  out System.Int32 val,
                                                                  out byte isNull,
                                                                  out byte isChanged,
                                                                  out DotsC_ErrorCode errorCode);

        //GetInt32Property
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetInt32Property(System.IntPtr blob,
                                                           System.Int64 property,
                                                           System.Int32 member,
                                                           System.Int32 index,
                                                           out System.Int32 val,
                                                           out byte isNull,
                                                           out byte isChanged,
                                                           out DotsC_ErrorCode errorCode);

        //GetInt64Property
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void  DotsC_GetInt64Property(System.IntPtr blob,
                                                            System.Int64  property,
                                                            System.Int32 member,
                                                            System.Int32 index,
                                                            out System.Int64 val,
                                                            out byte isNull,
                                                            out byte isChanged,
                                                            out DotsC_ErrorCode errorCode);

        //GetFloat32Property
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void  DotsC_GetFloat32Property(System.IntPtr blob,
                                                              System.Int64 property,
                                                              System.Int32 member,
                                                              System.Int32 index,
                                                              out float val,
                                                              out byte isNull,
                                                              out byte isChanged,
                                                              out DotsC_ErrorCode errorCode);

        //GetFloat64Property
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void  DotsC_GetFloat64Property(System.IntPtr blob,
                                                              System.Int64  property,
                                                              System.Int32 member,
                                                              System.Int32 index,
                                                              out double val,
                                                              out byte isNull,
                                                              out byte isChanged,
                                                              out DotsC_ErrorCode errorCode);

        //GetStringProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void  DotsC_GetStringProperty(System.IntPtr blob,
                                                             System.Int64  property,
                                                             System.Int32 member,
                                                             System.Int32 index,
                                                             out System.IntPtr val,
                                                             out byte isNull,
                                                             out byte isChanged,
                                                             out DotsC_ErrorCode errorCode);

        //GetTypeIdProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void  DotsC_GetTypeIdProperty(System.IntPtr blob,
                                                             System.Int64  property,
                                                             System.Int32 member,
                                                             System.Int32 index,
                                                             out System.Int64 val,
                                                             out byte isNull,
                                                             out byte isChanged,
                                                             out DotsC_ErrorCode errorCode);

        //GetEntityIdProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void  DotsC_GetEntityIdProperty(System.IntPtr blob,
                                                               System.Int64 property,
                                                               System.Int32 member,
                                                               System.Int32 index,
                                                               out DotsC_EntityId entityId,
                                                               out System.IntPtr instanceIdStr,
                                                               out byte isNull,
                                                               out byte isChanged,
                                                               out DotsC_ErrorCode errorCode);

        //GetObjectProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void  DotsC_GetObjectProperty(System.IntPtr blob,
                                                             System.Int64 property,
                                                             System.Int32 member,
                                                             System.Int32 index,
                                                             out System.IntPtr val,
                                                             out byte isNull,
                                                             out byte isChanged,
                                                             out DotsC_ErrorCode errorCode);

        //GetBinaryProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void  DotsC_GetBinaryProperty(System.IntPtr blob,
                                                             System.Int64 property,
                                                             System.Int32 member,
                                                             System.Int32 index,
                                                             out System.IntPtr val,
                                                             out System.Int32 size,
                                                             out byte isNull,
                                                             out byte isChanged,
                                                             out DotsC_ErrorCode errorCode);

        //************************************************************************************
        //* Functions for setting property member values
        //************************************************************************************
        //SetNullProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetNullProperty(ref System.IntPtr blob,
                                                          System.Int64 property,
                                                          System.Int32 member,
                                                          System.Int32 index,
                                                          out DotsC_ErrorCode errorCode);

        //SetBooleanProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetBooleanProperty(byte val,
                                                             ref System.IntPtr blob,
                                                             System.Int64 property,
                                                             System.Int32 member,
                                                             System.Int32 index,
                                                             out DotsC_ErrorCode errorCode);

        //SetEnumerationProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetEnumerationProperty(System.Int32 val,
                                                                 ref System.IntPtr blob,
                                                                 System.Int64 property,
                                                                 System.Int32 member,
                                                                 System.Int32 index,
                                                                 out DotsC_ErrorCode errorCode);

        //SetInt32Property
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetInt32Property(System.Int32 val,
                                                           ref System.IntPtr blob,
                                                           System.Int64 property,
                                                           System.Int32 member,
                                                           System.Int32 index,
                                                           out DotsC_ErrorCode errorCode);

        //SetInt64Property
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetInt64Property(System.Int64 val,
                                                           ref System.IntPtr blob,
                                                           System.Int64 property,
                                                           System.Int32 member,
                                                           System.Int32 index,
                                                           out DotsC_ErrorCode errorCode);

        //SetFloat32Property
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetFloat32Property(float val,
                                                             ref System.IntPtr blob,
                                                             System.Int64 property,
                                                             System.Int32 member,
                                                             System.Int32 index,
                                                             out DotsC_ErrorCode errorCode);

        //SetFloat64Property
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetFloat64Property(double val,
                                                             ref System.IntPtr blob,
                                                             System.Int64 property,
                                                             System.Int32 member,
                                                             System.Int32 index,
                                                             out DotsC_ErrorCode errorCode);

        //SetStringProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetStringProperty(System.IntPtr val,
                                                            ref System.IntPtr blob,
                                                            System.Int64 property,
                                                            System.Int32 member,
                                                            System.Int32 index,
                                                            out DotsC_ErrorCode errorCode);

        //SetTypeIdProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetTypeIdProperty(System.Int64 val,
                                                            ref System.IntPtr blob,
                                                            System.Int64 property,
                                                            System.Int32 member,
                                                            System.Int32 index,
                                                            out DotsC_ErrorCode errorCode);

        //SetEntityIdProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetEntityIdProperty(ref DotsC_EntityId entityId,
                                                              System.IntPtr instanceIdStr,
                                                              ref System.IntPtr blob,
                                                              System.Int64 property,
                                                              System.Int32 member,
                                                              System.Int32 index,
                                                              out DotsC_ErrorCode errorCode);

        //SetObjectProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetObjectProperty(System.IntPtr val,
                                                            ref System.IntPtr blob,
                                                            System.Int64 property,
                                                            System.Int32 member,
                                                            System.Int32 index,
                                                            out DotsC_ErrorCode errorCode);
        //SetBinaryProperty
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_SetBinaryProperty(System.IntPtr val,
                                                            System.Int32 size,
                                                            ref System.IntPtr blob,
                                                            System.Int64 property,
                                                            System.Int32 member,
                                                            System.Int32 index,
                                                            out DotsC_ErrorCode errorCode);
#endif

        //*********************************
        //* For real classes
        //*********************************
        //GetInitialSize
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_GetInitialSize(System.Int64 typeId);


        //DotsC_FormatBlob
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_FormatBlob(System.IntPtr blob,
                                                     System.Int32 blobSize,
                                                     System.Int64 typeId,
                                                     out System.IntPtr beginningOfUnused);
        //DotsC_CreateObjectMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_CreateObjectMember(System.IntPtr insideBlob,
                                                              System.Int32 blobSize,
                                                              System.Int64 typeId,
                                                              System.Int32 member,
                                                              System.Int32 index,
                                                              byte isChanged,
                                                              ref System.IntPtr beginningOfUnused);
        //DotsC_CreateStringMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_CreateStringMember(System.IntPtr insideBlob,
                                                             System.Int32 stringLength, //remember the null-termination!
                                                             System.Int32 member,
                                                             System.Int32 index,
                                                             byte isChanged,
                                                             ref System.IntPtr beginningOfUnused);

        //DotsC_CreateBinaryMember
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_CreateBinaryMember(System.IntPtr insideBlob,
                                                             System.Int32 binarySize,
                                                             System.Int32 member,
                                                             System.Int32 index,
                                                             byte isChanged,
                                                             ref System.IntPtr beginningOfUnused);


        //SetBooleanMemberInPreallocated
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_SetBooleanMemberInPreallocated(byte val,
                                                                         byte isNull,
                                                                         byte isChanged,
                                                                         System.IntPtr blob,
                                                                         System.Int32 member,
                                                                         System.Int32 index);
/*
        //SetEnumerationMemberInPreallocated
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_SetEnumerationMemberInPreallocated(double val,
                                                                         byte isNull,
                                                                         byte isChanged,
                                                                         System.IntPtr blob,
                                                                         System.Int32 member,
                                                                         System.Int32 index);*/

        //SetInt32MemberInPreallocated
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_SetInt32MemberInPreallocated(System.Int32 val,
                                                                       byte isNull,
                                                                       byte isChanged,
                                                                       System.IntPtr blob,
                                                                       System.Int32 member,
                                                                       System.Int32 index);

        //SetInt64MemberInPreallocated
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_SetInt64MemberInPreallocated(System.Int64 val,
                                                                       byte isNull,
                                                                       byte isChanged,
                                                                       System.IntPtr blob,
                                                                       System.Int32 member,
                                                                       System.Int32 index);

        //SetFloat32MemberInPreallocated
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_SetFloat32MemberInPreallocated(float val,
                                                                         byte isNull,
                                                                         byte isChanged,
                                                                         System.IntPtr blob,
                                                                         System.Int32 member,
                                                                         System.Int32 index);

        //SetFloat64MemberInPreallocated
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_SetFloat64MemberInPreallocated(double val,
                                                                         byte isNull,
                                                                         byte isChanged,
                                                                         System.IntPtr blob,
                                                                         System.Int32 member,
                                                                         System.Int32 index);

        //DotsC_SetHashedIdMemberInPreallocated
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_SetHashedIdMemberInPreallocated(System.Int64 hashVal,
                                                                          byte [] strVal,
                                                                          System.Int32 strLength,
                                                                          byte isNull,
                                                                          byte isChanged,
                                                                          System.IntPtr blob,
                                                                          System.Int32 member,
                                                                          System.Int32 index,
                                                                          ref System.IntPtr beginningOfUnused);


        //SetEntityIdMemberInPreallocated
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_SetEntityIdMemberInPreallocated(ref DotsC_EntityId val,
                                                                          byte[] strVal,
                                                                          System.Int32 strLength,
                                                                          byte isNull,
                                                                          byte isChanged,
                                                                          System.IntPtr blob,
                                                                          System.Int32 member,
                                                                          System.Int32 index,
                                                                          ref System.IntPtr beginningOfUnused);


        //GetPropertyMappingKind
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetPropertyMappingKind(System.Int64 typeId,
                                                                 System.Int64 propertyId,
                                                                 System.Int32 member,
                                                                 out DotsC_PropertyMappingKind mappingKind,
                                                                 out DotsC_ErrorCode errorCode);

        //GetClassMemberReference
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetClassMemberReference(System.Int64 typeId,
                                                                  System.Int64 propertyId,
                                                                  System.Int32 member,
                                                                  out System.IntPtr classMemberReference,
                                                                  out System.Int32 classMemberReferenceSize); //out

        //GetEnumerationChecksum
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetEnumerationChecksum(System.Int64 typeId,
                                                                 out System.Int64 checksum);

        //GetBooleanPropertyParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetBooleanPropertyParameter(System.Int64 typeId,
                                                                      System.Int64 propertyId,
                                                                      System.Int32 member,
                                                                      System.Int32 index,
                                                                      out byte val);

       //GetInt32PropertyParameter
       [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
       internal static extern void DotsC_GetInt32PropertyParameter(System.Int64 typeId,
                                                                   System.Int64 propertyId,
                                                                   System.Int32 member,
                                                                   System.Int32 index,
                                                                   out System.Int32 val);

        //GetInt64PropertyParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetInt64PropertyParameter(System.Int64 typeId,
                                                                    System.Int64 propertyId,
                                                                    System.Int32 member,
                                                                    System.Int32 index,
                                                                    out System.Int64 val);

        //GetFloat32PropertyParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetFloat32PropertyParameter(System.Int64 typeId,
                                                                      System.Int64 propertyId,
                                                                      System.Int32 member,
                                                                      System.Int32 index,
                                                                      out float val);

        //GetFloat64PropertyParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetFloat64PropertyParameter(System.Int64 typeId,
                                                                      System.Int64 propertyId,
                                                                      System.Int32 member,
                                                                      System.Int32 index,
                                                                      out double val);

        //GetStringPropertyParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetStringPropertyParameter(System.Int64 typeId,
                                                                     System.Int64 propertyId,
                                                                     System.Int32 member,
                                                                     System.Int32 index,
                                                                     out System.IntPtr val);

        //GetTypeIdPropertyParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetTypeIdPropertyParameter(System.Int64 typeId,
                                                                     System.Int64 propertyId,
                                                                     System.Int32 member,
                                                                     System.Int32 index,
                                                                     out System.Int64 val);


        //GetHashedIdPropertyParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetHashedIdPropertyParameter(System.Int64 typeId,
                                                                       System.Int64 propertyId,
                                                                       System.Int32 member,
                                                                       System.Int32 index,
                                                                       out System.Int64 hashVal,
                                                                       out System.IntPtr strVal);

        //GetEntityIdPropertyParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetEntityIdPropertyParameter(System.Int64 typeId,
                                                                       System.Int64 propertyId,
                                                                       System.Int32 member,
                                                                       System.Int32 index,
                                                                       out DotsC_EntityId entityId,
                                                                       out System.IntPtr instanceIdStr);

        //GetObjectPropertyParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetObjectPropertyParameter(System.Int64 typeId,
                                                                     System.Int64 propertyId,
                                                                     System.Int32 member,
                                                                     System.Int32 index,
                                                                     out System.IntPtr val);

        //GetBinaryPropertyParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetBinaryPropertyParameter(System.Int64 typeId,
                                                                     System.Int64 propertyId,
                                                                     System.Int32 member,
                                                                     System.Int32 index,
                                                                     out System.IntPtr val,
                                                                     out System.Int32 size);

        //DotsC_SetException
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_SetException(System.Int64 exceptionId,
                                                       System.IntPtr description);


        //TODO: re-add this when MONO has fixed their bug
#if FUNC_PTR_WORKAROUND
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetAndClearException(out System.Int64 exceptionId,
                                                               out System.IntPtr description,
                                                               out System.IntPtr dummy,
                                                               out byte wasSet);

#else
        //DotsC_GetAndClearException
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetAndClearException(out System.Int64 exceptionId,
                                                               out System.IntPtr description,
                                                               out DotsC_BytePointerDeleter deleter,
                                                               out byte wasSet);

#endif
        //DotsC_PeekAtException
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_PeekAtException(out System.Int64 exceptionId);

        //DotsC_GetDouFilePathForType
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetDouFilePathForType(System.Int64 typeId,
                                                                System.IntPtr buf,
                                                                System.Int32 bufSize,
                                                                out System.Int32 resultSize);
    }
}
