/******************************************************************************
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
        //* Static type information operations
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

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern System.Int32 DotsC_NumberOfExceptions();

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

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern System.IntPtr DotsC_MemberTypeName(MemberType memberType);

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

		//GetEnumerationChecksum
		[DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
		internal static extern void DotsC_GetEnumerationChecksum(System.Int64 typeId,
		                                                         out System.Int64 checksum);

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

		//GetMemberInfo
		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_GetMemberInfo(System.Int64 id,                //in
		                                                System.Int32 member,            //in
		                                                out MemberType mt,              //out
		                                                out System.IntPtr memberName,   //out
		                                                out System.Int64 complexType,   //out
		                                                out System.Int32 stringLength,  //out
		                                                out CollectionType collectionType, //out
		                                                out System.Int32 arrLength );   //out

		//GetMemberArraySizeProperty
		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern System.Int32 DotsC_GetMemberArraySizeProperty(System.Int64 classId,
		                                                                     System.Int64 propertyId,
		                                                                     System.Int32 propertyMember);

		//GetStringMemberMaxLength
		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern System.Int32 DotsC_GetStringMemberMaxLengthProperty(System.Int64 classId,
		                                                                           System.Int64 propertyId,
		                                                                           System.Int32 propertyMember);

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
		internal static extern void DotsC_GetParameterInfo(System.Int64 typeId,
								                           System.Int32 parameter,
								                           out MemberType memberType,
								                           out System.IntPtr parameterName,
		                                                   out System.Int64 complexType,
		                                                   out CollectionType collectionType,
		                                                   out System.Int32 numberOfValues);

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

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern byte DotsC_GetPropertyMappingKind(System.Int64 classTypeId,
		                                                         System.Int64 propertyTypeId,
		                                                         System.Int32 propertyMember,
	                                                  			 out DotsC_PropertyMappingKind mappingKind);

		//GetClassMemberReference
		[DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
		internal static extern void DotsC_GetClassMemberReference(System.Int64 typeId,
		                                                          System.Int64 propertyId,
		                                                          System.Int32 propertyMember,
		                                                          out System.IntPtr classMemberReference,
		                                                          out System.Int32 classMemberReferenceSize); //out

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_GetPropertyParameterReference(System.Int64 typeId,
		                                                                System.Int64 propertyId,
		                                                                System.Int32 propertyMember,
		                                                         		System.Int32 index,
		                                                         		out System.Int32 paramId, //out
		                                                                out System.Int32 paramValueIndex); //out

        //************************************************************************************
        //* Serialization
        //************************************************************************************
        //BetterBlobToXml
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_BlobToXml(System.IntPtr xmlDest,
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
		                                                          KeyValMode keyValMode,
                                                                  out System.Int32 val);

        //GetInt32Parameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetInt32Parameter(System.Int64 id,
                                                            System.Int32 parameter,
                                                            System.Int32 index,
		                                                    KeyValMode keyValMode,
                                                            out System.Int32 val);

        //GetInt64Parameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetInt64Parameter(System.Int64 id,
                                                            System.Int32 parameter,
                                                            System.Int32 index,
		                                                    KeyValMode keyValMode,
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
		                                                     KeyValMode keyValMode,
                                                             out System.IntPtr val);

		//GetTypeIdParameter
		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_GetTypeIdParameter(System.Int64 id,
		                                                     System.Int32 parameter,
		                                                     System.Int32 index,
		                                                     KeyValMode keyValMode,
		                                                     out System.Int64 val);

        //GetHashedIdParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_GetHashedIdParameter(System.Int64 id,
                                                               System.Int32 parameter,
                                                               System.Int32 index,
		                                                       KeyValMode keyValMode,
                                                               out System.Int64 hashVal,
                                                               out System.IntPtr strVal);

        //GetEntityIdParameter
        [DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_GetEntityIdParameter(System.Int64 id,
                                                               System.Int32 parameter,
                                                               System.Int32 index,
		                                                       KeyValMode keyValMode,
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

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern System.Int32 DotsC_DictionaryInt32KeyToIndex(System.Int64 typeId,
		                                                                    System.Int32 parameter,
		                                                                    System.Int32 key);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern System.Int32 DotsC_DictionaryInt64KeyToIndex(System.Int64 typeId,
		                                                                   System.Int32 parameter,
		                                                                   System.Int64 key);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_DictionaryStringKeyToIndex(System.Int64 typeId,
		                                                                    System.Int32 parameter,
		                                                                    System.IntPtr key);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_DictionaryEntityIdKeyToIndex(System.Int64 typeId,
		                                                                      System.Int32 parameter,
		                                                                      DotsC_EntityId key);

		//********************************************************
		//* Operations on blobs
		//********************************************************

		//GetTypeId
		[DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
		internal static extern System.Int64 DotsC_GetTypeId(System.IntPtr blob);

		//GetSize
		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern System.Int32 DotsC_GetSize(System.IntPtr blob);

		//CreateBlob
		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern System.IntPtr DotsC_AllocateBlob(System.Int32 size);

		//CreateAndCopyBlob
		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_CreateCopyOfBlob(out System.IntPtr to,
		                                                   System.IntPtr from);

		//DeleteBlob
		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_DeleteBlob(ref System.IntPtr blob);

		//************************************************************************************
		//* Read operations
		//************************************************************************************
		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern System.Int64 DotsC_CreateBlobReader(System.IntPtr blob);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_DeleteBlobReader(System.Int64 handle);


		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_GetNumberOfMemberValues(System.Int64 readerHandle,
		                                                                 System.Int32 member);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_ReadMemberStatus(System.Int64 readerHandle,
		                                                   out byte isNull,
		                                                   out byte isChanged,
		                                                   System.Int32 member,
		                                                   System.Int32 valueIndex);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_ReadInt32Member(System.Int64 readerHandle,
		                                                  out System.Int32 val,
		                                                  out byte isNull,
		                                                  out byte isChanged,
		                                                  System.Int32 member,
		                                                  System.Int32 valueIndex,
		                                                  KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_ReadInt64Member(System.Int64 readerHandle,
		                                                  out System.Int64 val,
		                                                  out byte isNull,
		                                                  out byte isChanged,
		                                                  System.Int32 member,
		                                                  System.Int32 valueIndex,
		                                                  KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_ReadFloat32Member(System.Int64 readerHandle,
		                                                  out float val,
		                                                  out byte isNull,
		                                                  out byte isChanged,
		                                                  System.Int32 member,
		                                                  System.Int32 valueIndex,
		                                                  KeyValMode keyValMode);


		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_ReadFloat64Member(System.Int64 readerHandle,
		                                                    out double val,
		                                                    out byte isNull,
		                                                    out byte isChanged,
		                                                    System.Int32 member,
		                                                    System.Int32 valueIndex,
		                                                    KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_ReadBooleanMember(System.Int64 readerHandle,
		                                                    out byte val,
		                                                    out byte isNull,
		                                                    out byte isChanged,
		                                                    System.Int32 member,
		                                                    System.Int32 valueIndex,
		                                                    KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_ReadStringMember(System.Int64 readerHandle,
		                                                    out System.IntPtr val,
		                                                    out byte isNull,
		                                                    out byte isChanged,
		                                                    System.Int32 member,
		                                                    System.Int32 valueIndex,
		                                                    KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_ReadHashedMember(System.Int64 readerHandle,
		                                                   out System.Int64 val,
		                                                   out System.IntPtr optionalStr,
		                                                   out byte isNull,
		                                                   out byte isChanged,
		                                                   System.Int32 member,
		                                                   System.Int32 valueIndex,
		                                                   KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_ReadEntityIdMember(System.Int64 readerHandle,
		                                                     out DotsC_EntityId val,
		                                                   	 out System.IntPtr optionalStr,
			                                                 out byte isNull,
			                                                 out byte isChanged,
			                                                 System.Int32 member,
			                                                 System.Int32 valueIndex,
			                                                 KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_ReadBinaryMember(System.Int64 readerHandle,
		                                                     out System.IntPtr val,
		                                                   	 out System.Int32 size,
		                                                     out byte isNull,
		                                                     out byte isChanged,
		                                                     System.Int32 member,
		                                                     System.Int32 valueIndex,
		                                                     KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_ReadObjectMember(System.Int64 readerHandle,
		                                                   out System.IntPtr val,
		                                                   out byte isNull,
		                                                   out byte isChanged,
		                                                   System.Int32 member,
		                                                   System.Int32 valueIndex,
		                                                   KeyValMode keyValMode);

		//************************************************************************************
		//* Write operations
		//************************************************************************************
		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern System.Int64 DotsC_CreateBlobWriter(System.Int64 typeId);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern System.Int64 DotsC_CreateBlobWriterFromBlob(System.IntPtr blob);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern System.Int64 DotsC_CreateBlobWriterFromReader(System.Int64 readerHandle);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_DeleteBlobWriter(System.Int64 writerHandle);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern System.Int32 DotsC_CalculateBlobSize(System.Int64 writerHandle);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_WriteBlob(System.Int64 writerHandle, System.IntPtr blob);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_WriteAllChangeFlags(System.Int64 writerHandle, byte changed);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_WriteChangeFlag(System.Int64 writerHandle,
		                                           			System.Int32 member,
		                                                  	System.Int32 index,
		                                           			byte changed);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern byte DotsC_MarkChanges(System.Int64 originalReader,
		                                              System.Int64 currentWriter);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_WriteInt32Member(System.Int64 writerHandle, System.Int32 val, byte isNull, byte isChanged,
		                                                   System.Int32 member, System.Int32 arrayIndex, KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_WriteInt64Member(System.Int64 writerHandle, System.Int64 val, byte isNull, byte isChanged, 
		                                                   System.Int32 member, System.Int32 arrayIndex, KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_WriteFloat32Member(System.Int64 writerHandle, float val, byte isNull, byte isChanged, 
		                                                     System.Int32 member, System.Int32 arrayIndex, KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_WriteFloat64Member(System.Int64 writerHandle, double val, byte isNull, byte isChanged, 
		                                                     System.Int32 member, System.Int32 arrayIndex, KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_WriteBooleanMember(System.Int64 writerHandle, byte val, byte isNull, byte isChanged, 
		                                                     System.Int32 member, System.Int32 arrayIndex, KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_WriteStringMember(System.Int64 writerHandle, System.IntPtr val, byte isNull, byte isChanged, 
		                                                    System.Int32 member, System.Int32 arrayIndex, KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_WriteHashedMember(System.Int64 writerHandle, System.Int64 hash, System.IntPtr str, byte isNull, byte isChanged, 
		                                                    System.Int32 member, System.Int32 arrayIndex, KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_WriteEntityIdMember(System.Int64 writerHandle, DotsC_EntityId val, System.IntPtr instanceString, byte isNull, byte isChanged, 
		                                                      System.Int32 member, System.Int32 arrayIndex, KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
        internal static extern void DotsC_WriteBinaryMember(System.Int64 writerHandle, System.IntPtr val, System.Int32 size, byte isNull, byte isChanged, 
		                                                    System.Int32 member, System.Int32 arrayIndex, KeyValMode keyValMode);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention=CallingConvention.Cdecl)]
		internal static extern void DotsC_WriteObjectMember(System.Int64 writerHandle, System.IntPtr blob, byte isNull, byte isChanged, 
		                                                    System.Int32 member, System.Int32 arrayIndex, KeyValMode keyValMode);

		//************************************************************************************
		//* Library exception handling
		//************************************************************************************
        //DotsC_SetException
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DotsC_SetException(System.Int64 exceptionId,
                                                       System.IntPtr description);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
		internal static extern void DotsC_AppendExceptionDescription(System.IntPtr moreDescription);


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

		//************************************************************************************
		//* Functions mostly indended for debugging
		//************************************************************************************
        //DotsC_GetDouFilePathForType
        [DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
		internal static extern System.IntPtr DotsC_GetDouFilePath(System.Int64 typeId);

		[DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
		internal static extern byte DotsC_TypeRepositoryLoadedByThisProcess();

		[DllImport(DOTS_KERNEL_NAME, CallingConvention = CallingConvention.Cdecl)]
		internal static extern void DotsC_GetTypeDescription(System.Int64 typeId,
		                                              		 System.IntPtr buf,
		                                              		 System.Int32 bufSize,
		                                              		 out System.Int32 resultSize);
      
    }
}
