// -*- coding: utf-8 -*-
/******************************************************************************
 *
 * Copyright Consoden AB, 2005-2015 (http://safir.sourceforge.net)
 *
 * Created by: Joel Ottosson / stjoot
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

package com.saabgroup.safir.dob.typesystem;

import java.nio.ByteBuffer;

/**
 * Java interface to dots_kernel.
 * The dots Kernel class is the raw interface to the C-world.
 */
public final class Kernel {
	/** dots_kernel enum */
	static public enum DotsC_PropertyMappingKind {
		MappedToNull,
		MappedToMember,
		MappedToParameter
	}   

	static {
		//load the jni dll
		System.loadLibrary("dots_java_jni");
	}


	//************************************************************************************
	//* Functions for hash numbers
	//************************************************************************************
	public static native long GenerateRandom64();
	public static native long Generate64(String id);

	//********************************************************
	//* Static type information operations
	//********************************************************
	public static native int NumberOfTypeIds();
	public static native int NumberOfClasses();
	public static native int NumberOfProperties();
	public static native int NumberOfEnumerations();
	public static native int NumberOfExceptions();
	public static native void GetAllTypeIds(long buf[], int bufSize, int size[]);
	public static native boolean TypeExists(long id);
	public static native boolean IsClass(long id);
	public static native boolean IsProperty(long id);
	public static native boolean IsEnumeration(long id);
	public static native boolean IsException(long id);
	public static native long TypeIdFromName(String str);
	public static native String GetTypeName(long id);
	public static native String MemberTypeName(int memberType);
	public static native int GetNumberOfEnumerationValues(long enumId);
	public static native String GetEnumerationValueName(long enumId, int enumVal);
	public static native int EnumerationValueFromName(long enumId, String enumValueName);
	public static native long GetEnumerationChecksum(long typeId);


    public static native String [] GetGeneratedJars();

	//********************************************************
	//* Functions for retrieving member info about object types
	//********************************************************
	public static native int GetNumberOfMembers(long id);
	public static native int GetMemberId(long id, String memberName);

	public static native void GetMemberInfo(  long id,                //in
			int member,            //in
			int[] memberType,      //out
			String[] memberName,   //out
			long[] complexType,   //out
			int[] stringLength,  //out
			int[] collectionType, //out
			int[] arrLength );   //out

	public static native int GetMemberArraySizeProperty(long classId, long propertyId, int propertyMember);

	public static native int GetStringMemberMaxLengthProperty(long classId, long propertyId, int propertyMember);

	//********************************************************
	//* Functions handling parameters
	//********************************************************
	public static native int GetNumberOfParameters(long id);

	public static native int GetParameterId(long id, String parameterName);

	public static native void GetParameterInfo(long typeId,
			int parameter,
			int[] memberType,
			String[] parameterName,
			long[] complexType,
			int[] collectionType,
			int[] numberOfValues);

	//************************************************************************************
	//* Type compatibility
	//************************************************************************************
	public static native boolean IsOfType(long theType, long ofType);

	public static native long [] GetCompleteType(long typeId);

	public static native long GetParentType(long theType);

	public static native void HasProperty(long classTypeId, long propertyTypeId, boolean[] hasProperty, boolean[] isInherited);

	public static native int GetPropertyMappingKind(long classTypeId, long propertyTypeId, int propertyMember);

	public static native void GetClassMemberReference(long typeId,
			long propertyId,
			int member,
			ByteBuffer classMemberReference[]);

	public static native void GetPropertyParameterReference(long typeId,
			long propertyId,
			int propertyMember,
			int index,
			int[] paramId, //out
			int[] paramValueIndex); //out

	//************************************************************************************
	//* Serialization
	//************************************************************************************
	public static native String BlobToXml(ByteBuffer blob);

	public static native void XmlToBlob(ByteBuffer blob [],
			ByteBuffer deleter [], //a very nasty way of passing a pointer back and forth
			String xmlSource);

	public static native String BlobToJson(ByteBuffer blob);

	public static native void JsonToBlob(ByteBuffer blob [],
			ByteBuffer deleter [], //a very nasty way of passing a pointer back and forth
			String jsonSource);

	public static native void InvokeDeleter(ByteBuffer deleter, ByteBuffer toDelete);

	public static native String BinaryToBase64(ByteBuffer binarySource, int size);
	public static native byte[] Base64ToBinary(String base64Source);

	//************************************************************************************
	//* Functions for retrieval of parameters
	//************************************************************************************
	public static native void GetBooleanParameter(long id,
			int parameter,
			int index,
			boolean[] val);

	public static native void GetEnumerationParameter(long id,
			int parameter,
			int index,
			int keyValMode,
			int[] val);

	public static native void GetInt32Parameter(long id,
			int parameter,
			int index,
			int keyValMode,
			int[] val);

	public static native void GetInt64Parameter(long id,
			int parameter,
			int index,
			int keyValMode,
			long[] val);

	public static native void GetFloat32Parameter(long id,
			int parameter,
			int index,
			float[] val);

	public static native void GetFloat64Parameter(long id,
			int parameter,
			int index,
			double[] val);

	public static native void GetStringParameter(long id,
			int parameter,
			int index,
			int keyValMode,
			String[] val);

	public static native void GetTypeIdParameter(long id,
			int parameter,
			int index,
			int keyValMode,
			long[] val);

	public static native void GetHashedIdParameter(long id,
			int parameter,
			int index,
			int keyValMode,
			long[] hashVal,
			String[] strVal);

	public static native void GetEntityIdParameter(long id,
			int parameter,
			int index,
			int keyValMode,
			long[] typeIdOut, long[] instanceId, String[] strVal);

	public static native void GetObjectParameter(long id,
			int parameter,
			int index,
			ByteBuffer blob []);

	public static native void GetBinaryParameter(long id,
			int parameter,
			int index,
			ByteBuffer blob []);

	public static native int DictionaryInt32KeyToIndex(long typeId,
			int parameter,
			int key);

	public static native int DictionaryInt64KeyToIndex(long typeId,
			int parameter,
			long key);

	public static native int DictionaryStringKeyToIndex(long typeId,
			int parameter,
			String key);

	public static native int DictionaryEntityIdKeyToIndex(long typeId,
			int parameter,
			long keyType, long keyInstance);


	//********************************************************
	//* Operations on blobs
	//********************************************************
	public static native long GetTypeId(ByteBuffer blob);
	public static native int GetSize(ByteBuffer blob);

	//************************************************************************************
	//* Read operations
	//************************************************************************************
	public static native long CreateBlobReader(ByteBuffer blob);

	public static native void DeleteBlobReader(long handle);

	public static native int GetNumberOfMemberValues(long readerHandle, int member);

	public static native void ReadMemberStatus(long readerHandle,
			boolean[] isNull,
			boolean[] isChanged,
			int member,
			int valueIndex);

	public static native void ReadInt32Member(long readerHandle,
			int[] val,
			boolean[] isNull,
			boolean[] isChanged,
			int member,
			int valueIndex,
			int keyValMode);

	public static native void ReadInt64Member(long readerHandle,
			long[] val,
			boolean[] isNull,
			boolean[] isChanged,
			int member,
			int valueIndex,
			int keyValMode);

	public static native void ReadFloat32Member(long readerHandle,
			float[] val,
			boolean[] isNull,
			boolean[] isChanged,
			int member,
			int valueIndex,
			int keyValMode);


	public static native void ReadFloat64Member(long readerHandle,
			double[] val,
			boolean[] isNull,
			boolean[] isChanged,
			int member,
			int valueIndex,
			int keyValMode);

	public static native void ReadBooleanMember(long readerHandle,
			boolean[] val,
			boolean[] isNull,
			boolean[] isChanged,
			int member,
			int valueIndex,
			int keyValMode);

	public static native void ReadStringMember(long readerHandle,
			String[] val,
			boolean[] isNull,
			boolean[] isChanged,
			int member,
			int valueIndex,
			int keyValMode);

	public static native void ReadHashedMember(long readerHandle,
			long[] val,
			String[] optionalStr,
			boolean[] isNull,
			boolean[] isChanged,
			int member,
			int valueIndex,
			int keyValMode);

	public static native void ReadEntityIdMember(long readerHandle,
			long[] typeVal, long[] instanceVal, String[] optionalString,
			boolean[] isNull,
			boolean[] isChanged,
			int member,
			int valueIndex,
			int keyValMode);

	public static native void ReadBinaryMember(long readerHandle,
			ByteBuffer[] val,
			boolean[] isNull,
			boolean[] isChanged,
			int member,
			int valueIndex,
			int keyValMode);

	public static native void ReadObjectMember(long readerHandle,
			ByteBuffer[] val,
			boolean[] isNull,
			boolean[] isChanged,
			int member,
			int valueIndex,
			int keyValMode);

	//************************************************************************************
	//* Write operations
	//************************************************************************************
	public static native long CreateBlobWriter(long typeId);
	public static native long CreateBlobWriterFromBlob(ByteBuffer blob);
	public static native long CreateBlobWriterFromReader(long readerHandle);
	public static native void DeleteBlobWriter(long writerHandle);
	public static native int CalculateBlobSize(long writerHandle);
	public static native void WriteBlob(long writerHandle, ByteBuffer blob);
	public static native void WriteAllChangeFlags(long writerHandle, boolean changed);
	public static native void WriteChangeFlag(long writerHandle, int member, int index, boolean changed);
	public static native boolean MarkChanges(long originalReader, long currentWriter);

	public static native void WriteInt32Member(long writerHandle, int val, boolean isNull, boolean isChanged,
			int member, int arrayIndex, int keyValMode);

	public static native void WriteInt64Member(long writerHandle, long val, boolean isNull, boolean isChanged,
			int member, int arrayIndex, int keyValMode);

	public static native void WriteFloat32Member(long writerHandle, float val, boolean isNull, boolean isChanged,
			int member, int arrayIndex, int keyValMode);

	public static native void WriteFloat64Member(long writerHandle, double val, boolean isNull, boolean isChanged,
			int member, int arrayIndex, int keyValMode);

	public static native void WriteBooleanMember(long writerHandle, boolean val, boolean isNull, boolean isChanged,
			int member, int arrayIndex, int keyValMode);

	public static native void WriteStringMember(long writerHandle, String val, boolean isNull, boolean isChanged,
			int member, int arrayIndex, int keyValMode);

	public static native void WriteHashedMember(long writerHandle, long hash, String str, boolean isNull, boolean isChanged,
			int member, int arrayIndex, int keyValMode);

	public static native void WriteEntityIdMember(long writerHandle, long typeVal, long instanceVal, String instanceString, boolean isNull, boolean isChanged,
			int member, int arrayIndex, int keyValMode);

	public static native void WriteBinaryMember(long writerHandle, ByteBuffer val, int size, boolean isNull, boolean isChanged,
			int member, int arrayIndex, int keyValMode);

	public static native void WriteObjectMember(long writerHandle, ByteBuffer blob, boolean isNull, boolean isChanged,
			int member, int arrayIndex, int keyValMode);

	//************************************************************************************
	//* Functions for library exceptions
	//************************************************************************************
	public static native void GetAndClearException(long [] exceptionId,
			String[] description,
			boolean [] wasSet);

	public static native void SetException(long exceptionId, String description);

	//************************************************************************************
	//* Special stuff
	//************************************************************************************
	//Get all the dou directories defined in typesystem.ini
	public static native String[] GetDouDirectories();

}
