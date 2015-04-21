// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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
final class Kernel {
    /** dots_kernel enum */
    static public enum DotsC_PropertyMappingKind {
        MappedToNull,
        MappedToMember,
        MappedToParameter
    }

    /** dots_kernel enum */
    static public enum DotsC_ErrorCode {
        NoError,
        ReadOnlyProperty,
        UnableToDereferenceProperty,
        IllegalValue
    }

    static {
        //load the jni dll
        System.loadLibrary("dots_java_jni");
    }

    //********************************************************
    //* Base operations on blobs
    //********************************************************

    public static native long GetTypeId(ByteBuffer blob);
    public static native int GetSize(ByteBuffer blob);
    public static native boolean IsAnythingChanged(ByteBuffer blob);

    //********************************************************
    //* Type information operations
    //********************************************************
    public static native int NumberOfTypeIds();
    public static native int NumberOfClasses();
    public static native int NumberOfProperties();
    public static native int NumberOfEnumerations();

    public static native void GetAllTypeIds(long buf [],
                                                  int bufSize,
                                                  int size []);
    public static native boolean TypeExists(long typeId);
    public static native boolean IsClass(long typeId);
    public static native boolean IsProperty(long typeId);
    public static native boolean IsEnumeration(long typeId);
    public static native boolean IsException(long typeId);
    public static native long TypeIdFromName(String name);
    public static native String GetTypeName(long typeId);
    public static native int GetNumberOfEnumerationValues(long enumId);
    public static native String GetEnumerationValueName(long enumId, int enumVal);
    public static native int GetEnumerationValueFromName(long enumId, String enumValueName);


    //***********************************************************
    //* Functions for retrieving member info about object types
    //***********************************************************
    public static native int GetNumberOfMembers(long typeId);
    public static native int GetMemberId(long typeId, String memberName);
    public static native String GetMemberName(long typeId, int member);
    public static native long GetComplexMemberTypeId(long typeId, int member);
    public static native void GetMemberInfo(long typeId,
                                            int member,
                                            int memberType[],
                                            String memberName[],
                                            long memberTypeId[],
                                            int stringLength[],
                                            boolean isArray[],
                                            int arrayLength[]);
    public static native int GetMemberArraySize(long typeId, int member);
    public static native int GetStringMemberMaxLength(long typeId, int member);
    public static native int GetMemberArraySizeProperty(long classId, long propId, int member);
    public static native String GetMemberTypeName(long typeId, int member);

    //***********************************************************************
    //* Functions retrieving definitions of parameter values in object types
    //***********************************************************************
    public static native int GetNumberOfParameters(long typeId);
    public static native int GetParameterId(long typeId, String parameterName);
    public static native String GetParameterName(long typeId, int parameter);
    public static native int GetParameterType(long typeId, int parameter);
    public static native String GetParameterTypeName(long typeId, int parameter);
    public static native int GetParameterArraySize(long typeId, int parameter);



    //************************************************************************************
    //* Type compatibility
    //************************************************************************************
    public static native boolean IsOfType(long typeId, long ofTypeId);
    public static native long [] GetCompleteType(long typeId);
    public static native long GetParentType(long typeId);
    public static native void HasProperty(long classTypeId, long propertyTypeId, boolean hasProperty[], boolean isInherited[]);

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

    public static native void GetBooleanParameter(long typeId, int parameter, int index, boolean val []);
    public static native void GetInt32Parameter(long typeId, int parameter, int index, int val []);
    public static native void GetInt64Parameter(long typeId, int parameter, int index, long val []);
    public static native void GetFloat32Parameter(long typeId, int parameter, int index, float val []);
    public static native void GetFloat64Parameter(long typeId, int parameter, int index, double val []);
    public static native void GetStringParameter(long typeId, int parameter, int index, String val []);
    public static native void GetHashedIdParameter(long typeId, int parameter, int index, long [] hashVal, String [] strVal);
    public static native void GetEntityIdParameter(long typeId, int parameter, int index, long [] typeIdOut, long [] instanceId, String [] strVal);
    public static native void GetObjectParameter(long typeId, int parameter, int index, ByteBuffer blob []);
    public static native void GetBinaryParameter(long typeId, int parameter, int index, ByteBuffer val []);

    //************************************************************************************
    //* Functions for retrieving member values
    //************************************************************************************
    public static native boolean IsChangedMember(ByteBuffer blob, int member, int index);

    public static native void GetBooleanMember(ByteBuffer blob, int member, int index, boolean value[], boolean isNull[], boolean isChanged[]);
    public static native void GetInt32Member(ByteBuffer blob, int member, int index, int value[], boolean isNull[], boolean isChanged[]);
    public static native void GetInt64Member(ByteBuffer blob, int member, int index, long value[], boolean isNull[], boolean isChanged[]);
    public static native void GetFloat32Member(ByteBuffer blob, int member, int index, float value[], boolean isNull[], boolean isChanged[]);
    public static native void GetFloat64Member(ByteBuffer blob, int member, int index, double value[], boolean isNull[], boolean isChanged[]);
    public static native void GetStringMember(ByteBuffer blob, int member, int index, String value [], boolean isNull[], boolean isChanged[]);
    public static native void GetBinaryMember(ByteBuffer blob, int member, int index, ByteBuffer val [], boolean isNull[], boolean isChanged[]);
    public static native void GetHashedIdMember(ByteBuffer blob, int member, int index, long hashVal [], String strVal [], boolean isNull[], boolean isChanged[]);
    public static native void GetEntityIdMember(ByteBuffer blob, int member, int index, long typeId[], long instanceId [], String strVal [], boolean isNull[], boolean isChanged[]);
    public static native void GetObjectMember(ByteBuffer blob, int member, int index, ByteBuffer[] childBlob, boolean isNull[], boolean isChanged[]);

    //************************************************************************************
    //* Functions for setting member values
    //************************************************************************************

    public static native void SetNullMember(ByteBuffer blob, int member, int index);
    public static native void SetBooleanMemberInPreallocated(boolean val, boolean isNull, boolean isChanged, ByteBuffer blob, int member, int index);
    public static native void SetInt32MemberInPreallocated(int val, boolean isNull, boolean isChanged, ByteBuffer blob, int member, int index);
    public static native void SetInt64MemberInPreallocated(long val, boolean isNull, boolean isChanged, ByteBuffer blob, int member, int index);
    public static native void SetFloat32MemberInPreallocated(float val, boolean isNull, boolean isChanged, ByteBuffer blob, int member, int index);
    public static native void SetFloat64MemberInPreallocated(double val, boolean isNull, boolean isChanged, ByteBuffer blob, int member, int index);
    public static native void SetHashedIdMemberInPreallocated(long hashVal,
                                                              String strVal,
                                                              int stringLength,
                                                              boolean isNull,
                                                              boolean isChanged,
                                                              ByteBuffer blob,
                                                              int member,
                                                              int index,
                                                              int beginningOfUnused []);

    public static native void SetEntityIdMemberInPreallocated(long typeId,
                                                              long instanceId,
                                                              String strVal,
                                                              int stringLength,
                                                              boolean isNull,
                                                              boolean isChanged,
                                                              ByteBuffer blob,
                                                              int member,
                                                              int index,
                                                              int beginningOfUnused []);


    //************************************************************************************
    //* Functions for retrieving property member values
    //************************************************************************************

    public static native void GetBooleanPropertyParameter(long typeId, long propertyId, int member, int index, boolean val []);
    public static native void GetInt32PropertyParameter(long typeId, long propertyId, int member, int index, int val []);
    public static native void GetInt64PropertyParameter(long typeId, long propertyId, int member, int index, long val []);
    public static native void GetFloat32PropertyParameter(long typeId, long propertyId, int member, int index, float val []);
    public static native void GetFloat64PropertyParameter(long typeId, long propertyId, int member, int index, double val []);
    public static native void GetStringPropertyParameter(long typeId, long propertyId, int member, int index, String val []);
    public static native void GetHashedIdPropertyParameter(long typeId, long propertyId, int member, int index, long [] hashVal, String [] strVal);
    public static native void GetEntityIdPropertyParameter(long typeId, long propertyId, int member, int index, long [] typeIdOut, long [] instanceId, String [] strVal);
    public static native void GetObjectPropertyParameter(long typeId, long propertyId, int member, int index, ByteBuffer blob []);
    public static native void GetBinaryPropertyParameter(long typeId, long propertyId, int member, int index, ByteBuffer val []);


    public static native void GetPropertyMappingKind(long typeId,
                                                     long propertyId,
                                                     int member,
                                                     int mappingKind[],
                                                     int errorCode[]);

    public static native void GetClassMemberReference(long typeId,
                                                      long propertyId,
                                                      int member,
                                                      ByteBuffer classMemberReference[]);
    //************************************************************************************
    //* Functions for library exceptions
    //************************************************************************************

    public static native void GetAndClearException(long [] exceptionId,
                                                   String[] description,
                                                   boolean [] wasSet);

    public static native void SetException(long exceptionId, String description);


    //*********************************
    //* For real classes
    //*********************************
    public static native int GetInitialSize(long typeId);

    public static native void FormatBlob(java.nio.ByteBuffer blob,
                                         int blobSize,
                                         long typeId,
                                         int beginningOfUnused []);

    public static native void CreateObjectMember(ByteBuffer insideBlob,
                                                 int blobSize,
                                                 long typeId,
                                                 int member,
                                                 int index,
                                                 boolean isChanged,
                                                 int beginningOfUnused []);

    public static native void CreateStringMember(ByteBuffer insideBlob,
                                                       int stringLength, //remember null-termination!
                                                       int member,
                                                       int index,
                                                       boolean isChanged,
                                                       int beginningOfUnused []);

    public static native void CreateBinaryMember(ByteBuffer insideBlob,
                                                 int binaryLength,
                                                 int member,
                                                 int index,
                                                 boolean isChanged,
                                                 int beginningOfUnused []);

    public static native long GetEnumerationChecksum(long typeId);


    //************************************************************************************
    //* Functions for hash numbers
    //************************************************************************************
    public static native long GenerateRandom64();
    public static native long Generate64(String id);


    //Get all the dou directories defined in typesystem.ini
    public static native String[] GetDouDirectories();
    
    //===========================================================
    //********************************************************
    //* Static type information operations
    //********************************************************
    public static native int DotsC_NumberOfTypeIds();
    public static native int DotsC_NumberOfClasses();
    public static native int DotsC_NumberOfProperties();
    public static native int DotsC_NumberOfEnumerations();
    public static native int DotsC_NumberOfExceptions();
    public static native void DotsC_GetAllTypeIds(long buf[], int bufSize, int size[]);
    public static native boolean DotsC_TypeExists(long id);
    public static native boolean DotsC_IsClass(long id);
    public static native boolean DotsC_IsProperty(long id);
    public static native boolean DotsC_IsEnumeration(long id);
    public static native boolean DotsC_IsException(long id);
    public static native long DotsC_TypeIdFromName(String str);
    public static native String DotsC_GetTypeName(long id);
    public static native String DotsC_MemberTypeName(int memberType);
    public static native int DotsC_GetNumberOfEnumerationValues(long enumId);
    public static native String DotsC_GetEnumerationValueName(long enumId, int enumVal);
    public static native int DotsC_EnumerationValueFromName(long enumId, String enumValueName);
    public static native long DotsC_GetEnumerationChecksum(long typeId);
    
    //********************************************************
    //* Functions for retrieving member info about object types
    //********************************************************
    public static native int DotsC_GetNumberOfMembers(long id);
    public static native int DotsC_GetMemberId(long id, String memberName);

    public static native void DotsC_GetMemberInfo(  long id,                //in
                                                    int member,            //in
                                                    int[] memberType,      //out
                                                    String[] memberName,   //out
                                                    long[] complexType,   //out
                                                    int[] stringLength,  //out
                                                    int[] collectionType, //out
                                                    int[] arrLength );   //out

    public static native int DotsC_GetMemberArraySizeProperty(long classId, long propertyId, int propertyMember);

    public static native int DotsC_GetStringMemberMaxLengthProperty(long classId, long propertyId, int propertyMember);

    //********************************************************
    //* Functions handling parameters
    //********************************************************
    public static native int DotsC_GetNumberOfParameters(long id);

    public static native int DotsC_GetParameterId(long id, String parameterName);

    public static native void DotsC_GetParameterInfo(long typeId,
                                                       int parameter,
                                                       int[] memberType,
                                                       String[] parameterName,
                                                       long[] complexType,
                                                       int[] collectionType,
                                                       int[] numberOfValues);

   //************************************************************************************
   //* Type compatibility
   //************************************************************************************
   public static native boolean DotsC_IsOfType(long theType, long ofType);

   public static native long [] DotsC_GetCompleteType(long typeId);

   public static native long DotsC_GetParentType(long theType);

   public static native void DotsC_HasProperty(long classTypeId, long propertyTypeId, boolean[] hasProperty, boolean[] isInherited);

   public static native int DotsC_GetPropertyMappingKind(long classTypeId, long propertyTypeId, int propertyMember);

   public static native void DotsC_GetClassMemberReference(long typeId,
                                                           long propertyId,
                                                           int member,
                                                           ByteBuffer classMemberReference[]);

   public static native void DotsC_GetPropertyParameterReference(long typeId,
                                                                 long propertyId,
                                                                 int propertyMember,
                                                                 int index,
                                                                 int[] paramId, //out
                                                                 int[] paramValueIndex); //out

    //************************************************************************************
    //* Serialization
    //************************************************************************************
    public static native String DotsC_BlobToXml(ByteBuffer blob);

    public static native void DotsC_XmlToBlob(ByteBuffer blob [],
                                        ByteBuffer deleter [], //a very nasty way of passing a pointer back and forth
                                        String xmlSource);

    public static native String DotsC_BlobToJson(ByteBuffer blob);

    public static native void DotsC_JsonToBlob(ByteBuffer blob [],
                                         ByteBuffer deleter [], //a very nasty way of passing a pointer back and forth
                                         String jsonSource);

    public static native void DotsC_InvokeDeleter(ByteBuffer deleter, ByteBuffer toDelete);

    public static native String DotsC_BinaryToBase64(ByteBuffer binarySource, int size);
    public static native byte[] DotsC_Base64ToBinary(String base64Source);

    //************************************************************************************
    //* Functions for retrieval of parameters
    //************************************************************************************
    public static native void DotsC_GetBooleanParameter(long id,
                                                          int parameter,
                                                          int index,
                                                          boolean[] val);

    public static native void DotsC_GetEnumerationParameter(long id,
                                                              int parameter,
                                                              int index,
                                                                int[] keyValMode,
                                                              int[] val);

    public static native void DotsC_GetInt32Parameter(long id,
                                                        int parameter,
                                                        int index,
                                                                int[] keyValMode,
                                                        int[] val);

    public static native void DotsC_GetInt64Parameter(long id,
                                                        int parameter,
                                                        int index,
                                                                int[] keyValMode,
                                                        long[] val);

    public static native void DotsC_GetFloat32Parameter(long id,
                                                          int parameter,
                                                          int index,
                                                          float[] val);

    public static native void DotsC_GetFloat64Parameter(long id,
                                                          int parameter,
                                                          int index,
                                                          double[] val);

    public static native void DotsC_GetStringParameter(long id,
                                                         int parameter,
                                                         int index,
                                                        int[] keyValMode,
                                                         String[] val);

    public static native void DotsC_GetTypeIdParameter(long id,
                                                         int parameter,
                                                         int index,
                                                         int[] keyValMode,
                                                         long[] val);

    public static native void DotsC_GetHashedIdParameter(long id,
                                                           int parameter,
                                                           int index,
                                                                   int[] keyValMode,
                                                           long[] hashVal,
                                                           String[] strVal);

    public static native void DotsC_GetEntityIdParameter(long id,
                                                           int parameter,
                                                           int index,
                                                           int[] keyValMode,
                                                           long[] typeIdOut, long[] instanceId, String[] strVal);

    public static native void DotsC_GetObjectParameter(long id,
                                                         int parameter,
                                                         int index,
                                                         ByteBuffer blob []);

    public static native void DotsC_GetBinaryParameter(long id,
                                                         int parameter,
                                                         int index,
                                                         ByteBuffer blob []);

    public static native int DotsC_DictionaryInt32KeyToIndex(long typeId,
                                                            int parameter,
                                                            int key);

    public static native int DotsC_DictionaryInt64KeyToIndex(long typeId,
                                                           int parameter,
                                                           long key);

    public static native int DotsC_DictionaryStringKeyToIndex(long typeId,
                                                            int parameter,
                                                            String key);

    public static native int DotsC_DictionaryEntityIdKeyToIndex(long typeId,
                                                                  int parameter,
                                                                  long keyType, long keyInstance);


      //********************************************************
      //* Operations on blobs
      //********************************************************
      public static native long DotsC_GetTypeId(ByteBuffer blob);
      public static native int DotsC_GetSize(ByteBuffer blob);

      //************************************************************************************
      //* Read operations
      //************************************************************************************
      public static native long DotsC_CreateBlobReader(ByteBuffer blob);

      public static native void DotsC_DeleteBlobReader(long handle);

      public static native int DotsC_GetNumberOfMemberValues(long readerHandle, int member);

      public static native void DotsC_ReadMemberStatus(long readerHandle,
                                                         boolean[] isNull,
                                                         boolean[] isChanged,
                                                         int member,
                                                         int valueIndex);

      public static native void DotsC_ReadInt32Member(long readerHandle,
                                                        int[] val,
                                                        boolean[] isNull,
                                                        boolean[] isChanged,
                                                        int member,
                                                        int valueIndex,
                                                        int keyValMode);

      public static native void DotsC_ReadInt64Member(long readerHandle,
                                                        long[] val,
                                                        boolean[] isNull,
                                                        boolean[] isChanged,
                                                        int member,
                                                        int valueIndex,
                                                        int keyValMode);

      public static native void DotsC_ReadFloat32Member(long readerHandle,
                                                        float[] val,
                                                        boolean[] isNull,
                                                        boolean[] isChanged,
                                                        int member,
                                                        int valueIndex,
                                                        int keyValMode);


      public static native void DotsC_ReadFloat64Member(long readerHandle,
                                                          double[] val,
                                                          boolean[] isNull,
                                                          boolean[] isChanged,
                                                          int member,
                                                          int valueIndex,
                                                          int keyValMode);

      public static native void DotsC_ReadBooleanMember(long readerHandle,
                                                          boolean[] val,
                                                          boolean[] isNull,
                                                          boolean[] isChanged,
                                                          int member,
                                                          int valueIndex,
                                                          int keyValMode);

      public static native void DotsC_ReadStringMember(long readerHandle,
                                                          String[] val,
                                                          boolean[] isNull,
                                                          boolean[] isChanged,
                                                          int member,
                                                          int valueIndex,
                                                          int keyValMode);

      public static native void DotsC_ReadHashedMember(long readerHandle,
                                                         long[] val,
                                                         String[] optionalStr,
                                                         boolean[] isNull,
                                                         boolean[] isChanged,
                                                         int member,
                                                         int valueIndex,
                                                         int keyValMode);

      public static native void DotsC_ReadEntityIdMember(long readerHandle,
                                                         long[] typeVal, long[] instanceVal, String[] optionalString,
                                                               boolean[] isNull,
                                                               boolean[] isChanged,
                                                               int member,
                                                               int valueIndex,
                                                               int keyValMode);

      public static native void DotsC_ReadBinaryMember(long readerHandle,
                                                           ByteBuffer[] val,
                                                           boolean[] isNull,
                                                           boolean[] isChanged,
                                                           int member,
                                                           int valueIndex,
                                                           int keyValMode);

      public static native void DotsC_ReadObjectMember(long readerHandle,
                                                         ByteBuffer[] val,
                                                         boolean[] isNull,
                                                         boolean[] isChanged,
                                                         int member,
                                                         int valueIndex,
                                                         int keyValMode);

     //************************************************************************************
     //* Write operations
     //************************************************************************************
     public static native long DotsC_CreateBlobWriter(long typeId);
     public static native long DotsC_CreateBlobWriterFromBlob(ByteBuffer blob);
     public static native long DotsC_CreateBlobWriterFromReader(long readerHandle);
     public static native void DotsC_DeleteBlobWriter(long writerHandle);
     public static native int DotsC_CalculateBlobSize(long writerHandle);
     public static native void DotsC_WriteBlob(long writerHandle, ByteBuffer[] blob);
     public static native void DotsC_WriteAllChangeFlags(long writerHandle, boolean changed);

     public static native void DotsC_WriteChangeFlag(long writerHandle, int member,int index, boolean changed);

     public static native boolean DotsC_MarkChanges(long originalReader, long currentWriter);

     public static native void DotsC_WriteInt32Member(long writerHandle, int val, boolean isNull, boolean isChanged,
                                                        int member, int arrayIndex, int keyValMode);

     public static native void DotsC_WriteInt64Member(long writerHandle, long val, boolean isNull, boolean isChanged,
                                                        int member, int arrayIndex, int keyValMode);

     public static native void DotsC_WriteFloat32Member(long writerHandle, float val, boolean isNull, boolean isChanged,
                                                          int member, int arrayIndex, int keyValMode);

     public static native void DotsC_WriteFloat64Member(long writerHandle, double val, boolean isNull, boolean isChanged,
                                                          int member, int arrayIndex, int keyValMode);

     public static native void DotsC_WriteBooleanMember(long writerHandle, boolean val, boolean isNull, boolean isChanged,
                                                          int member, int arrayIndex, int keyValMode);

     public static native void DotsC_WriteStringMember(long writerHandle, String val, boolean isNull, boolean isChanged,
                                                         int member, int arrayIndex, int keyValMode);

     public static native void DotsC_WriteHashedMember(long writerHandle, long hash, String str, boolean isNull, boolean isChanged,
                                                         int member, int arrayIndex, int keyValMode);

     public static native void DotsC_WriteEntityIdMember(long writerHandle, long typeVal, long instanceVal, String instanceString, boolean isNull, boolean isChanged,
                                                           int member, int arrayIndex, int keyValMode);

     public static native void DotsC_WriteBinaryMember(long writerHandle, ByteBuffer val, int size, boolean isNull, boolean isChanged,
                                                         int member, int arrayIndex, int keyValMode);

     public static native void DotsC_WriteObjectMember(long writerHandle, ByteBuffer blob, boolean isNull, boolean isChanged,
                                                         int member, int arrayIndex, int keyValMode);

}
