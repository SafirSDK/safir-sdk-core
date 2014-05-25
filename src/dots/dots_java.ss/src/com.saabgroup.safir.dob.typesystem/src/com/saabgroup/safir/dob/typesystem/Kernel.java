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

    public static native String [] GetGeneratedJars();
}
