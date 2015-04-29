/******************************************************************************
*
* Copyright Consoden AB AB, 2005-2015 (http://safir.sourceforge.net)
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
#ifndef _dots_kernel_h
#define _dots_kernel_h

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef dots_kernel_EXPORTS
#  define DOTS_KERNEL_API SAFIR_HELPER_DLL_EXPORT
#else
#  define DOTS_KERNEL_API SAFIR_HELPER_DLL_IMPORT
#endif
#define DOTS_KERNEL_LOCAL SAFIR_HELPER_DLL_LOCAL

#include <Safir/Dob/Typesystem/LanguageInterfaceDefs.h>

/**
 * This is the C main interface to the TypeSystem.
 */
extern "C"
{
    typedef void (*DotsC_BytePointerDeleter)(char * &);
    typedef DotsC_Int64 DotsC_Handle;
    typedef enum {DotsC_KeyMode=0, DotsC_ValueMode=1} DotsC_KeyValMode;

    //********************************************************
    //* Static type information operations
    //********************************************************
    // Function:    DotsC_NumberOfTypeIds
    // Parameters:  -
    // Returns:     number of existing type ids
    // Comments:    Get the number of type ids defined in the system.
    DOTS_KERNEL_API DotsC_Int32 DotsC_NumberOfTypeIds();

    // Function:    DotsC_NumberOfClasses
    // Parameters:  -
    // Returns:     number of existing classes
    // Comments:    Get the number of classes defined in the system.
    DOTS_KERNEL_API DotsC_Int32 DotsC_NumberOfClasses();

    // Function:    DotsC_NumberOfProperties
    // Parameters:  -
    // Returns:     number of existing properties
    // Comments:    Get the number of classes defined in the system.
    DOTS_KERNEL_API DotsC_Int32 DotsC_NumberOfProperties();

    // Function:    DotsC_NumberOfEnumerations
    // Parameters:  -
    // Returns:     number of existing enumeration types
    // Comments:    Get the number of enum types defined in the system.
    DOTS_KERNEL_API DotsC_Int32 DotsC_NumberOfEnumerations();

    // Function:    DotsC_NumberOfExceptions
    // Parameters:  -
    // Returns:     number of existing exception types
    // Comments:    Get the number of exception types defined in the system.
    DOTS_KERNEL_API DotsC_Int32 DotsC_NumberOfExceptions();

    // Function:    DotsC_GetAllTypeIds
    // Parameters:  buf - pointer to an array of size 'bufSize'
    //              bufSize - size of buf
    //              size - number of inserted typeId into buf
    // Returns:     -
    // Comments:    Get a list of all type id's that exists in the system.
    //              If you want all types the buffer size should be equal to
    //              DotsC_NumberOfTypeIds()
    DOTS_KERNEL_API void DotsC_GetAllTypeIds(DotsC_TypeId* buf,
                                             DotsC_Int32 bufSize,
                                             DotsC_Int32& size);

    // Function:    DotsC_TypeExists
    // Parameters:  typeId - id of class
    // Returns:     true if the type exists
    // Comments:    Check if type with specified type id exist
    DOTS_KERNEL_API bool DotsC_TypeExists(DotsC_TypeId typeId);

    // Function:    DotsC_IsClass
    // Parameters:  typeId - id of class
    // Returns:     true if the type exists as a class
    // Comments:    Check if type id belongs to a existing class
    DOTS_KERNEL_API bool DotsC_IsClass(DotsC_TypeId typeId);

    // Function:    DotsC_IsProperty
    // Parameters:  typeId - id of property
    // Returns:     true if the type exists as a property
    // Comments:    Check if type id belongs to a existing property
    DOTS_KERNEL_API bool DotsC_IsProperty(DotsC_TypeId typeId);

    // Function:    DotsC_IsEnumeration
    // Parameters:  typeId - id of enumeration type
    // Returns:     true if the type exists as an enumeration type
    // Comments:    Check if type id belongs to a existing enumeration type
    DOTS_KERNEL_API bool DotsC_IsEnumeration(DotsC_TypeId typeId);

    // Function:    DotsC_IsException
    // Parameters:  typeId - id of exception type
    // Returns:     true if the type exists as an exception type
    // Comments:    Check if type id belongs to a existing enumeration type
    DOTS_KERNEL_API bool DotsC_IsException(DotsC_TypeId typeId);

    // Function:    DotsC_TypeIdFromName
    // Parameters:  typeName -  The name shall contain namespaces and class name
    //                          with '.' as separator, example "MyNamespace1.MyNamespace2.MyClass"
    // Returns:     type id
    // Comments:    Calculates the type id for the given type name.
    DOTS_KERNEL_API DotsC_TypeId DotsC_TypeIdFromName(const char* typeName);

    // Function:    DotsC_GetTypeName
    // Parameters:  typeId -    id of type
    // Returns:     name of the type
    // Comments:    Gets the name associated with the specified type id
    DOTS_KERNEL_API const char* DotsC_GetTypeName(DotsC_TypeId typeId);

    // Function:    DotsC_MemberTypeName
    // Parameters:  memberType - member type to convert to readable name as defined in dou-files.
    // Returns:     name of member type
    // Comments:    Gets the name associated with the specified member type, as it is defined in dou-files.
    DOTS_KERNEL_API const char* DotsC_MemberTypeName(DotsC_MemberType memberType);

    // Function:    DotsC_GetNumberOfEnumerationValues
    // Parameters:  enumId -    id of enum type
    // Returns:     Number of enumeration values. -1 is returned if the type does not exist.
    // Comments:    Get the number of enumeration values the specified enumeration type has.
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetNumberOfEnumerationValues(DotsC_TypeId enumId);

    // Function:    DotsC_EnumerationValueName
    // Parameters:  enumId -    id of enum type
    //              enumVal -   the enumeration value
    // Returns:     String representation of the enumeration value.
    // Comments:    Get the string representation of the specified value for a enumeration type
    DOTS_KERNEL_API const char* DotsC_GetEnumerationValueName(DotsC_TypeId enumId, DotsC_EnumerationValue enumVal);

    // Function:    DotsC_EnumerationValueFromName
    // Parameters:  enumId          -   id of enum type
    //              enumValueName   -   string representation of the desired value
    // Returns:     Integer value for the enumeration value name
    // Comments:    Get integer value associated with the enumeration value for the specified enumeration type.
    DOTS_KERNEL_API DotsC_EnumerationValue DotsC_EnumerationValueFromName(DotsC_TypeId enumId, const char* enumValueName);

    DOTS_KERNEL_API void DotsC_GetEnumerationChecksum(const DotsC_TypeId typeId,
                                                      DotsC_TypeId & checksum);

    //  Functions for retrieving member info about object types
    //-----------------------------------------------------------
    // Function:    DotsC_GetNumberOfMembers
    // Parameters:  typeId - id of class or property
    // Returns:     number of members of the type, -1 if type does not exist
    // Comments:    Get the number of members for a class or property. Parameters are not included.
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetNumberOfMembers(DotsC_TypeId typeId);

    // Function:    DotsC_GetMemberId
    // Parameters:  typeId      -   id of class or property
    //              memberName  -   name of member as specified in xml description, case sensitive,
    // Returns:     id of the member
    // Comments:    Gets the member id for a member.
    DOTS_KERNEL_API DotsC_MemberIndex DotsC_GetMemberId(DotsC_TypeId typeId, const char* memberName);

    // Function:    DotsC_GetMemberInfo
    // Parameters:  typeId       -   id of class or property
    //              member       -   id of member
    //              memberType   -   the type of the member, out parameter.
    //              memberName   -   the name of the member, out parameter.
    //              complexType  -   if memberType is object or enumeration, this is the typeId of that type. out parameter.
    //              stringLength -   if memberType is string and the type is a class then this is the length of the string. Out parameter
    //              isArray      -   true if member is an array and type id is a class. Not applicable if type id is a property. Out parameter.
    //              arrayLength  -   maximum capacity of array if the member is an array. Not applicable if type id is a property.
    // Returns:     -
    // Comments:    Returns information about a specific data type member.
    //              If the type or member does not exist memberName is set to NULL and
    //              complexType, stringLength and arrayLength are set to -1.
    DOTS_KERNEL_API void DotsC_GetMemberInfo(DotsC_TypeId typeId,  //in
                                             DotsC_MemberIndex member,  //in
                                             DotsC_MemberType& memberType,//out
                                             const char*& memberName,           //out
                                             DotsC_TypeId& complexType,   //out
                                             DotsC_Int32& stringLength,   //out
                                             DotsC_CollectionType& collectionType, //out
                                             DotsC_Int32& arraySize);   //out

    // Function:    DotsC_GetMemberArraySizeProperty
    // Parameters:  classId         -   id of the class with a property
    //              propertyId      -   id of a property supported by the class
    //              propertyMember  -   the property member
    // Returns:     The array size of the property member.
    //              -1 if there is no such type or array or mapping defined
    // Comments:    Returns the array size of a property member.
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetMemberArraySizeProperty(DotsC_TypeId classId,
                                                                 DotsC_TypeId propertyId,
                                                                 DotsC_MemberIndex propertyMember);
    // Function:    DotsC_GetStringMemberMaxLengthProperty
    // Parameters:  classId         -   id of the class with a property
    //              propertyId      -   id of a property supported by the class
    //              propertyMember  -   the property member
    // Returns:     The max string length of the property member.
    //              -1 if there is no such type or mapping defined
    // Comments:    Returns the max string length of a property member.
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetStringMemberMaxLengthProperty(DotsC_TypeId classId,
                                                                       DotsC_TypeId propertyId,
                                                                       DotsC_MemberIndex propertyMember);

    // Parameters
    //-----------------------------------------------------------
    // Function:    DotsC_GetNumberOfParameters
    // Parameters:  typeId  -   id of class
    // Returns:     the number of parameters, -1 if type does not exist
    // Comments:    Gets the number of parameters defined in a class.
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetNumberOfParameters(DotsC_TypeId typeId);

    // Function:    DotsC_GetParameterId
    // Parameters:  typeId          -   id of class
    //              parameterName   -   name of the parameter as defined in the xml description, case sensitive.
    // Returns:     id of the parameter, -1 if it does not exist
    // Comments:    Gets id of a parameter.
    DOTS_KERNEL_API DotsC_ParameterIndex  DotsC_GetParameterId(DotsC_TypeId typeId, const char* parameterName);

    // Function:    DotsC_GetParameterType
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    // Returns:     type of the parameter
    // Comments:    Gets the type of a parameter.
    //              If the parameter does not exist the return value is undefined. Use GetParameterId
    //              to obtain a parameter that is guaranteed to exist.
    DOTS_KERNEL_API void DotsC_GetParameterInfo(DotsC_TypeId typeId,
                                                DotsC_ParameterIndex parameter,
                                                DotsC_MemberType& memberType,
                                                const char*& parameterName,
                                                DotsC_TypeId& complexTypeId,
                                                DotsC_CollectionType& collectionType,
                                                DotsC_Int32& numberOfValues);

    // Type compatibility
    //------------------------------------------------------------------------
    // Function:    DotsC_IsOfType
    // Parameters:  type    -   the type to check if it is of another type
    //              ofType  -   the type to compare to
    // Returns:     true of type is equal to ofType or if type inherits from ofType
    // Comments:    Checks if type is an instance of the ofType, direct or by inheritance
    DOTS_KERNEL_API bool DotsC_IsOfType(DotsC_TypeId type, DotsC_TypeId ofType);

    // Function:    DotsC_GetCompleteType
    // Parameters:  type        -   the base type
    //              buf         -   array of type id's
    //              bufSize     -   size of buf
    //              noResults   -   number of items that were inserted into 'buf', out parameter
    // Returns:     -
    // Comments:    Returns a list of all type id's that is of the given type by inheritance.
    //              The type 'type' will also be inserted in the list.
    //              If for example type A is the base class for type B and type C, GetCompleteType for
    //              type=A will return a list with A, B and C.
    DOTS_KERNEL_API void DotsC_GetCompleteType(DotsC_TypeId type,
                                               DotsC_TypeId* buf,
                                               DotsC_Int32 bufSize,
                                               DotsC_Int32& noResults);

    // Function:    DotsC_GetParentType
    // Parameters:  type        -   the type for which the parent type is requested
    // Returns:     type id for the parent
    // Comments:    Returns the typeId  for the parent class to 'type'. If type==object,
    //              then the object typeId is returned again.
    DOTS_KERNEL_API DotsC_TypeId DotsC_GetParentType(DotsC_TypeId type);

    // Function:    DotsC_HasProperty
    // Parameters:  classDotsC_TypeId     -   The type of the class to checked for a given property.
    //              propertyDotsC_TypeId  -   type of the property
    //              hasProperty     -   out parameter that is true if class has property
    //              isInherited     -   out parameter that indicates whether the property is
    //                                  set on the class itself or whether it was inherited from
    //                                  a parent class.
    // Returns:     -
    // Comments:    Checks objects with 'classDotsC_TypeId' have the property with typeId 'propertyDotsC_TypeId'.
    DOTS_KERNEL_API void DotsC_HasProperty(DotsC_TypeId classTypeId,
                                           DotsC_TypeId propertyTypeId,
                                           bool& hasProperty,
                                           bool& isInherited);

    DOTS_KERNEL_API bool DotsC_GetPropertyMappingKind(const DotsC_TypeId classTypeId,
                                                      const DotsC_TypeId propertyTypeId,
                                                      const DotsC_MemberIndex propertyMember,
                                                      DotsC_PropertyMappingKind & mappingKind);

    DOTS_KERNEL_API void DotsC_GetClassMemberReference(const DotsC_TypeId classTypeId,
                                                       const DotsC_TypeId propertyTypeId,
                                                       const DotsC_MemberIndex propertyMember,
                                                       const DotsC_Int32 * & classMemberReference, //out
                                                       DotsC_Int32 & classMemberReferenceSize); //out

    //Todo: Impelement and consider remove  PropertyParam-methods    
    DOTS_KERNEL_API void DotsC_GetPropertyParameterReference(const DotsC_TypeId typeId,
                                                             const DotsC_TypeId propertyId,
                                                             const DotsC_MemberIndex member,
                                                             const DotsC_Int32 index,
                                                             DotsC_ParameterIndex& paramId, //out
                                                             DotsC_Int32& paramValueIndex); //out


    //************************************************************************************
    //* Serialization
    //************************************************************************************

    // Function:    DotsC_BlobToXmlblob
    // Parameters:  xmlDest     -   result of serialization, will be a xml string. Out parameter
    //              blobSource  -   blob to serialize
    //              bufSize     -   size of xmlDest. string is null terminated.
    //              resultSize  -   if the buffer was big enough for the xml this holds the number
    //                              of bytes written (including null termination)
    //                              if it was too small it holds the size that was needed
    //                              (so resultSize > bufSize ==> try again with bigger buffer)
    // Returns:     -
    // Comments:    Serializes a blob to a xml string.
    DOTS_KERNEL_API void DotsC_BlobToXml(char * const xmlDest,
                                         const char * const blobSource,
                                         const DotsC_Int32 bufSize,
                                         DotsC_Int32 & resultSize);

    // Function:    DotsC_BlobToXml
    // Parameters:  blobDest    -   blob that is the result of the serialization, out parameter
    //              xmlSource   -   xml string to serialize
    // Returns:     -
    // Comments:    Serializes a xml string to a blob.
    DOTS_KERNEL_API void DotsC_XmlToBlob(char * & blobDest,
                                         DotsC_BytePointerDeleter & deleter,
                                         const char * const xmlSource);

    // Function:    DotsC_BlobToJson
    // Parameters:  jsonDest    -   result of serialization, will be a json string. Out parameter
    //              blobSource  -   blob to serialize
    //              bufSize     -   size of jsonDest. string is null terminated.
    //              resultSize  -   if the buffer was big enough for the json this holds the number
    //                              of bytes written (including null termination)
    //                              if it was too small it holds the size that was needed
    //                              (so resultSize > bufSize ==> try again with bigger buffer)
    // Returns:     -
    // Comments:    Serializes a blob to a json string.
    DOTS_KERNEL_API void DotsC_BlobToJson(char * const jsonDest,
                                          const char * const blobSource,
                                          const DotsC_Int32 bufSize,
                                          DotsC_Int32 & resultSize);
    
    // Function:    DotsC_JsonToBlob
    // Parameters:  blobDest    -   blob that is the result of the serialization, out parameter
    //              xmlSource   -   json string to serialize
    // Returns:     -
    // Comments:    Serializes a json string to a blob.
    DOTS_KERNEL_API void DotsC_JsonToBlob(char * & blobDest,
                                          DotsC_BytePointerDeleter & deleter,
                                          const char * const jsonSource);
    
    // Function:    DotsC_BinaryToBase64
    // Parameters:  binarySourceSize    -   number of bytes to be converted to Base64.
    // Returns:     number of characters in the Base64 converted representation. This is the
    //              required size of parameter 'base64Dest' in function DotsC_BinaryToBase64.
    // Comments:    Calculates required size of buffer for conversion of binary data to Base64.
    DOTS_KERNEL_API DotsC_Int32 DotsC_CalculateBase64BufferSize(const DotsC_Int32 binarySourceSize);

    // Function:    DotsC_BinaryToBase64
    // Parameters:  base64Dest    -   buffer to store the base64 encoded result
    //              destSize      -   size of destination buffer
    //              binarySource  -   binary data to convert to base64
    //              sourceSize    -   size of binary source
    //              resultSize    -   if the buffer was big enough this holds the number
    //                                of bytes written
    //                                if it was too small it holds the size that was needed
    //                                (so resultSize > bufSize ==> try again with bigger buffer)
    // Returns:     -
    // Comments:    Conversion of binary data to base64
    DOTS_KERNEL_API void DotsC_BinaryToBase64(char * const base64Dest,
                                              const DotsC_Int32 destSize,
                                              const char * const binarySource,
                                              const DotsC_Int32 sourceSize,
                                              DotsC_Int32 & resultSize);

    // Function:    DotsC_CalculateBinaryBufferSize
    // Parameters:  base64SourceSize    -   number of Base64 characters to be converted to binary format.
    // Returns:     number of bytes in binary format. This is the required size of parameter 'binaryDest'
    //              in function DotsC_Base64ToBinary.
    // Comments:    Calculates required size of buffer for conversion of Base64 data to binary.
    DOTS_KERNEL_API DotsC_Int32 DotsC_CalculateBinaryBufferSize(const DotsC_Int32 base64SourceSize);

    // Function:    DotsC_BinaryToBase64
    // Parameters:  binaryDest    -   buffer to store the binary result
    //              destSize      -   size of destination buffer
    //              base64Source  -   base64 encoded data to convert into binary format
    //              sourceSize    -   size of base64 source
    //              resultSize    -   if the buffer was big enough this holds the number
    //                                of bytes written
    //                                if it was too small it holds the size that was needed
    //                                (so resultSize > bufSize ==> try again with bigger buffer)
    // Returns:     -
    // Comments:    Conversion of Base64 characters to binary format
    DOTS_KERNEL_API void DotsC_Base64ToBinary(char * const binaryDest,
                                              const DotsC_Int32 destSize,
                                              const char * const base64Source,
                                              const DotsC_Int32 sourceSize,
                                              DotsC_Int32 & resultSize);

    //************************************************************************************
    //* Functions for retrieval of parameters
    //************************************************************************************
    // Function:    DotsC_GetBooleanParameter
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter boolean value.
    DOTS_KERNEL_API void DotsC_GetBooleanParameter(const DotsC_TypeId typeId,
                                                   const DotsC_ParameterIndex parameter,
                                                   const DotsC_Int32 index,
                                                   bool & val);

    // Function:    DotsC_GetEnumerationParameter
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter enumeration value.
    DOTS_KERNEL_API void DotsC_GetEnumerationParameter(const DotsC_TypeId typeId,
                                                       const DotsC_ParameterIndex parameter,
                                                       const DotsC_Int32 index,
                                                       const DotsC_KeyValMode keyValMode,
                                                       DotsC_EnumerationValue & val);

    // Function:    DotsC_GetInt32Parameter
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter 32-bits integer value.

    DOTS_KERNEL_API void DotsC_GetInt32Parameter(const DotsC_TypeId typeId,
                                                 const DotsC_ParameterIndex parameter,
                                                 const DotsC_Int32 index,
                                                 const DotsC_KeyValMode keyValMode,
                                                 DotsC_Int32 & val);

    // Function:    DotsC_GetInt64Parameter
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter 64-bits integer value.
    DOTS_KERNEL_API void DotsC_GetInt64Parameter(const DotsC_TypeId typeId,
                                                 const DotsC_ParameterIndex parameter,
                                                 const DotsC_Int32 index,
                                                 const DotsC_KeyValMode keyValMode,
                                                 DotsC_Int64 & val);

    // Function:    DotsC_GetFloat32Parameter
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter 32-bits float value.
    DOTS_KERNEL_API void DotsC_GetFloat32Parameter(const DotsC_TypeId typeId,
                                                   const DotsC_ParameterIndex parameter,
                                                   const DotsC_Int32 index,
                                                   DotsC_Float32 & val);

    // Function:    DotsC_GetFloat64Parameter
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter 64-bits float value.
    DOTS_KERNEL_API void DotsC_GetFloat64Parameter(const DotsC_TypeId typeId,
                                                   const DotsC_ParameterIndex parameter,
                                                   const DotsC_Int32 index,
                                                   DotsC_Float64 & val);

    // Function:    DotsC_GetStringParameter
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter string value.
    DOTS_KERNEL_API void DotsC_GetStringParameter(const DotsC_TypeId typeId,
                                                  const DotsC_ParameterIndex parameter,
                                                  const DotsC_Int32 index,
                                                  const DotsC_KeyValMode keyValMode,
                                                  const char * & val);

    // Function:    DotsC_GetTypeIdParameter
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter type id value.
    DOTS_KERNEL_API void DotsC_GetTypeIdParameter(const DotsC_TypeId typeId,
                                                  const DotsC_ParameterIndex parameter,
                                                  const DotsC_Int32 index,
                                                  const DotsC_KeyValMode keyValMode,
                                                  DotsC_TypeId & val);

    // Function:    DotsC_GetHashedIdParameter
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              hashVal     -   retrived value, out parameter
    //              strVal      -   string of value if there was one. NULL otherwise
    // Returns:     -
    // Comments:    Gets a parameter hashed id value.
    DOTS_KERNEL_API void DotsC_GetHashedIdParameter(const DotsC_TypeId typeId,
                                                    const DotsC_ParameterIndex parameter,
                                                    const DotsC_Int32 index,
                                                    const DotsC_KeyValMode keyValMode,
                                                    DotsC_Int64 & hashVal,
                                                    const char * & strVal);


    // Function:    DotsC_GetEntityIdParameter
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              entityId       - retrived value, out parameter
    //              instanceIdStr  - string of instance id if there was one. NULL otherwise
    // Returns:     -
    // Comments:    Gets a parameter object id value.
    DOTS_KERNEL_API void DotsC_GetEntityIdParameter(const DotsC_TypeId typeId,
                                                    const DotsC_ParameterIndex parameter,
                                                    const DotsC_Int32 index,
                                                    const DotsC_KeyValMode keyValMode,
                                                    DotsC_EntityId & entityId,
                                                    const char * & instanceIdStr);

    // Function:    DotsC_GetObjectParameter
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter object value.
    DOTS_KERNEL_API void DotsC_GetObjectParameter(const DotsC_TypeId typeId,
                                                  const DotsC_ParameterIndex parameter,
                                                  const DotsC_Int32 index,
                                                  const char * & val);

    // Function:    DotsC_GetBinaryParameter
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    //              size        -   number of bytes in val.
    // Returns:     -
    // Comments:    Gets a parameter object value.
    DOTS_KERNEL_API void DotsC_GetBinaryParameter(const DotsC_TypeId typeId,
                                                  const DotsC_ParameterIndex parameter,
                                                  const DotsC_Int32 index,
                                                  const char * & val,
                                                  DotsC_Int32 & size);



    /**
     * @brief DotsC_DictionaryKeyToIndex - Get index of the key. After index is retrieved, use the ordinary GetXXXParameter fetch the value.
     * @param typeId - Id of class that contains the parameter
     * @param parameter - Id of parameter
     * @param key - The dictionary key
     * @return Index or -1 if key does not exist.
     */
    DOTS_KERNEL_API DotsC_Int32 DotsC_DictionaryInt32KeyToIndex(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_Int32 key);
    DOTS_KERNEL_API DotsC_Int32 DotsC_DictionaryInt64KeyToIndex(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_Int64 key);
    DOTS_KERNEL_API DotsC_Int32 DotsC_DictionaryStringKeyToIndex(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const char* key);
    DOTS_KERNEL_API DotsC_Int32 DotsC_DictionaryEntityIdKeyToIndex(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_EntityId key);

    //********************************************************
    // Operations on blobs
    //********************************************************

    // Raw blob operations - does not need a BlobReader/BlobWriter
    //------------------------------------------------------------------------
    /**
     * @brief Get the type id of the blob.
     * @param blob [in] - The blob.
     * @return Type id of blob.
     */
    DOTS_KERNEL_API DotsC_TypeId DotsC_GetTypeId(const char* blob);

    /**
     * @brief Get size of blob.
     * @param blob [in] - The blob.
     * @return Size of blob.
     */
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetSize(const char* blob);

    /**
     * This method will allocate memory within dots_kernel.
     * The memory must be deleted with DotsC_DeleteBlob.
     *
     * @param to [in,out] - The copy to be created.
     * @param size [in] - Number of bytes to allocate.
     * @return Pointer to allocated memory.
     */
    DOTS_KERNEL_API char* DotsC_AllocateBlob(DotsC_Int32 size);

    /**
     * This method will create an exact copy of a blob. The blob 'to' shall not already
     * have been created since it will cause memory leaks. This method does not delete
     * the blob 'to' before it is allocated.
     *
     * @param to [in,out] - The copy to be created.
     * @param from [in] - The original blob.
     */
    DOTS_KERNEL_API void DotsC_CreateCopyOfBlob(char* & to, const char* from);

    /**
     * Deletes a blob. Blobs created within dots_kernel must be deleted there too.
     * This is the only method that performs a 'delete blob'. All other methods that
     * allocates relies on that this metod is used for deletion.
     *
     * @param blob [in,out] - The blob to be deleted.
     */
    DOTS_KERNEL_API void DotsC_DeleteBlob(char* & blob);

    // Read operations
    //------------------------------------------------------------------------
    /**
     * @brief Create a new instance of blob reader.
     * @param blob [in] - The blob to read.
     * @return Handle to a new blob reader instance.
     */
    DOTS_KERNEL_API DotsC_Handle DotsC_CreateBlobReader(const char* blob);

    /**
     * @brief Deletes an instance of blob reader.
     * @param handle [in] - Handle to the blob readet to be deleted.
     */
    DOTS_KERNEL_API void DotsC_DeleteBlobReader(DotsC_Handle handle);

    /**
     * Get the number of values for a member. If collection type is SingleValueCollectionType 1 will always be returned.
     * @param readerHandle [in] - Handle to blob reader.
     * @param member [in] - The member.
     * @return Number of values.
     */
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetNumberOfMemberValues(DotsC_Handle readerHandle, DotsC_MemberIndex member);

    /**
     * Read member status for a specific member.
     *
     * @param readerHandle [in] - Handle to blob reader.
     * @param isNull [out] - The isNull status. Set to true if null.
     * @param isChanged [out] - The isChanged status. Set to true if changed.
     * @param member [in] - The member.
     * @param valueIndex [in] - The value index.
     */
    DOTS_KERNEL_API void DotsC_ReadMemberStatus(DotsC_Handle readerHandle, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 valueIndex);

    /**
     * Read a value from the blob managed by the specified blob reader.
     *
     * @param readerHandle [in] - Handle to blob reader.
     * @param val [out] - Read value.
     * @param isNull [out] - Null flag.
     * @param isChanged [out] - Change flag.
     * @param member [in] - Member to read.
     * @param valueIndex [in] - Value index to read.
     * @param keyValMode [in] - Specify if we want to read a key or value. Key is only allowed if member is a dictionary.
     */
    DOTS_KERNEL_API void DotsC_ReadInt32Member(DotsC_Handle readerHandle, DotsC_Int32& val, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 valueIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_ReadInt64Member(DotsC_Handle readerHandle, DotsC_Int64& val, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 valueIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_ReadFloat32Member(DotsC_Handle readerHandle, DotsC_Float32& val, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 valueIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_ReadFloat64Member(DotsC_Handle readerHandle, DotsC_Float64& val, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 valueIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_ReadBooleanMember(DotsC_Handle readerHandle, bool& val, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 valueIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_ReadStringMember(DotsC_Handle readerHandle, const char*& val, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 valueIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_ReadHashedMember(DotsC_Handle readerHandle, DotsC_Int64& val, const char*& optionalStr, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 valueIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_ReadEntityIdMember(DotsC_Handle readerHandle, DotsC_EntityId& val, const char*& optionalStr, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 valueIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_ReadBinaryMember(DotsC_Handle readerHandle, const char*& val, DotsC_Int32& size, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 valueIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_ReadObjectMember(DotsC_Handle readerHandle, const char*& val, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 valueIndex, DotsC_KeyValMode keyValMode);


    // Write operations
    //------------------------------------------------------------------------
    /**
     * @brief Create a new instance of blob writer.
     * @param typeId [in] - The type of blob to be written.
     * @return Handle to a new blob writer instance.
     */
    DOTS_KERNEL_API DotsC_Handle DotsC_CreateBlobWriter(DotsC_TypeId typeId);

    /**
     * Create a new instance of blob writer from an existing blob. The existing blob will never be changed.
     *
     * @param blob [in] - Blob that initiates the writer
     * @return Handle to a new blob writer instance.
     */
    DOTS_KERNEL_API DotsC_Handle DotsC_CreateBlobWriterFromBlob(const char* blob);

    /**
     * Create a new instance of blob writer from an existing blobReader handle. The readHandle can
     * be deleted by calling DotsC_DeleteBlobReader at any time.
     *
     * @param readHandle [in] - Handle to a blobReader.
     * @return Handle to a new blob writer instance.
     */
    DOTS_KERNEL_API DotsC_Handle DotsC_CreateBlobWriterFromReader(DotsC_Handle readerHandle);

    /**
     * @brief Deletes an instance of blob writer.
     * @param writerHandle [in] - Handle to the blob writer to be deleted.
     */
    DOTS_KERNEL_API void DotsC_DeleteBlobWriter(DotsC_Handle writerHandle);

    /**
     * Calculate the needed size of the destination blob when calling WriteBlob with this writerHandle.
     * @param writerHandle [in] - Handle to blob writer.
     * @return Number of bytes needed to be allocated before serializing to a raw blob.
     */
    DOTS_KERNEL_API DotsC_Int32 DotsC_CalculateBlobSize(DotsC_Handle writerHandle);

    /**
     * Write content of a blob writer to a raw blob. The blobDest must be preallocated with enough capacity
     * to contain the raw blob. Use CalculateBlobSize to get the needed size.
     * @param writerHandle [in] - Handle to blob writer.
     * @param blobDest [in, out] - Preallocated raw blob. After this call blobDest contains the binary serialization.
     */
    DOTS_KERNEL_API void DotsC_WriteBlob(DotsC_Handle writerHandle, char* blobDest);

    /**
     * Recursively set changed flags for all members.
     *
     * @param writerHandle [in,out] - Handle to blob writer.
     * @param changed [in] - The value to set all change flags to
     */
    DOTS_KERNEL_API void DotsC_WriteAllChangeFlags(DotsC_Handle writerHandle, bool changed);

    /**
     * Set the change flag on one member (non-recursively).
     *
     * @param writerHandle [in,out] - Handle to a blob writer.
     * @param member [in] - id of the member.
     * @param index [in] - array index of member. Shall be 0 if member is not an array.
     * @param changed [in] - The value to set change flag to.
     */
    DOTS_KERNEL_API void DotsC_WriteChangeFlag(DotsC_Handle writerHandle,
                                               DotsC_MemberIndex member,
                                               DotsC_ArrayIndex index,
                                               bool changed);

    /**
     * Creates a BlobWriter and initiates it with current. Then all differences between current and original are
     * marked as changed. The original and current blobs are not changed at all, instead a completely new BlobWriter
     * is created and the caller is responsible for deleting it by calling DotsC_DeleteBlobWriter.
     * @param original [in] - The original blob.
     * @param current [in] - Current version of blob that will be diffed towards original.
     * @return Handle to a new blob writer instance containing a copy of current but with updated change flags.
     */

    /**
      * Diff content of currentWriter and originalReader and mark all changes in currentWriter;
      *
      * @param originalReader [in] - Reader handle to the original blob to compare towards.
      * @param currentWriter [in] - Writer handle to the current blob that will be compared and have ist change flags updated.
      * @return False if there were no changes found, else true. If false is returned nothing has been changed
      *         in currentWriter, if true change flags have been updated.
      */
    DOTS_KERNEL_API bool DotsC_MarkChanges(DotsC_Handle originalReader, DotsC_Handle currentWriter);

    /**
     * Write a value to the blob managed by the specified blob writer.
     * @param writerHandle [in] - Handle to blob writer.
     * @param val [in] - The value to write.
     * @param isNull [in] - Null flag.
     * @param isChanged [in] - Change flag.
     * @param member [in] - The member to write.
     * @param arrayIndex [in] - ArrayIndex, only relevant if member is an array.
     * @param keyValMode [in] - Specify if we want to write a key or value. Key is only allowed if member is a dictionary.
     */
    DOTS_KERNEL_API void DotsC_WriteInt32Member(DotsC_Handle writerHandle, DotsC_Int32 val, bool isNull, bool isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_WriteInt64Member(DotsC_Handle writerHandle, DotsC_Int64 val, bool isNull, bool isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_WriteFloat32Member(DotsC_Handle writerHandle, DotsC_Float32 val, bool isNull, bool isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_WriteFloat64Member(DotsC_Handle writerHandle, DotsC_Float64 val, bool isNull, bool isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_WriteBooleanMember(DotsC_Handle writerHandle, bool val, bool isNull, bool isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_WriteStringMember(DotsC_Handle writerHandle, const char* val, bool isNull, bool isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_WriteHashedMember(DotsC_Handle writerHandle, DotsC_Int64 hash, const char* str, bool isNull, bool isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_WriteEntityIdMember(DotsC_Handle writerHandle, DotsC_EntityId val, const char* instanceString, bool isNull, bool isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_WriteBinaryMember(DotsC_Handle writerHandle, const char* val, DotsC_Int32 size, bool isNull, bool isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode keyValMode);
    DOTS_KERNEL_API void DotsC_WriteObjectMember(DotsC_Handle writerHandle, const char* blob, bool isNull, bool isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode keyValMode);

    //************************************************************************************
    //* Library exception handling
    //************************************************************************************
    DOTS_KERNEL_API void DotsC_SetException(const DotsC_TypeId exceptionId,
                                            const char * const description);

    DOTS_KERNEL_API void DotsC_AppendExceptionDescription(const char * const moreDescription);

    DOTS_KERNEL_API void DotsC_GetAndClearException(DotsC_TypeId & exceptionId,
                                                    char * & description,
                                                    DotsC_BytePointerDeleter & deleter,
                                                    bool & wasSet);

    DOTS_KERNEL_API void DotsC_PeekAtException(DotsC_TypeId & exceptionId);

    //************************************************************************************
    //* Functions mostly indended for debugging
    //************************************************************************************

    // Function:    DotsC_GetDouFilePathForType
    // Parameters:  typeId      -   Type to find path for

    // Returns:     Pointer to a string containing full path to the dou-file.
    // Comments:    Get the full path to the dou file that the type id represents.
    DOTS_KERNEL_API const char* DotsC_GetDouFilePath(const DotsC_TypeId typeId);

    // Function:    DotsC_TypeRepositoryLoadedByThisProcess
    // Returns:     True if this process created the shared memory and loaded the type repository
    // Comments:    Check if this process created and the shared memory and loaded type repository
    //              or if it just opened it (i.e someone else already created it for us).
    DOTS_KERNEL_API bool DotsC_TypeRepositoryLoadedByThisProcess();
    
    // Function:    DotsC_GetTypeDescription
    // Parameters:  typeId      -   Type to describe. If 0 all types will be completely described. Very much text.
    //              buf         -   Buffer where file text description will be put, will be null terminated.
    //              bufSize     -   size of buf.
    //              resultSize  -   if the buffer was big enough for the description this holds the number
    //                              of bytes written
    //                              if it was too small it holds the size that was needed
    //                              (so resultSize > bufSize ==> try again with bigger buffer)
    //                              Includes the null termination at the end.
    // Returns:     -
    // Comments:    Get a text description of a type or the complete type repository.
    DOTS_KERNEL_API void DotsC_GetTypeDescription(const DotsC_TypeId typeId,
                                                  char * const buf,
                                                  const DotsC_Int32 bufSize,
                                                  DotsC_Int32 & resultSize);


#pragma pack(push)
#pragma pack(4)
    struct DotsC_GeneratedLibrary
    {
        char* name;

        DotsC_Int32 library; //boolean: 0 if override, 1 if library

        char* cppLibraryName;
        char* cppLibraryLocation;

        char* javaJarName;
        char* javaJarLocation;

        char* dotnetAssemblyName;
        char* dotnetAssemblyLocation;
    };
#pragma pack(pop)

    typedef void (*DotsC_GeneratedLibraryListDeleter)(DotsC_GeneratedLibrary* list, 
                                                      const DotsC_Int32 size);

    DOTS_KERNEL_API void DotsC_GetGeneratedLibraryList(DotsC_GeneratedLibrary*& generatedLibraries,
                                                       DotsC_Int32& size,
                                                       DotsC_GeneratedLibraryListDeleter& deleter);
}


#endif
