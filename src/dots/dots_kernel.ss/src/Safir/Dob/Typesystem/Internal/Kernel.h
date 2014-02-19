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

/***************************************************************
  Purpose:      The C main interface to DOTS.
***************************************************************/

#ifndef _dots_kernel_h
#define _dots_kernel_h

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef dots_kernel_EXPORTS
#  define DOTS_KERNEL_API SAFIR_HELPER_DLL_EXPORT
#else
#  define DOTS_KERNEL_API SAFIR_HELPER_DLL_IMPORT
#  define SAFIR_LIBRARY_NAME "dots_kernel"
#  define SAFIR_NO_DEBUG_LIBRARY_SUFFIX
#  include <Safir/Utilities/Internal/AutoLink.h>
#endif
#define DOTS_KERNEL_LOCAL SAFIR_HELPER_DLL_LOCAL

#include <Safir/Dob/Typesystem/LanguageInterfaceDefs.h>

extern "C"
{
    typedef void (*DotsC_BytePointerDeleter)(char * &);

    //********************************************************
    //* Base operations on blobs
    //*
    //* Warning: Be careful when using these methods. Wrong
    //*          usage will cause memory leaks.
    //********************************************************
    // Function:    DotsC_CreateBlob
    // Parameters:  typeId -    id of class
    //              blob - the blob to be created (NULL if typeId was not valid)
    // Comments:    Initializes blob to default size and members according to the given type id.
    //              Any already crated blob must be deleted before this function is called.
    //              No delete on 'blob' is done here.
    DOTS_KERNEL_API void DotsC_CreateBlob(const DotsC_TypeId typeId,
                                          char * & blob);

    // Function:    DotsC_DeleteBlob
    // Parameters:  blob - the blob to be deleted
    // Returns:     -
    // Comments:    Deletes a blob. Blobs created within dots_kernel must be deleted there too.
    //              This is the only method that performs a 'delete blob'. All other methods that
    //              allocates relies on that this metod is used for deletion.
    DOTS_KERNEL_API void DotsC_DeleteBlob(char * & blob);

    // Function:    DotsC_CreateCopyOfBlob
    // Parameters:  to - the copy to be created
    //              from - the original blob
    // Returns:     -
    // Comments:    This method will create an exact copy of a blob. The blob 'to' shall not already
    //              have been created since it will cause memory leaks. This method does not delete
    //              the blob 'to' before it is allocated.
    DOTS_KERNEL_API void DotsC_CreateCopyOfBlob(char * & to,
                                                const char * const from);

    // Function:    DotsC_GetTypeId
    // Parameters:  blob - the blob
    // Returns:     id of the object stored in the blob
    // Comments:    Gives the object id for the blob
    DOTS_KERNEL_API DotsC_TypeId DotsC_GetTypeId(const char * const blob);

    // Function:    DotsC_GetSize
    // Parameters:  blob - the blob
    // Returns:     size of the blob
    // Comments:    Gives the total size of the blob
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetSize(const char * const blob);

    // Function:    DotsC_IsAnythingChanged
    // Parameters:  blob - the blob
    // Returns:     true if any member has changed, else false.
    // Comments:    Returns true if any member in the blob has been changed
    //              since last call to DotsC_ResetChanged.
    DOTS_KERNEL_API bool DotsC_IsAnythingChanged(const char * const blob);

    // Function:    DotsC_ResetChanged
    // Parameters:  blob - the blob
    // Returns:     -
    // Comments:    Reset changed flags for all members in the blob.
    //               Note that this function is not recursive
    DOTS_KERNEL_API void DotsC_ResetChanged(char * const blob);

    /** Recursively set all change flags.
     *
     * Recursively set changed flags for all members in the blob.
     *
     * @param blob [in,out] - The blob to modify.
     * @param changed [in] - The value to set all change flags to
     */
    DOTS_KERNEL_API void DotsC_SetChanged(char * const blob, const bool changed);

    /** Set the change flag on one member (non-recursively).
     *
     * @param blob [in,out] - The blob to modify.
     * @param member [in] - id of the member.
     * @param index [in] - array index of member. Shall be 0 if member is not an array.
     * @param changed [in] - The value to set change flag to.
     */
    DOTS_KERNEL_API void DotsC_SetChangedHere(char * const blob,
                                              const DotsC_MemberIndex member,
                                              const DotsC_ArrayIndex index,
                                              const bool changed);

    // Function:    DotsC_SetChangedMembers
    // Parameters:  val -   the blob containing changes.
    //              blob -  blob to be changed
    // Returns:     -
    // Comments:    Apply changes in val on the blob. Note that val and blob must be of exactly the same type.
    DOTS_KERNEL_API void DotsC_SetChangedMembers(const char * const val,
                                                 char * & blob);

    // Function:    DotsC_SetChangedSinceLastRead
    // Parameters:  current -   the current version of the object.
    //              lastRead -  last read version of the object.
    // Returns:     -
    // Comments:    Set changed flags for all members in current that have been changed since last read object.
    DOTS_KERNEL_API void DotsC_SetChangedSinceLastRead(const char * const lastRead,
                                                       char * const current);

    //********************************************************
    //* Type information operations
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
    DOTS_KERNEL_API void DotsC_GetAllTypeIds(DotsC_TypeId * const buf,
                                             const DotsC_Int32 bufSize,
                                             DotsC_Int32 & size);

    // Function:    DotsC_TypeExists
    // Parameters:  typeId - id of class
    // Returns:     true if the type exists
    // Comments:    Check if type with specified type id exist
    DOTS_KERNEL_API bool DotsC_TypeExists(const DotsC_TypeId typeId);

    // Function:    DotsC_IsClass
    // Parameters:  typeId - id of class
    // Returns:     true if the type exists as a class
    // Comments:    Check if type id belongs to a existing class
    DOTS_KERNEL_API bool DotsC_IsClass(const DotsC_TypeId typeId);

    // Function:    DotsC_IsProperty
    // Parameters:  typeId - id of property
    // Returns:     true if the type exists as a property
    // Comments:    Check if type id belongs to a existing property
    DOTS_KERNEL_API bool DotsC_IsProperty(const DotsC_TypeId typeId);

    // Function:    DotsC_IsEnumeration
    // Parameters:  typeId - id of enumeration type
    // Returns:     true if the type exists as an enumeration type
    // Comments:    Check if type id belongs to a existing enumeration type
    DOTS_KERNEL_API bool DotsC_IsEnumeration(const DotsC_TypeId typeId);

    // Function:    DotsC_IsException
    // Parameters:  typeId - id of exception type
    // Returns:     true if the type exists as an exception type
    // Comments:    Check if type id belongs to a existing enumeration type
    DOTS_KERNEL_API bool DotsC_IsException(const DotsC_TypeId typeId);

    // Function:    DotsC_TypeIdFromName
    // Parameters:  typeName -  The name shall contain namespaces and class name
    //                          with '.' as separator, example "MyNamespace1.MyNamespace2.MyClass"
    // Returns:     type id
    // Comments:    Calculates the type id for the given type name.
    DOTS_KERNEL_API DotsC_TypeId DotsC_TypeIdFromName(const char * const typeName);

    // Function:    DotsC_GetTypeName
    // Parameters:  typeId -    id of type
    // Returns:     name of the type
    // Comments:    Gets the name associated with the specified type id
    DOTS_KERNEL_API const char* DotsC_GetTypeName(const DotsC_TypeId typeId);

    // Function:    DotsC_GetNumberOfEnumerationValues
    // Parameters:  enumId -    id of enum type
    // Returns:     Number of enumeration values. -1 is returned if the type does not exist.
    // Comments:    Get the number of enumeration values the specified enumeration type has.
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetNumberOfEnumerationValues(const DotsC_TypeId enumId);

    // Function:    DotsC_EnumerationValueName
    // Parameters:  enumId -    id of enum type
    //              enumVal -   the enumeration value
    // Returns:     String representation of the enumeration value.
    // Comments:    Get the string representation of the specified value for a enumeration type
    DOTS_KERNEL_API const char* DotsC_GetEnumerationValueName(const DotsC_TypeId enumId,
                                                              DotsC_EnumerationValue enumVal);

    // Function:    DotsC_EnumerationValueFromName
    // Parameters:  enumId          -   id of enum type
    //              enumValueName   -   string representation of the desired value
    // Returns:     Integer value for the enumeration value name
    // Comments:    Get integer value associated with the enumeration value for the specified enumeration type.
    DOTS_KERNEL_API DotsC_EnumerationValue DotsC_EnumerationValueFromName(const DotsC_TypeId enumId,
                                                                          const char * const enumValueName);

    //***********************************************************
    //* Functions for retrieving member info about object types
    //***********************************************************
    // Function:    DotsC_GetNumberOfMembers
    // Parameters:  typeId - id of class or property
    // Returns:     number of members of the type, -1 if type does not exist
    // Comments:    Get the number of members for a class or property. Parameters are not included.
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetNumberOfMembers(const DotsC_TypeId typeId);

    // Function:    DotsC_GetMemberId
    // Parameters:  typeId      -   id of class or property
    //              memberName  -   name of member as specified in xml description, case sensitive,
    // Returns:     id of the member
    // Comments:    Gets the member id for a member.
    DOTS_KERNEL_API DotsC_MemberIndex     DotsC_GetMemberId(const DotsC_TypeId typeId,
                                                            const char * const memberName);

    // Function:    DotsC_GetMemberName
    // Parameters:  typeId  -   id of class or property
    //              member  -   id of member
    // Returns:     name of the member
    // Comments:    Get the name of the specified member as it was defined in the xml description.
    DOTS_KERNEL_API const char* DotsC_GetMemberName(const DotsC_TypeId typeId,
                                                    const DotsC_MemberIndex member);

    // Function:    DotsC_GetComplexMemberTypeId
    // Parameters:  typeId  -   id of class or property
    //              member  -   id of member
    // Returns:     returns the typeId for an object or enumeration member. -1 is returned of the type does not exist
    // Comments:    If a member is of type object or enumeration, this method can be used to get the typeId for the class or enum that the
    //              member is of.
    DOTS_KERNEL_API DotsC_TypeId DotsC_GetComplexMemberTypeId(const DotsC_TypeId typeId,
                                                              const DotsC_MemberIndex member);

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
    DOTS_KERNEL_API void DotsC_GetMemberInfo(const DotsC_TypeId typeId,  //in
                                             const DotsC_MemberIndex member,  //in
                                             DotsC_MemberType & memberType,//out
                                             const char * & memberName,           //out
                                             DotsC_TypeId & complexType,   //out
                                             DotsC_Int32 & stringLength,   //out
                                             bool & isArray,                      //out
                                             DotsC_Int32 & arrayLength);   //out

    // Function:    DotsC_GetMemberArraySize
    // Parameters:  typeId      -   id of class or property
    //              member      -   id of member
    // Returns:     The array size of the member. If typeId is not a class, then -1 is returned.
    // Comments:    Returns the array size of a member.
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetMemberArraySize(const DotsC_TypeId typeId,
                                                         const DotsC_MemberIndex member);

    // Function:    DotsC_GetStringMemberMaxLength
    // Parameters:  typeId      -   id of class
    //              member      -   id of member
    // Returns:     The maximum length of the string member.
    //              If the member is not a string -1 is returned.
    //              If the id is not a class -1 is returned
    // Comments:    Returns the maximum string length of a member.
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetStringMemberMaxLength(const DotsC_TypeId typeId,
                                                               const DotsC_MemberIndex member);

    // Function:    DotsC_GetMemberArraySizeProperty
    // Parameters:  classId         -   id of the class with a property
    //              propertyId      -   id of a property supported by the class
    //              propertyMember  -   the property member
    // Returns:     The array size of the property member.
    //              -1 if there is no such type or array or mapping defined
    // Comments:    Returns the array size of a property member.
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetMemberArraySizeProperty(const DotsC_TypeId classId,
                                                                 const DotsC_TypeId propertyId,
                                                                 const DotsC_MemberIndex propertyMember);
    // Function:    DotsC_GetStringMemberMaxLengthProperty
    // Parameters:  classId         -   id of the class with a property
    //              propertyId      -   id of a property supported by the class
    //              propertyMember  -   the property member
    // Returns:     The max string length of the property member.
    //              -1 if there is no such type or mapping defined
    // Comments:    Returns the max string length of a property member.
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetStringMemberMaxLengthProperty(const DotsC_TypeId classId,
                                                                       const DotsC_TypeId propertyId,
                                                                       const DotsC_MemberIndex propertyMember);

    // Function:    DotsC_GetMemberTypeName
    // Parameters:  typeId  -   id of class or property
    //              member  -   id of member
    // Returns:     name of type
    // Comments:    Gets a string representation of the type of a member.
    DOTS_KERNEL_API const char* DotsC_GetMemberTypeName(const DotsC_TypeId typeId,
                                                        const DotsC_MemberIndex member);

    //***********************************************************************
    //* Functions retrieving definitions of parameter values in object types
    //***********************************************************************
    // Function:    DotsC_GetNumberOfParameters
    // Parameters:  typeId  -   id of class
    // Returns:     the number of parameters, -1 if type does not exist
    // Comments:    Gets the number of parameters defined in a class.
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetNumberOfParameters(const DotsC_TypeId typeId);

    // Function:    DotsC_GetParameterId
    // Parameters:  typeId          -   id of class
    //              parameterName   -   name of the parameter as defined in the xml description, case sensitive.
    // Returns:     id of the parameter, -1 if it does not exist
    // Comments:    Gets id of a parameter.
    DOTS_KERNEL_API DotsC_ParameterIndex  DotsC_GetParameterId(const DotsC_TypeId typeId,
                                                               const char * const parameterName);

    // Function:    DotsC_GetParameterName
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    // Returns:     name of the parameter
    // Comments:    Get the name of the specified parameter as it was defined in the xml description.
    //              If the parameter does not exist the return value is undefined. Use GetParameterId
    //              to obtain a parameter that is guaranteed to exist.
    DOTS_KERNEL_API const char* DotsC_GetParameterName(const DotsC_TypeId typeId,
                                                       const DotsC_ParameterIndex parameter);

    // Function:    DotsC_GetParameterType
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    // Returns:     type of the parameter
    // Comments:    Gets the type of a parameter.
    //              If the parameter does not exist the return value is undefined. Use GetParameterId
    //              to obtain a parameter that is guaranteed to exist.
    DOTS_KERNEL_API DotsC_MemberType DotsC_GetParameterType(const DotsC_TypeId typeId,
                                                            const DotsC_ParameterIndex parameter);

    // Function:    DotsC_GetParameterTypeName
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    // Returns:     name of type
    // Comments:    Gets a string representation of the type of a parameter.
    //              If the parameter does not exist the return value is undefined. Use GetParameterId
    //              to obtain a parameter that is guaranteed to exist.
    DOTS_KERNEL_API const char* DotsC_GetParameterTypeName(const DotsC_TypeId typeId,
                                                           const DotsC_ParameterIndex parameter);

    // Function:    DotsC_GetParameterArraySize
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    // Returns:     The array size of the parameter.
    // Comments:    Returns the array size of a parameter.
    //              If the parameter does not exist the return value is undefined. Use GetParameterId
    //              to obtain a parameter that is guaranteed to exist.
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetParameterArraySize(const DotsC_TypeId typeId,
                                                            const DotsC_ParameterIndex parameter);

    //************************************************************************************
    //* Type compatibility
    //************************************************************************************
    // Function:    DotsC_IsOfType
    // Parameters:  type    -   the type to check if it is of another type
    //              ofType  -   the type to compare to
    // Returns:     true of type is equal to ofType or if type inherits from ofType
    // Comments:    Checks if type is an instance of the ofType, direct or by inheritance
    DOTS_KERNEL_API bool DotsC_IsOfType(const DotsC_TypeId type,
                                        const DotsC_TypeId ofType);

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
    DOTS_KERNEL_API void DotsC_GetCompleteType(const DotsC_TypeId type,
                                               DotsC_TypeId * const buf,
                                               const DotsC_Int32 bufSize,
                                               DotsC_Int32 & noResults);

    // Function:    DotsC_GetParentType
    // Parameters:  type        -   the type for which the parent type is requested
    // Returns:     type id for the parent
    // Comments:    Returns the typeId  for the parent class to 'type'. If type==object,
    //              then the object typeId is returned again.
    DOTS_KERNEL_API DotsC_TypeId DotsC_GetParentType(const DotsC_TypeId type);

    // Function:    DotsC_HasProperty
    // Parameters:  classDotsC_TypeId     -   The type of the class to checked for a given property.
    //              propertyDotsC_TypeId  -   type of the property
    //              hasProperty     -   out parameter that is true if class has property
    //              isInherited     -   out parameter that indicates whether the property is
    //                                  set on the class itself or whether it was inherited from
    //                                  a parent class.
    // Returns:     -
    // Comments:    Checks objects with 'classDotsC_TypeId' have the property with typeId 'propertyDotsC_TypeId'.
    DOTS_KERNEL_API void DotsC_HasProperty(const DotsC_TypeId classTypeId,
                                           const DotsC_TypeId propertyTypeId,
                                           bool & hasProperty,
                                           bool & isInherited);

    //************************************************************************************
    //* Serialization
    //************************************************************************************

    // Function:    DotsC_BetterBlobToXml
    // Parameters:  xmlDest     -   result of serialization, will be a xml string. Out parameter
    //              blobSource  -   blob to serialize
    //              bufSize     -   size of xmlDest. string is null terminated.
    //              resultSize  -   if the buffer was big enough for the xml this holds the number
    //                              of bytes written (including null termination)
    //                              if it was too small it holds the size that was needed
    //                              (so resultSize > bufSize ==> try again with bigger buffer)
    // Returns:     -
    // Comments:    Serializes a blob to a xml string.
    DOTS_KERNEL_API void DotsC_BetterBlobToXml(char * const xmlDest,
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
                                                   const DotsC_ArrayIndex index,
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
                                                       const DotsC_ArrayIndex index,
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
                                                 const DotsC_ArrayIndex index,
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
                                                 const DotsC_ArrayIndex index,
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
                                                   const DotsC_ArrayIndex index,
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
                                                   const DotsC_ArrayIndex index,
                                                   DotsC_Float64 & val);

    // Function:    DotsC_GetStringParameter
    // Parameters:  typeId      -   id of class
    //              parameter   -   id of parameter
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter string value.
    DOTS_KERNEL_API void DotsC_GetStringParameter(const DotsC_TypeId typeId,
                                                  const DotsC_ParameterIndex parameter,
                                                  const DotsC_ArrayIndex index,
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
                                                  const DotsC_ArrayIndex index,
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
                                                    const DotsC_ArrayIndex index,
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
                                                    const DotsC_ArrayIndex index,
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
                                                  const DotsC_ArrayIndex index,
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
                                                  const DotsC_ArrayIndex index,
                                                  const char * & val,
                                                  DotsC_Int32 & size);

    //************************************************************************************
    //* Functions for retrieving member values
    //************************************************************************************
    // Function:    DotsC_IsNullMember
    // Parameters:  blob    -   blob containing the object
    //              member  -   id of member
    //              index   -   array index. If member is not an array index shall be 0.
    // Returns:     boolean value
    // Comments:    Get the if a member is null or a value.
    DOTS_KERNEL_API bool DotsC_IsNullMember(const char * const blob,
                                            const DotsC_MemberIndex member,
                                            const DotsC_ArrayIndex index);

    // Function:    DotsC_IsChangedMember
    // Parameters:  blob    -   blob containing the object
    //              member  -   id of member
    //              index   -   array index. If member is not an array index shall be 0.
    // Returns:     boolean value
    // Comments:    Get the if a member is changed
    DOTS_KERNEL_API bool DotsC_IsChangedMember(const char * const blob,
                                               const DotsC_MemberIndex member,
                                               const DotsC_ArrayIndex index);

    // Function:    DotsC_GetBooleanMember
    // Parameters:  blob    -   blob containing the object
    //              member  -   id of member
    //              index   -   array index. If member is not an array index shall be 0.
    //              val     -   the retrieved value.
    //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
    //              isChanged - indicates if the value has been changed.
    // Returns:     -
    // Comments:    Get a boolean member from a blob.
    DOTS_KERNEL_API void DotsC_GetBooleanMember(const char * const blob,
                                                const DotsC_MemberIndex member,
                                                const DotsC_ArrayIndex index,
                                                bool & val,
                                                bool & isNull,
                                                bool & isChanged);

    // Function:    DotsC_GetEnumerationMember
    // Parameters:  blob    -   blob containing the object
    //              member  -   id of member
    //              index   -   array index. If member is not an array index shall be 0.
    //              val     -   the retrieved value.
    //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
    //              isChanged - indicates if the value has been changed.
    // Returns:     -
    // Comments:    Get a enumeration member from a blob.
    DOTS_KERNEL_API void DotsC_GetEnumerationMember(const char * const blob,
                                                    const DotsC_MemberIndex member,
                                                    const DotsC_ArrayIndex index,
                                                    DotsC_EnumerationValue & val,
                                                    bool & isNull,
                                                    bool & isChanged);

    // Function:    DotsC_GetInt32Member
    // Parameters:  blob    -   blob containing the object
    //              member  -   id of member
    //              index   -   array index. If member is not an array index shall be 0.
    //              val     -   the retrieved value.
    //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
    //              isChanged - indicates if the value has been changed.
    // Returns:     -
    // Comments:    Get a 32-bits integer member from a blob.
    DOTS_KERNEL_API void DotsC_GetInt32Member(const char * const blob,
                                              const DotsC_MemberIndex member,
                                              const DotsC_ArrayIndex index,
                                              DotsC_Int32 & val,
                                              bool & isNull,
                                              bool & isChanged);

    // Function:    DotsC_GetInt64Member
    // Parameters:  blob    -   blob containing the object
    //              member  -   id of member
    //              index   -   array index. If member is not an array index shall be 0.
    //              val     -   the retrieved value.
    //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
    //              isChanged - indicates if the value has been changed.
    // Returns:     -
    // Comments:    Get a 64-bits integer member from a blob.
    DOTS_KERNEL_API void DotsC_GetInt64Member(const char * const blob,
                                              const DotsC_MemberIndex member,
                                              const DotsC_ArrayIndex index,
                                              DotsC_Int64 & val,
                                              bool & isNull,
                                              bool & isChanged);

    // Function:    DotsC_GetFloat32Member
    // Parameters:  blob    -   blob containing the object
    //              member  -   id of member
    //              index   -   array index. If member is not an array index shall be 0.
    //              val     -   the retrieved value.
    //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
    //              isChanged - indicates if the value has been changed.
    // Returns:     -
    // Comments:    Get a 32-bits float member from a blob.
    DOTS_KERNEL_API void DotsC_GetFloat32Member(const char * const blob,
                                                const DotsC_MemberIndex member,
                                                const DotsC_ArrayIndex index,
                                                DotsC_Float32 & val,
                                                bool & isNull,
                                                bool & isChanged);

    // Function:    DotsC_GetFloat64Member
    // Parameters:  blob    -   blob containing the object
    //              member  -   id of member
    //              index   -   array index. If member is not an array index shall be 0.
    //              val     -   the retrieved value.
    //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
    //              isChanged - indicates if the value has been changed.
    // Returns:     -
    // Comments:    Get a 64-bits float member from a blob.
    DOTS_KERNEL_API void DotsC_GetFloat64Member(const char * const blob,
                                                const DotsC_MemberIndex member,
                                                const DotsC_ArrayIndex index,
                                                DotsC_Float64 & val,
                                                bool & isNull,
                                                bool & isChanged);

    // Function:    DotsC_GetStringMember
    // Parameters:  blob    -   blob containing the object
    //              member  -   id of member
    //              index   -   array index. If member is not an array index shall be 0.
    //              val     -   the retrieved value.
    //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
    //              isChanged - indicates if the value has been changed.
    // Returns:     -
    // Comments:    Get a string member from a blob.
    DOTS_KERNEL_API void DotsC_GetStringMember(const char * const blob,
                                               const DotsC_MemberIndex member,
                                               const DotsC_ArrayIndex index,
                                               const char * & val,
                                               bool & isNull,
                                               bool & isChanged);

    // Function:    DotsC_GetTypeIdMember
    // Parameters:  blob    -   blob containing the object
    //              member  -   id of member
    //              index   -   array index. If member is not an array index shall be 0.
    //              val     -   the retrieved value.
    //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
    //              isChanged - indicates if the value has been changed.
    // Returns:     -
    // Comments:    Get a type id member from a blob.
    DOTS_KERNEL_API void DotsC_GetTypeIdMember(const char * const blob,
                                               const DotsC_MemberIndex member,
                                               const DotsC_ArrayIndex index,
                                               DotsC_TypeId & val,
                                               bool & isNull,
                                               bool & isChanged);


    // Function:    DotsC_GetHashedIdMember
    // Parameters:  blob    -   blob containing the object
    //              member  -   id of member
    //              index   -   array index. If member is not an array index shall be 0.
    //              hashVal -   the retrieved value.
    //              hashStr -   the string if there was one, NULL otherwise.
    //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
    //              isChanged - indicates if the value has been changed.
    // Returns:     -
    // Comments:    Get a object id member from a blob.
    DOTS_KERNEL_API void DotsC_GetHashedIdMember(const char * const blob,
                                                 const DotsC_MemberIndex member,
                                                 const DotsC_ArrayIndex index,
                                                 DotsC_Int64 & hashVal,
                                                 const char * & strVal,
                                                 bool & isNull,
                                                 bool & isChanged);

    // Function:    DotsC_GetEntityIdMember
    // Parameters:  blob    -   blob containing the object
    //              member  -   id of member
    //              index   -   array index. If member is not an array index shall be 0.
    //              entityId -   the retrieved value.
    //              instanceIdStr - the instance string if there was one, NULL otherwise.
    //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
    //              isChanged - indicates if the value has been changed.
    // Returns:     -
    // Comments:    Get a object id member from a blob.
    DOTS_KERNEL_API void DotsC_GetEntityIdMember(const char * const blob,
                                                 const DotsC_MemberIndex member,
                                                 const DotsC_ArrayIndex index,
                                                 DotsC_EntityId & entityId,
                                                 const char * & instanceIdStr,
                                                 bool & isNull,
                                                 bool & isChanged);

    // Function:    DotsC_GetObjectMember
    // Parameters:  blob    -   blob containing the object
    //              member  -   id of member
    //              index   -   array index. If member is not an array index shall be 0.
    //              val     -   the retrieved value.
    //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
    //              isChanged - indicates if the value has been changed.
    // Returns:     -
    // Comments:    Get a object member from a blob.
    DOTS_KERNEL_API void DotsC_GetObjectMember(const char * const blob,
                                               const DotsC_MemberIndex member,
                                               const DotsC_ArrayIndex index,
                                               const char * & val,
                                               bool & isNull,
                                               bool & isChanged);


    //Same as above, but val is non-const.
    DOTS_KERNEL_API void DotsC_GetWriteableObjectMember(char * const blob,
                                                        const DotsC_MemberIndex member,
                                                        const DotsC_ArrayIndex index,
                                                        char * & val,
                                                        bool & isNull,
                                                        bool & isChanged);


    // Function:    DotsC_GetBinaryMember
    // Parameters:  blob    -   blob containing the object
    //              member  -   id of member
    //              index   -   array index. If member is not an array index shall be 0.
    //              val     -   the retrieved value.
    //              size    -   number of bytes in val.
    //              isNull  -   indicates is this member is NULL. If true 'val' is not valid.
    //              isChanged - indicates if the value has been changed.
    // Returns:     -
    // Comments:    Get a object member from a blob.
    DOTS_KERNEL_API void DotsC_GetBinaryMember(const char * const blob,
                                               const DotsC_MemberIndex member,
                                               const DotsC_ArrayIndex index,
                                               const char * & val,
                                               DotsC_Int32 & size,
                                               bool & isNull,
                                               bool & isChanged);

    //************************************************************************************
    //* Functions for setting member values
    //************************************************************************************
    // Function:    DotsC_SetNullMember
    // Parameters:  blob        -   blob containing the member.
    //              member      -   id of the member
    //              index       -   array index of member. Shall be 0 if member is not an array.
    // Returns:     -
    // Comments:    Sets the member to null.
    DOTS_KERNEL_API void DotsC_SetNullMember(char * const blob,
                                             const DotsC_MemberIndex member,
                                             const DotsC_ArrayIndex index);

    // Function:    DotsC_SetBooleanMember
    // Parameters:  val     -   the value to be set
    //              blob    -   blob containing the member.
    //              member  -   id of the member
    //              index   -   array index of member. Shall be 0 if member is not an array.
    // Returns:     -
    // Comments:    Sets the value for a boolean member.
    DOTS_KERNEL_API void DotsC_SetBooleanMember(const bool val,
                                                char * & blob,
                                                const DotsC_MemberIndex member,
                                                const DotsC_ArrayIndex index);

    // Function:    DotsC_SetEnumerationMember
    // Parameters:  val     -   the value to be set
    //              blob    -   blob containing the member.
    //              member  -   id of the member
    //              index   -   array index of member. Shall be 0 if member is not an array.
    // Returns:     -
    // Comments:    Sets the value for a enumeration member.
    DOTS_KERNEL_API void DotsC_SetEnumerationMember(const DotsC_EnumerationValue val,
                                                    char * & blob,
                                                    const DotsC_MemberIndex member,
                                                    const DotsC_ArrayIndex index);

    // Function:    DotsC_SetInt32Member
    // Parameters:  val     -   the value to be set
    //              blob    -   blob containing the member.
    //              member  -   id of the member
    //              index   -   array index of member. Shall be 0 if member is not an array.
    // Returns:     -
    // Comments:    Sets the value for a 32-bits integer member.
    DOTS_KERNEL_API void DotsC_SetInt32Member(const DotsC_Int32 val,
                                              char * & blob,
                                              const DotsC_MemberIndex member,
                                              const DotsC_ArrayIndex index);

    // Function:    DotsC_SetInt64Member
    // Parameters:  val     -   the value to be set
    //              blob    -   blob containing the member.
    //              member  -   id of the member
    //              index   -   array index of member. Shall be 0 if member is not an array.
    // Returns:     -
    // Comments:    Sets the value for a 64-bits integer member.
    DOTS_KERNEL_API void DotsC_SetInt64Member(const DotsC_Int64 val,
                                              char * & blob,
                                              const DotsC_MemberIndex member,
                                              const DotsC_ArrayIndex index);

    // Function:    DotsC_SetFloat32Member
    // Parameters:  val     -   the value to be set
    //              blob    -   blob containing the member.
    //              member  -   id of the member
    //              index   -   array index of member. Shall be 0 if member is not an array.
    // Comments:    Sets the value for a 32-bits float member.
    DOTS_KERNEL_API void DotsC_SetFloat32Member(const DotsC_Float32 val,
                                                char * & blob,
                                                const DotsC_MemberIndex member,
                                                const DotsC_ArrayIndex index);

    // Function:    DotsC_SetFloat64Member
    // Parameters:  val     -   the value to be set
    //              blob    -   blob containing the member.
    //              member  -   id of the member
    //              index   -   array index of member. Shall be 0 if member is not an array.
    // Returns:     -
    // Comments:    Sets the value for a 64-bits float member.
    DOTS_KERNEL_API void DotsC_SetFloat64Member(const DotsC_Float64 val,
                                                char * & blob,
                                                const DotsC_MemberIndex member,
                                                const DotsC_ArrayIndex index);

    // Function:    DotsC_SetStringMember
    // Parameters:  val     -   the value to be set
    //              blob    -   blob containing the member.
    //              member  -   id of the member
    //              index   -   array index of member. Shall be 0 if member is not an array.
    // Returns:     -
    // Comments:    Sets the value for a string member.
    DOTS_KERNEL_API void DotsC_SetStringMember(const char * const val,
                                               char * & blob,
                                               const DotsC_MemberIndex member,
                                               const DotsC_ArrayIndex index);

    // Function:    DotsC_SetTypeIdMember
    // Parameters:  val     -   the value to be set
    //              blob    -   blob containing the member.
    //              member  -   id of the member
    //              index   -   array index of member. Shall be 0 if member is not an array.
    // Returns:     -
    // Comments:    Sets the value for a type id member.
    DOTS_KERNEL_API void DotsC_SetTypeIdMember(const DotsC_TypeId val,
                                               char * & blob,
                                               const DotsC_MemberIndex member,
                                               const DotsC_ArrayIndex index);


    // Function:    DotsC_SetHashedIdMember
    // Parameters:  hashVal     -   the value to be set.
    //              strVal      - the string value to set, can be NULL if need be.
    //              blob    -   blob containing the member.
    //              member  -   id of the member
    //              index   -   array index of member. Shall be 0 if member is not an array.
    // Returns:     -
    // Comments:    Sets the value for a hashed id member.
    DOTS_KERNEL_API void DotsC_SetHashedIdMember(const DotsC_Int64 hashVal,
                                                 const char * const strVal,
                                                 char * & blob,
                                                 const DotsC_MemberIndex member,
                                                 const DotsC_ArrayIndex index);

    // Function:    DotsC_SetEntityIdMember
    // Parameters:  val     -   the value to be set.
    //              instanceIdStr - the string value to set, can be NULL if need be.
    //              blob    -   blob containing the member.
    //              member  -   id of the member
    //              index   -   array index of member. Shall be 0 if member is not an array.
    // Returns:     -
    // Comments:    Sets the value for a object id member.
    //              NOTE! The parameter val is sent by reference because of Ada will not work
    //                    with a structure by value.
    DOTS_KERNEL_API void DotsC_SetEntityIdMember(const DotsC_EntityId& val,
                                                 const char * const instanceIdStr,
                                                 char * & blob,
                                                 const DotsC_MemberIndex member,
                                                 const DotsC_ArrayIndex index);

    // Function:    DotsC_SetObjectMember
    // Parameters:  val     -   the value to be set
    //              blob    -   blob containing the member.
    //              member  -   id of the member
    //              index   -   array index of member. Shall be 0 if member is not an array.
    // Returns:     -
    // Comments:    Sets the value for a object member.
    DOTS_KERNEL_API void DotsC_SetObjectMember(const char * const val,
                                               char * & blob,
                                               const DotsC_MemberIndex member,
                                               const DotsC_ArrayIndex index);

    // Function:    DotsC_SetBinaryMember
    // Parameters:  val     -   the value to be set
    //              numberOfBytes    -   number of bytes in val to be written into the blob
    //              blob    -   blob containing the member.
    //              member  -   id of the member
    //              index   -   array index of member. Shall be 0 if member is not an array.
    // Returns:     -
    // Comments:    Sets the value for a object member.
    DOTS_KERNEL_API void DotsC_SetBinaryMember(const char * const val,
                                               const DotsC_Int32 numberOfBytes,
                                               char * & blob,
                                               const DotsC_MemberIndex member,
                                               const DotsC_ArrayIndex index);


    //*********************************
    //* For "real classes"
    //*********************************

    //TODO: rename this function to reflect what it is actually doing (not size of whole class, just of the members in this class)
    DOTS_KERNEL_API DotsC_Int32 DotsC_GetInitialSize(const DotsC_TypeId typeId);

    DOTS_KERNEL_API void DotsC_FormatBlob(char * const blob,
                                          const DotsC_Int32 blobSize,
                                          const DotsC_TypeId typeId,
                                          char * & beginningOfUnused);

    DOTS_KERNEL_API void DotsC_CreateObjectMember(char * const insideBlob,
                                                  const DotsC_Int32 blobSize,
                                                  const DotsC_TypeId typeId,
                                                  const DotsC_MemberIndex member,
                                                  const DotsC_ArrayIndex index,
                                                  const bool isChanged,
                                                  char * & beginningOfUnused);

    DOTS_KERNEL_API void DotsC_CreateStringMember(char * const insideBlob,
                                                  const DotsC_Int32 stringLength, //remember the null-termination!
                                                  const DotsC_MemberIndex member,
                                                  const DotsC_ArrayIndex index,
                                                  const bool isChanged,
                                                  char * & beginningOfUnused);

    DOTS_KERNEL_API void DotsC_CreateBinaryMember(char * const insideBlob,
                                                  const DotsC_Int32 binarySize,
                                                  const DotsC_MemberIndex member,
                                                  const DotsC_ArrayIndex index,
                                                  const bool isChanged,
                                                  char * & beginningOfUnused);


    DOTS_KERNEL_API void DotsC_SetBooleanMemberInPreallocated(const bool val,
                                                              const bool isNull,
                                                              const bool isChanged,
                                                              char * const blob,
                                                              const DotsC_MemberIndex member,
                                                              const DotsC_ArrayIndex index);

    DOTS_KERNEL_API void DotsC_SetInt32MemberInPreallocated(const DotsC_Int32 val,
                                                            const bool isNull,
                                                            const bool isChanged,
                                                            char * const blob,
                                                            const DotsC_MemberIndex member,
                                                            const DotsC_ArrayIndex index);


    DOTS_KERNEL_API void DotsC_SetInt64MemberInPreallocated(const DotsC_Int64 val,
                                                            const bool isNull,
                                                            const bool isChanged,
                                                            char * const blob,
                                                            const DotsC_MemberIndex member,
                                                            const DotsC_ArrayIndex index);

    DOTS_KERNEL_API void DotsC_SetFloat32MemberInPreallocated(const DotsC_Float32 val,
                                                              const bool isNull,
                                                              const bool isChanged,
                                                              char * const blob,
                                                              const DotsC_MemberIndex member,
                                                              const DotsC_ArrayIndex index);

    DOTS_KERNEL_API void DotsC_SetFloat64MemberInPreallocated(const DotsC_Float64 val,
                                                              const bool isNull,
                                                              const bool isChanged,
                                                              char * const blob,
                                                              const DotsC_MemberIndex member,
                                                              const DotsC_ArrayIndex index);

    DOTS_KERNEL_API void DotsC_SetHashedIdMemberInPreallocated(const DotsC_Int64 hashVal,
                                                               const char * const strVal,
                                                               const DotsC_Int32 stringLength,
                                                               const bool isNull,
                                                               const bool isChanged,
                                                               char * const blob,
                                                               const DotsC_MemberIndex member,
                                                               const DotsC_ArrayIndex index,
                                                               char * & beginningOfUnused);

    DOTS_KERNEL_API void DotsC_SetEntityIdMemberInPreallocated(const DotsC_EntityId & entityId,
                                                               const char * const instanceIdStr,
                                                               const DotsC_Int32 stringLength,
                                                               const bool isNull,
                                                               const bool isChanged,
                                                               char * const blob,
                                                               const DotsC_MemberIndex member,
                                                               const DotsC_ArrayIndex index,
                                                               char * & beginningOfUnused);

    DOTS_KERNEL_API void DotsC_GetPropertyMappingKind(const DotsC_TypeId typeId,
                                                      const DotsC_TypeId propertyId,
                                                      const DotsC_MemberIndex member,
                                                      DotsC_PropertyMappingKind & mappingKind,
                                                      DotsC_ErrorCode & errorCode);

    DOTS_KERNEL_API void DotsC_GetClassMemberReference(const DotsC_TypeId typeId,
                                                       const DotsC_TypeId propertyId,
                                                       const DotsC_MemberIndex member,
                                                       const DotsC_Int32 * & classMemberReference, //out
                                                       DotsC_Int32 & classMemberReferenceSize); //out

    DOTS_KERNEL_API void DotsC_GetEnumerationChecksum(const DotsC_TypeId typeId,
                                                      DotsC_TypeId & checksum);

    //************************************************************************************
    //* Functions for retrieval of parameters in properties
    //************************************************************************************
    // Function:    DotsC_GetBooleanPropertyParameter
    // Parameters:  typeId      -   id of class
    //              propertyId  -   TypeId of the property
    //              member      -   member index
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter boolean value.
    DOTS_KERNEL_API void DotsC_GetBooleanPropertyParameter(const DotsC_TypeId typeId,
                                                           const DotsC_TypeId propertyId,
                                                           const DotsC_MemberIndex member,
                                                           const DotsC_ArrayIndex index,
                                                           bool & val);

    // Function:    DotsC_GetEnumerationPropertyParameter
    // Parameters:  enumId      -   id of enumeration type
    //              typeId      -   id of class
    //              propertyId  -   TypeId of the property
    //              member      -   member index
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    //              isMismatch  -   true if there is mismatch between caller and dots.
    // Returns:     -
    // Comments:    Gets a parameter enumeration value.
    DOTS_KERNEL_API void DotsC_GetEnumerationPropertyParameter(const DotsC_TypeId typeId,
                                                               const DotsC_TypeId propertyId,
                                                               const DotsC_MemberIndex member,
                                                               const DotsC_ArrayIndex index,
                                                               DotsC_EnumerationValue & val);

    // Function:    DotsC_GetInt32PropertyParameter
    // Parameters:  typeId      -   id of class
    //              propertyId  -   TypeId of the property
    //              member      -   member index
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter 32-bits integer value.
    DOTS_KERNEL_API void DotsC_GetInt32PropertyParameter(const DotsC_TypeId typeId,
                                                         const DotsC_TypeId propertyId,
                                                         const DotsC_MemberIndex member,
                                                         const DotsC_ArrayIndex index,
                                                         DotsC_Int32 & val);

    // Function:    DotsC_GetInt64PropertyParameter
    // Parameters:  typeId      -   id of class
    //              propertyId  -   TypeId of the property
    //              member      -   member index
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter 64-bits integer value.
    DOTS_KERNEL_API void DotsC_GetInt64PropertyParameter(const DotsC_TypeId typeId,
                                                         const DotsC_TypeId propertyId,
                                                         const DotsC_MemberIndex member,
                                                         const DotsC_ArrayIndex index,
                                                         DotsC_Int64 & val);

    // Function:    DotsC_GetFloat32PropertyParameter
    // Parameters:  typeId      -   id of class
    //              propertyId  -   TypeId of the property
    //              member      -   member index
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter 32-bits float value.
    DOTS_KERNEL_API void DotsC_GetFloat32PropertyParameter(const DotsC_TypeId typeId,
                                                           const DotsC_TypeId propertyId,
                                                           const DotsC_MemberIndex member,
                                                           const DotsC_ArrayIndex index,
                                                           DotsC_Float32 & val);

    // Function:    DotsC_GetFloat64PropertyParameter
    // Parameters:  typeId      -   id of class
    //              propertyId  -   TypeId of the property
    //              member      -   member index
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter 64-bits float value.
    DOTS_KERNEL_API void DotsC_GetFloat64PropertyParameter(const DotsC_TypeId typeId,
                                                           const DotsC_TypeId propertyId,
                                                           const DotsC_MemberIndex member,
                                                           const DotsC_ArrayIndex index,
                                                           DotsC_Float64 & val);

    // Function:    DotsC_GetStringPropertyParameter
    // Parameters:  typeId      -   id of class
    //              propertyId  -   TypeId of the property
    //              member      -   member index
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter string value.
    DOTS_KERNEL_API void DotsC_GetStringPropertyParameter(const DotsC_TypeId typeId,
                                                          const DotsC_TypeId propertyId,
                                                          const DotsC_MemberIndex member,
                                                          const DotsC_ArrayIndex index,
                                                          const char * & val);

    // Function:    DotsC_GetTypeIdPropertyParameter
    // Parameters:  typeId      -   id of class
    //              propertyId  -   TypeId of the property
    //              member      -   member index
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter type id value.
    DOTS_KERNEL_API void DotsC_GetTypeIdPropertyParameter(const DotsC_TypeId typeId,
                                                          const DotsC_TypeId propertyId,
                                                          const DotsC_MemberIndex member,
                                                          const DotsC_ArrayIndex index,
                                                          DotsC_TypeId & val);




    // Function:    DotsC_GetHashedIdPropertyParameter
    // Parameters:  typeId      -   id of class
    //              propertyId  -   TypeId of the property
    //              member      -   member index
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              hashVal     -   retrived value, out parameter
    //              strVal      -   string rep, if there was one, otherwise NULL
    // Returns:     -
    // Comments:    Gets a parameter object id value.
    DOTS_KERNEL_API void DotsC_GetHashedIdPropertyParameter(const DotsC_TypeId typeId,
                                                            const DotsC_TypeId propertyId,
                                                            const DotsC_MemberIndex member,
                                                            const DotsC_ArrayIndex index,
                                                            DotsC_Int64 & hashVal,
                                                            const char * & strVal);

    // Function:    DotsC_GetEntityIdPropertyParameter
    // Parameters:  typeId      -   id of class
    //              propertyId  -   TypeId of the property
    //              member      -   member index
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    //              instanceIdStr - string rep, if there was one, otherwise NULL
    // Returns:     -
    // Comments:    Gets a parameter object id value.
    DOTS_KERNEL_API void DotsC_GetEntityIdPropertyParameter(const DotsC_TypeId typeId,
                                                            const DotsC_TypeId propertyId,
                                                            const DotsC_MemberIndex member,
                                                            const DotsC_ArrayIndex index,
                                                            DotsC_EntityId & val,
                                                            const char * & instanceIdStr);

    // Function:    DotsC_GetObjectPropertyParameter
    // Parameters:  typeId      -   id of class
    //              propertyId  -   TypeId of the property
    //              member      -   member index
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    // Returns:     -
    // Comments:    Gets a parameter object value.
    DOTS_KERNEL_API void DotsC_GetObjectPropertyParameter(const DotsC_TypeId typeId,
                                                          const DotsC_TypeId propertyId,
                                                          const DotsC_MemberIndex member,
                                                          const DotsC_ArrayIndex index,
                                                          const char * & val);

    // Function:    DotsC_GetObjectPropertyParameter
    // Parameters:  typeId      -   id of class
    //              propertyId  -   TypeId of the property
    //              member      -   member index
    //              index       -   array index. If parameter is not an array index shall be 0.
    //              val         -   retrived value, out parameter
    //              size        -   size of the binary value
    // Returns:     -
    // Comments:    Gets a parameter object value.
    DOTS_KERNEL_API void DotsC_GetBinaryPropertyParameter(const DotsC_TypeId typeId,
                                                          const DotsC_TypeId propertyId,
                                                          const DotsC_MemberIndex member,
                                                          const DotsC_ArrayIndex index,
                                                          const char * & val,
                                                          DotsC_Int32 & size);
    
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
    //              buf         -   Buffer where file path will be put, will be null terminated
    //              bufSize     -   size of buf.
    //              resultSize  -   if the buffer was big enough for the xml this holds the number
    //                              of bytes written
    //                              if it was too small it holds the size that was needed
    //                              (so resultSize > bufSize ==> try again with bigger buffer)
    //                              -1 on failure to find type or dou file.
    //                              Includes the null termination at the end.
    // Returns:     -
    // Comments:    Get the full path to the dou file that the type id represents
    //              Note that this function looks at the disk every time it is called. No caching
    //              is performed at all. Not meant to be used in "real" code.
    DOTS_KERNEL_API void DotsC_GetDouFilePathForType(const DotsC_TypeId typeId,
                                                     char * const buf, 
                                                     const DotsC_Int32 bufSize, 
                                                     DotsC_Int32 & resultSize);

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
}


#endif
