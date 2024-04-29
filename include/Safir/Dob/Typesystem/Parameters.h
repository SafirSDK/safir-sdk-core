/******************************************************************************
*
* Copyright Saab AB, 2006-2013, 2024 (http://safirsdkcore.com)
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

#ifndef __DOTS_PARAMETERS_H__
#define __DOTS_PARAMETERS_H__

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/Object.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
/**
 * Functions for getting parameter information from types.
 *
 * With these operations you can get parameter values from types.
 * You can also get information about the parameters in a type, such as
 * parameter names and indexes, TypeIds of parameters etc.
 */
namespace Parameters
{
    /**
     * @name Information about parameters.
     */

    /** @{ */

    /**
     * Get the number of parameters defined in a class.
     *
     * @param typeId [in] - TypeId of class.
     * @return The number of parameters.
     * @throws IllegalValueException There is no such type defined.
     */
    DOTS_CPP_API Dob::Typesystem::Int32 GetNumberOfParameters(const Dob::Typesystem::TypeId typeId);

    /**
     * Gets index of a named parameter.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameterName [in] - Name of parameter.
     * @return index of the named parameter.
     * @throws IllegalValueException There is no such type or parameter defined.
     */
    DOTS_CPP_API Dob::Typesystem::ParameterIndex GetIndex(const Dob::Typesystem::TypeId typeId,
                                                          const std::wstring& parameterName);

    /**
     * Get the name of the specified parameter as it was defined in the xml description.
     *
     * If the parameter does not exist the returned value is undefined. Use
     * #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @return The name of the parameter.
     */
    DOTS_CPP_API const std::wstring GetName(const Dob::Typesystem::TypeId typeId,
                                            const Dob::Typesystem::ParameterIndex parameter);

    /**
     * Get the type name of the specified member as it was defined in the xml description.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @return Parameter type name.
     */
    DOTS_CPP_API std::wstring GetTypeName(const Dob::Typesystem::TypeId typeId,
                                          const Dob::Typesystem::ParameterIndex parameter);

    /**
     * Get the type of a parameter.
     *
     * If the parameter does not exist the returned value is undefined. Use
     * #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @return The type of the parameter.
     */
    DOTS_CPP_API MemberType GetType(const Dob::Typesystem::TypeId typeId,
                                    const Dob::Typesystem::ParameterIndex parameter);

    /**
     * Get the array size of a parameter.
     *
     * This function is identical to GetCollectionSize, which is the new name for this function.
     *
     * If the parameter does not exist the returned value is undefined. Use
     * #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @return The array size of the parameter, or 1 if it is not an array.
     */
    DOTS_CPP_API Dob::Typesystem::Int32 GetArraySize(const Dob::Typesystem::TypeId typeId,
                                                     const Dob::Typesystem::ParameterIndex parameter);

    /**
     * Get the number of elements in a collection parameter.
     *
     * This function works on arrays, sequences and dictionaries.
     *
     * If the parameter does not exist the returned value is undefined. Use
     * #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @return The size of the parameter, or 1 if it is not a collection.
     */
    DOTS_CPP_API Dob::Typesystem::Int32 GetCollectionSize(const Dob::Typesystem::TypeId typeId,
                                                          const Dob::Typesystem::ParameterIndex parameter);

    /**
     * Get information about a specific parameter.
     *
     * If the parameter does not exist the returned value is undefined. Use
     * #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param parameterType [out] - The type of the parameter.
     * @param keyType [out] - The type of the key if the parameter is a dictionary, otherwise undefined.
     * @param parameterName [out] - The name of the parameter.
     * @param parameterTypeId [out] - if parameterType is object or enumeration, this is the typeId of that type.
     *                                If parameterType is something else the value is -1.
     *                                For dictionaries this is the type of the value.
     * @param keyTypeId [out] - if the parameter is a dictionary and the key type is an enumeration this is
     *                          the type id of the enumeration. Otherwise undefined.
     * @param collectionType [out] - The parameter collection type, can be single value, array, sequence or dictionary.
     * @param numberOfValues [out] - Number of values if the parameter has. 1 if single value.
     */
    DOTS_CPP_API void GetInfo(const Dob::Typesystem::TypeId typeId,
                              const Dob::Typesystem::ParameterIndex parameter,
                              Dob::Typesystem::MemberType& parameterType,
                              Dob::Typesystem::MemberType& keyType,
                              std::wstring& parameterName,
                              Dob::Typesystem::TypeId& parameterTypeId,
                              Dob::Typesystem::TypeId& keyTypeId,
                              Dob::Typesystem::CollectionType & collectionType,
                              Dob::Typesystem::Int32& numberOfValues);

    /** @} */

    /**
     * @name Parameter values.
     */

    /** @{ */

    /**
     * Get a boolean parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    DOTS_CPP_API bool GetBoolean(const Dob::Typesystem::TypeId typeId,
                                 const Dob::Typesystem::ParameterIndex parameter,
                                 const Dob::Typesystem::ArrayIndex index);

    /**
     * Get an enumeration parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    DOTS_CPP_API Dob::Typesystem::Int32 GetEnumeration(const Dob::Typesystem::TypeId typeId,
                                                       const Dob::Typesystem::ParameterIndex parameter,
                                                       const Dob::Typesystem::ArrayIndex index);

    /**
     * Get an Int32 parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    DOTS_CPP_API Dob::Typesystem::Int32 GetInt32(const Dob::Typesystem::TypeId typeId,
                                                 const Dob::Typesystem::ParameterIndex parameter,
                                                 const Dob::Typesystem::ArrayIndex index);

    /**
     * Get an Int64 parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    DOTS_CPP_API Dob::Typesystem::Int64 GetInt64(const Dob::Typesystem::TypeId typeId,
                                                 const Dob::Typesystem::ParameterIndex parameter,
                                                 const Dob::Typesystem::ArrayIndex index);

    /**
     * Get a Float32 parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    DOTS_CPP_API Dob::Typesystem::Float32 GetFloat32(const Dob::Typesystem::TypeId typeId,
                                                     const Dob::Typesystem::ParameterIndex parameter,
                                                     const Dob::Typesystem::ArrayIndex index);

    /**
     * Get a Float64 parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    DOTS_CPP_API Dob::Typesystem::Float64 GetFloat64(const Dob::Typesystem::TypeId typeId,
                                                     const Dob::Typesystem::ParameterIndex parameter,
                                                     const Dob::Typesystem::ArrayIndex index);
    /**
     * Get a string parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    DOTS_CPP_API const std::wstring GetString(const Dob::Typesystem::TypeId typeId,
                                              const Dob::Typesystem::ParameterIndex parameter,
                                              const Dob::Typesystem::ArrayIndex index);

    /**
     * Get a TypeId parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    DOTS_CPP_API Dob::Typesystem::TypeId GetTypeId(const Dob::Typesystem::TypeId typeId,
                                                   const Dob::Typesystem::ParameterIndex parameter,
                                                   const Dob::Typesystem::ArrayIndex index);

    /**
     * Get a InstanceId parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    DOTS_CPP_API const Dob::Typesystem::InstanceId GetInstanceId(const Dob::Typesystem::TypeId typeId,
                                                                 const Dob::Typesystem::ParameterIndex parameter,
                                                                 const Dob::Typesystem::ArrayIndex index);

    /**
     * Get an EntityId parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    DOTS_CPP_API const Dob::Typesystem::EntityId GetEntityId(const Dob::Typesystem::TypeId typeId,
                                                             const Dob::Typesystem::ParameterIndex parameter,
                                                             const Dob::Typesystem::ArrayIndex index);

    /**
     * Get a ChannelId parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    DOTS_CPP_API const Dob::Typesystem::ChannelId GetChannelId(const Dob::Typesystem::TypeId typeId,
                                                               const Dob::Typesystem::ParameterIndex parameter,
                                                               const Dob::Typesystem::ArrayIndex index);

    /**
     * Get a HandlerId parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    DOTS_CPP_API const Dob::Typesystem::HandlerId GetHandlerId(const Dob::Typesystem::TypeId typeId,
                                                               const Dob::Typesystem::ParameterIndex parameter,
                                                               const Dob::Typesystem::ArrayIndex index);

    /**
     * Get an Object parameter value.
     *
     * This method will return a smart pointer to a new copy of the parameter.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    DOTS_CPP_API const Dob::Typesystem::ObjectPtr GetObject(const Dob::Typesystem::TypeId typeId,
                                                            const Dob::Typesystem::ParameterIndex parameter,
                                                            const Dob::Typesystem::ArrayIndex index);

    /**
     * Get a Binary parameter value.
     *
     * This method will retrieve a pointer to the binary data and the number of bytes the binary data contains.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    DOTS_CPP_API const Dob::Typesystem::Binary GetBinary(const Dob::Typesystem::TypeId typeId,
                                                         const Dob::Typesystem::ParameterIndex parameter,
                                                         const Dob::Typesystem::ArrayIndex index);

    /**
     * Get the index of a Int32 dictionary key.
     *
     * The index can then be used with the Get-mehtods above to retrieve the parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param key [in] - Dictionary key.
     * @return Index or -1 if key does not exist.
     */
    DOTS_CPP_API Int32 DictionaryKeyToIndex(const Dob::Typesystem::TypeId typeId,
                                            const Dob::Typesystem::ParameterIndex parameter,
                                            const Dob::Typesystem::Int32 key);

    /**
     * Get the index of a Int64 dictionary key.
     *
     * The index can then be used with the Get-mehtods above to retrieve the parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param key [in] - Dictionary key.
     * @return Index or -1 if key does not exist.
     */
    DOTS_CPP_API Int32 DictionaryKeyToIndex(const Dob::Typesystem::TypeId typeId,
                                            const Dob::Typesystem::ParameterIndex parameter,
                                            const Dob::Typesystem::Int64 key);

    /**
     * Get the index of a string dictionary key.
     *
     * The index can then be used with the Get-mehtods above to retrieve the parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param key [in] - Dictionary key.
     * @return Index or -1 if key does not exist.
     */
    DOTS_CPP_API Int32 DictionaryKeyToIndex(const Dob::Typesystem::TypeId typeId,
                                            const Dob::Typesystem::ParameterIndex parameter,
                                            const std::wstring& key);

    /**
     * Get the index of a EntityId dictionary key.
     *
     * The index can then be used with the Get-mehtods above to retrieve the parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param key [in] - Dictionary key.
     * @return Index or -1 if key does not exist.
     */
    DOTS_CPP_API Int32 DictionaryKeyToIndex(const Dob::Typesystem::TypeId typeId,
                                            const Dob::Typesystem::ParameterIndex parameter,
                                            const Dob::Typesystem::EntityId& key);

    /**
     * Get the index of a InstanceId dictionary key.
     *
     * The index can then be used with the Get-mehtods above to retrieve the parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param key [in] - Dictionary key.
     * @return Index or -1 if key does not exist.
     */
    DOTS_CPP_API Int32 DictionaryKeyToIndex(const Dob::Typesystem::TypeId typeId,
                                            const Dob::Typesystem::ParameterIndex parameter,
                                            const Dob::Typesystem::InstanceId& key);

    /**
     * Get the index of a HandlerId dictionary key.
     *
     * The index can then be used with the Get-mehtods above to retrieve the parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param key [in] - Dictionary key.
     * @return Index or -1 if key does not exist.
     */
    DOTS_CPP_API Int32 DictionaryKeyToIndex(const Dob::Typesystem::TypeId typeId,
                                            const Dob::Typesystem::ParameterIndex parameter,
                                            const Dob::Typesystem::HandlerId& key);

    /**
     * Get the index of a ChannelId dictionary key.
     *
     * The index can then be used with the Get-mehtods above to retrieve the parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param key [in] - Dictionary key.
     * @return Index or -1 if key does not exist.
     */
    DOTS_CPP_API Int32 DictionaryKeyToIndex(const Dob::Typesystem::TypeId typeId,
                                            const Dob::Typesystem::ParameterIndex parameter,
                                            const Dob::Typesystem::ChannelId& key);


    /** @} */


    /**
     * @name Parameter Dictionary keys.
     */

    /** @{ */


    /**
     * Get an enumeration parameter dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return Parameter key.
     */
    DOTS_CPP_API Dob::Typesystem::Int32 GetEnumerationDictionaryKey(const Dob::Typesystem::TypeId typeId,
                                                                    const Dob::Typesystem::ParameterIndex parameter,
                                                                    const Dob::Typesystem::ArrayIndex index);

    /**
     * Get an Int32 parameter dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return parameter dictionary key.
     */
    DOTS_CPP_API Dob::Typesystem::Int32 GetInt32DictionaryKey(const Dob::Typesystem::TypeId typeId,
                                                              const Dob::Typesystem::ParameterIndex parameter,
                                                              const Dob::Typesystem::ArrayIndex index);

    /**
     * Get an Int64 parameter dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return parameter dictionary key.
     */
    DOTS_CPP_API Dob::Typesystem::Int64 GetInt64DictionaryKey(const Dob::Typesystem::TypeId typeId,
                                                              const Dob::Typesystem::ParameterIndex parameter,
                                                              const Dob::Typesystem::ArrayIndex index);

    /**
     * Get a string parameter dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return parameter dictionary key.
     */
    DOTS_CPP_API const std::wstring GetStringDictionaryKey(const Dob::Typesystem::TypeId typeId,
                                                           const Dob::Typesystem::ParameterIndex parameter,
                                                           const Dob::Typesystem::ArrayIndex index);

    /**
     * Get a TypeId parameter dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return parameter dictionary key.
     */
    DOTS_CPP_API Dob::Typesystem::TypeId GetTypeIdDictionaryKey(const Dob::Typesystem::TypeId typeId,
                                                                const Dob::Typesystem::ParameterIndex parameter,
                                                                const Dob::Typesystem::ArrayIndex index);

    /**
     * Get a InstanceId parameter dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return parameter dictionary key.
     */
    DOTS_CPP_API const Dob::Typesystem::InstanceId GetInstanceIdDictionaryKey(const Dob::Typesystem::TypeId typeId,
                                                                              const Dob::Typesystem::ParameterIndex parameter,
                                                                              const Dob::Typesystem::ArrayIndex index);

    /**
     * Get an EntityId parameter dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return parameter dictionary key.
     */
    DOTS_CPP_API const Dob::Typesystem::EntityId GetEntityIdDictionaryKey(const Dob::Typesystem::TypeId typeId,
                                                                          const Dob::Typesystem::ParameterIndex parameter,
                                                                          const Dob::Typesystem::ArrayIndex index);

    /**
     * Get a ChannelId parameter dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return parameter dictionary key.
     */
    DOTS_CPP_API const Dob::Typesystem::ChannelId GetChannelIdDictionaryKey(const Dob::Typesystem::TypeId typeId,
                                                                            const Dob::Typesystem::ParameterIndex parameter,
                                                                            const Dob::Typesystem::ArrayIndex index);

    /**
     * Get a HandlerId parameter dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return parameter dictionary key.
     */
    DOTS_CPP_API const Dob::Typesystem::HandlerId GetHandlerIdDictionaryKey(const Dob::Typesystem::TypeId typeId,
                                                                            const Dob::Typesystem::ParameterIndex parameter,
                                                                            const Dob::Typesystem::ArrayIndex index);
    /** @} */


}
}
}
}

#endif
