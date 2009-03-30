/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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

#ifndef __BLOBOPERATIONS_H__
#define __BLOBOPERATIONS_H__

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/ValueContainers.h>
#include <Safir/Dob/Typesystem/EnumerationContainerBase.h>
#include <Safir/Dob/Typesystem/ObjectContainer.h>

namespace Safir
{
namespace Dob
{

namespace Typesystem
{
    /**
     * Operations on blobs.
     *
     * Functions for getting information from blobs and setting and getting
     * values in blobs.
     * Currently this functionality is meant mainly for internal use, but the functions
     * can be used to modify binary serializations of objects.
     * But be careful if you do, and talk to your closest DOB expert first.
     *
     * Note: Most of these methods have no checks on them to make sure that everything
     *       has worked okay. They will just return unexpected values if something went wrong.
     */
    class DOTS_API BlobOperations
    {
    public:

        /**
        * Extract the TypeId from a blob
        *
        * @param blob [in] - The blob to read from.
        * @return The TypeId from the blob.
        */
        static TypeId GetTypeId(const char * const blob);

        /**
        * Get the size of the blob contained by this object
        *
        * @param blob [in] - the blob.
        * @return the size of the blob, in bytes.
        */
        static Int32 GetSize(char const * const blob);

        /**
         * Check if any member is changed.
         *
         * This method will recursively check if any member in the blob has its change flag set.
         *
         * @return True if any member has changed.
         */
         static bool IsChanged(char const * const blob);

        /**
         * @name Value operations on Blobs.
         */
        /** @{ */

        /**
         * Find out if a member is changed.
         *
         * @param [in,out] blob - Blob to look in.
         * @param [in] member - The member to check.
         * @param [in] index - Array index in member to check. Shall be 0 if the member is not an array.
         */
        static bool IsChanged(const char * const blob,
                              const Dob::Typesystem::MemberIndex member,
                              const Dob::Typesystem::ArrayIndex index);
    
        /**
         * Set a member to null.
         *
         * This methods sets a given member (with index) to null in a blob.
         * If the member is not an array the index must be 0.
         *
         * @param [in,out] blob - Blob to set the member in.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void SetNull(char * const blob,
                            const Dob::Typesystem::MemberIndex member,
                            const Dob::Typesystem::ArrayIndex index);

        /**
         * Set a bool in a blob.
         *
         * This method will set a bool member in a blob.
         * If the isNull parameter is true then only the isChange and isNull flags are set in the blob,
         * not the value (so it can be any value).
         *
         * @param [in,out] blob - Blob to set the member in.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         * @param [in] value - The value that the member is to be set to (unless isNull is true).
         * @param [in] isNull - Should the value be set to null.
         * @param [in] isChanged - Should the value be set to changed.
         */
        static void Set(char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        const bool value,
                        const bool isNull,
                        const bool isChanged);

        /**
         * Get a bool from a blob.
         *
         * This method will get a bool member and the associated isNull and isChange values from a blob.
         * The value parameter is not valid if isNull is true.
         *
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         * @param [out] value - The value of the member (invalid if isNull is true)
         * @param [out] isNull - The isNull flag of the member.
         * @param [out] isChanged - The isChanged flag of the member.
         */
        static void Get(char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        bool & value,
                        bool & isNull,
                        bool & isChanged);


        /**
         * Set an Int32 or EnumerationValue in a blob.
         *
         * This method will set a Int32 or EnumerationValue member in a blob.
         * If the isNull parameter is true then only the isChange and isNull flags are set in the blob,
         * not the value (so it can be any value).
         *
         * @param [in,out] blob - Blob to set the member in.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         * @param [in] value - The value that the member is to be set to (unless isNull is true).
         * @param [in] isNull - Should the value be set to null.
         * @param [in] isChanged - Should the value be set to changed.
         */
        static void Set(char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        const Int32 value,
                        const bool isNull,
                        const bool isChanged);

        /**
         * Get a Int32 or EnumerationValue from a blob.
         *
         * This method will get a Int32 or EnumerationValue member and the associated isNull and isChange values from a blob.
         * The value parameter is not valid if isNull is true.
         *
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         * @param [out] value - The value of the member (invalid if isNull is true)
         * @param [out] isNull - The isNull flag of the member.
         * @param [out] isChanged - The isChanged flag of the member.
         */
        static void Get(char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        Int32 & value,
                        bool & isNull,
                        bool & isChanged);


        /**
         * Set an Int64, TypeId, InstanceId, ChannelId or a HandlerId in a blob.
         *
         * This method will set a Int64-based type member in a blob.
         * If the isNull parameter is true then only the isChange and isNull flags in the blob,
         * not the value (so it can be any value).
         *
         * @param [in,out] blob - Blob to set the member in.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         * @param [in] value - The value that the member is to be set to (unless isNull is true).
         * @param [in] isNull - Should the value be set to null.
         * @param [in] isChanged - Should the value be set to changed.
         */
        static void Set(char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        const Int64 value,
                        const bool isNull,
                        const bool isChanged);

        /**
         * Get an Int64 from a blob.
         *
         * This method will get a Int64-based member and the associated isNull and isChange values from a blob.
         * The value parameter is not valid if isNull is true.
         *
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         * @param [out] value - The value of the member (invalid if isNull is true)
         * @param [out] isNull - The isNull flag of the member.
         * @param [out] isChanged - The isChanged flag of the member.
         */
        static void Get(char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        Int64 & value,
                        bool & isNull,
                        bool & isChanged);


        /**
         * Set a Float32 in a blob.
         *
         * This method will set a Float32 member in a blob.
         * If the isNull parameter is true then only the isChange and isNull flags in the blob,
         * not the value (so it can be any value).
         *
         * @param [in,out] blob - Blob to set the member in.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         * @param [in] value - The value that the member is to be set to (unless isNull is true).
         * @param [in] isNull - Should the value be set to null.
         * @param [in] isChanged - Should the value be set to changed.
         */
        static void Set(char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        const Float32 value,
                        const bool isNull,
                        const bool isChanged);

        /**
         * Get a Float32 from a blob.
         *
         * This method will get a Float32 member and the associated isNull and isChange values from a blob.
         * The value parameter is not valid if isNull is true.
         *
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         * @param [out] value - The value of the member (invalid if isNull is true)
         * @param [out] isNull - The isNull flag of the member.
         * @param [out] isChanged - The isChanged flag of the member.
         */
        static void Get(char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        Float32 & value,
                        bool & isNull,
                        bool & isChanged);


        /**
         * Set a Float64 in a blob.
         *
         * This method will set a Float64 member in a blob.
         * If the isNull parameter is true then only the isChange and isNull flags in the blob,
         * not the value (so it can be any value).
         *
         * @param [in,out] blob - Blob to set the member in.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         * @param [in] value - The value that the member is to be set to (unless isNull is true).
         * @param [in] isNull - Should the value be set to null.
         * @param [in] isChanged - Should the value be set to changed.
         */
        static void Set(char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        const Float64 value,
                        const bool isNull,
                        const bool isChanged);

        /**
         * Get a Float64 from a blob.
         *
         * This method will get a Float64 member and the associated isNull and isChange values from a blob.
         * The value parameter is not valid if isNull is true.
         *
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         * @param [out] value - The value of the member (invalid if isNull is true)
         * @param [out] isNull - The isNull flag of the member.
         * @param [out] isChanged - The isChanged flag of the member.
         */
        static void Get(char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        Float64 & value,
                        bool & isNull,
                        bool & isChanged);

        /**
         * Get an InstanceId from a blob.
         *
         * This method will get an InstanceId member and the associated isNull and isChange values from a blob.
         * The value parameter is not valid if isNull is true.
         *
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         * @param [out] value - The value of the member (invalid if isNull is true)
         * @param [out] isNull - The isNull flag of the member.
         * @param [out] isChanged - The isChanged flag of the member.
         */
        static void Get(char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        InstanceId & value,
                        bool & isNull,
                        bool & isChanged);

        /**
         * Get an EntityId from a blob.
         *
         * This method will get an EntityId member and the associated isNull and isChange values from a blob.
         * The value parameter is not valid if isNull is true.
         *
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         * @param [out] value - The value of the member (invalid if isNull is true)
         * @param [out] isNull - The isNull flag of the member.
         * @param [out] isChanged - The isChanged flag of the member.
         */
        static void Get(char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        EntityId & value,
                        bool & isNull,
                        bool & isChanged);

        /**
         * Get a ChannelId from a blob.
         *
         * This method will get a ChannelId member and the associated isNull and isChange values from a blob.
         * The value parameter is not valid if isNull is true.
         *
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         * @param [out] value - The value of the member (invalid if isNull is true)
         * @param [out] isNull - The isNull flag of the member.
         * @param [out] isChanged - The isChanged flag of the member.
         */
        static void Get(char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        ChannelId & value,
                        bool & isNull,
                        bool & isChanged);

        /**
         * Get an HandlerId from a blob.
         *
         * This method will get an HandlerId member and the associated isNull and isChange values from a blob.
         * The value parameter is not valid if isNull is true.
         *
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         * @param [out] value - The value of the member (invalid if isNull is true)
         * @param [out] isNull - The isNull flag of the member.
         * @param [out] isChanged - The isChanged flag of the member.
         */
        static void Get(char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        HandlerId & value,
                        bool & isNull,
                        bool & isChanged);

        /**
         * Get a std::wstring from a blob.
         *
         * This method will get a std::wstring member and the associated isNull and isChange values from a blob.
         * The value parameter is not valid if isNull is true.
         *
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         * @param [out] value - The value of the member (invalid if isNull is true)
         * @param [out] isNull - The isNull flag of the member.
         * @param [out] isChanged - The isChanged flag of the member.
         */
        static void Get(char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        std::wstring & value,
                        bool & isNull,
                        bool & isChanged);

        /**
         * Get a blob from a blob.
         *
         * This method will get a blob member and the associated isNull and isChange values from a blob.
         * The value parameter is not valid if isNull is true.
         *
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         * @param [out] childBlob - The child blob (invalid if isNull is true)
         * @param [out] isNull - The isNull flag of the member.
         * @param [out] isChanged - The isChanged flag of the member.
         */
        static void Get(char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        const char * & childBlob,
                        bool & isNull,
                        bool & isChanged);

        /**
         * Get a Binary a blob.
         *
         * This method will get a Dob::Typesystem::Binary member and the associated isNull and isChange values from a blob.
         * The value parameter is not valid if isNull is true.
         *
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         * @param [out] value - The value of the member (invalid if isNull is true)
         * @param [out] isNull - The isNull flag of the member.
         * @param [out] isChanged - The isChanged flag of the member.
         */
        static void Get(char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        Dob::Typesystem::Binary & value,
                        bool & isNull,
                        bool & isChanged);

        /** @} */

        /**
         * @name Container operations on Blobs.
         * These operations use the values found in the containers to set the values in the blobs.
         */
        /** @{ */

        /**
         * Set a boolean in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in,out] blob - Blob to set the member in.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const BooleanContainer & value,
                        char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get a boolean from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(BooleanContainer & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set an enumeration in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in,out] blob - Blob to set the member in.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const EnumerationContainerBase & value,
                        char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an enumeration from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(EnumerationContainerBase & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Set an Int32 in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in,out] blob - Blob to set the member in.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const Int32Container & value,
                        char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an Int32 from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(Int32Container & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Set an Int64 in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in,out] blob - Blob to set the member in.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const Int64Container & value,  //will be used on TypeIdContainers too
                        char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an Int64 from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(Int64Container & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Set a Float32 in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in,out] blob - Blob to set the member in.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const Float32Container & value,
                        char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get a Float32 from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(Float32Container & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Set a Float64 in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in,out] blob - Blob to set the member in.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const Float64Container & value,
                        char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get a Float64 from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(Float64Container & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set an InstanceId in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in,out] blob - Blob to set the member in.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const InstanceIdContainer & value,
                        char * const blob,
                        char * & beginningOfUnused,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an InstanceId from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(InstanceIdContainer & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set an EntityId in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in,out] blob - Blob to set the member in.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const EntityIdContainer & value,
                        char * const blob,
                        char * & beginningOfUnused,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an EntityId from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(EntityIdContainer & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set an ChannelId in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in,out] blob - Blob to set the member in.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const ChannelIdContainer & value,
                        char * const blob,
                        char * & beginningOfUnused,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an ChannelId from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(ChannelIdContainer & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set an HandlerId in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in,out] blob - Blob to set the member in.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const HandlerIdContainer & value,
                        char * const blob,
                        char * & beginningOfUnused,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an HandlerId from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(HandlerIdContainer & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set a string in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in,out] blob - Blob to set the member in.
         * @param [in,out] beginningOfUnused - The start of the unused dynamic part of the blob.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const StringContainer & value,
                        char * const blob,
                        char * & beginningOfUnused,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get a string from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(StringContainer & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Set an Object in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] object - The container whose values to use.
         * @param [in,out] blob - Blob to set the member in.
         * @param [in,out] beginningOfUnused - The start of the unused dynamic part of the blob.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const ObjectContainerBase & object,
                        char * const blob,
                        char * & beginningOfUnused,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an Object from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] object - The container in which to put the values.
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(ObjectContainerBase & object,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set Binary in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] binary - The container whose values to use.
         * @param [in,out] blob - Blob to set the member in.
         * @param [in,out] beginningOfUnused - The start of the unused dynamic part of the blob.
         * @param [in,out] beginningOfUnused - The start of the unused dynamic part of the blob.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const BinaryContainer & binary,
                        char * const blob,
                        char * & beginningOfUnused,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an Object from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] binary - The container in which to put the values.
         * @param [in] blob - Blob to get the member from.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(BinaryContainer & binary,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);







        /** @} */

        /**
         * Get the static blob size of an type, but excluding the size that is inherited from parent classes.
         *
         * This is very much an internal function!
         * Unless you have a really good reason to use this function you should stay clear of it.
         * TODO: Rename this function to something that reflects what it actually does.
         * @param typeId [in] - The TypeId of a DOB class.
         * @return The amount of space needed for the static part of the type.
         */
        static Int32 GetInitialSize(const TypeId typeId);
    };
}
}
}
#endif

