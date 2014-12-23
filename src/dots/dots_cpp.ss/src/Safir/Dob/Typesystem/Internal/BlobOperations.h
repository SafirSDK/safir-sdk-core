/******************************************************************************
*
* Copyright Saab AB, 2004-2014 (http://safir.sourceforge.net)
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
* GNU General Public License for more Internals.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#pragma once

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/ValueContainers.h>
#include <Safir/Dob/Typesystem/EnumerationContainerBase.h>
#include <Safir/Dob/Typesystem/ObjectContainer.h>
#include <Safir/Dob/Typesystem/SequenceContainer.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
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
    class DOTS_CPP_API BlobOperations
    {
    public:

        enum KeyValueMode {KeyMode, ValueMode};

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
         * Allocate and create a copy of a blob. The returned blob must be
         * deallocated using BlobOperations::Delete.
         *
         * @param blob [in] - The blob to be copied.
         * @return Copy of blob;
         */
        static char* CreateCopy(const char* blob);

        /**
         * Delete a blob that has been allocated by CreateCopy.
         *
         * @param blob [in,out] - Blob to be deleted.
         */
        static void Delete(char* & blob);

        static bool IsChanged(const char* blob,
                              const Dob::Typesystem::MemberIndex member,
                              const Dob::Typesystem::ArrayIndex index);
        //REMOVE
        static void SetChangedHere(char* blob,
                                   const Dob::Typesystem::MemberIndex member,
                                   const Dob::Typesystem::ArrayIndex index,
                                   bool val);
        //REMOVE
        static void SetChanged(char* blob, bool val);
        //REMOVE

        static void Diff(const char* originalBlob, char* & currentBlob);


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
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const BooleanContainer& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get a boolean from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(BooleanContainer& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set an enumeration in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const EnumerationContainerBase& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an enumeration from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(EnumerationContainerBase& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Set an Int32 in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const Int32Container& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an Int32 from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(Int32Container& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Set an Int64 in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const Int64Container& value,  //will be used on TypeIdContainers too
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an Int64 from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(Int64Container& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Set a Float32 in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const Float32Container& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get a Float32 from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(Float32Container& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Set a Float64 in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const Float64Container& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get a Float64 from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(Float64Container& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set an InstanceId in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const InstanceIdContainer& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an InstanceId from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(InstanceIdContainer& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set an EntityId in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const EntityIdContainer& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an EntityId from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(EntityIdContainer& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set an ChannelId in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const ChannelIdContainer& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an ChannelId from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(ChannelIdContainer& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set an HandlerId in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const HandlerIdContainer& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an HandlerId from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(HandlerIdContainer& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set a string in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const StringContainer& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get a string from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(StringContainer& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Set an Object in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const ObjectContainerBase& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an Object from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(ObjectContainerBase& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set Binary in a blob.
         *
         * If the container is null then the member will be set to null in the blob.
         * The change flag from the container will be set in the blob.
         *
         * @param [in] value - The container whose values to use.
         * @param [in] handle - Handle to a BlobWriter.
         * @param [in] member - The member to be set.
         * @param [in] index - Array index in member to set. Shall be 0 if the member is not an array.
         */
        static void Set(const BinaryContainer& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Get Binary from a blob.
         *
         * This method will get the member and the associated isNull and isChange values from a blob and
         * put them in the container.
         *
         * @param [out] value - The container in which to put the values.
         * @param [in] handle - Handle to a BlobReader.
         * @param [in] member - The member to get.
         * @param [in] index - Array index in member to get. Shall be 0 if the member is not an array.
         */
        static void Get(BinaryContainer& value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        //---------------------------------------------------------
        // Get values
        //--------------------------------------------------------
        static void Get(bool& val,
                        bool& isNull,
                        bool& isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Get(Safir::Dob::Typesystem::Int32& val,
                        bool& isNull,
                        bool& isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Get(Safir::Dob::Typesystem::Int64& val,
                        bool& isNull,
                        bool& isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Get(Safir::Dob::Typesystem::Float32& val,
                        bool& isNull,
                        bool& isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Get(Safir::Dob::Typesystem::Float64& val,
                        bool& isNull,
                        bool& isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Get(std::wstring& val,
                        bool& isNull,
                        bool& isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Get(Safir::Dob::Typesystem::InstanceId& val,
                        bool& isNull,
                        bool& isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Get(Safir::Dob::Typesystem::HandlerId& val,
                        bool& isNull,
                        bool& isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Get(Safir::Dob::Typesystem::ChannelId& val,
                        bool& isNull,
                        bool& isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Get(Safir::Dob::Typesystem::EntityId& val,
                        bool& isNull,
                        bool& isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Get(Safir::Dob::Typesystem::ObjectPtr& val,
                        bool& isNull,
                        bool& isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Get(Safir::Dob::Typesystem::Binary& val,
                        bool& isNull,
                        bool& isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        //---------------------------------------------------------
        // Set values
        //--------------------------------------------------------
        static void Set(bool val,
                        bool isNull,
                        bool isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Set(Safir::Dob::Typesystem::Int32 val,
                        bool isNull,
                        bool isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Set(Safir::Dob::Typesystem::Int64 val,
                        bool isNull,
                        bool isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Set(Safir::Dob::Typesystem::Float32 val,
                        bool isNull,
                        bool isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Set(Safir::Dob::Typesystem::Float64 val,
                        bool isNull,
                        bool isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Set(const std::wstring& val,
                        bool isNull,
                        bool isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Set(const Safir::Dob::Typesystem::InstanceId& val,
                        bool isNull,
                        bool isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Set(const Safir::Dob::Typesystem::HandlerId& val,
                        bool isNull,
                        bool isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Set(const Safir::Dob::Typesystem::ChannelId& val,
                        bool isNull,
                        bool isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Set(const Safir::Dob::Typesystem::EntityId& val,
                        bool isNull,
                        bool isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Set(const Safir::Dob::Typesystem::ObjectPtr& val,
                        bool isNull,
                        bool isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);

        static void Set(const Safir::Dob::Typesystem::Binary& val,
                        bool isNull,
                        bool isChanged,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex valueIndex,
                        KeyValueMode mode);
    };
}
}
}
}
