/******************************************************************************
*
* Copyright Saab AB, 2004-2015 (http://safirsdkcore.com)
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
#ifndef __DOTS_INTERNAL_BLOB_H__
#define __DOTS_INTERNAL_BLOB_H__

#include <cstdint>
#include <memory>
#include <Safir/Dob/Typesystem/LanguageInterfaceDefs.h>
#include <Safir/Dob/Typesystem/ToolSupport/ParseError.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
namespace Internal
{
    class AnyObject; //forward declaration of protoBuf type

    /**
     * Wrapper around a protobub AnyObject. No checks made here that a type is correct according to dou-files
     */
    class DOTS_INTERNAL_API Blob
    {
    public:
        static std::int32_t GetSize(const char* blob);
        static std::int64_t GetTypeId(const char* blob);

        //create new empty blob of specific type and initiates all members with null and unchanged
        Blob(std::int64_t typeId, int numberOfMembers);

        //create blob object by deserialize a raw blob
        Blob(const char* blob);

        //calculate the blob size
        std::int32_t CalculateBlobSize();

        //get known size of blob, does not calculate
        std::int32_t Size() const {return m_blobSize;}

        //get typeId of blob
        std::int64_t TypeId() const {return m_typeId;}

        //serialize current content to destBlob
        void Serialize(char* destBlob);

        //----------------------------------------
        // Read
        //----------------------------------------
        /**
         * Check if member is changed at member top level.
         *
         * This causes undefined behaviour if called on anything
         * other than a SequenceCollectionType or DictionaryCollectionType.
         */
        bool IsChangedTopLevel(int member) const;

        /**
         * Check change flag on a member (non-recursively)
         *
         * If member is a SingleValueCollectionType then index must be 0.
         * If member is DictionaryCollectionType or ArrayCollectionType then
         * index must be within bounds.
         * Other uses are undefined behavior.
         * Calling on SequenceCollectionType is undefined behaviour.
         */
        bool IsChangedHere(int member,
                           int index) const;


        //get number of values in current member
        int NumberOfValues(int member) const;

        //get isNull and isChanged (non-recursive)
        void ValueStatus(int member, int index, bool& isNull, bool& isChanged) const;

        //get keys, undefined behaviour if IsNull=true
        std::int32_t GetKeyInt32(int member, int index) const;
        std::int64_t GetKeyInt64(int member, int index) const;
        std::int64_t GetKeyHash(int member, int index) const;
        const char* GetKeyString(int member, int index) const;

        //get value
        std::int32_t GetValueInt32(int member, int index) const;
        std::int64_t GetValueInt64(int member, int index) const;
        float GetValueFloat32(int member, int index) const;
        double GetValueFloat64(int member, int index) const;
        bool GetValueBool(int member, int index) const;
        std::int64_t GetValueHash(int member, int index) const;
        const char* GetValueString(int member, int index) const;
        std::pair<const char*, std::int32_t> GetValueBinary(int member, int index) const;

        //----------------------------------------
        // Write
        //----------------------------------------
        //set isChanged on top level, only meaningful for sequences and dictionaries
        void SetChangedTopLevel(int member, bool isChanged);

        //append a new value to current member and set the changeFlag and isNull=true
        //returns the index of the inserted value.
        int AddValue(int member, bool isChanged);

        //set change flat on individual value
        void SetChangedHere(int member, int index, bool isChanged);

        //set key for last added value
        void SetKeyInt32(int member, int index, std::int32_t val);
        void SetKeyInt64(int member, int index, std::int64_t val);
        void SetKeyHash(int member, int index, std::int64_t val);
        void SetKeyString(int member, int index, const char* val);

        //set value for last added value, after a value have been set IsNull=false
        void SetValueInt32(int member, int index, std::int32_t val);
        void SetValueInt64(int member, int index, std::int64_t val);
        void SetValueFloat32(int member, int index, float val);
        void SetValueFloat64(int member, int index, double val);
        void SetValueBool(int member, int index, bool val);
        void SetValueHash(int member, int index, std::int64_t val);
        void SetValueString(int member, int index, const char* val);
        void SetValueBinary(int member, int index, const char* val, std::int32_t size);

    private:
        static const size_t HeaderSize=sizeof(std::int32_t)+sizeof(std::int64_t);

        std::int32_t m_blobSize;
        std::int64_t m_typeId;

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251)
#endif
        std::shared_ptr<AnyObject> m_object;
#ifdef _MSC_VER
#pragma warning (pop)
#endif
    };

    namespace BlobUtils
    {
        //Readers
        template <class T> struct Reader;

        template <> struct Reader<DotsC_Int32>
        {
            static DotsC_Int32 Key(const Internal::Blob& blob, int member, int index) {return blob.GetKeyInt32(member, index);}
            static DotsC_Int32 Value(const Internal::Blob& blob, int member, int index) {return blob.GetValueInt32(member, index);}
        };

        template <> struct Reader<DotsC_Int64>
        {
            static DotsC_Int64 Key(const Internal::Blob& blob, int member, int index) {return blob.GetKeyInt64(member, index);}
            static DotsC_Int64 Value(const Internal::Blob& blob, int member, int index) {return blob.GetValueInt64(member, index);}
        };

        template <> struct Reader<DotsC_Float32>
        {
            static DotsC_Float32 Value(const Internal::Blob& blob, int member, int index) {return blob.GetValueFloat32(member, index);}
        };

        template <> struct Reader<DotsC_Float64>
        {
            static DotsC_Float64 Value(const Internal::Blob& blob, int member, int index) {return blob.GetValueFloat64(member, index);}
        };

        template <> struct Reader<bool>
        {
            static bool Value(const Internal::Blob& blob, int member, int index) {return blob.GetValueBool(member, index);}
        };

        template <> struct Reader<const char*> //string
        {
            static const char* Key(const Internal::Blob& blob, int member, int index)
            {
                return blob.GetKeyString(member, index);
            }
            static const char* Value(const Internal::Blob& blob, int member, int index)
            {
                return blob.GetValueString(member, index);
            }
        };

        template <> struct Reader< std::pair<DotsC_Int64, const char*> > //hashed value
        {
            static std::pair<DotsC_Int64, const char*> Key(const Internal::Blob& blob, int member, int index)
            {
                return std::make_pair(blob.GetKeyHash(member, index), blob.GetKeyString(member, index));
            }
            static std::pair<DotsC_Int64, const char*> Value(const Internal::Blob& blob, int member, int index)
            {
                return std::make_pair(blob.GetValueHash(member, index), blob.GetValueString(member, index));
            }
        };

        template <> struct Reader< std::pair<DotsC_EntityId, const char*> > //entity id
        {
            static std::pair<DotsC_EntityId, const char*> Key(const Internal::Blob& blob, int member, int index)
            {
                std::pair<DotsC_EntityId, const char*> entityId;
                entityId.first.typeId=blob.GetKeyInt64(member, index);
                entityId.first.instanceId=blob.GetKeyHash(member, index);
                entityId.second=blob.GetKeyString(member, index);
                return entityId;
            }
            static std::pair<DotsC_EntityId, const char*> Value(const Internal::Blob& blob, int member, int index)
            {
                std::pair<DotsC_EntityId, const char*> entityId;
                entityId.first.typeId=blob.GetValueInt64(member, index);
                entityId.first.instanceId=blob.GetValueHash(member, index);
                entityId.second=blob.GetValueString(member, index);
                return entityId;
            }
        };

        template <> struct Reader< std::pair<const char*, DotsC_Int32> > //binary
        {
            static std::pair<const char*, DotsC_Int32> Value(const Internal::Blob& blob, int member, int index)
            {
                return blob.GetValueBinary(member, index);
            }
        };

        struct BlobAccess
        {
            template <class T>
            static const Safir::Dob::Typesystem::ToolSupport::Internal::Blob& GetBlob(const T& obj)
            {
                return obj.m_blob;
            }

            template <class T>
            static const typename T::RepositoryType* GetRepository(const T& obj)
            {
                return obj.m_repository;
            }
        };
    }

}
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport

#endif
