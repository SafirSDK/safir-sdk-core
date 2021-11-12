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
#ifndef __DOTS_INTERNAL_BLOB_READER_H__
#define __DOTS_INTERNAL_BLOB_READER_H__

#include <string>
#include <vector>
#include <sstream>
#include <Safir/Dob/Typesystem/ToolSupport/TypeRepository.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/InternalDefs.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/Blob.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
    /**
     * This class is used to unpack and read blobs created by the BlobWriter class.
     * The methods for reading values are templated. The memberTypes maps to c++ types
     * like described bellow. All strings must be NULL-terminated. Optional strings shall be set to NULL if not present.
     *
     * Supported types:
     * ----------------
     *      Int32       => DostC_Int32
     *      Int64       => DostC_Int64
     *      Float32     => DostC_Float32
     *      Float64     => DostC_Float64
     *      TypeId      => DotsC_TypeId
     *      Enumeration => DotsC_EnumerationValue
     *      String      => const char*
     *      InstanceId  => pair<DotsC_Int64, const char* optional_string>
     *      HandlerId   => pair<DotsC_Int64, const char* optional_string>
     *      ChannelId   => pair<DotsC_Int64, const char* optional_string>
     *      EntityId    => pair<DotsC_EntityId, const char* optional_instance_string>
     *      Binary      => pair<const char* data, DostC_Int32 size>
     *      Object      => pair<const char* data, DostC_Int32 size> (a valid blob pointer and blob size)
     */
    template <class RepositoryT, class Traits=Safir::Dob::Typesystem::ToolSupport::TypeRepositoryTraits<RepositoryT> >
    class BlobReader
    {
    public:
        typedef typename Traits::RepositoryType RepositoryType;
        typedef typename Traits::ClassDescriptionType ClassDescriptionType;
        typedef typename Traits::MemberDescriptionType MemberDescriptionType;
        typedef typename Traits::PropertyDescriptionType PropertyDescriptionType;
        typedef typename Traits::ExceptionDescriptionType ExceptionDescriptionType;
        typedef typename Traits::ParameterDescriptionType ParameterDescriptionType;
        typedef typename Traits::EnumDescriptionType EnumDescriptionType;
        typedef typename Traits::MemberMappingDescriptionType MemberMappingDescriptionType;
        typedef typename Traits::PropertyMappingDescriptionType PropertyMappingDescriptionType;
        typedef typename Traits::CreateRoutineDescriptionType CreateRoutineDescriptionType;

        /**
         * @brief Static method. Get the size of a blob without having to unpack the whole blob.
         * @param blob [in] - The blob.
         * @return Number of bytes.
         */
        static DotsC_Int32 GetSize(const char* blob) {return Internal::Blob::GetSize(blob);}

        /**
         * @brief Static method. Get the typeId of a blob without having to unpack the whole blob.
         * @param blob [in] - The blob.
         * @return TypeId of the blob.
         */
        static DotsC_TypeId GetTypeId(const char* blob) {return Internal::Blob::GetTypeId(blob);}

        /**
         * @brief Constructor - Creates a reader object that unpacks the blob and makes it possible to read its content.
         * @param rep [in] - A type repository to use when interpreting the blob content.
         * @param blob [in] - A valid blob like the one created by BlobWriter class.
         */
        BlobReader(const RepositoryT* rep, const char* blob)
            :m_repository(rep)
            ,m_blob(blob)
            ,m_classDescription(rep->GetClass(m_blob.TypeId()))
            ,m_memberDescription(NULL)
            ,m_memberIndex(-1)
        {
        }

        BlobReader(const BlobReader&) = delete;
        BlobReader& operator=(const BlobReader&) = delete;

        /**
         * @brief Get the size of the blob.
         * @return Number of bytes.
         */
        DotsC_Int32 Size() const {return m_blob.Size();}

        /**
         * @brief Get the type id of the blob.
         * @return TypeId of the blob.
         */
        DotsC_TypeId TypeId() const {return m_blob.TypeId();}

        /**
         * Check if the member is changed at top level.
         *
         * This causes undefined behaviour if called on anything
         * other than a SequenceCollectionType or DictionaryCollectionType.
         *
         * For sequences and dictionaries changed at top level indicates that the collection has changed in some way.
         * @param member
         * @return
         */
        bool IsChangedTopLevel(DotsC_MemberIndex member) const
        {
            return m_blob.IsChangedTopLevel(member);
        }

        /**
         * Check change flags on all members, recursively.
         *
         * Will check top level change flags as well!
         */
        bool IsChangedRecursive() const
        {
            for (int member=0; member<m_classDescription->GetNumberOfMembers(); ++member)
            {
                if (IsChangedRecursive(member))
                {
                    return true;
                }
            }
            return false;
        }

        /**
         * Check change flag on a member, recursively
         *
         * Will check top level change flags as well!
         */
        bool IsChangedRecursive(DotsC_MemberIndex member) const
        {
            MoveToMember(member);
            const DotsC_CollectionType collectionType = m_memberDescription->GetCollectionType();

            //Check top level change flag for things that have it.
            if (collectionType == SequenceCollectionType || collectionType == DictionaryCollectionType)
            {
                if (m_blob.IsChangedTopLevel(member))
                {
                    return true;
                }
            }

            const int size = m_blob.NumberOfValues(member);

            //sequences are a bit different from other members, since
            //they do not have change flags on the values. But they
            //do potentially have nestled change flags if it is a
            //sequence of objects.
            if (collectionType == SequenceCollectionType)
            {
                if (m_memberDescription->GetMemberType() == ObjectMemberType)
                {
                    for (int index = 0; index < size; ++index)
                    {
                        bool dummy=false, isNull=false;
                        m_blob.ValueStatus(member, index, isNull, dummy);
                        if (!isNull)
                        {
                            std::pair<const char*, DotsC_Int32> obj=m_blob.GetValueBinary(member, index);
                            const BlobReader inner(m_repository, obj.first);
                            if (inner.IsChangedRecursive())
                            {
                                return true;
                            }
                        }
                    }
                }
            }
            else
            {
                //first check all the change flags that are directly in the member
                for (int index = 0; index < size; ++index)
                {
                    if (IsChangedHere(member, index))
                    {
                        return true;
                    }
                }
                //ok, if there are objects in here we need to recurse

                if (m_memberDescription->GetMemberType() == ObjectMemberType)
                {
                    for (int index = 0; index < size; ++index)
                    {
                        bool dummy=false, isNull=false;
                        m_blob.ValueStatus(member, index, isNull, dummy);
                        if (!isNull)
                        {
                            std::pair<const char*, DotsC_Int32> obj=m_blob.GetValueBinary(member, index);
                            const BlobReader inner(m_repository, obj.first);
                            if (inner.IsChangedRecursive())
                            {
                                return true;
                            }
                        }
                    }
                }
            }

            //no changes...
            return false;
        }

        /**
         * Check change flag on a member (non-recursively)
         *
         * If member is a SingleValueCollectionType then index must be 0.
         * If member is DictionaryCollectionType or ArrayCollectionType then
         * index must be within bounds.
         * Other uses are undefined behavior.
         * Calling on SequenceCollectionType is undefined behaviour.
         */
        bool IsChangedHere(DotsC_MemberIndex member,
                           DotsC_Int32 index) const
        {
#ifndef NDEBUG
            MoveToMember(member);
            assert(m_memberDescription->GetCollectionType() != SequenceCollectionType);
            //array index is asserted in Blob.cpp
#endif
            return m_blob.IsChangedHere(member,index);
        }


        /**
         * @brief Get the number of values for the member. Only collections may contain more than one value.
         * @param member [in] - The member.
         * @return Number of values.
         */
        int NumberOfValues(DotsC_MemberIndex member) const {return m_blob.NumberOfValues(member);}

        /**
         * @brief Convenience method for checking if a member is null. All status flags are also fetched with the ReadValue method.
         * @param member [in] - Member index of the member to to check..
         * @param valueIndex [in] - The value to check. Must be in range 0 to NumberOfValues()-1.
         * @param isNull [out] - True if value is null.
         * @param isChanged [out] - True if value has changed.
         */
        void ReadStatus(DotsC_MemberIndex member, int valueIndex, bool& isNull, bool& isChanged) const
        {
            MoveToMember(member);
            m_blob.ValueStatus(member, valueIndex, isNull, isChanged);
        }

        /**
         * Reads the key element of a member value. Only applicable for dictionary members.
         * Supported key types: Int32, Int64, TypeId, Enumeration, String, InstanceId, HandlerId, ChannelId, EntityId.
         *
         * @param member [in] - Member index of the member to read.
         * @param valueIndex [in] - The value to read. Must be in range 0 to NumberOfValues()-1.
         * @return The key value.
         */
        template <class Key>
        Key ReadKey(DotsC_MemberIndex member, int valueIndex) const
        {
            MoveToMember(member);
            if (m_memberDescription->GetCollectionType()!=DictionaryCollectionType)
            {
                ThrowWrongCollectionType();
            }
            return Internal::BlobUtils::Reader<Key>::Key(m_blob, member, valueIndex);
        }

        /**
         * Read the value element of a member value.
         *
         * @param member [in] - Member index of the member to read.
         * @param valueIndex [in] - The value to read. Must be in range 0 to NumberOfValues()-1.
         * @param val [out] - The value that was read.
         * @param isNull [out] - True if value is null.
         * @param isChanged [out] - True if value has changed.
         */
        template <class Val>
        void ReadValue(DotsC_MemberIndex member, int valueIndex, Val& val, bool& isNull, bool& isChanged) const
        {
            MoveToMember(member);
            m_blob.ValueStatus(member, valueIndex, isNull, isChanged);
            if (!isNull)
            {
                val=Internal::BlobUtils::Reader<Val>::Value(m_blob, member, valueIndex);
            }
        }

    private:
        const RepositoryType* m_repository;
        Safir::Dob::Typesystem::ToolSupport::Internal::Blob m_blob;
        const ClassDescriptionType* m_classDescription;
        mutable const MemberDescriptionType* m_memberDescription;
        mutable DotsC_MemberIndex m_memberIndex;

        inline void MoveToMember(DotsC_MemberIndex member) const
        {
            if (m_memberIndex!=member)
            {
                m_memberDescription=m_classDescription->GetMember(member);
                m_memberIndex=member;
            }
        }

        inline void ThrowWrongCollectionType() const
        {
            std::ostringstream os;
            os<<"Trying to write data of wrong collection type to a blob for member '"<<m_memberDescription->GetName()<<"' in class '"<<m_classDescription->GetName()<<"'";
            throw std::logic_error(os.str());
        }

        friend struct Internal::BlobUtils::BlobAccess;
    };
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport

#endif
