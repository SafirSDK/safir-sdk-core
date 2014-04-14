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
#ifndef __DOTS_INTERNAL_BLOB_WRITER_H__
#define __DOTS_INTERNAL_BLOB_WRITER_H__

#include <assert.h>
#include <string>
#include <vector>
#include <sstream>
#include <boost/noncopyable.hpp>
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
     * This class is used to create blobs by writing member values and the finally calling the CopyRawBlob-method.
     * The methods for writing values to blobs are templated on the value type. The memberTypes maps to c++ types
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
     *      Object      => pair<const char* blob, DostC_Int32 size> (a valid blob pointer and blob size)
     */
    template <class RepositoryT, class Traits=Safir::Dob::Typesystem::ToolSupport::TypeRepositoryTraits<RepositoryT> >
    class BlobWriter : private boost::noncopyable
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
         * @brief Constructor - Creates a new writeable blob of specified type. Initial state isNull=true and isChanged=false.
         * @param rep [in] - A type repository to use when creating a valid blob.
         * @param typeId [in] - Type of this blob. Type descripton must exist in the type repository.
         */
        BlobWriter(const RepositoryT* rep, DotsC_TypeId typeId)
            :m_repository(rep)
            ,m_classDescription(m_repository->GetClass(typeId))
            ,m_memberDescription(NULL)
            ,m_memberIndex(-1)
            ,m_valueIndex(-1)
            ,m_blob(typeId, m_classDescription->GetNumberOfMembers())
        {
            Init();
        }

        /**
         * @brief Calculate the size of the blob in bytes.
         * @return Size in bytes.
         */
        DotsC_Int32 CalculateBlobSize() {return m_blob.CalculateBlobSize();}

        /**
         * @brief Copy the binarey blob into a destination buffer. The destBlob must already have been allocated and
         * the size of destBlob must be at least the number of bytes retured by a preceeding call to CalculateBlobSize().
         * @param destBlob [in] - Pointer to an allocated buffer of sufficient size.
         */
        void CopyRawBlob(char* destBlob) {m_blob.Serialize(destBlob);}

        /**
         * @brief Set the top level isChanged flag. Useful for empty collectons that still may have isChanged=true.
         * @param member[in] - Member index of the member.
         * @param isChanged - Indicates if the member value is changed at top level.
         */
        void SetChangedTopLevel(DotsC_MemberIndex member, bool isChanged) {m_blob.SetChangedTopLevel(member, isChanged);}

        /**
         * Write member value to the a blob. This method is used to write values to single value members, sequences and sets.
         * It is not allowed to use this method to write values to arrays and dictionaries (see WriteArrayValue and WriteDictionaryValue).
         * If the member is a sequence or set, a new value is added to the collection for each call to this method.
         * If the member is a set, only the following types are supported as value:
         *      Int32, Int64, TypeId, Enumeration, String, InstanceId, HandlerId, ChannelId, EntityId.
         * This method will throw logic_error if used with wrong input data.
         *
         * @param member [in] - Member index of the member to be written.
         * @param val [in] - Member value. Use a dummy if isNull=true. See supported types in class comments above.
         * @param isNull [in] - True if the member value null. In that case val is not in use.
         * @param isChanged [in] - Indicates if the member value is changed.
         */
        template <class Val>
        void WriteValue(DotsC_MemberIndex member, const Val& val, bool isNull, bool isChanged)
        {
            MoveToMember(member);
            DotsC_CollectionType ct=m_memberDescription->GetCollectionType();
            if (ct==NoCollectionType)
            {
                m_valueIndex=0;
                m_blob.SetChanged(m_memberIndex, m_valueIndex, isChanged);
            }
            else if (ct==RangeCollectionType || ct==SetCollectionType)
            {
                m_valueIndex=m_blob.AddValue(member, isChanged);
            }
            else
            {
                ThrowWrongCollectionType();
            }

            if (!isNull)
            {
                WriteValue(val);
            }
        }

        /**
         * Write member value to the a blob. This method is used to write values to single value members, sequences and sets.
         * It is not allowed to use this method to write values to arrays and dictionaries (see WriteArrayValue and WriteDictionaryValue).
         * If the member is a sequence or set, a new value is added to the collection. If the member is single value the only value is written.
         * This method will throw logic_error if used with wrong input data.
         *
         * @param member [in] - Member index of the member to be written.
         * @param index [in] - Array index of the value to be written.
         * @param val [in] - Member value. Use a dummy if isNull=true. See supported types in class comments above.
         * @param isNull [in] - True if the member value null. In that case val is not in use.
         * @param isChanged [in] - Indicates if the member value is changed.
         */
        template <class Val>
        void WriteArrayValue(DotsC_MemberIndex member, DotsC_ArrayIndex index, const Val& val, bool isNull, bool isChanged)
        {            
            MoveToMember(member);
            if (m_memberDescription->GetCollectionType()!=ArrayCollectionType)
            {
                ThrowWrongCollectionType();
            }
            m_valueIndex=index;
            m_blob.SetChanged(m_memberIndex, m_valueIndex, isChanged);
            if (!isNull)
            {
                WriteValue(val);
            }
        }

        /**
         * Writes a new dictionary <key, value>-item to the blob. This method is only allowed to use for
         * dictionary memgbers. Throws logic_error if used on members with wrong collection or member type.
         * If isNull is true, the value is not used, in that case just use a dummy value like a NULL char.
         * Val can be any of the supported types but as key only the following types are supported:
         * Supported key types: Int32, Int64, TypeId, Enumeration, String, InstanceId, HandlerId, ChannelId, EntityId.
         * This method will throw logic_error if used with wrong input data.
         *
         * @param member [in] - Member index of the member to be written.
         * @param key [in] - Key value if the member. See supported key types above.
         * @param val [in] - Member value. Use a dummy if isNull=true. See supported types in class comments above.
         * @param isNull [in] - True if the member value null. In that case val is not in use.
         * @param isChanged [in] - Indicates if the member value is changed.
         */
        template <class Key, class Val>
        void WriteDictionaryValue(DotsC_MemberIndex member, const Key& key, const Val& val, bool isNull, bool isChanged)
        {
            MoveToMember(member);
            if (m_memberDescription->GetCollectionType()!=HashtableCollectionType)
            {
                ThrowWrongCollectionType();
            }
            m_valueIndex=m_blob.AddValue(m_memberIndex, isChanged);
            WriteKey(key);
            if (!isNull)
            {
                WriteValue(val);
            }
        }

    private:
        const RepositoryType* m_repository;
        const ClassDescriptionType* m_classDescription;
        const MemberDescriptionType* m_memberDescription;
        DotsC_MemberIndex m_memberIndex;
        DotsC_ArrayIndex m_valueIndex;
        Safir::Dob::Typesystem::ToolSupport::Internal::Blob m_blob;

        inline void Init()
        {
            //Add values to single value members and arrays since they are not allowed to be empty.
            //Other collections (set, sequence, dictionary) are allowed to be empty.
            for (DotsC_MemberIndex memberIndex=0; memberIndex<m_classDescription->GetNumberOfMembers(); ++memberIndex)
            {
                const MemberDescriptionType* member=m_classDescription->GetMember(memberIndex);
                switch (member->GetCollectionType())
                {
                case NoCollectionType:
                {
                    m_blob.AddValue(memberIndex, false);
                }
                    break;

                case ArrayCollectionType:
                {
                    for (int arrayIndex=0; arrayIndex<member->GetArraySize(); ++arrayIndex)
                    {
                        m_blob.AddValue(memberIndex, false);
                    }
                }
                    break;

                default:
                    break;
                }
            }

        }

        inline void MoveToMember(DotsC_MemberIndex member)
        {
            if (m_memberIndex!=member)
            {
                m_memberDescription=m_classDescription->GetMember(member);
                m_memberIndex=member;
            }
        }

        inline void ThrowWrongMemberType() const
        {
            std::ostringstream os;
            os<<"Trying to write data of wrong memberType to a blob for member '"<<m_memberDescription->GetName()<<"' in class '"<<m_classDescription->GetName()<<"'";
            throw std::logic_error(os.str());
        }

        inline void ThrowWrongCollectionType() const
        {
            std::ostringstream os;
            os<<"Trying to write data of wrong collectionType to a blob for member '"<<m_memberDescription->GetName()<<"' in class '"<<m_classDescription->GetName()<<"'";
            throw std::logic_error(os.str());
        }

        //-----------------------
        // write keys
        //-----------------------
        void WriteKey(DotsC_Int32 key)
        {
            m_blob.SetKeyInt32(m_memberIndex, m_valueIndex, key);
        }

        void WriteKey(DotsC_Int64 key)
        {
            m_blob.SetKeyInt64(m_memberIndex, m_valueIndex, key);

        }
        void WriteKey(const char* key)
        {
            m_blob.SetKeyString(m_memberIndex, m_valueIndex, key);
        }

        void WriteKey(const std::pair<DotsC_Int64, const char *>& key)
        {
            m_blob.SetKeyHash(m_memberIndex, m_valueIndex, key.first);
            if (key.second)
            {
                m_blob.SetKeyString(m_memberIndex, m_valueIndex, key.second);
            }
        }

        void WriteKey(const std::pair<DotsC_EntityId, const char*>& key)
        {
            m_blob.SetKeyInt64(m_memberIndex, m_valueIndex, key.first.typeId);
            WriteKey(std::pair<DotsC_Int64, const char *>(key.first.instanceId, key.second));
        }

        //-----------------------
        // write values
        //-----------------------
        void WriteValue(DotsC_Int32 val)
        {
            assert(m_memberDescription->GetMemberType()==Int32MemberType);
            m_blob.SetValueInt32(m_memberIndex, m_valueIndex, val);
        }

        void WriteValue(DotsC_Int64 val)
        {
            assert(m_memberDescription->GetMemberType()==Int64MemberType);
            m_blob.SetValueInt64(m_memberIndex, m_valueIndex, val);
        }

        void WriteValue(DotsC_Float32 val)
        {
            assert(m_memberDescription->GetMemberType()==Float32MemberType);
            m_blob.SetValueFloat32(m_memberIndex, m_valueIndex, val);
        }

        void WriteValue(DotsC_Float64 val)
        {
            assert(m_memberDescription->GetMemberType()==Float64MemberType);
            m_blob.SetValueFloat64(m_memberIndex, m_valueIndex, val);
        }

        void WriteValue(bool val)
        {
            assert(m_memberDescription->GetMemberType()==BooleanMemberType);
            m_blob.SetValueBool(m_memberIndex, m_valueIndex, val);
        }

        void WriteValue(const char* val)
        {
            assert(m_memberDescription->GetMemberType()==StringMemberType);
             m_blob.SetValueString(m_memberIndex, m_valueIndex, val);
        }

        void WriteValue(const std::pair<DotsC_Int64, const char *>& val) //hashed val
        {
            assert(m_memberDescription->GetMemberType()==InstanceIdMemberType || m_memberDescription->GetMemberType()==ChannelIdMemberType || m_memberDescription->GetMemberType()==HandlerIdMemberType);
            m_blob.SetValueHash(m_memberIndex, m_valueIndex, val.first);
            if (val.second)
            {
                m_blob.SetValueString(m_memberIndex, m_valueIndex, val.second);
            }
        }

        void WriteValue(const std::pair<DotsC_EntityId, const char*>& val) //entityId with optional instance string
        {
            assert(m_memberDescription->GetMemberType()==EntityIdMemberType);
            m_blob.SetValueInt64(m_memberIndex, m_valueIndex, val.first.typeId);
            m_blob.SetValueHash(m_memberIndex, m_valueIndex, val.first.instanceId);
            if (val.second)
            {
                m_blob.SetValueString(m_memberIndex, m_valueIndex, val.second);
            }
        }

        void WriteValue(const std::pair<const char*, DotsC_Int32>& val) //binary data or object
        {
            assert(m_memberDescription->GetMemberType()==BinaryMemberType || m_memberDescription->GetMemberType()==ObjectMemberType);
            m_blob.SetValueBinary(m_memberIndex, m_valueIndex, val.first, val.second);
        }
    };
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport

#endif
