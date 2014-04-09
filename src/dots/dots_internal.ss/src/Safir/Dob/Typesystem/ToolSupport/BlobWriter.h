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
     */
    template <class RepositoryT, class Traits=Safir::Dob::Typesystem::ToolSupport::TypeRepositoryTraits<RepositoryT> >
    class BlobWriter
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
            ,m_blob(typeId)
        {
            for (int member=0; member<m_memberDescription->GetNumberOfMembers(); ++member)
            {
                m_blob.AddMember(member);
            }
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
         * Writes a value to the blob. If the member is not array or map, key is neglected.
         * Supported key types are defined below. If isNull is true, the value is not used,
         * in that case just use a dummy value like a NULL char;
         *
         * Supported array key types:
         *      Int32       => DostC_Int32
         *
         * Supported map key types:
         *      Int32       => DostC_Int32
         *      Int64       => DostC_Int64
         *      TypeId      => DotsC_TypeId
         *      Enumeration => DotsC_EnumerationValue
         *      String      => const char*
         *      InstanceId  => DotsC_Int64 or pair<DotsC_Int64, const char* optional_string>
         *      HandlerId   => DotsC_Int64 or pair<DotsC_Int64, const char* optional_string>
         *      ChannelId   => DotsC_Int64 or pair<DotsC_Int64, const char* optional_string>
         *      EntityId    => pair<DotsC_EntityId, const char* optional_instance_string>
         *
         * Supported Val types:
         *      Int32       => DostC_Int32
         *      Int64       => DostC_Int64
         *      Float32     => DostC_Float32
         *      Float64     => DostC_Float64
         *      TypeId      => DotsC_TypeId
         *      Enumeration => DotsC_EnumerationValue
         *      String      => const char*
         *      InstanceId  => DotsC_Int64 or pair<DotsC_Int64, const char* optional_string>
         *      HandlerId   => DotsC_Int64 or pair<DotsC_Int64, const char* optional_string>
         *      ChannelId   => DotsC_Int64 or pair<DotsC_Int64, const char* optional_string>
         *      EntityId    => pair<DotsC_EntityId, const char* optional_instance_string>
         *      Binary      => pair<const char* data, DostC_Int32 size>
         *      Object      => pair<const char* blob, DostC_Int32 size> OR const char* blob
         *
         * @param member [in] - Member index of the member to be written.
         * @param key [in] - Key value if the member is an array or map.
         * @param val [in] - Member value. Use a dummy if isNull=true.
         * @param isNull [in] - True if the member value null. In that case val is not in use.
         * @param hasChanged [in] - Indicates if the member value has changed.
         */
        template <class Key, class Val>
        void Write(DotsC_MemberIndex member, const Key& key, const Val& val, bool isNull, bool hasChanged)
        {
            if (isNull && !hasChanged)
            {
                return; //this is the default and is not present in blob.
            }

            MoveToMember(member);

            if (hasChanged)
            {
                m_blob.SetChangedHere(true);
            }

            m_blob.AddValue(hasChanged);

            if (m_memberDescription->GetCollectionType()==ArrayCollectionType || m_memberDescription->GetCollectionType()==HashtableCollectionType)
            {
                WriteKey(key);
            }

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
        Safir::Dob::Typesystem::ToolSupport::Internal::Blob m_blob;

        void MoveToMember(DotsC_MemberIndex member)
        {
            if (m_memberIndex!=member)
            {
                if (!m_blob.MoveToMember(member))
                {
                    m_blob.AddMember(member);
                }
            }

            m_memberDescription=m_classDescription->GetMember(member);
            m_memberIndex=member;
        }

        inline void CheckType(DotsC_MemberType memberType) const
        {
            if (m_memberDescription->GetCollectionType()!=memberType)
            {
                std::ostringstream os;
                os<<"Trying to write data of wrong type to a blob for member '"<<m_memberDescription->GetName()<<"' in class '"<<m_classDescription->GetName()<<"'";
                throw std::logic_error(os.str());
            }
        }

        //-----------------------
        // write keys
        //-----------------------
        void WriteKey(DotsC_Int32 key)
        {
            m_blob.SetKeyInt32(key);
        }

        void WriteKey(DotsC_Int64 key)
        {
            m_blob.SetKeyInt64(key);

        }
        void WriteKey(const char* key)
        {
            m_blob.SetKeyString(key);
        }

        void WriteKey(const std::pair<DotsC_Int64, const char *>& key)
        {
            m_blob.SetKeyHash(key.first);
            if (key.second)
            {
                m_blob.SetKeyString(key.second);
            }
        }

        void WriteKey(const std::pair<DotsC_EntityId, const char*>& key)
        {
            m_blob.SetKeyInt64(key.first.typeId);
            WriteKey(std::pair<DotsC_Int64, const char *>(key.first.instanceId, key.second));
        }

        //-----------------------
        // write values
        //-----------------------
        void WriteValue(DotsC_Int32 val)
        {
            assert(m_memberDescription->GetMemberType()==Int32MemberType);
            m_blob.SetValueInt32(val);
        }

        void WriteValue(DotsC_Int64 val)
        {
            assert(m_memberDescription->GetMemberType()==Int64MemberType);
            m_blob.SetValueInt64(val);
        }

        void WriteValue(DotsC_Float32 val)
        {
            assert(m_memberDescription->GetMemberType()==Float32MemberType);
            m_blob.SetValueFloat32(val);
        }

        void WriteValue(DotsC_Float64 val)
        {
            assert(m_memberDescription->GetMemberType()==Float64MemberType);
            m_blob.SetValueFloat64(val);
        }

        void WriteValue(bool val)
        {
            assert(m_memberDescription->GetMemberType()==BooleanMemberType);
            m_blob.SetValueBool(val);
        }

        void WriteValue(const char* val)
        {
            if (m_memberDescription->GetMemberType()==StringMemberType)
                m_blob.SetValueString(val);
            else if (m_memberDescription->GetMemberType()==ObjectMemberType)
                WriteValue(std::pair<const char*, DotsC_Int32>(val, Internal::Blob::GetSize(val)));
            else
                assert(false);
        }

        void WriteValue(const std::pair<DotsC_Int64, const char *>& val) //hashed val
        {
            assert(m_memberDescription->GetMemberType()==InstanceIdMemberType || m_memberDescription->GetMemberType()==ChannelIdMemberType || m_memberDescription->GetMemberType()==HandlerIdMemberType);
            m_blob.SetValueHash(val.first);
            if (val.second)
            {
                m_blob.SetValueString(val.second);
            }
        }

        void WriteValue(const std::pair<DotsC_EntityId, const char*>& val) //entityId with optional instance string
        {
            assert(m_memberDescription->GetMemberType()==EntityIdMemberType);
            m_blob.SetValueInt64(val.first.typeId);
            m_blob.SetValueHash(val.first.instanceId);
            if (val.second)
            {
                m_blob.SetValueString(val.second);
            }
        }

        void WriteValue(const std::pair<const char*, DotsC_Int32>& val) //binary data or object
        {
            assert(m_memberDescription->GetMemberType()==BinaryMemberType || m_memberDescription->GetMemberType()==ObjectMemberType);
            m_blob.SetValueBinary(val.first, val.second);
        }
    };
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport

#endif
