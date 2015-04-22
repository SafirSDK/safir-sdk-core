/******************************************************************************
*
* Copyright Consoden AB, 2004-2015 (http://safir.sourceforge.net)
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
#include <Safir/Dob/Typesystem/ToolSupport/BlobReader.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeUtilities.h>

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
     *      Object      => pair<const char* blob, DostC_Int32 size> or another BlobWriter
     *
     */
    template <class RepositoryT, class Traits=Safir::Dob::Typesystem::ToolSupport::TypeRepositoryTraits<RepositoryT> >
    class BlobWriter : private boost::noncopyable
    {
    public:
        typedef BlobWriter<RepositoryT, Traits> BlobWriterType;
        typedef BlobReader<RepositoryT, Traits> BlobReaderType;
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

        BlobWriter(const BlobReaderType& reader)
            :m_repository(Internal::BlobUtils::BlobAccess::GetRepository<BlobReader<RepositoryT, Traits> >(reader))
            ,m_classDescription(m_repository->GetClass(reader.TypeId()))
            ,m_memberDescription(NULL)
            ,m_memberIndex(-1)
            ,m_valueIndex(-1)
            ,m_blob(Internal::BlobUtils::BlobAccess::GetBlob<BlobReader<RepositoryT, Traits> >(reader))
        {
        }

        /**
         * @brief Get the type id of this BlobWriter.
         * @return TypeId of the blob writer.
         */
        DotsC_TypeId TypeId() const {return m_blob.TypeId();}

        /**
         * @brief Calculate the size of the blob in bytes.
         * @return Size in bytes.
         */
        DotsC_Int32 CalculateBlobSize() const {return m_blob.CalculateBlobSize();}

        /**
         * @brief Copy the binarey blob into a destination buffer. The destBlob must already have been allocated and
         * the size of destBlob must be at least the number of bytes retured by a preceeding call to CalculateBlobSize().
         * @param destBlob [in] - Pointer to an allocated buffer of sufficient size.
         */
        void CopyRawBlob(char* destBlob) const {m_blob.Serialize(destBlob);}

        /**
         * @brief Set the top level isChanged flag. Useful for empty collectons that still may have isChanged=true.
         *
         * @param member [in] - Member index of the member.
         * @param isChanged [in] - Indicates if the member value is changed at top level.
         */
        void SetChangedTopLevel(DotsC_MemberIndex member, bool isChanged)
        {
            const MemberDescriptionType* md=m_classDescription->GetMembner(member);
            DotsC_CollectionType collectionType=md->GetCollectionType();
            if (collectionType==SequenceCollectionType || collectionType==DictionaryCollectionType)
            {
                m_blob.SetChangedTopLevel(member, isChanged);
            }
        }

        /**
         * @brief Set the change flag for a member value.
         * @param member [in] - Member index of the member.
         * @param valueIndex [in] - Index of the value. If array this is the arrayIndex, if dictionary you
         *                          have to find out the value index in some way. Sequences only have top-level change flag.
         * @param isChanged [in] - The change flag value to set.
         */
        void SetChanged(DotsC_MemberIndex member, DotsC_Int32 valueIndex, bool isChanged) {m_blob.SetChanged(member, valueIndex, isChanged);}

        /**
         * Write member key to the a blob. Only use this when member is a dictionary otherwize the blob will be corrupt.
         * A new key with value null will be added to the collection for each call to this method. Make sure to call
         * WriteValue after a call to this method to set the correct value for the key.
         * Valid types for Key is:
         *      Int32, Int64, TypeId, Enumeration, String, InstanceId, HandlerId, ChannelId, EntityId.
         * This method will throw logic_error if used with wrong input data.
         *
         * @param member [in] - Member index of the member to be written.
         * @param key [in] - Key value if the member is a dictionary.
         */
        template <class Key>
        void WriteKey(DotsC_MemberIndex member, const Key& key)
        {
            MoveToMember(member);
            if (m_memberDescription->GetCollectionType()==DictionaryCollectionType)
            {
                m_valueIndex=m_blob.AddValue(m_memberIndex, false);
                WriteKey(key);
            }
            else
            {
                throw std::logic_error("WriteKey was called on member thats not a dictionary.");
            }
        }

        /**
         * Write member value to the a blob.
         * For dictionaries make sure to call WriteKey immediately before calling WriteValue since the WriteValue.
         * If the member is an array, the index is used. For all other collection types index is ignored.
         * If the member is a sequence, a new value is added to the collection for each call to this method.
         * If the member is a dictionary, the key must have been written before the call to this method.
         * This method will throw logic_error if used with wrong input data.
         *
         * @param member [in] - Member index of the member to be written.
         * @param index [in] - Array index of the value to be written. Ignored if CollectionType is not Array.
         * @param val [in] - Member value. Use a dummy if isNull=true. See supported types in class comments above.
         * @param isNull [in] - True if the member value null. In that case val is not in use.
         * @param isChanged [in] - Indicates if the member value is changed.
         */
        template <class Val>
        void WriteValue(DotsC_MemberIndex member, DotsC_Int32 index, const Val& val, bool isNull, bool isChanged)
        {
            MoveToMember(member);
            switch (m_memberDescription->GetCollectionType())
            {
            case SingleValueCollectionType:
            {
                m_valueIndex=0;
                m_blob.SetChanged(m_memberIndex, m_valueIndex, isChanged);
            }
                break;
            case ArrayCollectionType:
            {
                m_valueIndex=index;
                m_blob.SetChanged(m_memberIndex, m_valueIndex, isChanged);
            }
                break;
            case SequenceCollectionType:
            {
                m_valueIndex=m_blob.AddValue(m_memberIndex, isChanged);
            }
                break;
            case DictionaryCollectionType:
            {
                m_blob.SetChanged(m_memberIndex, m_valueIndex, isChanged);
            }
                break;
            }

            if (!isNull)
            {
                WriteValue(val);
            }
        }

        /**
         * @brief SetAllChangeFlags - Recursively set all change flags to specified value.
         * @param isChanged [in] - Value to set all change flags to.
         */
        void SetAllChangeFlags(bool isChanged)
        {
            bool dummy=false, isNull=false;

            for (int memIx=0; memIx<m_classDescription->GetNumberOfMembers(); ++memIx)
            {
                const MemberDescriptionType* md=m_classDescription->GetMember(memIx);

                switch (md->GetCollectionType())
                {
                case SingleValueCollectionType:
                {
                    m_blob.SetChanged(memIx, 0, isChanged);
                    if (md->GetMemberType()==ObjectMemberType)
                    {
                        m_blob.ValueStatus(memIx, 0, isNull, dummy);
                        if (!isNull)
                        {
                            std::pair<const char*, DotsC_Int32> obj=m_blob.GetValueBinary(memIx, 0);
                            BlobWriterType inner(BlobReaderType(m_repository, obj.first));
                            inner.SetAllChangeFlags(isChanged);
                            WriteValue(memIx, 0, inner, isNull, isChanged);
                        }
                    }
                }
                    break;
                case ArrayCollectionType:
                {
                    if (md->GetMemberType()==ObjectMemberType)
                    {
                        for (int valIx=0; valIx<m_blob.NumberOfValues(memIx); ++valIx)
                        {
                            m_blob.ValueStatus(memIx, valIx, isNull, dummy);
                            if (isNull)
                            {
                                m_blob.SetChanged(memIx, valIx, isChanged);
                            }
                            else
                            {
                                std::pair<const char*, DotsC_Int32> obj=m_blob.GetValueBinary(memIx, valIx);
                                BlobWriterType inner(BlobReaderType(m_repository, obj.first));
                                inner.SetAllChangeFlags(isChanged);
                                WriteValue(memIx, valIx, inner, isNull, isChanged);
                            }
                        }
                    }
                    else
                    {
                        for (int valIx=0; valIx<m_blob.NumberOfValues(memIx); ++valIx)
                        {
                            m_blob.SetChanged(memIx, valIx, isChanged);
                        }
                    }
                }
                    break;
                case SequenceCollectionType:
                {
                    m_blob.SetChangedTopLevel(memIx, isChanged);
                    if (md->GetMemberType()==ObjectMemberType)
                    {
                        MoveToMember(memIx);
                        for (int valIx=0; valIx<m_blob.NumberOfValues(memIx); ++valIx)
                        {
                            m_blob.ValueStatus(memIx, valIx, isNull, dummy);
                            if (!isNull)
                            {
                                std::pair<const char*, DotsC_Int32> obj=m_blob.GetValueBinary(memIx, valIx);
                                BlobWriterType inner(BlobReaderType(m_repository, obj.first));
                                inner.SetAllChangeFlags(isChanged);
                                m_valueIndex=valIx;
                                WriteValue(inner);
                            }
                        }
                    }
                }
                    break;
                case DictionaryCollectionType:
                {
                    m_blob.SetChangedTopLevel(memIx, isChanged);
                    MoveToMember(memIx);

                    for (int valIx=0; valIx<m_blob.NumberOfValues(memIx); ++valIx)
                    {
                        m_blob.SetChanged(memIx, valIx, isChanged);
                        if (md->GetMemberType()==ObjectMemberType)
                        {
                            m_blob.ValueStatus(memIx, valIx, isNull, dummy);
                            if (!isNull)
                            {
                                std::pair<const char*, DotsC_Int32> obj=m_blob.GetValueBinary(memIx, valIx);
                                BlobWriterType inner(BlobReaderType(m_repository, obj.first));
                                inner.SetAllChangeFlags(isChanged);
                                m_valueIndex=valIx;
                                WriteValue(inner);
                            }
                        }
                    }
                }
                    break;
                }
            }
        }

        /**
         * @brief MarkChanges - Set change flag for all members that differs between this blob and the reader blob.
         * @param reader [in] - Blob reader to compare against.
         * @return True if there was any differences between this and reader.
         */
        bool MarkChanges(const BlobReaderType& reader)
        {
            if (TypeId()!=reader.TypeId())
                return true;

            bool diff=false;
            const Internal::Blob& other=Internal::BlobUtils::BlobAccess::GetBlob(reader);

            for (int memIx=0; memIx<m_classDescription->GetNumberOfMembers(); ++memIx)
            {
                const MemberDescriptionType* md=m_classDescription->GetMember(memIx);

                switch (md->GetCollectionType())
                {
                case SingleValueCollectionType:
                {
                    if (Diff(other, md, memIx, 0, 0))
                    {
                        m_blob.SetChanged(memIx, 0, true);
                        diff=true;
                    }
                }
                    break;
                case ArrayCollectionType:
                {
                    for (int valIx=0; valIx<md->GetArraySize(); ++valIx)
                    {
                        if (Diff(other, md, memIx, valIx, valIx))
                        {
                            m_blob.SetChanged(memIx, valIx, true);
                            diff=true;
                        }
                    }
                }
                    break;
                case SequenceCollectionType:
                {
                    if (m_blob.NumberOfValues(memIx)!=other.NumberOfValues(memIx))
                    {
                        diff=true;
                        m_blob.SetChangedTopLevel(memIx, true);
                    }
                    else
                    {
                        for (int valIx=0; valIx<m_blob.NumberOfValues(memIx); ++valIx)
                        {
                            if (Diff(other, md, memIx, valIx, valIx))
                            {
                                m_blob.SetChangedTopLevel(memIx, true);
                                diff=true;
                                break;
                            }
                        }
                    }
                }
                    break;
                case DictionaryCollectionType:
                {
                    //olika antal, eller någon i this som inte finns i other -> topLevel
                    //different size, i.e top leve changed
                    if (m_blob.NumberOfValues(memIx)!=other.NumberOfValues(memIx))
                    {
                        m_blob.SetChangedTopLevel(memIx, true);
                        diff=true;
                    }

                    typedef std::map<DotsC_Int64, int> Uki;
                    Uki myUki, otherUki;
                    UniversalKeyToIndex(m_blob, md, memIx, myUki);
                    UniversalKeyToIndex(other, md, memIx, otherUki);

                    for (Uki::const_iterator myIt=myUki.begin(); myIt!=myUki.end(); ++myIt)
                    {
                        Uki::const_iterator otherIt=otherUki.find(myIt->first);
                        if (otherIt==otherUki.end())
                        {
                            //not found, we have something that is new
                            m_blob.SetChangedTopLevel(memIx, true);
                            m_blob.SetChanged(memIx, myIt->second, true);
                            diff=true;
                        }
                        else if (Diff(other, md, memIx, myIt->second, otherIt->second))
                        {
                            m_blob.SetChanged(memIx, myIt->second, true);
                            diff=true;
                        }
                    }
                }
                    break;
                }
            }

            return diff;
        }

    private:
        const RepositoryType* m_repository;
        const ClassDescriptionType* m_classDescription;
        const MemberDescriptionType* m_memberDescription;
        DotsC_MemberIndex m_memberIndex;
        DotsC_Int32 m_valueIndex;
        mutable Safir::Dob::Typesystem::ToolSupport::Internal::Blob m_blob;

        inline void Init()
        {
            //Add values to single value members and arrays since they are not allowed to be empty.
            //Other collections (sequence, dictionary) are allowed to be empty.
            for (DotsC_MemberIndex memberIndex=0; memberIndex<m_classDescription->GetNumberOfMembers(); ++memberIndex)
            {
                const MemberDescriptionType* member=m_classDescription->GetMember(memberIndex);
                switch (member->GetCollectionType())
                {
                case SingleValueCollectionType:
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
            assert(m_memberDescription->GetMemberType()==Int32MemberType || m_memberDescription->GetMemberType()==EnumerationMemberType);
            m_blob.SetValueInt32(m_memberIndex, m_valueIndex, val);
        }

        void WriteValue(DotsC_Int64 val)
        {
            assert(m_memberDescription->GetMemberType()==Int64MemberType || m_memberDescription->GetMemberType()==TypeIdMemberType);
            m_blob.SetValueInt64(m_memberIndex, m_valueIndex, val);
        }

        void WriteValue(DotsC_Float32 val)
        {
            //assert(m_memberDescription->GetMemberType()==Float32MemberType);
            m_blob.SetValueFloat32(m_memberIndex, m_valueIndex, val);
        }

        void WriteValue(DotsC_Float64 val)
        {
            //assert(m_memberDescription->GetMemberType()==Float64MemberType);
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

        void WriteValue(const std::pair<char*, DotsC_Int32>& val) //binary data or object
        {
            assert(m_memberDescription->GetMemberType()==BinaryMemberType || m_memberDescription->GetMemberType()==ObjectMemberType);
            m_blob.SetValueBinary(m_memberIndex, m_valueIndex, val.first, val.second);
        }

        void WriteValue(const BlobWriterType& val) //object
        {
            assert(m_memberDescription->GetMemberType()==ObjectMemberType);
            std::vector<char> bin(static_cast<size_t>(val.CalculateBlobSize()));
            val.CopyRawBlob(&bin[0]);
            m_blob.SetValueBinary(m_memberIndex, m_valueIndex, &bin[0], bin.size());
        }

        bool Diff(const Internal::Blob& other, const MemberDescriptionType* md, int memberIndex, int myValueIndex, int otherValueIndex)
        {
            bool meIsNull=false, meIsChanged=false;
            bool otherIsNull=false, otherIsChanged=false;

            m_blob.ValueStatus(memberIndex, myValueIndex, meIsNull, meIsChanged);
            other.ValueStatus(memberIndex, otherValueIndex, otherIsNull, otherIsChanged);

            if (meIsNull!=otherIsNull)
            {
                if (!meIsNull && md->GetMemberType()==ObjectMemberType)
                {
                    std::pair<const char*, DotsC_Int32> obj=m_blob.GetValueBinary(memberIndex, myValueIndex);
                    BlobWriterType inner(BlobReaderType(m_repository, obj.first));
                    inner.SetAllChangeFlags(true);
                    MoveToMember(memberIndex);
                    m_valueIndex=myValueIndex;
                    WriteValue(inner);
                }
                return true;
            }
            else if (!meIsNull)
            {
                switch(md->GetMemberType())
                {
                case BooleanMemberType:
                    return m_blob.GetValueBool(memberIndex, myValueIndex)!=other.GetValueBool(memberIndex, otherValueIndex);

                case Int32MemberType:
                case EnumerationMemberType:
                    return m_blob.GetValueInt32(memberIndex, myValueIndex)!=other.GetValueInt32(memberIndex, otherValueIndex);

                case Int64MemberType:
                case TypeIdMemberType:
                    return m_blob.GetValueInt64(memberIndex, myValueIndex)!=other.GetValueInt64(memberIndex, otherValueIndex);

                case InstanceIdMemberType:
                case ChannelIdMemberType:
                case HandlerIdMemberType:
                    return m_blob.GetValueHash(memberIndex, myValueIndex)!=other.GetValueHash(memberIndex, otherValueIndex);

                case EntityIdMemberType:
                    return (m_blob.GetValueInt64(memberIndex, myValueIndex)!=other.GetValueInt64(memberIndex, otherValueIndex)) ||
                            (m_blob.GetValueHash(memberIndex, myValueIndex)!=other.GetValueHash(memberIndex, otherValueIndex));

                case StringMemberType:
                    return strcmp(m_blob.GetValueString(memberIndex, myValueIndex), other.GetValueString(memberIndex, otherValueIndex))!=0;

                case ObjectMemberType:
                {
                    std::pair<const char*, boost::int32_t> meInner=m_blob.GetValueBinary(memberIndex, myValueIndex);
                    std::pair<const char*, boost::int32_t> otherInner=other.GetValueBinary(memberIndex, otherValueIndex);
                    if (meInner.second!=otherInner.second || memcmp(meInner.first, otherInner.first, static_cast<size_t>(meInner.second))!=0)
                    {
                        //not binary equal, something is probably different
                        BlobWriterType inner(BlobReaderType(m_repository, meInner.first));
                        BlobReaderType otherReader(m_repository, otherInner.first);
                        bool diff=inner.MarkChanges(otherReader);
                        if (diff)
                        {
                            MoveToMember(memberIndex);
                            m_valueIndex=myValueIndex;
                            WriteValue(inner);
                            return true;
                        }
                    }
                    return false;
                }
                    break;

                case BinaryMemberType:
                {
                    std::pair<const char*, boost::int32_t> a=m_blob.GetValueBinary(memberIndex, myValueIndex);
                    std::pair<const char*, boost::int32_t> b=other.GetValueBinary(memberIndex, otherValueIndex);
                    return a.second!=b.second || memcmp(a.first, b.first, static_cast<size_t>(a.second))!=0;
                }
                    break;

                case Float32MemberType:
                case Ampere32MemberType:
                case CubicMeter32MemberType:
                case Hertz32MemberType:
                case Joule32MemberType:
                case Kelvin32MemberType:
                case Kilogram32MemberType:
                case Meter32MemberType:
                case MeterPerSecond32MemberType:
                case MeterPerSecondSquared32MemberType:
                case Newton32MemberType:
                case Pascal32MemberType:
                case Radian32MemberType:
                case RadianPerSecond32MemberType:
                case RadianPerSecondSquared32MemberType:
                case Second32MemberType:
                case SquareMeter32MemberType:
                case Steradian32MemberType:
                case Volt32MemberType:
                case Watt32MemberType:
                    return m_blob.GetValueFloat32(memberIndex, myValueIndex)!=other.GetValueFloat32(memberIndex, otherValueIndex);

                case Float64MemberType:
                case Ampere64MemberType:
                case CubicMeter64MemberType:
                case Hertz64MemberType:
                case Joule64MemberType:
                case Kelvin64MemberType:
                case Kilogram64MemberType:
                case Meter64MemberType:
                case MeterPerSecond64MemberType:
                case MeterPerSecondSquared64MemberType:
                case Newton64MemberType:
                case Pascal64MemberType:
                case Radian64MemberType:
                case RadianPerSecond64MemberType:
                case RadianPerSecondSquared64MemberType:
                case Second64MemberType:
                case SquareMeter64MemberType:
                case Steradian64MemberType:
                case Volt64MemberType:
                case Watt64MemberType:
                    return m_blob.GetValueFloat64(memberIndex, myValueIndex)!=other.GetValueFloat64(memberIndex, otherValueIndex);

                } //end switch-statement
            }
            return false; //both values are null, i.e not changed
        }

        static void UniversalKeyToIndex(const Internal::Blob& blob,
                                        const MemberDescriptionType* md,
                                        int memberIndex,
                                        std::map<DotsC_Int64, int>& keyToIndex)
        {
            //, InstanceId, HandlerId, ChannelId, EntityId.
            switch (md->GetKeyType())
            {
            case Int32MemberType:
            case EnumerationMemberType:
            {
                for (int i=0; i<blob.NumberOfValues(memberIndex); ++i)
                {
                     keyToIndex.insert(std::make_pair(TypeUtilities::ToUnifiedDictionaryKey(blob.GetKeyInt32(memberIndex, i)), i));
                }
            }
                break;

            case Int64MemberType:
            case TypeIdMemberType:
            {
                for (int i=0; i<blob.NumberOfValues(memberIndex); ++i)
                {
                     keyToIndex.insert(std::make_pair(TypeUtilities::ToUnifiedDictionaryKey(blob.GetKeyInt64(memberIndex, i)), i));
                }
            }
                break;

            case StringMemberType:
            {
                for (int i=0; i<blob.NumberOfValues(memberIndex); ++i)
                {
                     keyToIndex.insert(std::make_pair(TypeUtilities::ToUnifiedDictionaryKey(blob.GetKeyString(memberIndex, i)), i));
                }
            }
                break;

            case InstanceIdMemberType:
            case HandlerIdMemberType:
            case ChannelIdMemberType:
            {
                for (int i=0; i<blob.NumberOfValues(memberIndex); ++i)
                {
                     keyToIndex.insert(std::make_pair(TypeUtilities::ToUnifiedDictionaryKey(blob.GetKeyHash(memberIndex, i)), i));
                }
            }
                break;

            case EntityIdMemberType:
            {
                for (int i=0; i<blob.NumberOfValues(memberIndex); ++i)
                {
                    DotsC_EntityId eid={blob.GetKeyInt64(memberIndex, i), blob.GetKeyHash(memberIndex, i)};
                    keyToIndex.insert(std::make_pair(TypeUtilities::ToUnifiedDictionaryKey(eid), i));
                }
            }
                break;

            default:
                break;
            }

        }
    };
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport

#endif
