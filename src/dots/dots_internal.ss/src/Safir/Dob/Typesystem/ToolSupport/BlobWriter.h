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

        BlobWriter(const RepositoryT* rep, DotsC_TypeId typeId)
            :m_repository(rep)
            ,m_classDescription(m_repository->GetClass(typeId))
            ,m_memberInfo()
            ,m_header(0, typeId)
            ,m_blob()
        {
        }

        DotsC_Int32 CalculateBlobSize() const {return m_blob.CalculateBlobSize();}
        void CopyRawBlob(char* destBlob) const {m_blob.Serialize(destBlob);}


        template <class Key, class Val>
        void Write(DotsC_MemberIndex member, const Key& key, const Val& val, bool isNull, bool hasChanged)
        {
            MoveToMember(member);

//            switch (m_memberDescription->GetCollectionType())
//            {
//            case NoCollectionType:
//                WriteSingleValue(val, isNull, hasChanged);
//                break;
//            case RangeCollectionType:
//                WriteRangeValue(val, isNull, hasChanged);
//                break;
//            case SetCollectionType:
//                WriteSetValue(val, isNull, hasChanged);
//                break;
//            case HashtableCollectionType:
//                WriteHashtableValue(val, key, isNull, hasChanged);
//                break;
//            }
        }



    private:

        struct MemberInfo
        {
            const MemberDescriptionType* memberDescription;
            DotsC_MemberIndex memberIndex; //the memnber we are currently writing
            MemberInfo() : m_memberDescription(NULL), m_memberIndex(-1), m_blobOffset(0) {}
        };

        const RepositoryType* m_repository;
        const ClassDescriptionType* m_classDescription;
        MemberInfo m_memberInfo;        
        Safir::Dob::Typesystem::ToolSupport::Internal::Blob m_blob;

        void MoveToMember(DotsC_MemberIndex member)
        {
            if (m_memberInfo.memberIndex!=member)
            {
                if (!m_blob.MoveToMember(member))
                {
                    m_blob.AddMember(member);
                }
            }

            m_memberInfo.memberDescription=m_classDescription->GetMember(member);
            m_memberInfo.memberIndex=member;
        }

        template <class Val>
        void WriteArrayElement(DotsC_MemberIndex member, DotsC_ArrayIndex index, const Val& val, bool isNull, bool hasChanged)
        {
            if (member!=m_currentMember)
            {
                StartNewMember(member);
            }

            if (m_memberDescription->GetCollectionType()==NoCollectionType)
            {
                //write singel value
            }
        }
    };
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport

#endif
