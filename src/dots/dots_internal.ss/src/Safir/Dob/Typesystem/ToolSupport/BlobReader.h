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
     */
    template <class RepositoryT, class Traits=Safir::Dob::Typesystem::ToolSupport::TypeRepositoryTraits<RepositoryT> >
    class BlobReader : private boost::noncopyable
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
         * @return Size of the blob.
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
            ,m_classDescription(rep->GetClassDescription(m_blob.TypeId()))
            ,m_memberDescription(NULL)
            ,m_memberIndex(-1)
        {
        }

        /**
         * Check if the member is changed at top level. For simple values this is the same as the change flag of the value,
         * but for collections and objects changed at top level indicates that the collection or object has changed in some way.
         * @param member
         * @return
         */
        bool IsChangedTopLevel(DotsC_MemberIndex member) const {return m_blob.IsChangedTopLevel(member);}

        /**
         * @brief Get the number of values for the member. Only collections may contain more than one value.
         * @param member [in] - The member.
         * @return Number of values.
         */
        int NumberOfValues(DotsC_MemberIndex member) const {return m_blob.NumberOfValues(member);}



    private:
        const RepositoryType* m_repository;
        Safir::Dob::Typesystem::ToolSupport::Internal::Blob m_blob;
        const ClassDescriptionType* m_classDescription;
        const MemberDescriptionType* m_memberDescription;
        DotsC_MemberIndex m_memberIndex;

        inline void MoveToMember(DotsC_MemberIndex member)
        {
            if (m_memberIndex!=member)
            {
                m_memberDescription=m_classDescription->GetMember(member);
                m_memberIndex=member;
            }
        }
    };
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport

#endif
